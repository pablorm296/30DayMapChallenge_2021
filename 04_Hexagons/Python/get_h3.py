import h3
import geopandas as geopd
from shapely.geometry import Polygon
import json
import argparse
import logging
import os

logging.basicConfig(level = logging.INFO, format = "%(levelname)s: %(message)s")

# Function to load Mexico shapefile
def read_mex_shapefile(path:str = "../../Common/MarcoGeoestadístico/01_Entidades.shp"):
    new_geo_df = geopd.read_file(path)
    return new_geo_df

# Function to subset a GeoDataFrame by state name
def filter_by_state(geo_df: geopd.GeoDataFrame, state_name: str, name_var: str = "NOMGEO"):
    geo_df = geo_df[geo_df[name_var] == state_name]
    return geo_df

# Function to reproject a GeoDataFrame
def reproject(geo_df: geopd.GeoDataFrame, epsg:int = 4326):
    geo_df = geo_df.to_crs(epsg = epsg)
    return geo_df

# Function to get a dict-formatted GeoJSON from a GeoDataFrame
def get_geojson(geo_df: geopd.GeoDataFrame):
    geo_json = geo_df.to_json()
    geo_json_as_dict = json.loads(geo_json)
    return geo_json_as_dict

# Function to get the features from a dict-formatted GeoJSON é
def get_features(geo_json:dict):
    features = geo_json["features"][0]["geometry"]
    return features

# Function to polyfill h3 hexagons on a GeoJSON at a given resolution 
# This function returns the indexes of the hexagons located at the area covered by the GeoJSON
def get_h3_from_polygon(geojson_polygon: dict, resolution:int):
    h3_set = h3.polyfill_geojson(geojson_polygon, resolution)
    h3_list = list(h3_set)
    return h3_list

# Function to generate a GeoDataFrame from a list of H3 hexagons
# Each hexagons will contain it's unique id and it's polygon
def h3_to_geodataframe(list_of_h3:list):
    # Define a data container
    data = {"hex_id": [], "geometry": []}

    for hex_id in list_of_h3:
        # Add hex id
        data["hex_id"].append(hex_id)

        # Get hex boundaries
        hex_boundaires = h3.h3_to_geo_boundary(hex_id, geo_json = True)

        # Boundaries as polygon
        hex_polygon = Polygon(hex_boundaires)

        # Add polygon
        data["geometry"].append(hex_polygon)

    # Data as geodataframe
    new_geodf = geopd.GeoDataFrame(data, geometry = "geometry", crs = "EPSG:4326")

    # Return data
    return new_geodf

# Function that sets up argument parsing and reads arguments from the console
def parse_arguments() -> dict:
    # Init parser
    main_parser = argparse.ArgumentParser(description = "Generates GeoJSON files containing the H3 hex grid that covers the specified state in Mexico.")

    # Init arguments
    main_parser.add_argument('state', type = str, help = "Name of the state.")
    main_parser.add_argument("--resolutions", type = int, nargs = "*", required = True, help = "H3 resolution (can be multiple resolutions).")
    main_parser.add_argument("--base", type = str, required = True, help = "Path to the base shapefile (must be a shapefile containing states as polygons).")
    main_parser.add_argument("--out", default = "Out/", type = str, help = "Path to the output directory.")

    # Parse arguments
    parser_result = main_parser.parse_args()
    parser_result_as_dict = vars(parser_result)

    return parser_result_as_dict


def main():

    logging.info("Parsing arguments...")
    args = parse_arguments()

    # Check if output dir exists
    if not os.path.isdir(args["out"]):
        os.mkdir(args["out"])

    # Read base shapefile
    logging.info("Loading base shapefile...")
    base_geodf = read_mex_shapefile(path = args["base"])

    # Filter base geodf by state
    logging.info("Subseting base shapefile (state: {0})...".format(args["state"]))
    state_geodf = filter_by_state(base_geodf, args["state"])

    # Reproject geodf
    state_geodf = reproject(state_geodf)

    # Get GeoJSON features
    state_geoJSON = get_geojson(state_geodf)
    state_features = get_features(state_geoJSON)

    # For each resolution
    logging.info("Processing resolution levels...")
    for resolution_level in args["resolutions"]:
        
        logging.info(">> Res level: {0} ##########".format(resolution_level))

        # Get id
        state_name = args["state"]
        state_name = "_".join(state_name.split())
        id = "H3-{0}-{1}".format(state_name, resolution_level)

        # Get list of h3 hexagons
        logging.info(">> Getting H3 indexes...")
        h3_list = get_h3_from_polygon(state_features, resolution_level)

        # Get geodf from h3_list
        logging.info(">> Getting H3 polygons...")
        h3_geodf:geopd.GeoDataFrame = h3_to_geodataframe(h3_list)

        logging.info(">> Writing H3 polygons to file ({0})...".format("{0}{1}.geojson".format(args["out"], id)))
        h3_geodf.to_file("{0}{1}.geojson".format(args["out"], id), driver = "GeoJSON")

    logging.info("Done!")


if __name__ == "__main__":

    logging.info("Script is __main__...")
    main()