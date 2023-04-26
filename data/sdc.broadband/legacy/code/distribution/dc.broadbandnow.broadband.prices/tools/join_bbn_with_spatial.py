import requests
import numpy as np
import os
import pandas as pd
from tqdm import tqdm
import argparse
import pandas as pd
import requests
from tqdm import tqdm
import math
from io import StringIO
import warnings
import logging
import pathlib
import shutil
import geopandas as gpd
from glob import glob
from shapely import wkt
from matplotlib import pyplot as plt
import contextily as cx


def main(
    input_file,
    input_sj_file,
    output_file,
    county_fip,
):
    warnings.filterwarnings("ignore")
    df = pd.read_csv(input_file)
    df["speed"] = df["speed"].apply(lambda x: int(x.split(" ")[0]))
    df = df[df["speed"] > 100]  # we limit to speeds of above 100 Mb
    dfs = []
    for address in tqdm(df["address"].unique()):
        pdf = df[df["address"] == address]
        dfs.append(pdf.nsmallest(1, "speed"))

    tdf = pd.concat(dfs)
    gdf = pd.read_csv(input_sj_file)
    mdf = pd.merge(tdf, gdf, on="address", how="inner")
    mdf.to_csv(output_file, index=False)
    logging.info(
        "[%s] File export successful: %s" % (os.path.isfile(output_file), output_file)
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Create a sptially joined data frame the final output broadbandnow data with the geometry file in the pipeline"
    )

    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="The input csv with broadbandnow results",
        required=True,
    )
    parser.add_argument(
        "-s",
        "--input_sj_file",
        type=str,
        help="The spatial joined csv file prior to querying bbn",
        required=True,
    )
    parser.add_argument(
        "-c",
        "--county_fip",
        type=str,
        help="The fip for the county to visualize",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="The output csv where matches are found",
        required=True,
    )
    parser.add_argument(
        "-v",
        "--verbose",
        help="Show debugging outputs",
        action=argparse.BooleanOptionalAction,
    )
    parser.add_argument(
        "-f",
        "--force",
        action=argparse.BooleanOptionalAction,
        help="Whether or not to override the output file",
        required=False,
        default=False,
    )

    args = parser.parse_args()
    log_level = logging.INFO
    if args.verbose:
        log_level = logging.DEBUG

    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)

    assert os.path.isfile(args.input_file), "input file is invalid"
    test_df = pd.read_csv(args.input_file)
    assert "address" in test_df.columns

    if args.output_file and not args.force:
        assert not os.path.isfile(args.output_file), "output file is invalid"

    main(
        args.input_file,
        args.input_sj_file,
        args.output_file,
        args.county_fip,
    )
