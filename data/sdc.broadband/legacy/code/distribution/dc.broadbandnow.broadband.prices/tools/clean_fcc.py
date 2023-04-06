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
from glob import glob


def s2p(s):
    # converts a string to possibly coordinates
    try:
        return (s.split(",")[0], s.split(",")[1])
    except Exception as e:
        return None


def main(input_file, output_file):
    warnings.filterwarnings("ignore")
    df = pd.read_csv(input_file)
    df["coordinate"] = df["coordinate"].apply(lambda x: s2p(x))
    df = df[df["coordinate"].notna()]  # remove all empty ones without coordinates
    df["lon"] = df["coordinate"].apply(lambda x: x[0])
    df["lat"] = df["coordinate"].apply(lambda x: x[1])
    df = df[["street", "lon", "lat"]]
    df.to_csv(output_file, index=False)
    logging.info("[%s] File saved to: %s" % (os.path.isfile(output_file), output_file))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given the output of the fcc area, clean the columns and convert the coordinate scheme into lat lon in preparation for spatial joining. The cleaned csv would only contain 3 columns: (street, lon, lat)"
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="The input csv filepath",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="The output filepath",
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
        help="Whether or not to override the output directory files",
        required=False,
        default=False,
    )

    args = parser.parse_args()
    log_level = logging.INFO
    if args.verbose:
        log_level = logging.DEBUG

    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)

    assert os.path.isfile(args.input_file)
    if not args.force:
        assert not os.path.isfile(args.output_file)

    main(args.input_file, args.output_file)
