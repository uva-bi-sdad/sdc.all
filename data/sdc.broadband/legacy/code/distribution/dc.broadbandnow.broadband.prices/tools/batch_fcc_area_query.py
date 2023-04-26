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
import os
import time

# import traceback


def main(input_fips, input_dir, force):
    for fip in tqdm(input_fips):
        fp = os.path.join(input_dir, "%s.csv.xz" % fip)

        if not os.path.isfile(fp):
            logging.info("File not found: %s" % fp)
            continue

        command = "python fcc_area_query.py -i %s -o temp_%s_fcc/" % (fp, fip)
        if force:
            command += " -f"
        os.system(command)
        logging.info("Waiting for 15 seconds")
        time.sleep(15)  # can try to skip if fcc already ran. But how?


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given input fips and an input directory, try to run all fcc area queries, assuming the name is <fip>.csv.xz"
    )
    parser.add_argument(
        "-i",
        "--input_county_fips",
        nargs="+",
        help="A list of county fip(s) to filter from the database",
        required=True,
    )
    parser.add_argument(
        "-d",
        "--input_dir",
        type=str,
        help="The input directory",
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
    main(args.input_county_fips, args.input_dir, args.force)
