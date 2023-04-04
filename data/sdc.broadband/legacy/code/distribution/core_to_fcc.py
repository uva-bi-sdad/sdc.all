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
import psycopg2
from decouple import config

# import traceback


def download_data(county_fips, output_file):
    # decouple so that passwords are not stored
    conn = psycopg2.connect(
        pg_connection_dict={
            "dbname": config("dbname"),
            "user": config("user"),
            "password": config("password"),
            "port": config("port"),
            "host": config("host"),
        }
    )
    cur = conn.cursor()
    cur.execute(
        "\copy (SELECT situs_address,geoid_cnty FROM %s WHERE geoid_cnty = '%s') TO '~/%s.csv' CSV header;"
        % (config("dbname"), county_fips, county_fips)
    )
    sql = "COPY (SELECT * FROM a_table WHERE month=6) TO STDOUT WITH CSV DELIMITER ';'"
    with open("/mnt/results/month/table.csv", "w") as file:
        cur.copy_expert(sql, file)
    cur.close()
    conn.close()


def main(county_fip, output_file, force):
    warnings.filterwarnings("ignore")
    state_fips = pd.read_csv(
        "https://raw.githubusercontent.com/uva-bi-sdad/national_address_database/main/data/fips_state.csv",
        dtype={"fips": object},
    )
    state = state_fips[state_fips["fips"] == county_fip[:2]]["state"].values[0]
    state_abbr = (
        state_fips[state_fips["fips"] == county_fip[:2]]["abbr"].values[0].upper()
    )
    county_fips = pd.read_csv(
        "https://github.com/uva-bi-sdad/national_address_database/raw/main/data/fips_county.csv.xz",
        dtype={"fips": object},
    )
    county = county_fips[county_fips["fips"] == county_fip].values[0]

    df = download_data(county_fip)
    df = df.dropna()
    bdf = pd.DataFrame()
    # # load csv's where the fips is a string

    bdf["street"] = df["situs_address"].apply(lambda x: x.split(" %s" % state_abbr)[0])
    bdf["county"] = county
    bdf["state"] = state_abbr
    bdf["zip"] = df["situs_address"].apply(lambda x: x.split("%s " % state_abbr)[-1])

    bdf.to_csv(output_file, index=False)
    return os.path.isfile(bdf)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given a corelogic template, convert to a csv that can be queried to the fcc"
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="The input csv",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="The output csv",
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
        help="Whether or not to override the output file, if it already exists",
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

    success = main(args.input_file, args.output_file, args.force)
    print("[%s] Output to %s successful" % (success, args.output_file))
