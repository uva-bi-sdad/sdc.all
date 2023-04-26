import os
import argparse
import logging
import pandas as pd
import datetime
import json
from tqdm import tqdm
import sys


def has_been_parsed(county_fip, output_dir):
    requirements = {
        "1) corelogic downloaded": os.path.join(output_dir, "%s.csv.xz" % county_fip),
        "2) fcc area api cross checked": os.path.join(
            output_dir, "%s_geocoded.csv.xz" % county_fip
        ),
        "3) census shape file downloaded": os.path.join(
            output_dir, "tl_2020_%s_tabblock20.zip" % county_fip
        ),
        "4) fcc geocoded file cleaned": os.path.join(
            output_dir, "%s_cleaned.csv.xz" % county_fip
        ),
        "5) fcc geocoded file spatial joined with census shape files": os.path.join(
            output_dir, "%s_spatial_joined.csv.xz" % county_fip
        ),
        "6) broadbandnow query completed": os.path.join(
            output_dir,
            "%s_%s_broadband_prices.csv.xz" % (county_fip, datetime.date.today().year),
        ),
        "7) broadbandnow output joined spatially": os.path.join(
            output_dir, "%s_bbn_space_joined.csv.xz" % county_fip
        ),
    }

    success = all([os.path.isfile(k) for k in requirements])

    for key in requirements:
        requirements[key] = (os.path.isfile(requirements[key]), requirements[key])

    return success, requirements


def main(county_fip, output_dir, pba, test):
    # Try making some temporary directories to store information

    pbar.set_description("Checking county fip: %s" % county_fip)

    success, req = has_been_parsed(county_fip, output_dir)
    if success:
        pbar.set_description("[%s] has already been parsed" % county_fip)
        # print("[X] %s" % json.dumps(req, sort_keys=True, indent=4))
        print(all(req[key] for key in req))
        for key in req:
            print("\t[%s]: %s" % (req[key], key))
        return
    else:
        print(all(req[key] for key in req))
        for key in req:
            print("\t[%s]: %s" % (req[key], key))

    if test:
        return
    os.system("mkdir -p %s" % output_dir)
    os.system("mkdir -p temp_%s_fcc" % county_fip)
    fcc_dir = "temp_%s_fcc" % county_fip
    pbar.set_description("Cross matching with fcc area api")
    pbar.set_description("Combining the csv")
    fcc_geocoded_filepath = os.path.join(output_dir, "%s_geocoded.csv.xz" % county_fip)
    os.system("python combine_csv.py -i %s -o %s -f" % (fcc_dir, fcc_geocoded_filepath))
    if not os.path.isfile(fcc_geocoded_filepath):
        logging.info("No combined geocoded file found")
        return
    else:
        logging.info("[%s] has not yet been parsed")

    # Download county shapefiles
    pbar.set_description("Downloading shape geometry")
    os.system(
        "wget -nc -P %s https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/TABBLOCK/2020/tl_2020_%s_tabblock20.zip"
        % (output_dir, county_fip)
    )

    if not os.path.isfile(
        os.path.join(output_dir, "tl_2020_%s_tabblock20.zip" % county_fip)
    ):
        logging.info("Shape files not downloaded")
        return

    geocoded_filepath = os.path.join(output_dir, "%s_geocoded.csv.xz" % county_fip)
    if not os.path.isfile(geocoded_filepath):
        logging.info("No geocoded file found: %s" % geocoded_filepath)
        return

    logging.info("Found combined geocoded file: %s" % geocoded_filepath)

    cleaned_fcc_filpath = os.path.join(output_dir, "%s_cleaned.csv.xz" % county_fip)

    # Clean downloaded fcc data
    pbar.set_description("Cleaning downloaded fcc data")
    os.system(
        "python clean_fcc.py -i %s -o %s -f"
        % (
            geocoded_filepath,
            cleaned_fcc_filpath,
        )
    )
    if not os.path.isfile(cleaned_fcc_filpath):
        logging.info("Claned fcc filepath is not a file: %s" % cleaned_fcc_filpath)
        return

    spatial_joined_filepath = os.path.join(
        output_dir, "%s_spatial_joined.csv.xz" % county_fip
    )

    # Spatiall join the cleaned data with the shapefiles
    os.system(
        "python spatial_join.py -i %s -s %s -c %s -o %s -f"
        % (
            cleaned_fcc_filpath,
            os.path.join(output_dir, "tl_2020_%s_tabblock20.zip" % county_fip),
            "%s" % county_fip,
            spatial_joined_filepath,
        )
    )

    if not os.path.isfile(spatial_joined_filepath):
        logging.info(
            "Spatial joined filepath is not a file: %s" % spatial_joined_filepath
        )
        return

    # Query broadbandnow
    pbar.set_description("Prepping for broadbandnow download")
    temp_bbn_dir = "temp_%s_bbn/" % (county_fip)
    os.system("mkdir -p %s" % temp_bbn_dir)
    os.system(
        "python bbn_scraper.py -i %s -c street -o %s -l -c address"
        % (
            spatial_joined_filepath,
            temp_bbn_dir,
        )
    )

    if not os.path.isdir(temp_bbn_dir):
        logging.info("Temporary bbn folder is a folder: %s" % temp_bbn_dir)
        return

    pbar.set_description("Combining csv of bbn prices")
    os.system(
        "python combine_csv.py -i %s -o %s -f"
        % (
            temp_bbn_dir,
            os.path.join(
                output_dir,
                "%s_%s_broadband_prices.csv.xz"
                % (county_fip, datetime.date.today().year),
            ),
        )
    )
    bbn_file = os.path.join(
        output_dir,
        "%s_%s_broadband_prices.csv.xz" % (county_fip, datetime.date.today().year),
    )
    if not os.path.isfile(bbn_file):
        logging.info("File is not found: %s" % bbn_file)
        return

    pbar.set_description("Spatial joining broadbandnow data")

    os.system(
        "python join_bbn_with_spatial.py -i %s -s %s -o %s -c %s"
        % (
            bbn_file,
            os.path.join(output_dir, "%s_spatial_joined.csv.xz" % county_fip),
            os.path.join(output_dir, "%s_bbn_space_joined.csv.xz" % county_fip),
            "%s" % county_fip,
        )
    )

    joined_bbn_file = os.path.join(
        output_dir, "%s_bbn_space_joined.csv.xz" % county_fip
    )
    if not os.path.isfile(joined_bbn_file):
        logging.info("Joined bbn file not found: %s" % joined_bbn_file)
        return


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given csv(s) with address information, return a cleaned broadbandnow csv with geometry that can be visualized"
    )
    parser.add_argument(
        "-i",
        "--input_county_fips",
        nargs="+",
        help="A list of county fip(s) to filter from the database",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_dir",
        type=str,
        help="The output directory where the processed artifacts are dumped",
        required=True,
    )
    parser.add_argument(
        "-v",
        "--verbose",
        help="Show debugging outputs",
        action=argparse.BooleanOptionalAction,
    )
    parser.add_argument(
        "-t",
        "--test",
        help="Do not actually run the functions",
        action=argparse.BooleanOptionalAction,
    )

    args = parser.parse_args()
    log_level = logging.INFO
    if args.verbose:
        log_level = logging.DEBUG

    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)

    for fip in args.input_county_fips:
        assert len(fip) == 5, "[%s] not 5 characters long" % (fip)
    assert os.path.isdir(args.output_dir), "output dir is invalid: %s" % args.output_dir

    pbar = tqdm(args.input_county_fips)
    for fip in pbar:
        pbar.set_description("Parsing: %s" % fip)
        main(
            fip,
            args.output_dir,
            pbar,
            args.test,
        )
