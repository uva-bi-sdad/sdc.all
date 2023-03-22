import numpy as np
import os
import pandas as pd
from tqdm import tqdm
import argparse

# import traceback
import logging
import pathlib


def main(input_dir, output_filepath):
    dfs = []
    for file in tqdm(os.listdir(input_dir)):
        suffix = pathlib.Path(file).suffix
        # print(suffix)
        if suffix == ".csv":
            df = pd.read_csv(os.path.join(input_dir, file))
            dfs.append(df)

    tdf = pd.concat(dfs)
    tdf.to_csv(output_filepath, index=False)
    print(tdf)
    print("[%s] Saved to: %s" % (os.path.isfile(output_filepath), output_filepath))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Given a directory of csvs that can be concatenated and an output filename, create a combined csv"
    )
    parser.add_argument(
        "-i",
        "--input_dir",
        type=str,
        help="The input csv",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="The output filename",
        required=True,
    )
    parser.add_argument("-v", "--verbose", action=argparse.BooleanOptionalAction)
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

    assert os.path.isdir(args.input_dir)
    if not args.force:
        assert not os.path.isfile(args.output_file)

    main(args.input_dir, args.output_file)
