import os
import logging
from pathlib import Path
from enum import Enum
import pandas as pd
from datetime import datetime
import json
import urllib.request
from string import Template
import traceback
from pytz import timezone
from tqdm import tqdm
from tests.test import Test


def evaluate_folder(column_name, dirpath):
    values = []

    for dir in tqdm(os.listdir(dirpath)):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        for path in tqdm(list(Path(subdir).rglob("distribution/**/*"))):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent

            if path.suffix in [".xz", ".csv"]:
                full_path = path.resolve()
                try:
                    df = pd.read_csv(full_path, low_memory=False)
                    if column_name in df.columns:
                        values.extend(list(df[column_name].unique()))
                except:
                    print(traceback.format_exc())
    return values


if __name__ == "__main__":

    test = Test(
        __file__,
        "Measure info structure",
        "Checks whether measures info files have and only have a prescribed list of allowable keys, and the keys themselves only contain allowable data types",
    )

    column_name = input("input column name to list set of values: ")

    # values = evaluate_folder("measure_type", "./data")
    values = evaluate_folder(column_name, "./data")
    print(set(values))
