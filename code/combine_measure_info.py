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

"""
Iterate through each folder and consolidate existing measure infos to create a finalized list for use in the dc.metadata repository offline
"""


def combine_measure_info(dirpath):
    merged_json = {}
    fail = []
    for dir in tqdm(os.listdir(dirpath)):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue

        for path in sorted(Path(subdir).rglob("distribution/**/*")):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent

            if path.name == "measure_info.json":
                try:
                    j = json.load(open(path.resolve()))
                    merged_json |= j  # merge two dictionaries
                except:
                    print(traceback.format_exc())
                    fail.append(path.resolve())
    return merged_json, fail


if __name__ == "__main__":
    merged_json, fail = combine_measure_info(".")
    # print(merged_json)
    print("Merge completed. Total fail count: %s" % (len(fail)))
    print("-" * 80)
    for f in fail:
        print(f)
    print("-" * 80)
    with open("./data/temp/measure_info_all.json", "w") as f:
        json.dump(merged_json, f, sort_keys=True, indent=4)
