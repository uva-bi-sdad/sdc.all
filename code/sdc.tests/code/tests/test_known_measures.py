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
from test import Test


def evaluate_folder(known_measure_set, dirpath):
    report = ""

    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        for path in Path(subdir).rglob("distribution/**/*"):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent

            if path.suffix in [".xz", ".csv"]:
                full_path = path.resolve()
                try:
                    df = pd.read_csv(full_path, low_memory=False)
                    if not "measure" in df.columns:
                        report += "\t<p>[NO MEASURE COL] %s</p>\n" % (full_path)
                        continue

                    measures = set(df["measure"])
                    is_valid = len(measures - known_measure_set) == 0

                    if is_valid:
                        report += "\t<p>[VALID] %s</p>\n" % (full_path)
                    else:
                        report += (
                            "\t<p>[INVALID] Count: %s, Values: %s, Path: %s </p>\n"
                            % (
                                len(measures - known_measure_set),
                                measures - known_measure_set,
                                full_path,
                            )
                        )
                except:
                    print(traceback.format_exc())
                    report += "\t<p>[ERROR] %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":

    # example stand in; could potentially change location
    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/measure_info_all.json"
    ) as url:
        measure_info = json.load(url)

    known_measure_set = set(measure_info.keys())

    print("Length of known measures set: %s" % len(known_measure_set))
    test = Test(
        __file__,
        "Known measures",
        "Checks whether measures in files matchces a set of known measures",
    )

    report = evaluate_folder(known_measure_set, "./data")
    test.export_html(report)
