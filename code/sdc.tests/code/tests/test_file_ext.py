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
from test import Test


def evaluate_folder(dirpath):
    report = ""

    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        for path in Path(subdir).rglob("data/distribution/**/*"):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent

            if path.suffix not in [".xz", ".csv"]:
                full_path = path.name

                if full_path not in ["measure_info.json"]:
                    report += "\t<p>[INCORRECT FILE EXTENSION] %s</p>\n" % (full_path)

                """
                try:
                    df = pd.read_csv(path.resolve())
                    cols = set(df.columns)
                    is_valid = len(cols.intersection(req_cols)) == len(
                        req_cols
                    ) and len(df.columns) == len(req_cols)

                    if is_valid:
                        report += "\t<p>[VALID] %s</p>\n" % (full_path)
                    elif len(cols.intersection(req_cols)) > 1:
                        report += "\t<p>[MISSING %s] Needs: %s, Has: %s, %s </p>\n" % (
                            len(req_cols - cols),
                            req_cols - cols,
                            cols,
                            full_path,
                        )
                    else:
                        report += "\t<p>[NO OVERLAP] %s</p>\n" % (full_path)
                except:
                    print(traceback.format_exc())
                    report += "\t<p>[ERROR] %s</p>\n" % (full_path)
               """
    return report


if __name__ == "__main__":
    test = Test(
        __file__,
        "Column Test",
        "Checks whether or not file (not a .xz or .csv) extensions are correct?",
    )
    report = evaluate_folder("./data")
    test.export_html(report)
