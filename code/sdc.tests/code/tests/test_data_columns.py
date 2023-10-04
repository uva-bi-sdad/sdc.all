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


def evaluate_folder(req_cols, dirpath):
    report = ""

    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        for path in sorted(Path(subdir).rglob("distribution/**/*")):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent

            if path.suffix in [".xz", ".csv"]:
                full_path = path.name
                try:
                    df = pd.read_csv(path.resolve())
                    cols = set(df.columns)
                    # is_valid = len(cols.intersection(req_cols)) == len(
                    #     req_cols
                    # ) and len(df.columns) == len(req_cols)
                    is_valid = len(req_cols - cols) == 0

                    if is_valid:
                        report += "\t<p><font color='#009E73'> [VALID] </font> %s</p>\n" % (full_path)
                    elif len(cols.intersection(req_cols)) > 1:
                        report += "\t<p><font color='#D55E00'> [MISSING %s] </font> Needs: %s, Has: %s, %s </p>\n" % (
                            len(req_cols - cols),
                            req_cols - cols,
                            cols,
                            full_path,
                        )
                    else:
                        report += "\t<p>[NO OVERLAP] %s</p>\n" % (full_path)
                except:
                    print(traceback.format_exc())
                    report += "\t<p><font color='#D55E00'> [ERROR] </font> %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":

    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/column_structure.json"
    ) as url:
        req_cols = json.load(url)

    req_cols = set(req_cols)
    print("Columns found: %s" % req_cols)

    test = Test(
        __file__,
        "Column Test",
        "Checks whether or not csvs have the predetermined column names for each csv",
    )

    report = evaluate_folder(req_cols, "./data")
    test.export_html(report)
