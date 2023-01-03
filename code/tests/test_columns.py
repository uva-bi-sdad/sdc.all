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
    return report


if __name__ == "__main__":

    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/data_repo_structure/main/col_names.json"
    ) as url:
        req_cols = json.load(url)

    req_cols = set(req_cols)
    print(req_cols)

    report = evaluate_folder(req_cols, "./data")
    tz = timezone("EST")
    time_checked = datetime.now(tz).strftime("%Y-%m-%d %H:%M:%S")

    print(time_checked)

    t = Template(
        """
    <html>
        <head>
        <title>Column Test</title>
        </head>
        <body>
            Last updated: $time_checked
            $report
        </body>
    </html>
    """
    )

    print(t.substitute(time_checked=time_checked, report=report))
    with open("./docs/column_test.html", "w") as f:
        f.write(t.substitute(time_checked=time_checked, report=report))
