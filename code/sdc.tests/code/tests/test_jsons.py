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


def evaluate_folder(dirpath):
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

            if path.suffix in [".json"]:
                full_path = path.resolve()
                try:
                    j = json.load(open(path.resolve()))
                    report += "\t<p><font color='#009E73'> [VALID] </font> %s</p>\n" % (full_path)
                except:
                    print(traceback.format_exc())
                    report += "\t<p><font color='#D55E00'> [FAIL] </font> %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":
    report = evaluate_folder("./data")
    test = Test(
        __file__,
        "Json test",
        "Checks whether encountered jsons are valid jsons that can be read",
    )
    test.export_html(report)
