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


def evaluate_folder(max_file_length, dirpath):
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
                is_valid = (max_file_length - len(full_path)) >= 0
                if is_valid:
                    report += "\t<p>[VALID] %s</p>\n" % (full_path)
                else:
                    report += "\t<p>[TOO LONG] %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":
    test = Test(
        __file__,
        "File name lenght check",
        "Checks whether or not file names are less than a prescribed 100 character limit for accessibility on windows platforms",
    )

    report = evaluate_folder(100, "./data")
    test.export_html(report)
