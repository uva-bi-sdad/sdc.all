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
                full_path = path.name
                try:
                    j = json.load(open(path.resolve()))
                    report += "\t<p>[VALID] %s</p>\n" % (full_path)
                except:
                    print(traceback.format_exc())
                    report += "\t<p>[FAIL] %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":
    report = evaluate_folder("./data")
    tz = timezone("EST")
    time_checked = datetime.now(tz).strftime("%Y-%m-%d %H:%M:%S")
    print(time_checked)

    t = Template(
        """
    <html>
        <head>
        <title>JSON Test</title>
        <p>
            This test looks for JSON files and attempts to open them. Files that can be looaded pass this test.
        </p>
        </head>
        <body>
            Last updated: $time_checked
            $report
        </body>
    </html>
    """
    )

    print(t.substitute(time_checked=time_checked, report=report))
    with open("./docs/test_json.html", "w") as f:
        f.write(t.substitute(time_checked=time_checked, report=report))
