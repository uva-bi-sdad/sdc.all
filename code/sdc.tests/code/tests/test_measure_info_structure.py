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


def evaluate_folder(measure_keys, source_keys, dirpath):
    report = ""

    for dir in sorted(os.listdir(dirpath)):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        for path in Path(subdir).rglob("distribution/**/*"):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            if os.path.basename(path.resolve()) == "measure_info.json":
                full_path = path.resolve()
                try:
                    # print(full_path)
                    with open(full_path, "r") as f:
                        j = json.load(f)

                    invalid_measures = []
                    for key in j.keys():
                        measure_valid = (
                            len(measure_keys - j[key].keys()) == 0
                        )  # it's ok to have more, just not less
                        if not measure_valid:
                            invalid_measures.append((key, measure_keys - j[key].keys()))

                    invalid_sources = []
                    if "sources" in j.keys():
                        for key in j["sources"].keys():
                            if len(source_keys - set(j["sources"][key].keys())) > 0:
                                invalid_sources.append(
                                    (key, source_keys - set(j["sources"][key].keys()))
                                )

                        sources_valid = (
                            True  # if you reached the end without encountering errors
                        )

                    measure_valid = len(invalid_measures) <= 0
                    sources_valid = len(invalid_sources) <= 0

                    if measure_valid and sources_valid:
                        report += "\t<p><font color='#009E73'> [VALID] </font> %s</p>\n" % (full_path)
                    else:
                        report += (
                            "\t<p><font color='#D55E00'> [INVALID] </font> Invalid measures: %s, Invalid sources: %s, Path: %s </p>\n"
                            % (
                                invalid_measures,
                                invalid_sources,
                                full_path,
                            )
                        )
                except:
                    print(traceback.format_exc())
                    report += "\t<p><font color='#D55E00'> [ERROR] </font> %s</p>\n" % (full_path)
    return report


if __name__ == "__main__":

    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/measure_structure.json"
    ) as url:
        measure_structure = set(json.load(url))

    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/source_structure.json"
    ) as url:
        source_structure = set(json.load(url))

    test = Test(
        __file__,
        "Measure info structure",
        "Checks whether measures info files have and only have a prescribed list of allowable keys",
    )

    report = evaluate_folder(measure_structure, source_structure, "./data")
    test.export_html(report)
