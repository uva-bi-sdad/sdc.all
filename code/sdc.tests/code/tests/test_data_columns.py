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
import re


def evaluate_folder(req_cols, dirpath):
    report = ""

    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
    
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        if dir == "sdc.geographies":
            report += "NA"
            continue
        
        # CHECK EACH DATA/DISTRIBUTION FOLDER ------------------------
        
        for path in sorted(Path(subdir).rglob("data/distribution")):
            
            logging.debug("\tEvaluating: %s" % path.name)

            # get path info
            
            try:    
                parent_dir = str(path).split('sdc.').pop()
                parent_dir = re.search('/(.*?)/data/distribution', parent_dir).group(1)
            except:
                parent_dir = "PARENT_DIRECTORY"

            # LOOP THROUGH EACH DATA FILE IN FOLDER -----------------
            
            for f in path.iterdir():
            
                if not os.path.isfile(f):
                    # if path is not a file, skip to the next file to check
                    continue
            
                if f.suffix in [".xz", ".csv"]:
                
                    try:
                        df = pd.read_csv(f.resolve())
                        cols = set(df.columns)
                        missing_cols = list(req_cols.difference(cols))
                        extra_cols = list(cols.difference(req_cols))

                        if len(missing_cols) > 0:
                            report += "\t<p><font color='#D55E00'> [MISSING COLUMNS] </font> %s %s: %s </p>\n" % (missing_cols, parent_dir, f.name)
                        if len(extra_cols) > 0:
                            report += "\t<p><font color='#0072B2'> [EXTRA COLUMNS] </font> %s %s: %s </p>\n" % (extra_cols, parent_dir, f.name)
                        
                        if (len(missing_cols) + len(extra_cols) == 0):
                            report += "\t<p><font color='#009E73'> [VALID] </font> %s: %s</p>\n" % (parent_dir, f.name)
   
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR] </font> %s: %s</p>\n" % (parent_dir, f.name)
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
