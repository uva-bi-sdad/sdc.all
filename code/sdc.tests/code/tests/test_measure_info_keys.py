# tests for correctness of measure info keys per variable

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


def evaluate_folder(req_keys, source_req_keys, dirpath):
    report = ""

    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
    
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        # CHECK EACH DATA/DISTRIBUTION FILE ------------------------
        
        for path in sorted(Path(subdir).rglob("distribution/**/*")):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue
                
            full_path = path.name
            
            if full_path not in ["measure_info.json"]:
                # if file is not a measure info file, skip to next
                continue

            # execute these statements for measure info files
                
            try:    
                parent_dir = str(path.parent).split('sdc.').pop()
                parent_dir = re.search('/(.*?)/data/distribution', parent_dir).group(1)
            except:
                parent_dir = "PARENT_DIRECTORY"
            
            try:
                f = open(path.resolve())
                mi = json.load(f)
                f.close()
                
                # get measure info keys for each variable

                for var in mi.keys():
                
                    # skip references entries
                    if var == "_references":
                        continue
                
                    key_list = list(mi[var].keys())

                    # check for missing and extra keys
                    missing_keys = list(req_keys.difference(set(key_list)))
                    extra_keys = list(set(key_list).difference(req_keys))
                    
                    # check keys in "sources"                    
                    missing_source_keys = []
                    extra_source_keys = []
                    if "sources" in key_list:
                        for i in range(len(mi[var]['sources'])):
                            source_key_list = list(mi[var]["sources"][i].keys())
                        
                            missing_source_keys = list(source_req_keys.difference(set(source_key_list)))
                            extra_source_keys = list(set(source_key_list).difference(source_req_keys))
                                            
                    if len(missing_keys) > 0:
                        report += "\t<p><font color='#D55E00'> [MISSING KEYS] </font> %s %s: %s </p>\n" % (missing_keys, parent_dir, var)
                    if len(extra_keys) > 0:
                        report += "\t<p><font color='#D55E00'> [EXTRA KEYS] </font> %s %s: %s </p>\n" % (extra_keys, parent_dir, var)
                    if len(missing_source_keys) > 0:
                        report += "\t<p><font color='#D55E00'> [MISSING SOURCE KEYS] </font> %s %s: %s </p>\n" % (missing_source_keys, parent_dir, var)
                    if len(extra_source_keys) > 0:
                        report += "\t<p><font color='#D55E00'> [EXTRA SOURCE KEYS] </font> %s %s: %s </p>\n" % (extra_source_keys, parent_dir, var)
                    if (len(missing_keys) + len(extra_keys) + len(missing_source_keys) + len(extra_source_keys) == 0):
                        report += "\t<p><font color='#009E73'> [VALID] </font> %s: %s </p>\n" % (parent_dir, var)            
            
            except:
                print(traceback.format_exc())
                report += "\t<p><font color='#D55E00'> [ERROR - CANNOT READ JSON] </font> %s: %s</p>\n" % (parent_dir, full_path)
                
    return report


if __name__ == "__main__":
    
    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/measure_structure.json"
    ) as url:
        req_keys = set(json.load(url))
        
    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/data/source_structure.json"
    ) as url:
        source_req_keys = set(json.load(url))
    
    test = Test(
        __file__,
        "Measure Info Key Test",
        "Checks whether measure info files have valid keys for each variable",
    )
    
    report = evaluate_folder(req_keys, source_req_keys, "./data")
    test.export_html(report)
