# Test for adhering to file name conventions

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
import re


def evaluate_folder(dirpath):
    report = ""

    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
   
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        # CHECK EACH DATA/DISTRIBUTION FILE ------------------------
        
        for path in Path(subdir).rglob("data/distribution/**/*"):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue
                
            parent_dir = str(path.parent).split('sdc.').pop()
            parent_dir = re.search('/(.*?)/data/distribution', parent_dir).group(1)
            full_path = path.name  
                
            # check measure_info files
                
            if path.suffix in [".json"]: 
                
                if full_path not in ["measure_info.json"]:
                    report += "\t<p><font color='red'> [ERROR: MEASURE INFO FILE NAME] </font> %s: %s</p>\n" % (parent_dir, full_path)
                else: 
                    report += "\t<p><font color='green'> [VALID] </font> %s: %s</p>\n" % (parent_dir, full_path)
                    
            # check data files        
            
            elif path.suffix in [".xz", ".csv"]:

                if not (full_path.startswith('ncr') or full_path.startswith('va')):
                    report += "\t<p><font color='red'> [ERROR: DATA FILE NAME] </font> %s: %s</p>\n" % (parent_dir, full_path)
                else: 
                    report += "\t<p><font color='green'> [VALID] </font> %s: %s</p>\n" % (parent_dir, full_path)
                  
            else:  
                report += "\t<p><font color='red'> [ERROR: DATA FILE NAME] </font> %s: %s</p>\n" % (parent_dir, full_path)
                    
                    
    return report


if __name__ == "__main__":
    test = Test(
        __file__,
        "File Name Test",
        "Checks file name conventions",
    )
    report = evaluate_folder("./data")
    test.export_html(report)
