# Test for distribution code existence when distribution data exists

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
        
        # create list of data/distribution directories
        
        data_dirs = set()
        
        for path in Path(subdir).rglob("data/distribution/**/*"):
            
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue
            
            if path.suffix in [".xz", ".csv", ".geojson"]:
                data_dirs.add(str(path.parent))
        
        
        # create list of code/distribution directories        
            
        code_dirs = set()
        
        for path in Path(subdir).rglob("code/distribution/**/*"):
            
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue
            
            if path.suffix in [".R", ".Rmd", ".py", ".ipynb"]:
                code_dirs.add(str(path.parent))
    
        
        # Find data/distribution files that do not have corresponding code/distribution files
        
        code_dirs = list(code_dirs)
        code_dirs = [d.replace("code/distribution", "data/distribution") for d in code_dirs]
        code_dirs = set(code_dirs)
        
        good_dirs = sorted(list(data_dirs.intersection(code_dirs)))
        bad_dirs = sorted(list(data_dirs.difference(code_dirs)))
        
        # report results
        
        if len(good_dirs) > 0:
            report += "\t<p><font color='#009E73'> [DISTRIBUTION DATA AND CODE] </font></p>\n" 
            for d in good_dirs:
                report += "\t<p> %s </p>\n" % (d)
            
        if len(bad_dirs) > 0:    
            report += "\t<p><font color='#D55E00'> [DISTRIBUTION DATA AND NO CODE] </font></p>\n" 
            for d in bad_dirs:
                report += "\t<p> %s </p>\n" % (d)
          
                
    return report


if __name__ == "__main__":
    test = Test(
        __file__,
        "Code Existence Test",
        "Checks if distribution code exists when distribution data exists",
    )
    report = evaluate_folder("./data")
    test.export_html(report)
