# tests for valid region_types in data files 

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


def evaluate_folder(dirpath, valid_types):
    report = ""

    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
    
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        # CHECK EACH DATA/DISTRIBUTION FOLDER ------------------------
        
        for path in sorted(Path(subdir).rglob("data/distribution")):
            #print(path)
    
            logging.debug("\tEvaluating: %s" % path.name)

            # get path info
            
            try:    
                parent_dir = str(path).split('sdc.').pop()
                parent_dir = re.search('/(.*?)/data/distribution', parent_dir).group(1)
            except:
                parent_dir = "PARENT_DIRECTORY"
        
            # LOOP THROUGH EACH DATA FILE IN FOLDER -----------------
            
            for f in path.iterdir(): #os.listdir(path):
            
                if not os.path.isfile(f):
                    # if path is not a file, skip to the next file to check
                    continue
                    
                #print(f.name)                  

                if f.suffix in [".xz", ".csv"]:
                    # file is a data file - extract region types
                
                    try:
                        df = pd.read_csv(f.resolve(), dtype = str)  
                        data_region_types = df["region_type"].unique()   
                         
                        nonvalid_types = list(set(data_region_types).difference(set(valid_types)))    
                            
                        if len(nonvalid_types) == 0:
                            report += "\t<p><font color='#009E73'> [VALID] </font> %s: %s </p>\n" % (parent_dir, f.name) 
                        else: 
                            report += "\t<p><font color='#D55E00'> [INVALID REGION TYPES] </font> %s %s: %s </p>\n" % (nonvalid_types, parent_dir, f.name)
                        
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR - NO REGION TYPE COLUMN] </font> %s: %s</p>\n" % (parent_dir, f.name)

    return report


if __name__ == "__main__":
    
    with urllib.request.urlopen(
        "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/src/data_repo_structure/region_types.json"
    ) as url:
        valid_types = json.load(url)

    
    test = Test(
        __file__,
        "Region Types Validity Test",
        "Checks whether region types in data files are valid",
    )
    
    report = evaluate_folder("./data", valid_types)
    test.export_html(report)
