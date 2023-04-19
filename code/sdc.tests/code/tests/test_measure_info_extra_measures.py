# tests that measures that appear in measure_info.json also appear in a data file in the same folder 


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


def evaluate_folder(dirpath):
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
            
            # LOOP THROUGH EACH FILE IN FOLDER -----------------
            
            data_measures = []
            mi_measures = []
            
            for f in path.iterdir(): #os.listdir(path):
            
                if not os.path.isfile(f):
                    # if path is not a file, skip to the next file to check
                    continue
                    
                #print(f.name)                  
            
                if f.suffix in [".xz", ".csv"]:
                    # file is a data file - extract measure names
                
                    try:
                        df = pd.read_csv(f.resolve(), dtype = str)  
                        data_measures.extend(df["measure"].unique())
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR - NO MEASURE COLUMN] </font> %s: %s</p>\n" % (parent_dir, f.name)

                elif f.name in ["measure_info.json"]: 
                    # file is a measure_info file - get measure names
                      
                    with open(f.resolve(), "r") as mi_f:
                        mi = json.load(mi_f)

                    # get measures 
                    mi_measures.extend(mi.keys())
            
            # check for extra measures in measure_info
            if "_references" in mi_measures:
                mi_measures.remove("_references")

            extra_measures = list(set(mi_measures).difference(set(data_measures)))
                
            if len(extra_measures) == 0:
                report += "\t<p><font color='#009E73'> [VALID] </font> %s: measure_info.json </p>\n" % (parent_dir)  
            else: 
                report += "\t<p><font color='#D55E00'> [EXTRA MEASURES] </font> %s %s: measure_info.json </p>\n" % (extra_measures, parent_dir)
         
    return report


if __name__ == "__main__":
    
    test = Test(
        __file__,
        "Measure Info Extra Measures Test",
        "Checks whether measure_info files have any extra measures not contained in any corresponding data files",
    )
    
    report = evaluate_folder("./data")
    test.export_html(report)
