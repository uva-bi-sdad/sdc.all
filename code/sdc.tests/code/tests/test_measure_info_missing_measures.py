# tests for measures that appear in a data file but not in the measure_info.json in the same folder 


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

        if dir == "sdc.geographies":
            report += "NA"
            continue
        
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
                
            # get measure_info keys
            
            try:
                mi_path = str(path) + "/measure_info.json"
            
                with open(mi_path, "r") as mi_f:
                    mi = json.load(mi_f)
 
                mi_measures = mi.keys()
            except: 
                print(traceback.format_exc())
                report += "\t<p><font color='#D55E00'> [ERROR - NO MEASURE INFO FILE] </font> %s </p>\n" % (parent_dir)
                continue
        
            # LOOP THROUGH EACH DATA FILE IN FOLDER -----------------
            
            for f in path.iterdir(): #os.listdir(path):
            
                if not os.path.isfile(f):
                    # if path is not a file, skip to the next file to check
                    continue
                    
                #print(f.name)                  
            
                if f.suffix in [".xz", ".csv"]:
                    # file is a data file - extract measure names
                
                    try:
                        df = pd.read_csv(f.resolve(), dtype = str)  
                        data_measures = df["measure"].unique()   
                         
                        missing_measures = list(set(data_measures).difference(set(mi_measures)))    
                            
                        if len(missing_measures) == 0:
                            report += "\t<p><font color='#009E73'> [VALID] </font> [EXISTING MEASURES] %s %s: %s </p>\n" % (list(mi_measures), parent_dir, f.name)  
                        else: 
                            report += "\t<p><font color='#D55E00'> [MISSING MEASURES] </font> %s <br> [EXISTING MEASURES] %s %s: %s </p>\n" % (missing_measures, list(mi_measures), parent_dir, f.name)
                        
                        
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR - NO MEASURE COLUMN] </font> %s: %s</p>\n" % (parent_dir, f.name)

    return report


if __name__ == "__main__":
    
    test = Test(
        __file__,
        "Measure Info Missing Measures Test",
        "Checks whether measure_info files are missing any measures contained in corresponding data files",
    )
    
    report = evaluate_folder("./data")
    test.export_html(report)
