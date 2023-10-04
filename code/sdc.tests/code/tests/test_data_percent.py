# tests that percent measures are values between 0-100 (not 0-1)   

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
            
            logging.debug("\tEvaluating: %s" % path.name)

            # get path info
            
            try:    
                parent_dir = str(path).split('sdc.').pop()
                parent_dir = re.search('/(.*?)/data/distribution', parent_dir).group(1)
            except:
                parent_dir = "PARENT_DIRECTORY"
            
            # get measures that are percents 
            
            try:
                mi_path = str(path) + "/measure_info.json"
            
                with open(mi_path, "r") as mi_f:
                    mi = json.load(mi_f)
    
                percent_measures = []
                for measure in mi.keys():
                    if measure == "_references":
                        continue
                
                    if mi[measure]['measure_type'] == 'percent':
                        percent_measures.append(measure)      
    
            except: 
                print(traceback.format_exc())
                report += "\t<p><font color='#D55E00'> [ERROR - MEASURE INFO FILE] </font> %s </p>\n" % (parent_dir)
                continue
            
            # LOOP THROUGH EACH DATA FILE IN FOLDER -----------------
            
            for f in path.iterdir():               
            
                if not os.path.isfile(f):
                    # if path is not a file, skip to the next file to check
                    continue                 
            
                if f.suffix in [".xz", ".csv"]:                
                
                    # check if percent data is in the range 0-100
                    try:
                        df = pd.read_csv(f.resolve(), dtype = str) 
                        
                        for perc_meas in percent_measures:
                            filt_df = df[df['measure'] == perc_meas]
                            
                            # if measure exists in data file
                            if filt_df.shape[0] > 0:
                                filt_df["value"] = filt_df["value"].astype(float)
                                min_val = filt_df['value'].min()
                                max_val = filt_df['value'].max()
                        
                                if (min_val < 0) or (max_val <= 1.01):  # 1.01 in case of numerical issue
                                    report += "\t<p><font color='#D55E00'> [ERROR - INVALID RANGE] </font> %s: %s, %s</p>\n" % (parent_dir, f.name, perc_meas)                             
                                else:
                                    report += "\t<p><font color='#009E73'> [VALID] </font> %s: %s, %s</p>\n" % (parent_dir, f.name, perc_meas)                
                                    
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR - DATA FILE] </font> %s: %s</p>\n" % (parent_dir, f.name)
         
    return report


if __name__ == "__main__":
    
    test = Test(
        __file__,
        "Valid Percent Data Test",
        "Checks if percent data is in the range 0-100 (not 0-1)",
    )
    
    report = evaluate_folder("./data")
    test.export_html(report)
