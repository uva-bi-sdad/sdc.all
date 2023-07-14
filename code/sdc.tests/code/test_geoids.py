# tests for valid geoids in data files 

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
                        data_geoid_years = df[["geoid", "year"]].drop_duplicates()  
                         
                        nonvalid_geoids = data_geoid_years.merge(valid_geoid_years, on = ['geoid','year'], how = 'left')  
                        nonvalid_geoids = nonvalid_geoids[nonvalid_geoids['valid'].isnull()]
                        nonvalid_geoids = nonvalid_geoids.drop(columns=['valid'])
                        nonvalid_geoids["report"] = nonvalid_geoids['geoid'].str.cat(nonvalid_geoids['year'], sep='|')
                            
                        if len(nonvalid_geoids) == 0:
                            report += "\t<p><font color='#009E73'> [VALID] </font> %s: %s </p>\n" % (parent_dir, f.name) 
                        else: 
                            report += "\t<p><font color='#D55E00'> [INVALID GEOIDS] </font> %s %s: %s </p>\n" % (nonvalid_geoids['report'], parent_dir, f.name)
                        
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR - NO GEOID COLUMN] </font> %s: %s</p>\n" % (parent_dir, f.name)

    return report


if __name__ == "__main__":
    
    master_geographies = "https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/geographies.csv"
    df = pd.read_csv(master_geographies, dtype = str)  
    valid_geoid_years = df[["geoid", "year"]].drop_duplicates()
    valid_geoid_years["valid"] = "yes"
    
    test = Test(
        __file__,
        "Geoids Validity Test",
        "Checks whether geoids in data files are valid",
    )
    
    report = evaluate_folder("./data", valid_geoid_years)
    test.export_html(report)