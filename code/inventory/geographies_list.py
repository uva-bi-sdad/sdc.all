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
import re
import geopandas as gpd


def evaluate_folder(dirpath):
    report = ""
            
    #
    # CREATE GEOGRAPHIES DF -------------------------------------
    #

    geographies_df = pd.DataFrame(columns = ['geoid', 'region_name', 'region_type', 'year'])
    
    # LOOP THROUGH SDC.GEOGRAPHIES REPO  ---------------------
    
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
            
            full_path = path.name

            if path.suffix in [".geojson"]:                
               
                try:
                    # read in a geojson
                    path = str(path)
                    df = gpd.read_file(path, ignore_geometry=True)
                    
                    # remove geographies column of df
                    df = pd.DataFrame(df)
        
                    # update geographies df --------------------
                    geographies_df = pd.concat([geographies_df, df], ignore_index = True) 
                    
                    report += "\t<p><font color='green'> [ADDED TO CSV] </font> %s%s</p>\n" % (dirpath, full_path)
                    
                except:
                    print(traceback.format_exc())
                    report += "\t<p><font color='red'> [ERROR] </font> %s/%s</p>\n" % (dirpath, full_path)
           
    # change formatting of "And" to "and", "Of" to "of" and "city" to "City" 
    geographies_df['region_name'] = geographies_df['region_name'].str.replace(' Of ',' of ')
    geographies_df['region_name'] = geographies_df['region_name'].str.replace(' And ',' and ')
    geographies_df['region_name'] = geographies_df['region_name'].str.replace('city','City')
    
    # deduplicate rows of geographies df  
    final_geographies_df = geographies_df.drop_duplicates()
            
    return report, final_geographies_df


if __name__ == "__main__":

    # with urllib.request.urlopen(
    #    "https://raw.githubusercontent.com/uva-bi-sdad/data_repo_structure/main/col_names.json"
    # ) as url:
    #    req_cols = json.load(url)

    # req_cols = set(req_cols)
    # print(req_cols)

    report, inv_final_df = evaluate_folder("./data/sdc.geographies")
    time_checked = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    print(time_checked)

    t = Template(
        """
    <html>
        <head>
        <title>Data Inventory Report</title>
        </head>
        <body>
            Last updated: $time_checked
            $report
        </body>
    </html>
    """
    )
    
    # write report
    print(t.substitute(time_checked=time_checked, report=report))
    
    with open("./docs/geographies_report.html", "w") as f:
        f.write(t.substitute(time_checked=time_checked, report=report))
        
    # write inventory    
    inv_final_df.to_csv("./docs/geographies_metadata.csv", index=False)
