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


def evaluate_folder(dirpath):
    report = ""
            
    #
    # CREATE INVENTORY DF -------------------------------------
    #

    inventory_df = pd.DataFrame(columns = ['CATEGORY', 'MEASURE', 'START_YEAR', 'END_YEAR'])
    
    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
    
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        # extract category from data folder name
        data_cat = dir.split('.').pop()
        
        # CHECK EACH DATA/DISTRIBUTION FILE ------------------------
        
        for path in Path(subdir).rglob("data/distribution/**/*"):
            logging.debug("\tEvaluating: %s" % path.name)

            if not os.path.isfile(path):
                # if path is not a file, skip to the next file to check
                continue

            parent_dir = path.parent
            
            full_path = path.name

            if full_path not in ["measure_info.json"]:                
               
                try:
                    df = pd.read_csv(path.resolve(), dtype = str)  
                    
                    state = df["geoid"].str[0:2]
                    df["state"] = ["DC" if st == "11" else "MD" if st == "24" else "VA" for st in state]
                        
                    counts = df.groupby(['measure', 'year', 'state', 'region_type']).agg(count=('geoid', 'count'))
                    counts = counts.reset_index()  # move multiindex to columns
                    
                    # loop through measures in each data file --------------------
      
                    measures = df["measure"].unique()
        
                    for m in measures:
                        temp = counts[counts['measure'] == m] 
                        temp["st_geo"] = temp['state'] + "_" + temp['region_type'].str.lower()
                        temp["collected"] = (temp['count'] > 0)  # only checks if we have 1 measurement at geography level
                        temp = temp.drop(columns=['state', 'region_type', 'count'])
                        temp = temp.pivot(index=['measure', 'year'] , columns='st_geo', values='collected').reset_index()
        
                        # check if we have data for each geography and year listed
        
                        nr, nc = temp.shape
                        csums = temp.iloc[:,2:11].sum()
                        idx = (csums == nr)
                    
                        # check if years are contiguous - not done, just got start and end year

                        start_yr = min(temp["year"])
                        end_yr = max(temp["year"]) 
                
                        # update inventory df --------------------
        
                        collected_geos = ["X" if idx[i] else None for i in range(len(idx))]

                        temp_row = {'CATEGORY': data_cat, 'MEASURE': m, 'START_YEAR': start_yr, 'END_YEAR': end_yr} 
                        for i in range(len(csums)):
                            temp_row.update({csums.index[i]: collected_geos[i]})
                        
                        temp_df = pd.DataFrame([temp_row])
                        inventory_df = pd.concat([inventory_df, temp_df], ignore_index = True) 
                    
                    report += "\t<p><font color='green'> [INVENTORIED] </font> %s</p>\n" % (full_path)
                    
                except:
                    print(traceback.format_exc())
                    report += "\t<p><font color='red'> [ERROR] </font> %s</p>\n" % (full_path)
                       
    # combine inventory information for measures listed more than once (e.g. in an NCR and VA file) 

    inv_final_df = inventory_df.groupby(["CATEGORY", "MEASURE", "START_YEAR", "END_YEAR"]).apply(lambda x: x.ffill().bfill())
    inv_final_df.drop_duplicates(keep="first", inplace=True, ignore_index=True) 
    
    # list geography columns in abc-order
    inv_final_df = inv_final_df[ ["CATEGORY", "MEASURE", "START_YEAR", "END_YEAR"] + sorted(inv_final_df.columns[4:]) ]
            
    return report, inv_final_df


if __name__ == "__main__":

    # with urllib.request.urlopen(
    #    "https://raw.githubusercontent.com/uva-bi-sdad/data_repo_structure/main/col_names.json"
    # ) as url:
    #    req_cols = json.load(url)

    # req_cols = set(req_cols)
    # print(req_cols)

    report, inv_final_df = evaluate_folder("./data")
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
    with open("./docs/inventory_report.html", "w") as f:
        f.write(t.substitute(time_checked=time_checked, report=report))

    # write inventory    
    inv_final_df.to_csv("./docs/inventory.csv", index=False)
