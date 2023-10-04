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

def geoid_to_region_type(geoid):   

    id_len = len(geoid)
    
    if (id_len==2):
        r_type = "state"
    elif (id_len==5):
        r_type = "county"
    #elif (id_len==7):
    #    r_type = "Place"
    #elif (id_len==10):
    #    r_type = "County Subdivision"
    elif (id_len==11):
        r_type = "tract"
    elif (id_len==12):
        r_type = "block group"
    elif (id_len==15):
        r_type = "block"
    elif "_ahec_" in geoid:
        r_type = "AHEC region"
    elif "_ca_" in geoid:
        r_type = "civic association"
    elif "_hd_" in geoid:
        r_type = "health district"
    elif "_hsr_" in geoid:
        r_type = "human services region"
    elif "_pd_" in geoid:
        r_type = "planning district"
    elif "_sd_" in geoid:
        r_type = "supervisor district"
    elif "_zc_" in geoid:
        r_type = "zip code"
    else:
        r_type = "unidentified"
        
    return r_type



def evaluate_folder(dirpath):
    report = ""
            
    #
    # CREATE INVENTORY DF -------------------------------------
    #

    inventory_df = pd.DataFrame(columns = ['CATEGORY', 'MEASURE', 'SHORT_NAME', 'START_YEAR', 'END_YEAR'])
    
    # LOOP THROUGH EACH REPO (SDC. ...) ---------------------
    
    for dir in os.listdir(dirpath):
        subdir = os.path.join(dirpath, dir)
        if not os.path.isdir(subdir):
            continue
        report += "<h3> %s </h3>\n" % (dir)
        
        # extract category from data folder name
        data_cat = dir.split('.').pop()
        
        # CHECK EACH DATA/DISTRIBUTION FILE ------------------------
        
        for path in Path(subdir).rglob("data/distribution"):
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
                    
                        state = df["geoid"].str[0:2]
                        df["state"] = ["DC" if st == "11" else "MD" if st == "24" else "VA" for st in state]
                    
                        if "region_type" not in df.columns:
                            df["region_type"] = [geoid_to_region_type(g) for g in df["geoid"]]
                        
                        counts = df.groupby(['measure', 'year', 'state', 'region_type']).agg(count=('geoid', 'count'))
                        counts = counts.reset_index()  # move multiindex to columns
                    
                        # loop through measures in each data file --------------------
      
                        measures = df["measure"].unique()
        
                        for m in measures:
                            temp = counts[counts['measure'] == m] 
                            temp["st_geo"] = temp['state'] + "_" + temp['region_type'].str.lower()
                            temp["collected"] = (temp['count'] > 0)  # only checks if we have 1 measurement at geo level
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
            
                            try:
                                short_n = mi[m]['short_name']
                            except: 
                                print(traceback.format_exc())
                                short_n = ""

                            temp_row = {'CATEGORY': data_cat, 'MEASURE': m, 'SHORT_NAME': short_n, 'START_YEAR': start_yr, 'END_YEAR': end_yr} 
                            for i in range(len(csums)):
                                temp_row.update({csums.index[i]: collected_geos[i]})
                        
                            temp_df = pd.DataFrame([temp_row])
                            inventory_df = pd.concat([inventory_df, temp_df], ignore_index = True) 
                    
                        report += "\t<p><font color='#009E73'> [INVENTORIED] </font> %s: %s</p>\n" % (parent_dir, f.name)
                    
                    except:
                        print(traceback.format_exc())
                        report += "\t<p><font color='#D55E00'> [ERROR] </font> %s: %s</p>\n" % (parent_dir, f.name)
                       
    # combine inventory information for measures listed more than once (e.g. in an NCR and VA file) 

    inv_final_df = inventory_df.groupby(["CATEGORY", "MEASURE", "SHORT_NAME", "START_YEAR", "END_YEAR"]).apply(lambda x: x.ffill().bfill())
    inv_final_df.drop_duplicates(keep="first", inplace=True, ignore_index=True) 
    
    # list geography columns in abc-order
    inv_final_df = inv_final_df[ ["CATEGORY", "MEASURE", "SHORT_NAME", "START_YEAR", "END_YEAR"] + sorted(inv_final_df.columns[5:]) ]
            
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
    with open("./docs/data_inventory_report.html", "w") as f:
        f.write(t.substitute(time_checked=time_checked, report=report))

    # write inventory    
    inv_final_df.to_csv("./docs/data_inventory.csv", index=False)
