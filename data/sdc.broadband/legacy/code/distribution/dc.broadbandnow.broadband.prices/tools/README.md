# Broadbandnow tools

Theoretically, one should be able to accomplish the entire series of tasks without writing a single line of python code. The main goal is that people can call the toolkit functions and end up with the same data set


## Example steps to parsing Fulton County (13121)

1. `python core_to_fcc.py -i 13121 -o temp/13121.csv.xz -ft` Get the addresses given a county fips from corelogic
2. `python fcc_area_query.py -i temp/13121.csv.xz -o temp_fcc -f` Use those address and cross match with fcc area api
3. `python combine_csv.py -i temp_fcc -o temp/13121_geocoded.csv.xz` Combine the files in the ouput address database into a single file
4. `wget -P temp/ https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/TABBLOCK/2020/tl_2020_13121_tabblock20.zip` Download a large zip file into the temp directory of the block shapefiles
5. `python clean_fcc.py -i temp/13121_geocoded.csv.xz -o temp/13121_cleaned.csv.xz` Clean the output data in preparation to be joined with the downloaded shape files
6. `python spatial_join.py -i temp/13121_cleaned.csv.xz -s temp/tl_2020_13121_tabblock20.zip -c 13121 -o temp/13121_spatial_joined.csv.xz` Spatial join the data with the downloaded shapefiles and sample the address at (n=1) per block
7. `python bbn_scraper.py -i temp/13121_spatial_joined.csv.xz -c street -o ../../../../data/dc.broadbandnow.broadband.prices/13121_bbn/ -l -c address` Scrape broadbandnow on the resultant addresses (in the street column) and export the data to a folder
