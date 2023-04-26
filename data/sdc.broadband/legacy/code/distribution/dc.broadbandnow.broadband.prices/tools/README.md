# Broadbandnow tools

Theoretically, one should be able to accomplish the entire series of tasks without writing a single line of python code. The main goal is that people can call the toolkit functions and end up with the same data set


## Example steps to compiling data for Fulton County (13121)

1. `python core_to_fcc.py -i 13121 -o temp` Get the addresses given a county fips from corelogic
2. `python fcc_area_query.py -i temp/13121.csv.xz -o temp_fcc -f` Use those address and cross match with fcc area api
3. `python combine_csv.py -i temp_fcc -o temp/13121_geocoded.csv.xz` Combine the files in the ouput address database into a single file
4. `wget -P temp/ https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/TABBLOCK/2020/tl_2020_13121_tabblock20.zip` Download a large zip file into the temp directory of the block shapefiles
5. `python clean_fcc.py -i temp/13121_geocoded.csv.xz -o temp/13121_cleaned.csv.xz` Clean the output data in preparation to be joined with the downloaded shape files
6. `python spatial_join.py -i temp/13121_cleaned.csv.xz -s temp/tl_2020_13121_tabblock20.zip -c 13121 -o temp/13121_spatial_joined.csv.xz` Spatial join the data with the downloaded shapefiles and sample the address at (n=1) per block
7. `python bbn_scraper.py -i temp/13121_spatial_joined.csv.xz -c street -o ../../../../data/dc.broadbandnow.broadband.prices/temp_13121_bbn/ -l -c address` Scrape broadbandnow on the resultant addresses (in the street column) and export the data to a folder
8. `python combine_csv.py -i ../../../../data/dc.broadbandnow.broadband.prices/temp_13121_bbn -o ../../../../data/dc.broadbandnow.broadband.prices/13121_2023_broadband_prices.csv.xz` Combine the dataset into a single csv
9. `python join_bbn_with_spatial.py -i ../../../../data/dc.broadbandnow.broadband.prices/13121_2023_broadband_prices.csv.xz -s temp/13121_spatial_joined.csv.xz -o temp/13121_bbn_space_joined.csv.xz -c 13121` Join the final bbn parsed data with the previous address-blocked spatial joined geometry for easy plotting
10. `python visualize.py -i temp/13121_bbn_space_joined.csv.xz -o temp/13121.png -l "Fulton County"` Plot the results


## Example steps to compiling data for a batch in Georgia
```python
atlanta_fips = ['Fulton County','Clayton County','Fayette County','Henry County','Rockdale County','Gwinnett County','Forsyth County','Cherokee County','Cobb County','Douglas County']
df = pd.read_csv('https://github.com/uva-bi-sdad/national_address_database/raw/main/data/fips_county.csv', dtype={'fips':object})
atlanta_fips = [x.lower().replace(' ','_') for x in atlanta_fips] # clean the data
pdf = df[df['county'].isin(atlanta_fips)]
pdf = pdf[pdf['fips'].apply(lambda x: x[:2] == '13')]
      fips           county
423  13057  cherokee_county
426  13063   clayton_county
428  13067      cobb_county
443  13097   douglas_county
451  13113   fayette_county
453  13117   forsyth_county
455  13121    fulton_county
462  13135  gwinnett_county
470  13151     henry_county
517  13247  rockdale_county    

rdf =pdf[~pdf['county'].isin(parsed)]
list(rdf['fips'])
remaining = ['13057', '13067', '13097', '13113', '13117', '13135', '13151', '13247']
# 13057 13067 13097 13113 13117 13135 13151 13247
```

## Batching Area Codes
```python
python core_to_fcc.py -i <fips> -o temp
python batch_fcc_area.py -i <fips> -d temp # check the temp directory for all the fips and assume temp_<fip>_fcc/ as output directory for the geocoded files
```