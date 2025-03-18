Explanation of each function in the `tract_conversions.R` file, a set of tools for handling census tract data conversions between 2010 and 2020 boundaries. These functions collectively provide a toolkit for working with census data across different years, allowing for meaningful comparisons despite boundary changes between 2010 and 2020.

## 1. `get_2010_2020_bound_changes()`

This function determines how census tract boundaries changed from 2010 to 2020. It:
- Takes parameters for resolution ('tract' or 'block group') and optional geoids to filter by
- Downloads the appropriate crosswalk file from the Census Bureau
- Processes the data to determine the type of change for each tract
- Classifies changes into three categories:
  - 'same': tract boundaries did not change
  - 'split': a tract was split but the overall bounds remained the same
  - 'moved': tract boundaries were moved/changed
- Returns a dataframe with the relationship between 2010 and 2020 geographies, including area measurements and the new "type_change" classification

## 2. `create_crosswalk()`

This function generates a comprehensive crosswalk file for different geographical levels:
- Takes a parameter of geoids (geographic identifiers)
- Determines the resolution (tract or block group) based on the length of the geoids
- Calls `get_2010_2020_bound_changes()` for each unique resolution
- Combines all crosswalk data into a single dataframe
- Returns a unified crosswalk with columns: geoid10, geoid20, area10, area20, area_part, and type_change

## 3. `convert_2010_to_2020_bounds()`

This function redistributes 2010 data to match 2020 census boundaries:
- Takes parameters for the input data, geoid column name, and value column name
- Validates the data structure (checks for character geoids and uniqueness)
- Creates a crosswalk for the provided geoids
- Handles redistribution differently based on boundary change type:
  - For 'same' and 'split' tracts: values remain the same
  - For 'moved' tracts: values are weighted based on the percentage of area overlap
- Returns a dataframe with 2020 geoids and redistributed values

## 4. `standardize_all()`

This function standardizes a complete dataset to fit 2020 tract boundaries:
- Takes parameters for data and a geographic filter level (state or county)
- Processes each year and measure in the dataset
- For years before 2020, it calls `convert_2010_to_2020_bounds()` to standardize the data
- Adds suffixes to distinguish between original data ('_geo10' or '_geo20') and standardized data ('_geo20')
- Combines the original and standardized data
- Filters the result to maintain consistency with the original geographic scope
- Returns the complete standardized dataset

