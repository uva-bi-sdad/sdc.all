library(dplyr)

get_2010_2020_bound_changes <- function(res='tract', geoids=NULL) {
  #' determines if/how census tract boundaries changed from 2010 to 2020. Adds
  #' an additional "type_change" column to 2010-2020 tract relationship file
  #' indicating whether a tract did not change ('same'), a tract was split but
  #' bounds did not change ('split'), or a tracts bounds moved ('moved').
  #' param: res(character - default 'tract') -> resolution to get changes for.
  #' param: geoids(character vector - default NULL) -> geoids to get changes for.
  #'                                    if none provided, returns all of US
  #' return: dataframe of relationship data with "type_change" column.

  # get crosswalk data for given resolution
  if (res == 'tract') {
    file_path <- 'https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt'
    res_code <- 'TRACT'
    
    delimiter <- '|'
    
    crosswalk <- read.csv(file_path, sep=delimiter, 
                          colClasses=c("GEOID_TRACT_10"="character", 
                                       "GEOID_TRACT_20"="character"))
  }
  else if (res == 'block group') {
    file_path <- 'https://www2.census.gov/geo/docs/maps-data/data/rel2020/blkgrp/tab20_blkgrp20_blkgrp10_st51.txt'
    res_code <- 'BLKGRP'
    
    delimiter <- '|'
    
    crosswalk <- read.csv(file_path, sep=delimiter, 
                          colClasses=c("GEOID_BLKGRP_10"="character", 
                                       "GEOID_BLKGRP_20"="character"))
  }
  else {
    stop("invalid resolution. valid resolutions: 'tract', 'block group'")
  }
  
  # select necessary columns and rename
  crosswalk <- crosswalk[, c(paste0("GEOID_", res_code, "_20"), 
                                    paste0("GEOID_", res_code, "_10"),
                                    paste0('AREALAND_', res_code, '_20'), 
                                    paste0('AREALAND_', res_code, '_10'),
                                    'AREALAND_PART')] %>% filter(AREALAND_PART != 0)
  colnames(crosswalk) = c('geoid20', 'geoid10', 'area20', 'area10', 'area_part')
  
  if(!is.null(geoids)) crosswalk <- crosswalk[crosswalk$geoid10 %in% geoids, ]
  
  # determine if/how boundaries changed ---------------------------------------
  # get counts for 2010 and 2020 geoids
  crosswalk <- crosswalk %>% group_by(geoid20) %>%
    mutate(count_20 = n())
  crosswalk <- crosswalk %>% group_by(geoid10) %>%
    mutate(count_10 = n())

  # Identifying geoids that are same
  geoid_10_20 <- crosswalk %>% select(geoid10, area20) %>%
    group_by(geoid10) %>%
    summarise(area20 = sum(area20)) %>%
    select(geoid10, match_area = area20)
  
  crosswalk <- left_join(crosswalk, geoid_10_20, by='geoid10')
  crosswalk <- crosswalk %>% mutate(type_change = case_when(
    count_10 == 1 & count_20 == 1 ~ 'same',
    area10 == match_area ~ 'split',
    TRUE ~ 'moved')) %>% 
    select(-c(count_10, count_20, match_area)) %>% 
    ungroup() 
  
  return(crosswalk)
}

create_crosswalk <- function(geoids) {
  #' generates a crosswalk file inclusive of all geography levels possible that 
  #' exist in geoids. 
  #' param: geoids(character vector) -> geoids to get crosswalk for.
  #' return: dataframe of relationship data. columns: 
  #'         geoid10, geoid20, area10, area20, area_part, type_change
  
  # unique resolutions from string length
  resolutions <- unique(nchar(geoids))
  
  crosswalk <- NULL
  for (res in resolutions) {
    
    # get crosswalk based on resolution
    if (res == 11) {
      temp <- get_2010_2020_bound_changes(res='tract', geoids=geoids)
      crosswalk <- rbind(crosswalk, temp)
    }
    else if (res == 12) {
      temp <- get_2010_2020_bound_changes(res='block group', geoids=geoids)
      crosswalk <- rbind(crosswalk, temp)
    }
    else {
      print(paste("crosswalk not available for resolution:", res))
    }
  }
  
  return(crosswalk)
}

convert_2010_to_2020_bounds <- function(data, geoid_col='geoid', val_col='value') {
  #' redistributes 2010 data based on 2020 census boundaries
  #' param: data(dataframe) -> data to redistribute. contains geoid(2010) and value attributes.
  #' param: geoid_col(character - default='geoid') -> name of column with 2010 geoids
  #' return: redistributed(dataframe) -> data redistributed to 2020 tract boundaries.
  #'            contains geoids(2020) and redistributed values
  
  geoids <- unique(data[,geoid_col])

  if (class(data[, geoid_col]) != 'character') {
    stop("geoids should be characters")
  }
  if (length(data[, geoid_col]) > length(geoids)) {
    stop("geoids are not unique -- data cannot contain more than one entry per geoid. 
         please double check that data only spans one year, measure, etc.")
  }

  # warn if data does not contain information on all tracts
  if (sum(is.na(data[val_col])) > 0) {
    warning("data contains missing or NULL values. the value of any new tract that overlaps with a NULL value will be coerced to NULL. 
            if this is an issue, we recommend manual insertion of values based on your contextual specifications.")
  }
  
  # standardize data naming
  data <- data[, c(geoid_col, val_col)]
  names(data)[names(data) == val_col] <- 'value'
  
  options(scipen = 999)
  
  # get relationship data
  crosswalk <- create_crosswalk(geoids)
  
  # join data with crosswalk
  # for 2010 tracts that are split, data gets distributed to each new tract
  joined <- crosswalk %>% left_join(data, by=c('geoid10' = geoid_col))
  
  # case when tract borders don't change (same or split), no more changes necessary
  same_bounds <- data.frame(joined)[joined$type_change=='same' | joined$type_change=='split', ]  %>%
    group_by(geoid20) %>% summarise(value=first(value))
  
  # case when borders are moved
  moved_bounds <- joined[joined$type_change == 'moved', ]
  
  # calculate weighted mean based on percent overlap
  moved_bounds <- moved_bounds %>%
    mutate(pct_overlap = area_part / area20) %>%
    mutate(value = value * pct_overlap) %>%
    group_by(geoid20) %>% 
    summarise(value = sum(value)) 

  # bind two cases
  redistributed <- rbind(same_bounds, moved_bounds) %>% 
    rename(geoid=geoid20) 

  colnames(redistributed)[names(redistributed) == 'value'] <- val_col
  
  return(redistributed)
}


standardize_all <- function(data, filter_geo='state') {
  #' takes in a finalized tract dataset and standardizes the values to fit 
  #' 2020 tract boundaries. assumes that the dataset fits the data commons 
  #' conventions for finalized data 
  #'  (columns: geoid, year, measure, value, moe, region_type ||  
  #'  measure names: snake case - underscore delimeter)
  #' param: data(dataframe) -> tract data fitting data commons conventions 
  #'                         (outlined above)
  #' param: filter_geo(character - default='state') -> geographic level to keep 
  #'                         consistent with original data. (for example, if 
  #'                         your original data only contains specific states
  #'                         like Virginia, use 'state'. if your original 
  #'                         data only contains specific counties like 
  #'                         Fairfax and Arlington, use 'county')
  #' return: same dataframe with values standardized to 2020 boundaries
  
  # list of years and measures
  years <- unique(data$year)
  measures <- unique(data$measure)
  
  # add '_orig_{census year}' to delineate original data
  original <- data %>% mutate(measure = paste0(measure, '_orig_', 
                                           as.character(
                                             (year%/%10)*10) # convert year to census year
                                           )) 
  
  standardized <- NULL
  
  # standardize for all years < 2020 and measures
  for (yr in years) {
    
    # standardize if year is less than 2020
    if (yr < 2020) {
      for (meas in measures) {
        temp <- data.frame(data) %>% filter(year == yr, measure == meas, nchar(geoid) == 11)

        # get standardized data '_std' delineates calculated data
        converted <- convert_2010_to_2020_bounds(temp) %>%
          mutate(year = yr, measure = paste0(meas, '_std'), moe = '',
                 region_type = case_when(
                   nchar(geoid) == 11 ~ 'tract',
                   nchar(geoid) == 12 ~ 'block group'
                )) # reapply descriptive columns

        # merge to final
        standardized <- rbind(standardized, converted)
        
      }
    }
  }

  # combine standardized data and original data
  final <- rbind(standardized, original)
  
  if (filter_geo == 'state') {
    geoids <- unique(substr(original$geoid, 1, 2))
    final <- final %>% filter(substr(geoid, 1, 2) %in% geoids)
  }
  else if (filter_geo == 'county') {
    geoids <- unique(substr(original$geoid, 1, 5))
    final <- final %>% filter(substr(geoid, 1, 5) %in% geoids)
  }
  
  return(final)
}
