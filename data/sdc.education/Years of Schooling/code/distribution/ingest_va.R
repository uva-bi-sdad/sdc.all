library(tidycensus)
library(data.table)
library(dplyr)
library(tidyr)


#function for calculating ays
get_ays <- function(acs_data, tract_geoid) {
  pop_mf <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_001"), c("estimate")][[1]]
  pop_m <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_002"), c("estimate")][[1]]
  pop_f <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_043"), c("estimate")][[1]]
  
  m_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_004", "B15001_012", "B15001_020", "B15001_028", "B15001_036"),]
  m_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_005", "B15001_013", "B15001_021", "B15001_029", "B15001_037"),]
  m_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_006", "B15001_014", "B15001_022", "B15001_030", "B15001_038"),]
  m_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_007", "B15001_015", "B15001_023", "B15001_031", "B15001_039"),]
  m_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_008", "B15001_016", "B15001_024", "B15001_032", "B15001_040"),]
  m_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_009", "B15001_017", "B15001_025", "B15001_033", "B15001_041"),]
  m_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_010", "B15001_018", "B15001_026", "B15001_034", "B15001_042"),]
  
  f_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_045", "B15001_053", "B15001_061", "B15001_069", "B15001_077"),]
  f_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_046", "B15001_054", "B15001_062", "B15001_070", "B15001_078"),]
  f_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_047", "B15001_055", "B15001_063", "B15001_071", "B15001_079"),]
  f_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_048", "B15001_056", "B15001_064", "B15001_072", "B15001_080"),]
  f_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_049", "B15001_057", "B15001_065", "B15001_073", "B15001_081"),]
  f_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_050", "B15001_058", "B15001_066", "B15001_074", "B15001_082"),]
  f_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_051", "B15001_059", "B15001_067", "B15001_075", "B15001_083"),]
  
  lt_9gr <- rbind(m_lt_9gr, f_lt_9gr)
  lt_9gr_ays <- (sum(lt_9gr$estimate)/pop_mf) * 7.5
  
  hs_grad_no <- rbind(m_hs_grad_no, f_hs_grad_no)
  hs_grad_no_ays <- (sum(hs_grad_no$estimate)/pop_mf) * 11
  
  hs_grad_yes <- rbind(m_hs_grad_yes, f_hs_grad_yes)
  hs_grad_yes_ays <- (sum(hs_grad_yes$estimate)/pop_mf) * 12
  
  col_some <- rbind(m_col_some, f_col_some)
  col_some_ays <- (sum(col_some$estimate)/pop_mf) * 13
  
  col_asoc <- rbind(m_col_asoc, f_col_asoc)
  col_asoc_ays <- (sum(col_asoc$estimate)/pop_mf) * 14
  
  col_bach <- rbind(m_col_bach, f_col_bach)
  col_bach_ays <- (sum(col_bach$estimate)/pop_mf) * 16
  
  col_grad <- rbind(m_col_grad, f_col_grad)
  col_grad_ays <- (sum(col_grad$estimate)/pop_mf) * 19
  
  ays <- sum(lt_9gr_ays, hs_grad_no_ays, hs_grad_yes_ays, col_some_ays, col_asoc_ays, col_bach_ays, col_grad_ays)
  ays
}



#function for getting different years acs data
get_acsdata <- function(geography, table, state, survey, start_year, end_year) {
  acsdata_list <- list()
  for (year in start_year:end_year) {
    acsdata <- get_acs(geography = geography,
                       table = table,
                       year = year,
                       state = state,
                       survey = survey)
    acsdata_list[[paste0("acsdata", year)]] <- acsdata
  }
  list2env(acsdata_list, envir = .GlobalEnv)
}

#get acs data for five years
get_acsdata(geography = "tract",
            table = "B15001",
            state = "VA",
            survey = "acs5",
            start_year = 2017,
            end_year = 2021)



#different years calculation

for (year in c(2021, 2020, 2019, 2018, 2017)) {
  # Get the data for the current year
  acsdata_year <- get(paste0("acsdata", year))
  
  # Calculate AYS for all tracts
  if (exists("dt_all")) rm(dt_all)
  unq_tracts <- unique(acsdata_year$GEOID)
  for (t in unq_tracts) {
    print(t)
    ays_t <- get_ays(acsdata_year, t)
    if (exists("dt_all")) dt_all <- data.table::rbindlist(list(dt_all, data.table::data.table(geoid = t, ays = ays_t)))
    else dt_all <- data.table::data.table(geoid = t, ays = ays_t)
  }
  
  # save to file
  data.table::fwrite(dt_all, paste0("data/testloop_edu_attain_VA_", year, ".csv"))
  
  dt_all$year = year
  
  # Transform to range [0, 1]
  mn <- min(dt_all$ays, na.rm = T)
  mx <- max(dt_all$ays, na.rm = T)
  scaled <- (dt_all$ays - mn)/(mx-mn)
  dt_scaled <- data.table::data.table(geoid = dt_all$geoid, ays = dt_all$ays, score = scaled, year = dt_all$year)
  # save to file
  data.table::fwrite(dt_scaled, paste0("data/testloop_edu_attain_VA_scaled_", year, ".csv"))
}





# Combine all output files into a single data table
file_names <- c("data/testloop_edu_attain_VA_scaled_2021.csv", "data/testloop_edu_attain_VA_scaled_2020.csv", "data/testloop_edu_attain_VA_scaled_2019.csv", "data/testloop_edu_attain_VA_scaled_2018.csv", "data/testloop_edu_attain_VA_scaled_2017.csv")
ays_df <- data.table::rbindlist(lapply(file_names, function(f) data.table::fread(f)))

write.csv(ays_df, "data/ays_df.csv")


#The below code is to get names of all tracts to combine them
# Loop over the years of interest
for (year in c(2017, 2018, 2019, 2020, 2021)) {
  
  # Get the data for Virginia for the given year
  acsdata <- get_acs(geography = "tract",
                     table = "B15001",
                     state = "VA",
                     geometry = F,
                     year = year)
  
  # Convert the data to a data frame
  acsdf <- data.frame(acsdata)
  
  # Create a new data frame with only the unique tracts and corresponding entries in the 'name' column
  new_df <- acsdf %>%
    distinct(GEOID, .keep_all = TRUE) %>%
    select(GEOID, NAME)
  
  # Assign the new data frame to an object with a specific name
  assign(paste0("acsdf", year), new_df)
}

#we will get files like acsdf2021,......2017
#######
# Create a list of data frames
df_list <- list(acsdf2021, acsdf2020, acsdf2019, acsdf2018, acsdf2017)

# Create a new list to store the new data frames
new_df_list <- list()

# Iterate over each data frame in the list
for (i in seq_along(df_list)) {
  # Extract the i-th data frame
  df <- df_list[[i]]
  
  # Create a new data frame with only the unique tracts and corresponding entries in the 'name' column
  new_df <- df %>%
    distinct(GEOID, .keep_all = TRUE) %>%
    select(GEOID, NAME)
  
  # Append the new data frame to the list
  new_df_list[[i]] <- new_df
}

# Use do.call() and rbind() to combine the new data frames into a single data frame
namedf <- do.call(rbind, new_df_list)



##next combine both dataframes (namedf, aysdf)


ays_df <-cbind(ays_df, namedf$NAME)

names(ays_df)[names(ays_df) == 'V2'] <- 'NAME'

#write.csv(ays_df,"data/ays_df_VA.csv")


###Creating the correct format


###Creating ays_df in format
va_tract_ays_2017_2021 <- data.frame(
  geoid = ays_df$geoid,
  year = ays_df$year,
  NAME = ays_df$NAME,
  measure = "average_years_schooling",
  region_type = "tract",
  value = ays_df$ays
)

###Creating ays_df in format
va_tract_ays_scaled_2017_2021 <- data.frame(
  geoid = ays_df$geoid,
  year = ays_df$year,
  NAME = ays_df$NAME,
  measure = "ays_index",
  region_type = "tract",
  value = ays_df$score
)


write.csv(va_tract_ays_2017_2021,"data/va_tract_ays_2017_2021.csv")
write.csv(va_tract_ays_scaled_2017_2021,"data/va_tract_ays_scaled_2017_2021.csv")



##Creating long format



wide <- ays_df
names(wide)[2] <- 'average'
names(wide)[3] <- 'index'

long <- wide %>% 
  pivot_longer(
    cols  = `average`: `index`, 
    names_to = "measure_type",
    values_to = "value"
  )

long <- long %>% mutate(measure = case_when(measure_type == "average" ~ "average_years_schooling", 
                                            measure_type == "index" ~ "scaled_score"),
                        region_type = "tract")

saveRDS(long , file = 'data/VA_ays_long.RDS')
write.csv(long, "data/VA_ays_long.csv")


