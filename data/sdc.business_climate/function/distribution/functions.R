# List of functions used in the business climate repos
library(dplyr)
library(readr)
library(stringr)
library(tidyr)




# variables_checks <- function(data,topic){# The function check if variables required to estimate metrics are observed in the data}



#update_all <- function(path){This function update all the repo or specific portion of the repo}



business_dynamics <- function(data,geolevels,topic,prefix,savepath='default',entry=TRUE,exit=TRUE,count=TRUE,size=TRUE){
  # this function compute all the metrics for a specific topic and save the output
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topic - topic/classification of interest. values are c('Industry','Minority_owned','Industry_Minority_owned','Total'). Only one topic can be listed.
  #       prefix - prefix name of the data. refers to the area of interest. example va059 for Fairfax County or ncr for National Capital Region
  #       savepath - path where the data would be saved. if empty it would be saved on the default path: "Business_characteristics/topic/data/distribution/"
  
  # name topics
  if ('Industry' %in% topic){topic_name <- 'industry'}
  if ('Minority_owned' %in% topic){topic_name <- 'minority'}
  if ('Industry_Minority_owned' %in% topic){topic_name <- c('industry','minority')}
  if ('Total' %in% topic){topic_name <- 'aggregate'}
  
  # run the function for each metrics
  if(entry){
    temp_entry <- entry(data,geolevels,topic_name) 
    print('entry metrics completed')
    }
  
  if(exit){ 
    temp_exit <- exit(data,geolevels,topic_name) 
    print('exit metrics completed')
    }
  
  if(count){ 
    temp_count <- count(data,geolevels,topic_name) 
    print('count metrics completed')
    }
  
  if(size){ 
    temp_size <- size(data,geolevels,topic_name) 
    print('size metrics completed')
  }
  
  # combine all metrics and save the final data
  output <- rbind(temp_entry,temp_exit,temp_count)
  
  # name the data
  data_name <- paste0(prefix,"_")
  if ("county" %in% geolevels) {data_name <- paste0(data_name,'ct')}
  if ("tract" %in% geolevels) {data_name <- paste0(data_name,'tr')}
  if ("block group" %in% geolevels) {data_name <- paste0(data_name,'bg')}
  data_name <- paste0(data_name,"_mi_",min(output$year),'_',max(output$year),'_business_metrics_by_',topic,'.csv.xz')
  
  # save in a distribution folder
  if(savepath=='default') {savepath <- paste0("Business_characteristics/",topic,"/data/distribution/") }
  readr::write_csv(output, xzfile(paste0(savepath,data_name), compression = 9))
  print('save data completed')
  
  # return the data saved
  return(output)
}




employment_dynamics <- function(data,geolevels,topic,prefix,savepath='default',job_creation=TRUE,job_destruction=TRUE,job_count=TRUE){
  # This function measure the employment dynamics through jobs creation and jobs destruction
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topic - topic/classification of interest. values are c('Industry','Minority_owned','Industry_Minority_owned','Total'). Only one topic can be listed.
  #       prefix - prefix name of the data. refers to the area of interest. example va059 for Fairfax County or ncr for National Capital Region
  #       savepath - path where the data would be saved. if empty it would be saved on the default path: "Business_characteristics/topic/data/distribution/"
  
  # name topics
  if ('Industry' %in% topic){topic_name <- 'industry'}
  if ('Minority_owned' %in% topic){topic_name <- 'minority'}
  if ('Industry_Minority_owned' %in% topic){topic_name <- c('industry','minority')}
  if ('Total' %in% topic){topic_name <- 'aggregate'}
  
  # run the function for each metrics
  if(job_creation){
    temp_jc <- job_creation(data,geolevels,topic_name) 
    print('jobs creation metrics completed')
  }
  
  if(job_destruction){ 
    temp_jd <- exit(data,geolevels,topic_name) 
    print('jobs destruction metrics completed')
  }
  
  if(job_count){ 
    temp_count <- job_count(data,geolevels,topic_name) 
    print('jobs count metrics completed')
  }
  
  
  # combine all metrics and save the final data
  output <- rbind(temp_jc,temp_jd,temp_count)
  
  # name the data
  data_name <- paste0(prefix,"_")
  if ("county" %in% geolevels) {data_name <- paste0(data_name,'ct')}
  if ("tract" %in% geolevels) {data_name <- paste0(data_name,'tr')}
  if ("block group" %in% geolevels) {data_name <- paste0(data_name,'bg')}
  data_name <- paste0(data_name,"_mi_",min(output$year),'_',max(output$year),'_employment_metrics_by_',topic,'.csv.xz')
  
  # save in a distribution folder
  if(savepath=='default') {savepath <- paste0("Employment/",topic,"/data/distribution/") }
  readr::write_csv(output, xzfile(paste0(savepath,data_name), compression = 9))
  print('save data completed')
  
  # return the data saved
  return(output)
}


entry <- function(data,geolevels,topics){
  # this function compute business entry metrics: count of new businesses, percent of new businesses (entry-rate),
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(total_business=length(duns),
                new_business=sum(entry, na.rm=T),
                entry_rate=100*new_business/total_business) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,new_business,entry_rate) %>%
      pivot_longer(!c('geoid','year','topic'), names_to='measure', values_to='value') %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



exit <- function(data,geolevels,topics){
  # this function compute business exit metrics: count of closed businesses, percent of closed businesses (entry-rate),
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(total_business=length(duns),
                exit_business=sum(exit, na.rm=T),
                exit_rate=100*exit_business/total_business) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,exit_business,exit_rate) %>%
      pivot_longer(!c('geoid','year','topic'), names_to='measure', values_to='value') %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



count <- function(data,geolevels,topics){
  # this function compute count the number of businesses by geolevels and topics
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(measure='number_business',
                value=length(duns)) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,measure,value) %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



size <- function(data,geolevels,topics){
  # this function count small and sole-proprietor businesses
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(total_business=length(duns),
                small_business=sum(small, na.rm=T),
                soloproprio_business=sum(sole_proprietor, na.rm=T),
                perc_small=100*small_business/total_business,
                perc_soloproprio=100*soloproprio_business/total_business) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,small_business,soloproprio_business,perc_small,perc_soloproprio) %>%
      pivot_longer(!c('geoid','year','topic'), names_to='measure', values_to='value') %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



job_creation <- function(data,geolevels,topics){
  # this function count the total jobs create by firms type
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(duns) %>%
      arrange(desc(year), .by_group=TRUE) %>%
      mutate(employment_diff = -c(NA, diff(employment)))%>%
      ungroup() %>%
      filter((entry==1)|(employment_diff>0)) %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(business_create_job=length(duns),
                job_creation_new=sum(entry*employment, na.rm=T),
                job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
                total_job_creation=job_creation_new+job_creation_active,
                perc_job_creation_new=100*job_creation_new/total_job_creation,
                perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,business_create_job,job_creation_new,job_creation_active,total_job_creation,perc_job_creation_new,perc_job_creation_active) %>%
      pivot_longer(!c('geoid','year','topic'), names_to='measure', values_to='value') %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



job_destruction <- function(data,geolevels,topics){
  # this function count the total jobs create by firms type
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(duns) %>%
      arrange(desc(year), .by_group=TRUE) %>%
      mutate(employment_diff = -c(NA, diff(employment)))%>%
      ungroup() %>%
      filter((exit==1)|(employment_diff<0)) %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(business_destruction_job=length(duns),
                job_destruction_exit=sum(exit*employment, na.rm=T),
                job_destruction_active=-sum((1-exit)*employment_diff, na.rm=T),
                total_job_destruction=job_destruction_exit+job_destruction_active,
                perc_job_destruction_exit=100*job_destruction_exit/total_job_destruction,
                perc_job_destruction_active=100*job_destruction_active/total_job_destruction) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,business_destruction_job,job_destruction_exit,job_destruction_active,total_job_destruction,perc_job_destruction_exit,perc_job_destruction_active) %>%
      pivot_longer(!c('geoid','year','topic'), names_to='measure', values_to='value') %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



job_count <- function(data,geolevels,topics){
  # this function count the total jobs create by firms type
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned_','non_minority_owned_'),
           aggregate='') %>%
    tidyr::unite("topic", topics, sep = "", remove = FALSE)
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(!!!syms(geoid),year,topic) %>%
      summarize(measure='total_employment',
                value=sum(employment, na.rm=T)) %>%
      select(geoid=c(!!!syms(geoid)),year,topic,measure,value) %>%
      mutate(measure=paste0(topic,measure),
             moe='') %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe)
    
    output <- rbind(output,temp)
  }
  
  # return the final data
  return(output)
}



industry_concentration <- function(data,prefix){
  # This metric measure the degree of industry competition within a county using the Herfindalh Hirschman index. 
  # Herfindalh Hirschman index measures measure the concentration of employment share by few firms. values range from [0-10000]
  # inputs: 
  #       data - microdata used to compute metrics
  #       group - businesses groups of interest. values are c('all','minority')
  # outputs: final dataset with all the metrics by industry
  # 
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           industry=paste0(naics_name,'_'),
           geoid=substr(geoid,1,5))
  
  # compute metrics
  output <- data %>%
    group_by(geoid,year,industry) %>%
    summarise(emp_industry = sum(employment),
              measure = 'Herfindalh_Hirschman_index',
              value = round(sum( (100*employment/emp_industry)^2, na.rm=T),2) , .groups='keep') %>%
    select(geoid,year,industry,measure,value) %>%
    mutate(measure=paste0(industry,measure),
           moe='') %>%
    ungroup() %>%
    filter(!is.na(value)) %>%
    select(geoid,year,measure,value,moe)
  
  # name the data. the index is only computed at the county level
  data_name <- paste0(prefix,"_ct")
  data_name <- paste0(data_name,"_mi_",min(output$year),'_',max(output$year),'_herfindalh_index_by_industry.csv.xz')
  
  # save in a distribution folder
  savepath <- paste0("Employment/Industry/data/distribution/")
  readr::write_csv(output, xzfile(paste0(savepath,data_name), compression = 9))
  print('save data completed')
  
  # return the data saved
  return(output)
  print('Herfindalh Hirschman index completed')
}



location_quotient <- function(data,geolevels){
  # this function estimate the industry location quotient. 
  # It measure the employment share of that industry in an area relative to the employment share of that industry in the country
  # inputs: 
  #       data - microdata used to compute metrics
  #       geolevels - vector of census geo-levels of interest. values are c('tract', 'block group', 'county')
  #       topics - topic relevant for study. values are c('industry','minority')
  # outputs: final dataset with all the metrics by topics and geolevels
  #       
  
  # data treatment. build all census geo, and also combine all topics (if there is multiple topics)
  data <- data %>% 
    mutate(geoid=as.character(geoid),
           geoid_blockgroup= geoid,
           geoid_tract=substr(geoid,1,11),
           geoid_county=substr(geoid,1,5),
           industry=paste0(naics_name,'_'),
           minority=if_else(minority==1,'minority_owned','non_minority_owned'))
  
  # build the data for each census geolevels.
  output <- NULL
  for (geo in geolevels){
    # remove space if there is one in geolevels
    geo <- str_replace(geo,' ','')
    geoid <- paste0('geoid_',geo)
    
    # compute metrics
    temp <- data %>%
      group_by(geoid_county,year) %>%
      mutate(emp_cnty=sum(employment, na.rm=T)) %>%
      group_by(geoid_county,industry,year) %>%
      mutate(emp_cnty_ind=sum(employment, na.rm=T)) %>%
      group_by(!!!syms(geoid),year) %>%
      mutate(emp_geo=sum(employment, na.rm=T)) %>%
      group_by(!!!syms(geoid),industry,year) %>%
      summarise(emp_geo=mean(emp_geo),
                emp_cnty_ind=mean(emp_cnty_ind),
                emp_cnty=mean(emp_cnty),
                emp_geo_ind=sum(employment, na.rm=T),
                share_ind_cnty = emp_cnty_ind/emp_cnty,
                share_ind_geo = emp_geo_ind/emp_geo,
                value=round((share_ind_geo)/(share_ind_cnty),2),
                measure='Location_quotient') %>%
      select(geoid=c(!!!syms(geoid)),year,industry,measure,value) %>%
      mutate(measure=paste0(industry,measure),
             moe='')%>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      select(geoid,year,measure,value,moe) 
    
    # combine all data
    output <- rbind(output,temp)
  }
  
  # name the data. the index is only computed at the county level
  data_name <- paste0(prefix,"_")
  if ("county" %in% geolevels) {data_name <- paste0(data_name,'ct')}
  if ("tract" %in% geolevels) {data_name <- paste0(data_name,'tr')}
  if ("block group" %in% geolevels) {data_name <- paste0(data_name,'bg')}
  data_name <- paste0(data_name,"_mi_",min(output$year),'_',max(output$year),'_location_quotient_by_industry.csv.xz')
  
  # save in a distribution folder
  savepath <- paste0("Employment/Industry/data/distribution/")
  readr::write_csv(output, xzfile(paste0(savepath,data_name), compression = 9))
  print('save data completed')
  
  # return the data saved
  return(output)
  print('Location quotient completed')
}

