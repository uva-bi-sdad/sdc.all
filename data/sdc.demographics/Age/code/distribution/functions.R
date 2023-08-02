prepare_combine <- function(topic) {
  #' combine demographic distribution data from each model
  #' param: topic(character); topic to combine data under
  #' return: none - creates a new, aggregated data file
  
  # get distribution data from each model
  uploadpath <- paste0(topic, "/data/working/model/")
  files <- list.files(uploadpath)
  
  # combine data into one file
  combined <- NULL
  for (file in files) {
    data <- read.csv(paste0(uploadpath, file))
    combined <- rbind(combined, data)
  }
  
  # save to distribution
  yearlist <- unique(combined$year)
  savepath <- paste0(topic, "/data/distribution/")
  readr::write_csv(combined, xzfile(paste0(savepath, "va_hsrsdpdzccttrbg_sdad_",
                                           min(yearlist),"_",max(yearlist),"_",
                                           tolower(topic), 
                                           "_demographics.csv.xz"), 
                                    compression = 9))
}

run_data_prep <- function(topic) {
  #' runs files that get and clean data
  #' param: topic(character); topic to combine data under
  #' return: none - gathers, cleans, and saves data
  
  runpath <- paste0(topic, "/code/distribution/")
  
  # files are dependent on each others outputs --> 
  #    must be run in this order to update properly
  source(paste0(runpath, 'ingest_acs.R'))
  print('ingest done')
  
  source(paste0(runpath, 'prepare_direct_expansion_model.R'))
  print('direct done')
  
  source(paste0(runpath, 'prepare_refine_expansion_model.R'))
  print('parcels done')
  
  # combine data from models
  prepare_combine(topic)
  print('combine done')
}

update_measure_names <- function(path, filename, changes, save_new = NULL) {
  #' updates names of measures in measure col of a dataframe
  #' param: path(character): file path
  #' param: filename(character): name of file to update
  #' param: changes(named vector): maps old measure names to new measure names
  #' param: save_new(character): optional, default none - 
  #'                             name to save updated file as. if NULL, will
  #'                             overwrite the old file path
  #' return: none - updates file to new measure names 
  #'                (default save over old file)
  
  df <- read.csv(paste0(path, filename))
  
  `%>%` <- magrittr::`%>%` # defines operator
  
  # update measure names
  for (name in names(changes)) {
    df <- df %>% 
      mutate(measure=case_when(measure==name ~ changes[name],
                               .default = measure))
  }
  
  # default overwrite old file
  if (is.null(save_new)) {
    readr::write_csv(df, xzfile(paste0(path, filename), compression = 9))
  }
  
  # else save as new file
  else {
    readr::write_csv(df, xzfile(paste0(path, save_new), compression = 9))
  }
  
}
