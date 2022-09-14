
# table names----
outcome_table_name<-paste0(outcome_table_stem,"_Outcomes")
strata_table_name<-paste0(outcome_table_stem,"_Strata")

# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

outcome_cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                       results_database_schema,".",
                                       outcome_table_name)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# drop any outcome cohort with less than 5 people
outcome_cohorts_db %>%
  group_by(cohort_definition_id) %>% tally()

outcome_cohorts<-outcome_cohorts_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) 

# Run incidence rate analysis ----

# strata_id<- tbl(db, sql(paste0("SELECT * FROM ",
#                    results_database_schema,".", strata_table_name))) %>% 
#   group_by(cohort_definition_id) %>% 
#   tally() %>% 
#   select(cohort_definition_id) %>% 
#   pull()


info(logger, 'RUNNING INCIDENCE RATE ANALYSIS')
source(here("2_Analysis","IncidenceAnalysis.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

# add outcome names to analysis_settings
outcome_cohorts<-outcome_cohorts %>% 
  left_join(cohortDefinitionSet %>% 
              select("cohortId", "cohortName") %>% 
              rename("cohort_definition_id"="cohortId"))

inc$analysis_settings<-inc$analysis_settings %>% 
  left_join(cohortDefinitionSet %>% 
              select("cohortId", "cohortName") %>% 
              mutate(cohortId=as.character(cohortId)) %>%       
              rename("cohort_id_outcome"="cohortId") %>%       
              rename("outcome_name"="cohortName"),
            by="cohort_id_outcome")


# save
write.csv(inc$incidence_estimates, 
          paste0(output.folder, "/Incidence_estimates_", db.name, ".csv"))
write.csv(inc$analysis_settings, 
          paste0(output.folder, "/analysis_settings_", db.name, ".csv"))
write.csv(inc$attrition, 
          paste0(output.folder, "/attrition_", db.name, ".csv"))

# # zip results
print("Zipping results to output folder")
unlink(paste0(output.folder, "/OutputToShare_", db.name, ".zip"))
zipName <- paste0(output.folder, "/OutputToShare_", db.name, ".zip")

files<-c(log_file,
         paste0(output.folder, "/Incidence_estimates_", db.name, ".csv"),
         paste0(output.folder, "/analysis_settings_", db.name, ".csv"),
         paste0(output.folder, "/attrition_", db.name, ".csv"))
files <- files[file.exists(files)==TRUE]
createZipFile(zipFile = zipName,
              rootFolder=output.folder,
              files = files)


print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
# readLines(log_file)

