if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

start<-Sys.time()
# extra options for running -----
# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.exposure.cohorts<-TRUE

# to capture mortality 
mortality.captured <- FALSE

# to run for just one exposure/ outcome pair
run.as.test<-FALSE

# run main exposure/ outcome pairs only
run.main.analyses.only<-FALSE

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"


# link to db tables -----
person_db<-tbl(db, sql(paste0("SELECT * FROM ",
                              cdm_database_schema,
                              ".person")))
observation_period_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                          cdm_database_schema,
                                          ".observation_period")))
visit_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        cdm_database_schema,
                                        ".visit_occurrence")))
condition_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                            cdm_database_schema,
                                            ".condition_occurrence")))
drug_era_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                cdm_database_schema,
                                ".drug_era")))
concept_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept")))
concept_ancestor_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_ancestor")))

if(mortality.captured==TRUE){
death_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                cdm_database_schema,
                                ".death")))
}

# result table names ----
cohortTableExposures<-paste0(cohortTableStem)

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

Pop<-person_db %>% 
  inner_join(exposure.cohorts_db,
             by = c("person_id" = "subject_id" )) %>%
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date,
         cohort_definition_id)  %>% 
  left_join(observation_period_db %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
            by = "person_id") %>% 
  collect()


# Run analysis ----
info(logger, 'RUNNING ANALYSIS')
source(here("2_Analysis","Analysis.R"))
info(logger, 'ANALYSIS RAN')



print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
# readLines(log_file)

