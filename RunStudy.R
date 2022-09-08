if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

start<-Sys.time()
# extra options for running -----
# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.outcome.cohorts<-TRUE

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
cohortTableOutcomes<-paste0(cohortTableStem)

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# study cohorts ----

# study.cohorts<-bind_rows(
#   exposure.cohorts %>% 
#   select(id, name),
#   data.frame(
#   name=c("Any first-dose",
#   "Any full-dose",
#   "Viral vector first-dose",
#   "mRNA first-dose",
#   "Viral vector full-dose",
#   "mRNA full-dose",
#   "mRNA second-dose after viral vector first-dose")) %>% 
#   mutate(id=((max(exposure.cohorts$id)+1):c(max(exposure.cohorts$id)+7))))
#   
# 
# # get earliest vaccine ----
# earliest.date<-exposure.cohorts_db %>% 
#   filter(cohort_definition_id!="301") %>% 
#   summarise(vax.start.date=min(cohort_start_date, na.rm=TRUE)) %>% 
#   collect() %>% pull() 
# earliest.date<-dmy(format(earliest.date, "%d/%m/%Y"))
# 
# # get database end date -----
# db.end.date<-observation_period_db %>% 
#     summarise(max(observation_period_end_date, na.rm=TRUE)) %>% 
#     collect() %>%  pull()
# db.end.date<-dmy(format(db.end.date, "%d/%m/%Y"))

# Run analysis ----
info(logger, 'RUNNING ANALYSIS')
source(here("2_Analysis","Analysis.R"))
info(logger, 'ANALYSIS RAN')

# Tidy up and save ----
Survival.summary<-bind_rows(Survival.summary, .id = NULL)
Survival.summary$db<-db.name
Survival.summary<-Survival.summary %>% 
  group_by(group, strata, outcome,pop, pop.type,
           outcome.name,prior.obs.required, surv.type) %>% 
  mutate(cum.n.event=cumsum(n.event))

Cohort.age.plot.data<-bind_rows(Cohort.age.plot.data, .id = NULL)


save(Patient.characteristcis, 
     file = paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"))
save(Survival.summary, 
     file = paste0(output.folder, "/Survival.summary_", db.name, ".RData"))
save(Model.estimates, 
     file = paste0(output.folder, "/Model.estimates_", db.name, ".RData"))
save(Cohort.age.plot.data, 
     file = paste0(output.folder, "/Cohort.age.plot.data_", db.name, ".RData"))

# Time taken
x <- abs(as.numeric(Sys.time()-start, units="secs"))
info(logger, paste0("Study took: ", 
                    sprintf("%02d:%02d:%02d:%02d", 
                            x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/% 
                              60,  x %% 60 %/% 1)))

# # zip results
print("Zipping results to results folder")
unlink(paste0(output.folder, "/OutputToShare_", db.name, ".zip"))
zipName <- paste0(output.folder, "/OutputToShare_", db.name, ".zip")

files<-c(log_file,
         paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"),
         paste0(output.folder, "/Survival.summary_", db.name, ".RData"),
         paste0(output.folder, "/Model.estimates_", db.name, ".RData") ,
         paste0(output.folder, "/Cohort.age.plot.data_", db.name, ".RData") )
files <- files[file.exists(files)==TRUE]
createZipFile(zipFile = zipName,
              rootFolder=output.folder,
              files = files)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
# readLines(log_file)

