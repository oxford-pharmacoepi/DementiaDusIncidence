
# table names----
outcome_table_name <-paste0(outcome_table_stem,"_o")
strata_table_name <-paste0(outcome_table_stem,"_strata")
strata_table_name1 <-paste0(outcome_table_stem,"_strata1")
strata_table_name1year <-paste0(outcome_table_stem,"_strata2")
strata_table_name2year <-paste0(outcome_table_stem,"_strata3")
feature_disease_table_name <-paste0(outcome_table_stem,"_disease")
feature_medication_table_name <-paste0(outcome_table_stem,"_medication")

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

# Run incidence rate analysis ----
info(logger, 'RUNNING INCIDENCE RATE ANALYSIS')
source(here("2_Analysis","IncidenceAnalysis.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

print("Done!")
print("-- If all has worked, you can export the Results folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)

