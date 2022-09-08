#### USING COHORT GENERATOR ######
# Get cohort details MEDICATIONS -----

if(create.medication.cohorts==TRUE){
  print(paste0("- Getting medication cohorts"))
  info(logger, "- Getting medication cohorts")

cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "MedicationCohorts", "json"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "MedicationCohorts", "json",
                     cohortJsonFiles[i])
  cohortJson <- readChar(working.json, file.info(working.json)$size)
  cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
  sql <- buildCohortQuery(cohortExpression, 
                          options = CirceR::createGenerateOptions(generateStats = TRUE))
  
  cohortDefinitionSet[[i]]<-tibble(atlasId = i,
                                   cohortId = i,
                                   cohortName = str_replace(cohortJsonFiles[i],".json",""),
                                   json=cohortJson,
                                   sql=sql,
                                   logicDescription = NA,
                                   generateStats=TRUE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

# Names of tables to be created during study run ----- 
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableStem)

# Create the tables in the database -----
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set -----
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# get stats  -----
CohortGenerator::exportCohortStatsTables(
  connectionDetails = connectionDetails,
  connection = NULL,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  cohortStatisticsFolder = here("Results"),
  incremental = FALSE)

# get the number of people in the cohort
getCohortCounts(connectionDetails = connectionDetails,
                cohortDatabaseSchema = results_database_schema,
                cohortTable = cohortTableNames$cohortTable)


# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  connection = NULL)


medication.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        results_database_schema,".",
                                        cohortTableExposures)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# dbListObjects(db) #shows results 
# dbListObjects(db, Id(schema = "results")) # states the results tables


# drop any medication cohorts with less than 5 people
medication.cohorts_db %>%
  group_by(cohort_definition_id) %>% tally()

medication.cohorts<-medication.cohorts_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) 

} else {

  print(paste0("- Medication cohorts skipped"))
  info(logger, "- Medication cohorts skipped")
  
}




# Get cohort details Comorbidity-----

if(create.comorbidity.cohorts==TRUE){
  
print(paste0("- Getting Comorbidity cohorts"))
info(logger, "- Getting Comorbidity cohorts")

cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "ComorbidityCohorts", "json"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]



cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "ComorbidityCohorts", "json",
                     cohortJsonFiles[i])
  cohortJson <- readChar(working.json, file.info(working.json)$size)
  cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
  sql <- buildCohortQuery(cohortExpression, 
                          options = CirceR::createGenerateOptions(generateStats = TRUE))
  
  cohortDefinitionSet[[i]]<-tibble(atlasId = i,
                                   cohortId = i,
                                   cohortName = str_replace(cohortJsonFiles[i],".json",""),
                                   json=cohortJson,
                                   sql=sql,
                                   logicDescription = NA,
                                   generateStats=TRUE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

# Names of tables to be created during study run ----- 
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableStem)

# Create the tables in the database -----
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set -----
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# get stats  -----
CohortGenerator::exportCohortStatsTables(
  connectionDetails = connectionDetails,
  connection = NULL,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  cohortStatisticsFolder = here("Results"),
  incremental = FALSE)

# get the number of people in the cohort
getCohortCounts(connectionDetails = connectionDetails,
                cohortDatabaseSchema = results_database_schema,
                cohortTable = cohortTableNames$cohortTable)


# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  connection = NULL)


comorbidities.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                       results_database_schema,".",
                                       cohortTableComorbidity)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# dbListObjects(db) #shows results 
# dbListObjects(db, Id(schema = "results")) # states the results tables


# drop any exposure cohorts with less than 5 people
comorbidity.cohorts_db %>%
  group_by(cohort_definition_id) %>% tally()

comorbidity.cohorts<-comorbidity.cohorts_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) 


} else {
  
  print(paste0("- Comorbidity cohorts skipped"))
  info(logger, "- Comorbidity cohorts skipped")
  
}



# Get cohort details OUTCOME-----

if(create.comorbidities.cohorts==TRUE){
  
  print(paste0("- Getting Outcome cohorts"))
  info(logger, "- Getting Outcome cohorts")
  
  cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "OutcomeCohorts", "json"))
  cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]
  
  
  
  cohortDefinitionSet <- list()
  for(i in 1:length(cohortJsonFiles)){
    working.json<-here("1_InstantiateCohorts", "OutcomeCohorts", "json",
                       cohortJsonFiles[i])
    cohortJson <- readChar(working.json, file.info(working.json)$size)
    cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
    sql <- buildCohortQuery(cohortExpression, 
                            options = CirceR::createGenerateOptions(generateStats = TRUE))
    
    cohortDefinitionSet[[i]]<-tibble(atlasId = i,
                                     cohortId = i,
                                     cohortName = str_replace(cohortJsonFiles[i],".json",""),
                                     json=cohortJson,
                                     sql=sql,
                                     logicDescription = NA,
                                     generateStats=TRUE)
  }
  cohortDefinitionSet<-bind_rows(cohortDefinitionSet)
  
  # Names of tables to be created during study run ----- 
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableStem)
  
  # Create the tables in the database -----
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortTableNames = cohortTableNames,
                                      cohortDatabaseSchema = results_database_schema)
  
  # Generate the cohort set -----
  CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTableNames = cohortTableNames,
                                     cohortDefinitionSet = cohortDefinitionSet)
  
  # get stats  -----
  CohortGenerator::exportCohortStatsTables(
    connectionDetails = connectionDetails,
    connection = NULL,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    cohortStatisticsFolder = here("Results"),
    incremental = FALSE)
  
  # get the number of people in the cohort
  getCohortCounts(connectionDetails = connectionDetails,
                  cohortDatabaseSchema = results_database_schema,
                  cohortTable = cohortTableNames$cohortTable)
  
  
  # drop cohort stats table
  CohortGenerator::dropCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    connection = NULL)
  
  
  outcome.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                               results_database_schema,".",
                                               cohortTableOutcomes)))%>% 
    mutate(cohort_definition_id=as.integer(cohort_definition_id)) 
  
  # dbListObjects(db) #shows results 
  # dbListObjects(db, Id(schema = "results")) # states the results tables
  
  
  # drop any exposure cohorts with less than 5 people
  outcome.cohorts_db %>%
    group_by(cohort_definition_id) %>% tally()
  
  outcome.cohorts<-outcome.cohorts_db %>% 
    group_by(cohort_definition_id) %>% 
    tally() %>% 
    collect() %>% 
    filter(n>5) 
  
  
} else {
  
  print(paste0("- outcome cohorts skipped"))
  info(logger, "- outcome cohorts skipped")
  
}


# disconnect ----
disconnect(conn)
