# Outcome cohorts ----
# Get cohort details
cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "OutcomeCohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "OutcomeCohorts",
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
                                   generateStats=FALSE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

if(create_outcome_cohorts==TRUE){
  print(paste0("- Getting outcome cohorts"))
  info(logger, "- Getting outcome cohorts")
# Names of tables to be created during study run
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = outcome_table_name)

# Create the tables in the database
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  connection = NULL)
} else {
  
  print(paste0("- Outcome cohorts skipped"))
  info(logger, "- Outcome cohorts skipped")
  
}





# strata cohorts ----
# Get cohort details
cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "StrataCohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "StrataCohorts",
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
                                   generateStats=FALSE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

if(create_strata_cohorts==TRUE){
  print(paste0("- Getting strata cohorts"))
  info(logger, "- Getting strata cohorts")
  # Names of tables to be created during study run
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = strata_table_name)
  
  # Create the tables in the database
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortTableNames = cohortTableNames,
                                      cohortDatabaseSchema = results_database_schema)
  
  # Generate the cohort set
  CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTableNames = cohortTableNames,
                                     cohortDefinitionSet = cohortDefinitionSet)
  
  # drop cohort stats table
  CohortGenerator::dropCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    connection = NULL)
} else {
  
  print(paste0("- Strata cohorts skipped"))
  info(logger, "- Strata cohorts skipped")
  
}























