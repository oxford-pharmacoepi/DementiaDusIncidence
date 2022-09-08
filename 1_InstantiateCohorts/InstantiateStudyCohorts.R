#### USING COHORT GENERATOR ######

if(create.outcome.cohorts==TRUE){
  
  print(paste0("- Getting cohorts"))
  info(logger, "- Getting cohorts")
  
# Get cohort details -----
cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "Cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "Cohorts",
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


exposure.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        results_database_schema,".",
                                        cohortTableExposures)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# dbListObjects(db) #shows results 
# dbListObjects(db, Id(schema = "results")) # states the results tables


# drop any exposure cohorts with less than 5 people
exposure.cohorts_db %>%
  group_by(cohort_definition_id) %>% tally()

exposure.cohorts<-exposure.cohorts_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) 

} else {
  
  print(paste0("- cohorts skipped"))
  info(logger, "- cohorts skipped")
  
}





















