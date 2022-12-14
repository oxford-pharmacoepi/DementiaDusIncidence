# add code for getting denominator and estimating incidence below

# dementia population  -----
print(paste0("- Getting denominator: dementia population"))
info(logger, "- Getting denominator: dementia population")
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  strataTable = strata_table_name ,
  strataCohortId = 1 ,
  ageGroups =list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365,
  verbose = TRUE
)

#cdm$denominator %>% tally() # to check numbers in denominator population
#attrition(cdm$denominator) # to grab the attrition

print(paste0("- Got denominator: dementia population"))
info(logger, "- Got denominator: dementia population")


# Estimate incidence -------
print(paste0("- Getting incidence: dementia population"))
info(logger, "- Getting incidence: dementia population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5
)


print(paste0("- Got incidence: dementia population"))
info(logger, "- Got incidence: dementia population")


# Get the results ----------------
print(paste0("- Gathering incidence results: dementia population"))
info(logger, "- Gathering incidence results: dementia population")

# remotes::install_github("darwin-eu-dev/IncidencePrevalence@update_attrition")
# 
# study_results <- gatherIncidencePrevalenceResults(
#   resultList=list(inc),
#   outcomeCohortId = 1,
#   outcomeCohortName = "donepezil",
#   databaseName = db.name)
# # 
# dplyr::glimpse(study_results$incidence_estimates)


# study_results<- gatherResults(prevalence_estimates, 
#                              outcomeCohortId = outcome_cohorts$cohortId,
#                              outcomeCohortName = outcome_cohorts$cohortName, 
#                              databaseName = db_name)



print(paste0("- Got incidence results: dementia population"))
info(logger, "- Got incidence results: dementia population")

# Export the results -----
print(paste0("- Exporting incidence results: dementia population"))
info(logger, "- Exporting incidence results: dementia population")

# exportIncidencePrevalenceResults(result=study_results, 
#                                  zipName="example_results",
#                                  outputFolder=here::here()) 


print(paste0("- Exported incidence results: dementia population"))
info(logger, "- Exported incidence results: dementia population")

