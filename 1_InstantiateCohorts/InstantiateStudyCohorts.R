# instantiate outcome cohorts
info(logger, "- getting outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

info(logger, "- getting outcomes")


cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts,
                         cohortTableName = outcome_table_name,
                         overwrite = TRUE
)


info(logger, "- got outcomes")

# instantiate strata cohorts
info(logger, "- getting strata definitions")
strata_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "StrataCohorts"
))

info(logger, "- getting strata")
cdm <- generateCohortSet(cdm, strata_cohorts,
                         cohortTableName = strata_table_name,
                         overwrite = TRUE
)


info(logger, "- got strata")

info(logger, "- getting strata updated definitions")
strata_cohorts1 <- readCohortSet(here(
  "1_InstantiateCohorts",
  "StrataCohorts",
  "DrugDiagDem"
))

info(logger, "- getting strata updated updated")
cdm <- generateCohortSet(cdm, strata_cohorts1,
                         cohortTableName = strata_table_name1,
                         overwrite = TRUE
)


info(logger, "- got strata updated")

#instantiate feature cohorts (disease)
info(logger, "- getting feature for diseases definitions")

disease_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "DiseaseCohorts"
))

info(logger, "- getting features: diseases")

cdm <- generateCohortSet(cdm, disease_cohorts,
                         cohortTableName = feature_disease_table_name,
                         overwrite = TRUE
)

info(logger, "- got features for diseases")

# instantiate feature cohorts (medications)
info(logger, "- getting feature for medications definitions")

medication_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "MedicationCohorts"
))

info(logger, "- getting features: medications")

cdm <- generateCohortSet(cdm, medication_cohorts,
                         cohortTableName = feature_medication_table_name,
                         overwrite = TRUE
)

info(logger, "- got features for medications")

