
# general population
print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")
dpop_gpop <- collect_denominator_pops(
  db = db,
  cdm_database_schema = cdm_database_schema,
  study_days_prior_history = 365,
  study_start_date = as.Date("2007-01-01"),
  study_sex_stratas = c("Male", "Female", "Both"),
  study_age_stratas = list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  )
)

print(paste0("- Got denominator: general population"))
info(logger, "- Got denominator: general population")

print(paste0("- Getting incidence: general population"))
info(logger, "- Getting incidence: general population")
denominator <- dpop$denominator_populations
outcome_ids <- unique(outcome_cohorts$cohort_definition_id)
inc <- collect_pop_incidence(db,
  results_schema_outcomes = results_database_schema,
  table_name_outcomes = outcome_table_name,
  study_denominator_pop = denominator,
  cohort_ids_denominator_pops = "1",
  cohort_ids_outcomes = outcome_ids,
  time_intervals = "Years",
  repetitive_events = FALSE,
  outcome_washout_windows = NULL,
  confidence_interval = "none",
  minimum_cell_count = 5
)
print(paste0("- Got incidence: general population"))
info(logger, "- Got incidence: general population")


# within strata
print(paste0("- Getting denominator: strata"))
info(logger, "- Getting denominator: strata")
dpop_strata <- collect_denominator_pops(
  db = db,
  cdm_database_schema = cdm_database_schema,
  study_days_prior_history = 365,
  study_start_date = as.Date("2007-01-01"),
  study_sex_stratas = c("Male", "Female", "Both"),
  study_age_stratas = list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  strata_schema = results_database_schema,
  table_name_strata = strata_table_name,
  strata_cohort_id = "1"
)

print(paste0("- Got denominator: strata"))
info(logger, "- Got denominator: strata")

print(paste0("- Getting incidence: strata"))
info(logger, "- Getting incidence: strata")
denominator <- dpop$denominator_populations
outcome_ids <- unique(outcome_cohorts$cohort_definition_id)
inc <- collect_pop_incidence(db,
                             results_schema_outcomes = results_database_schema,
                             table_name_outcomes = outcome_table_name,
                             study_denominator_pop = denominator,
                             cohort_ids_denominator_pops = "1",
                             cohort_ids_outcomes = outcome_ids,
                             time_intervals = "Years",
                             repetitive_events = FALSE,
                             outcome_washout_windows = NULL,
                             confidence_interval = "none",
                             minimum_cell_count = 5
)
print(paste0("- Got incidence: strata"))
info(logger, "- Got incidence: strata")