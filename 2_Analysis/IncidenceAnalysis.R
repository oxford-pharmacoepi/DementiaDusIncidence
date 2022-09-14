
# first get the denominator population
print(paste0("- Getting denominator"))
info(logger, "- Getting denominator")
dpop <- collect_denominator_pops(db = db,
                         cdm_database_schema = cdm_database_schema,
                         study_days_prior_history = 365,
                         study_start_date= as.Date("2007-01-01"),
                         study_sex_stratas = c("Male","Female", "Both"),
                         study_age_stratas = list(c(40,150),
                                c(40,65),
                                c(65,150)))

print(paste0("- Got denominator"))
info(logger, "- Got denominator")

print(paste0("- Getting incidence"))
info(logger, "- Getting incidence")
denominator <- dpop$denominator_populations
inc <- collect_pop_incidence(db,
                             results_schema_outcomes = results_database_schema,
                             table_name_outcomes = outcome_table_name,
                             study_denominator_pop = denominator,
                             cohort_ids_denominator_pops = "1",
                             cohort_ids_outcomes=outcome_cohorts$cohort_definition_id,
                             time_intervals="Years",
                             repetitive_events=FALSE,
                             outcome_washout_windows=NULL,
                             confidence_interval = "none",
                             minimum_cell_count = 5
                             )
print(paste0("- Got incidence"))
info(logger, "- Got incidence")





