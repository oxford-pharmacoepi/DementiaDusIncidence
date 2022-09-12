
# first get the denominator population
print(paste0("- Getting denominator"))
info(logger, "- Getting denominator")
dpop <- collect_denominator_pops(db = db,
                         cdm_database_schema = cdm_database_schema,
                         study_age_stratas = list(c(50,150)))
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
                             minimum_cell_count = 0
                             )
print(paste0("- Got incidence"))
info(logger, "- Got incidence")





