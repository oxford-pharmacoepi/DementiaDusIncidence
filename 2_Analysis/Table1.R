# incidence overall - for participants -----
# going to return participants so we can use them for patient profiles
overall_denominator_id <- cohortSet(cdm$denominator) %>% 
  filter(age_group == "40;150") %>% 
  filter(sex == "Both") %>% 
  pull(cohort_definition_id)

#grab the incidence for overall for the outcome cohorts
inc_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  denominatorCohortId = overall_denominator_id,
  outcomeTable = outcome_table_name,
  interval = "overall",
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE,
  returnParticipants = TRUE,
  tablePrefix = paste0(outcome_table_stem, "_overall_inc")
)

#grabs the outcomes from the inc_overall analysis
outcomes <- incidenceSet(inc_overall) %>% 
  arrange(analysis_id)

# get BMI no longer in use
# remove any records after the study date and remove any BMI outside of normal ranges
# will do this once as takes time to compute
# BMI <- cdm$measurement %>%
#   filter(measurement_concept_id == 3038553) %>%
#   filter(measurement_date < enddate) %>%
#   filter(value_as_number >= 15) %>% 
#   filter(value_as_number< 60) %>% 
#   collect()

drugs_names <- c("anyAntiDementiaDrugUser",
                 "galantamine",
                 "rivastigmine",
                 "memantine",
                 "donepezil")

# grab the characteristics for each outcome
characteristics <- list()
#for(i in seq_along(outcomes$outcome_cohort_name) ){
for(i in seq_along(outcome_cohorts$cohort_name) ){
  cdm$working_participants <- participants(inc_overall, i) %>% 
    select("subject_id", "outcome_start_date") %>% 
    filter(!is.na(outcome_start_date)) %>% 
    rename("cohort_start_date" = "outcome_start_date")
  cdm$working_participants <- cdm$working_participants %>% 
    addDemographics(cdm) %>% 
    flagCohortPresence(cdm = cdm,
                       targetCohortTable = feature_disease_table_name,
                       window = c(-Inf, 0), 
                       nameStyle = "{cohort_name}") %>% 
    flagCohortPresence(cdm = cdm,
                       targetCohortTable = feature_medication_table_name,
                       window = c(-90, 0), 
                       nameStyle = "{cohort_name}") 
  
  
  working_participants<- cdm$working_participants %>% 
    collect() 
  
  # calculate the charlson
  # update cohorts with different weights
working_participants <- working_participants %>% 
    mutate(diabetes_with_chronic_complications = ifelse("diabetes_with_chronic_complications" %in% names(.), diabetes_with_chronic_complications, 0)) %>% 
    mutate(hemoplegia_or_paralegia = ifelse("hemoplegia_or_paralegia" %in% names(.), hemoplegia_or_paralegia, 0)) %>% 
    mutate(renal_disease = ifelse("renal_disease" %in% names(.), renal_disease, 0)) %>% 
    mutate(any_malignancy = ifelse("any_malignancy" %in% names(.), any_malignancy, 0)) %>% 
    mutate(moderate_to_severe_liver_disease = ifelse("moderate_to_severe_liver_disease" %in% names(.), moderate_to_severe_liver_disease, 0)) %>% 
    mutate(metastatic_solid_tumor = ifelse("metastatic_solid_tumor" %in% names(.), metastatic_solid_tumor, 0)) %>% 
    mutate(aids = ifelse("aids" %in% names(.), aids, 0)) %>% 
    mutate(diabetes_with_chronic_complications = ifelse(diabetes_with_chronic_complications == 1, 2, 0)) %>%
    mutate(hemoplegia_or_paralegia = ifelse(hemoplegia_or_paralegia == 1, 2, 0)) %>%
    mutate(renal_disease = ifelse(renal_disease == 1, 2, 0)) %>%
    mutate(any_malignancy = ifelse(any_malignancy == 1, 2, 0)) %>%
    mutate(moderate_to_severe_liver_disease = ifelse(moderate_to_severe_liver_disease == 1, 3, 0)) %>%
    mutate(metastatic_solid_tumor = ifelse(metastatic_solid_tumor == 1, 6, 0)) %>% 
    mutate(aids = ifelse(aids == 1, 6, 0)) 

  #summing the charlson components
  working_participants <- working_participants %>% 
    mutate(charlson = myocardial_infarction + 
             congestive_heart_failure + 
             cerebrovascular_disease + 
             dementia_charlson + 
             chronic_pulmonary_disease +
             rheumatologic_disease +
             peptic_ulcer_disease +
           mild_liver_disease +
           diabetes_with_chronic_complications +
           hemoplegia_or_paralegia +
           renal_disease +
           any_malignancy +
           moderate_to_severe_liver_disease +
           metastatic_solid_tumor +
           aids )
  
#create a categorical value for charlson
working_participants <- working_participants %>% 
    mutate(charlson=ifelse(is.na(charlson),0,charlson))
# categorise charlson ----
working_participants <- working_participants %>% 
    mutate(charlson_cat =
             ifelse(charlson==0, "0",
                    ifelse(charlson==1, "1",
                           ifelse(charlson==2, "2",
                                  ifelse(charlson>=3, "3+", NA )))))


# join working_participants with BMI no longer in use for study
# BMI_results <- working_participants %>% 
#   left_join(BMI %>% 
#               select(person_id, value_as_number,measurement_date) %>%
#               rename(bmi.all_time=value_as_number,
#                      bmi.measurement_date=measurement_date), by = c("subject_id" = "person_id")) %>% 
#   mutate(BMI_valid = cohort_start_date > bmi.measurement_date ) %>% 
#   filter(BMI_valid == TRUE) # remove any BMI values after the cohort start date
#   
# #get the closest value to the cohort start date
# BMI_results <- BMI_results %>% 
#   arrange(subject_id, desc(bmi.measurement_date)) %>% 
#   group_by(subject_id) %>% 
#   mutate(seq=1:length(subject_id)) %>%  
#   filter(seq==1) %>% 
#   select(-seq) %>% 
#   ungroup()
# 
# # merge results back into working participants
# working_participants <- working_participants %>% 
#   left_join(BMI_results %>% 
#               select(subject_id, bmi.all_time,bmi.measurement_date), by = c("subject_id"))

# column names due to bug in inc/prev

working_table <- bind_rows(
    working_participants %>% 
      summarise(val = as.character(n())) %>% 
      mutate(var="N"),
    working_participants %>% 
      summarise(val = as.character(median(age)))  %>% 
      mutate(var="Median age"),
    working_participants %>% 
      summarise(val = as.character(median(prior_history)))  %>% 
      mutate(var="Median prior history (days)"),
    working_participants %>% 
      summarise(val = as.character(median(charlson)))  %>% 
      mutate(var="Median Charlson Index"),
    working_participants %>% 
      summarise(val = as.character(round(mean(charlson),2)))  %>% 
      mutate(var="Mean Charlson Index"),
    working_participants%>% 
      filter(sex == "Male") %>% 
      summarise(val = as.character(n()))  %>% 
      mutate(var="N male"),
    
    working_participants %>% 
      group_by(sex) %>%
      summarise(n = n()) %>%
      mutate(val = paste0(round(n / sum(n) * 100, 0), "%")) %>% 
      ungroup() %>% 
      filter(sex == "Male") %>%
      mutate(var="% Male") %>%
      select("val", "var") ,
    
    working_participants %>% 
      filter(sex == "Female") %>% 
      summarise(val = as.character(n()))  %>% 
      mutate(var="N Female") ,
  
    working_participants %>% 
      group_by(sex) %>%
      summarise(n = n()) %>%
      mutate(val = paste0(round(n / sum(n) * 100, 0), "%")) %>% 
      ungroup() %>% 
      filter(sex == "Female") %>%
      mutate(var="% Female") %>%
      select("val", "var") 
    

    
    )

  
  f_names <- colnames(cdm$working_participants)
  f_names <- str_subset(f_names, paste(
    "subject_id", "cohort_start_date", "age", "sex", "prior_history",
    "future_observation", 
    "myocardial_infarction" , 
      "congestive_heart_failure",
      "cerebrovascular_disease" ,
      "dementia_charlson" ,
      "chronic_pulmonary_disease",
      "rheumatologic_disease",
      "peptic_ulcer_disease",
    "mild_liver_disease",
    "diabetes_with_chronic_complications",
    "hemoplegia_or_paralegia",
    "renal_disease",
    "any_malignancy",
    "moderate_to_severe_liver_disease",
    "metastatic_solid_tumor",
    "aids",
    "anyantidementiadruguser" ,
    "donepezil",
    "memantine",
    "rivastigmine" ,
    "galantamine",
    
    sep="|"), negate = TRUE)
  
  for(j in seq_along(f_names)){
    working_name <- glue::glue("{f_names[[j]]}")
    working_table <- bind_rows(working_table,
                               working_participants %>%
                                 summarise(n=sum(!!rlang::sym(working_name)),
                                           percent=(n/n())*100) %>% 
                                 mutate(val = paste0(n, " (", 
                                                     round(percent,2),
                                                     "%)")) %>% 
                                 select(!c("percent", "n")) %>% 
                                 mutate(var=working_name)
    ) 
  }
  
  characteristics[[i]] <- working_table %>% 
    rename(!!drugs_names[i]:="val")
  
}

# combine into single table
for(i in 1:(length(characteristics)-1)){
  characteristics[[1]] <- characteristics[[1]] %>% 
    left_join(characteristics[[i+1]])
}

table_characteristics <- characteristics[[1]] %>% 
  relocate("var")


# save the results
write_csv(
  table_characteristics, 
  here::here("Results", db.name, paste0("table_characteristics", cdmName(cdm), ".csv"))
)


# extra code not used
#SMOKING
# Non smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#  Pop %>%
#   select("person_id", "outcome_start_date") %>% 
#   inner_join(cdm$observation %>%
#                filter( observation_concept_id == 4222303 ) , 
#              by=c("person_id"), copy = TRUE)  %>% 
#   filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#   filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#   select(c(person_id, observation_date, outcome_start_date)) %>% 
#   distinct() %>%
#   group_by(person_id, outcome_start_date) %>%
#   filter(observation_date == max(observation_date)) %>%
#   rename("non_smoker_date"="observation_date"),
#  by= c("person_id", "outcome_start_date")) %>% 
#   compute()
# 
# # Smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>%
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$observation %>%
#                    filter( 
#                      observation_concept_id == 4141787 | # Smoking started
#                        observation_concept_id == 44789712 | # want to stop smoking
#                        observation_concept_id == 40486518 | # failed attempt to stop smoking
#                        observation_concept_id == 	4046886 | # smoking reduced 
#                        observation_concept_id == 4058137 | # tried giving up smoking
#                        observation_concept_id == 4216174 | # not interested in giving up smoking
#                        observation_concept_id == 4215409 | # Ready to stop smoking
#                        observation_concept_id == 4190573 | # Thinking about stopping smoking
#                        observation_concept_id == 4052948 | # Keeps trying to stop smoking
#                        observation_concept_id ==  4144271 | # Tobacco smoking consumption
#                        observation_concept_id == 4298794
#                      ) , 
#                  by=c("person_id"), copy = TRUE)  %>% 
#       filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#       filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#       select(c(person_id, observation_date, outcome_start_date)) %>% 
#       distinct() %>%
#       group_by(person_id, outcome_start_date) %>%
#       filter(observation_date == max(observation_date)) %>%
#       rename("Smoker_date"="observation_date"),
#     by= c("person_id", "outcome_start_date")) %>% 
#   compute()
# 
# # previous smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>%
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$observation %>%
#                    filter( 
#                      observation_concept_id == 4052032 | # stopped smoking
#                        observation_concept_id == 4052466 # date ceased smoking
#                    ) , 
#                  by=c("person_id"), copy = TRUE)  %>% 
#       filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#       filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#       select(c(person_id, observation_date, outcome_start_date)) %>% 
#       distinct() %>%
#       group_by(person_id, outcome_start_date) %>%
#       filter(observation_date == max(observation_date)) %>%
#       rename("PrevSmoker_date"="observation_date"),
#     by= c("person_id", "outcome_start_date")) %>% 
#   compute()


# # remove smokers who are now previous smokers
# Pop <- Pop %>%
#   mutate(Smoker_date = replace(Smoker_date, Smoker_date < PrevSmoker_date, NA))
# 
# # replace the dates with text and remove the dates
# Pop <- Pop %>%
#   mutate(SmokingStatus = NA,
#          SmokingStatus = replace(SmokingStatus, !is.na(Smoker_date), "Smoker"),
#          SmokingStatus = replace(SmokingStatus, !is.na(PrevSmoker_date), "Previous Smoker"),
#          SmokingStatus = replace(SmokingStatus, !is.na(non_smoker_date), "Non Smoker")) %>%
#   select(!c(Smoker_date, PrevSmoker_date, non_smoker_date))


  