# incidence overall - for participants -----
# going to return participants so we can use them for patient profiles
overall_denominator_id <- cohortSet(cdm$denominator) %>% 
  filter(age_group == "40 to 150") %>% 
  filter(sex == "Both") %>% 
  pull(cohort_definition_id)

#grab the incidence for overall for the outcome cohorts
inc_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  denominatorCohortId = overall_denominator_id,
  outcomeTable = outcome_table_name,
  interval = "overall",
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = FALSE,
  returnParticipants = TRUE
)

drugs_names <- inc_overall$outcome_cohort_name

# grab the characteristics for each outcome
characteristics <- list()

for(i in seq_along(inc_overall$outcome_cohort_id) ){
  cdm$working_participants <- participants(inc_overall, i) %>% 
    select("subject_id", "outcome_start_date") %>% 
    filter(!is.na(outcome_start_date)) %>% 
    rename("cohort_start_date" = "outcome_start_date")
  cdm$working_participants <- cdm$working_participants %>% 
    addDemographics(cdm) %>% 
    addCohortIntersectFlag(cdm = cdm,
                       targetCohortTable = feature_disease_table_name,
                       window = c(-Inf, 0), 
                       nameStyle = "{cohort_name}") %>% 
    addCohortIntersectFlag(cdm = cdm,
                       targetCohortTable = feature_medication_table_name,
                       window = c(-90, 0), 
                       nameStyle = "{cohort_name}") 
  
  
  working_participants<- cdm$working_participants %>% 
    collect() 
  
  # calculate the charlson
  # update cohorts with different weights
  if(!'diabetes_with_chronic_complications' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(diabetes_with_chronic_complications = 0)
  if(!'hemoplegia_or_paralegia' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(hemoplegia_or_paralegia = 0)
  if(!'renal_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(renal_disease = 0)
  if(!'any_malignancy' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(any_malignancy = 0)
  if(!'moderate_to_severe_liver_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(moderate_to_severe_liver_disease = 0)
  if(!'metastatic_solid_tumor' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(metastatic_solid_tumor = 0)
  if(!'aids' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(aids = 0)
  if(!'mild_liver_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(mild_liver_disease = 0)
  if(!'cerebrovascular_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(cerebrovascular_disease = 0)
  if(!'myocardial_infarction' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(myocardial_infarction = 0)
  if(!'congestive_heart_failure' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(congestive_heart_failure = 0)
  if(!'dementia_charlson' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(dementia_charlson = 0)
  if(!'chronic_pulmonary_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(chronic_pulmonary_disease = 0)
  if(!'rheumatologic_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(rheumatologic_disease = 0)
  if(!'peptic_ulcer_disease' %in% names(working_participants)) working_participants <- working_participants %>% tibble::add_column(peptic_ulcer_disease = 0)

working_participants <- working_participants %>% 
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

  