# grab the analysis ID's for drug populations in dementia strata
settings_table1 <- settings(inc) %>%
  filter(analysis_interval == "overall" & denominator_cohort_id == 3 )


pops <- list()

for (i in 1:length(settings_table1$analysis_id)){
  #extract the participants for each outcome
  pops[[i]] <-cdm$person %>%
    inner_join(participants(inc, analysisId = settings_table1$analysis_id[i]),
               by = c("person_id" = "subject_id" ), copy = TRUE) %>%
    select(person_id,gender_concept_id,
           year_of_birth, month_of_birth, day_of_birth,
           cohort_start_date,
           cohort_end_date,
           outcome_start_date,
           analysis_id)  %>%
    left_join(cdm$observation_period %>%
                select("person_id",  "observation_period_start_date", "observation_period_end_date") %>%
                distinct(),
              by = "person_id") %>%
    left_join(cdm$death %>%
                select("person_id",  "death_date") %>%
                distinct(),
              by = "person_id") %>%
    collect()
  
  pops[[i]] <- pops[[i]]  %>%
    mutate(outcome_cohort_name = settings_table1$outcome_cohort_name[i]) %>%
    mutate(outcome_cohort_id = settings_table1$outcome_cohort_id[i])
  
}

Pop <- dplyr::bind_rows(pops) %>%
  mutate(Outcome = !is.na(outcome_start_date))

# subset denominator to those who do not have an outcome (i.e. no drug)
PopDenom <- Pop %>% filter(Outcome == FALSE)

# subset the population to those who have an outcome
Pop <- Pop %>% filter(Outcome == TRUE)

# format data -----
#add age -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month
  Pop <-Pop %>%
    mutate(age=floor(as.numeric((ymd(outcome_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else {
  Pop <- Pop %>%
    mutate(age= lubridate::year(outcome_start_date)-year_of_birth)
}

# # age age groups ----
Pop <- Pop %>%
  mutate(age_gr=ifelse(age<30,  "18-29",
                       ifelse(age>=30 &  age<=39,  "30-39",
                              ifelse(age>=40 & age<=49,  "40-49",
                                     ifelse(age>=50 & age<=59,  "50-59",
                                            ifelse(age>=60 & age<=69, "60-69",
                                                   ifelse(age>=70 & age<=79, "70-79",
                                                          ifelse(age>=80 & age<=89, "80-89",
                                                                 ifelse(age>=90, ">=90",
                                                                        NA))))))))) %>%
  mutate(age_gr= factor(age_gr,
                        levels = c("18-29","30-39","40-49", "50-59",
                                   "60-69", "70-79","80-89",">=90")))
table(Pop$age_gr, useNA = "always")

# # reformat gender
# # add gender -----
# #8507 male
# #8532 female
Pop <-Pop %>%
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>%
  mutate(gender= factor(gender,
                        levels = c("Male", "Female")))
table(Pop$gender, useNA = "always")

# # if missing (or unreasonable) age or gender, drop ----
Pop <-Pop %>%
  filter(!is.na(age)) %>%
  filter(age>=18) %>%
  filter(age<=110) %>%
  filter(!is.na(gender))


# medications
for(i in seq_along(medication_cohorts$cohortId)){
  working_name <- glue::glue("{medication_cohorts$cohortName[[i]]}")
  working_id <- medication_cohorts$cohortId[[i]]
  Pop <-
    Pop %>%
    left_join(
      Pop %>%
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm[[feature_medication_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     rename("feature_end_date"="cohort_end_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!cohort_definition_id),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(
          # overlapping
          (feature_start_date <= (outcome_start_date-days(-1)) &
             feature_end_date >= (outcome_start_date-days(-1))) |
            # ending in window
            (feature_end_date >= (outcome_start_date-days(180)) &
               feature_end_date <= (outcome_start_date-days(-1)))) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_name:=1),
      by="person_id") %>%
    compute()
}


# conditions

for(i in seq_along(disease_cohorts$cohortId)){
  
  working_name <- glue::glue("{disease_cohorts$cohortName[[i]]}")
  working_id <- disease_cohorts$cohortId[[i]]
  Pop <- 
    Pop %>% 
    left_join(
      Pop %>% 
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm[[feature_disease_table_name]] %>% 
                     rename("feature_start_date"="cohort_start_date") %>% 
                     filter(cohort_definition_id== working_id ) %>% 
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(feature_start_date < outcome_start_date) %>% 
        select(person_id) %>% 
        distinct() %>% 
        mutate(!!working_name:=1),
      by="person_id")  %>% 
    compute()
  
}





  