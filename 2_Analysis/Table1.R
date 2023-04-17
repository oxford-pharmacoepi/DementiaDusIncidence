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
outcomes <- incidenceSet(inc_overall)

# grab the characteristics for each outcome
characteristics <- list()
for(i in seq_along(outcomes$outcome_cohort_id)){
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
                       window = c(-365, -1), 
                       nameStyle = "{cohort_name}") 
  
  
  working_participants<- cdm$working_participants %>% 
    collect() 
  
  # calculate the charlson
  # update cohorts with different weights
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
             peptic_ulcer_disease,
           mild_liver_disease,
           diabetes_with_chronic_complications,
           hemoplegia_or_paralegia,
           renal_disease,
           any_malignancy,
           moderate_to_severe_liver_disease,
           metastatic_solid_tumor,
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
  table(person$charlson_cat, useNA = "always")
  
working_table <- bind_rows(
    working_participants %>% 
      summarise(val = as.character(n())) %>% 
      mutate(var="N"),
    working_participants %>% 
      summarise(val = as.character(median(age)))  %>% 
      mutate(var="Median age"),
    working_participants%>% 
      filter(sex == "Male") %>% 
      summarise(val = as.character(n()))  %>% 
      mutate(var="N male"))
  
  f_names <- colnames(cdm$working_participants)
  f_names <- str_subset(f_names, paste(
    "subject_id", "cohort_start_date", "age", "sex", "prior_history",
    "future_observation", sep="|"), negate = TRUE)
  
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
    rename(!!outcomes$outcome_cohort_name[i]:="val")
  
}

# combine into single table
for(i in 1:(length(characteristics)-1)){
  characteristics[[1]] <- characteristics[[1]] %>% 
    left_join(characteristics[[i+1]])
}

table_characteristics <- characteristics[[1]] %>% 
  relocate("var")











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

# subset denominator to those who do not have an outcome (i.e. no drug) but have dementia
PopDenom <- Pop %>% filter(Outcome == FALSE)

# subset the population to those who have an outcome
Pop <- Pop %>% filter(Outcome == TRUE)


### OUTCOME COHORTS ####
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


### GENERAL POPULATION COHORT ####
# people with dementia who are not taking dementia medications - characteristics from cohort start date (this is not the date of their dementia diagnosis)
# format data -----
#add age -----
PopDenom$age<- NA
if(sum(is.na(PopDenom$day_of_birth))==0 & sum(is.na(PopDenom$month_of_birth))==0){
  # if we have day and month
  PopDenom <-PopDenom %>%
    mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else {
  PopDenom <- PopDenom %>%
    mutate(age= lubridate::year(cohort_start_date)-year_of_birth)
}

# # age age groups ----
PopDenom <- PopDenom %>%
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
table(PopDenom$age_gr, useNA = "always")

# # reformat gender
# # add gender -----
# #8507 male
# #8532 female
PopDenom <-PopDenom %>%
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>%
  mutate(gender= factor(gender,
                        levels = c("Male", "Female")))
table(PopDenom$gender, useNA = "always")

# # if missing (or unreasonable) age or gender, drop ----
PopDenom <-PopDenom %>%
  filter(!is.na(age)) %>%
  filter(age>=18) %>%
  filter(age<=110) %>%
  filter(!is.na(gender))


# medications
for(i in seq_along(medication_cohorts$cohortId)){
  working_name <- glue::glue("{medication_cohorts$cohortName[[i]]}")
  working_id <- medication_cohorts$cohortId[[i]]
  PopDenom <-
    PopDenom %>%
    left_join(
      PopDenom %>%
        select("person_id", "cohort_start_date") %>% 
        inner_join(cdm[[feature_medication_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     rename("feature_end_date"="cohort_end_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!cohort_definition_id),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(
          # overlapping
          (feature_start_date <= (cohort_start_date-days(-1)) &
             feature_end_date >= (cohort_start_date-days(-1))) |
            # ending in window
            (feature_end_date >= (cohort_start_date-days(180)) &
               feature_end_date <= (cohort_start_date-days(-1)))) %>%
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
  PopDenom <- 
    PopDenom %>% 
    left_join(
      PopDenom %>% 
        select("person_id", "cohort_start_date") %>% 
        inner_join(cdm[[feature_disease_table_name]] %>% 
                     rename("feature_start_date"="cohort_start_date") %>% 
                     filter(cohort_definition_id== working_id ) %>% 
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(feature_start_date < cohort_start_date) %>% 
        select(person_id) %>% 
        distinct() %>% 
        mutate(!!working_name:=1),
      by="person_id")  %>% 
    compute()
  
}



# Non smokers
Pop <- 
  Pop %>% 
  left_join(
 Pop %>%
  select("person_id", "outcome_start_date") %>% 
  inner_join(cdm$observation %>%
               filter( observation_concept_id == 4222303 ) , 
             by=c("person_id"), copy = TRUE)  %>% 
  filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
  filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
  select(c(person_id, observation_date, outcome_start_date)) %>% 
  distinct() %>%
  group_by(person_id, outcome_start_date) %>%
  filter(observation_date == max(observation_date)) %>%
  rename("non_smoker_date"="observation_date"),
 by= c("person_id", "outcome_start_date")) %>% 
  compute()

# Smokers
Pop <- 
  Pop %>% 
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>% 
      inner_join(cdm$observation %>%
                   filter( 
                     observation_concept_id == 4141787 | # Smoking started
                       observation_concept_id == 44789712 | # want to stop smoking
                       observation_concept_id == 40486518 | # failed attempt to stop smoking
                       observation_concept_id == 	4046886 | # smoking reduced 
                       observation_concept_id == 4058137 | # tried giving up smoking
                       observation_concept_id == 4216174 | # not interested in giving up smoking
                       observation_concept_id == 4215409 | # Ready to stop smoking
                       observation_concept_id == 4190573 | # Thinking about stopping smoking
                       observation_concept_id == 4052948 | # Keeps trying to stop smoking
                       observation_concept_id ==  4144271 | # Tobacco smoking consumption
                       observation_concept_id == 4298794
                     ) , 
                 by=c("person_id"), copy = TRUE)  %>% 
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>% 
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("Smoker_date"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>% 
  compute()

# previous smokers
Pop <- 
  Pop %>% 
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>% 
      inner_join(cdm$observation %>%
                   filter( 
                     observation_concept_id == 4052032 | # stopped smoking
                       observation_concept_id == 4052466 # date ceased smoking
                   ) , 
                 by=c("person_id"), copy = TRUE)  %>% 
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>% 
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("PrevSmoker_date"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>% 
  compute()


# remove smokers who are now previous smokers
Pop <- Pop %>%
  mutate(Smoker_date = replace(Smoker_date, Smoker_date < PrevSmoker_date, NA))

# replace the dates with text and remove the dates
Pop <- Pop %>%
  mutate(SmokingStatus = NA,
         SmokingStatus = replace(SmokingStatus, !is.na(Smoker_date), "Smoker"),
         SmokingStatus = replace(SmokingStatus, !is.na(PrevSmoker_date), "Previous Smoker"),
         SmokingStatus = replace(SmokingStatus, !is.na(non_smoker_date), "Non Smoker")) %>%
  select(!c(Smoker_date, PrevSmoker_date, non_smoker_date))



# get BMI - no BMI measurements in AURUM works with other measurements e.g. 37393857
# BMI<-tbl(db, sql("SELECT * FROM measurement")) %>%
#   filter(measurement_concept_id == 3038553) %>%
#   collect()

# BMI <- cdm$measurement %>%
#   filter(measurement_concept_id == 3038553) %>%
#   head(10) %>% collect()


  
  



           
           
           
           
           
           
           



# get_summary_characteristics<-function(data){
#   
#   summary_characteristics<- bind_rows(
#     data %>% 
#       count() %>% 
#       mutate(var="N"),
#     data %>% 
#       summarise(mean=nice.num.count(mean(age)),
#                 standard_deviation = nice.num(sd(age)),
#                 median = nice.num(median(age)),
#                 interquartile_range=paste0(nice.num.count(quantile(age,probs=0.25)),  " to ",
#                                            nice.num.count(quantile(age,probs=0.75)))) %>% 
#       mutate(var="age"),
#     data %>% 
#       group_by(age_group) %>% 
#       summarise(n=n(),
#                 percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
#       rename("var"="age_group") %>% 
#       mutate(var=paste0("Age group: ", var)),
#     data %>% 
#       mutate(sex=factor(sex, levels=c("Male", "Female"))) %>% 
#       group_by(sex) %>% 
#       summarise(n=n(),
#                 percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
#       rename("var"="sex") %>% 
#       mutate(var=paste0("Sex: ", var)),
#     data %>% 
#       group_by(index_year_month) %>% 
#       count() %>% 
#       mutate(percent=paste0(nice.num(n/nrow(data)*100),  "%")) %>% 
#       rename("var"="index_year_month") %>% 
#       mutate(var=paste0("Month: ", var)),
#     data %>% 
#       summarise(mean=nice.num.count(mean(outpatient_vist)),
#                 standard_deviation = nice.num(sd(outpatient_vist)),
#                 median = nice.num(median(outpatient_vist)),
#                 interquartile_range=paste0(nice.num.count(quantile(outpatient_vist,probs=0.25)),  " to ",
#                                            nice.num.count(quantile(outpatient_vist,probs=0.75)))) %>% 
#       mutate(var="outpatient vists"),
#     data %>% 
#       summarise(mean=nice.num.count(mean(hospital_visit)),
#                 standard_deviation = nice.num(sd(hospital_visit)),
#                 median = nice.num(median(hospital_visit)),
#                 interquartile_range=paste0(nice.num.count(quantile(hospital_visit,probs=0.25)),  " to ",
#                                            nice.num.count(quantile(hospital_visit,probs=0.75)))) %>% 
#       mutate(var="outpatient vists"))
#   
#   
#   
#   
#   for(i in seq_along(feature_condition_cohorts$cohortId)){
#     working_name <- glue::glue("{feature_condition_cohorts$cohortName[[i]]}")
#     summary_characteristics <- bind_rows(summary_characteristics,
#                                          data %>% 
#                                            summarise(n=sum(!is.na(!!rlang::sym(working_name))),
#                                                      percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
#                                            mutate(var=working_name)
#     )
#   }
#   
#   for(i in seq_along(feature_medication_cohorts$cohortId)){
#     working_name <- glue::glue("{feature_medication_cohorts$cohortName[[i]]}")
#     summary_characteristics <- bind_rows(summary_characteristics,
#                                          data %>% 
#                                            summarise(n=sum(!is.na(!!rlang::sym(working_name))),
#                                                      percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
#                                            mutate(var=working_name)
#     )
#   }
#   
#   # filter any less than 5
#   summary_characteristics <- summary_characteristics %>% 
#     mutate(mean=ifelse(!is.na(n) & n<5, NA, mean)) %>% 
#     mutate(percent=ifelse(!is.na(n) & n<5, NA, percent)) %>% 
#     mutate(interquartile_range=ifelse(!is.na(n) & n<5, NA, interquartile_range)) %>% 
#     mutate(standard_deviation=ifelse(!is.na(n) & n<5, NA, standard_deviation)) %>% 
#     mutate(n=ifelse(!is.na(n) & n<5, "<5", n))
#   
#   return(summary_characteristics %>% 
#            relocate(any_of(c("var", "n", "percent",
#                              "mean", "standard_deviation",
#                              "median", "interquartile_range"))))
#   
# }


# write.csv(summary_characteristics_outcomes,
#           paste0(output_folder, "/summary_characteristics_pre_vax_", 
#                  db_name,"_", working_cohortName,".csv") ,
#           row.names = FALSE)
# 
# write.csv(summary_characteristics_general_pop,
#           paste0(output_folder, "/summary_characteristics_pre_vax_", 
#                  db_name,"_", working_cohortName,".csv") ,
#           row.names = FALSE)

outcomes <- incidenceSet(inc_overall)

characteristics <- list()
for(i in seq_along(outcomes$outcome_cohort_id)){
  cdm$working_participants <- participants(inc_overall, i) %>% 
    select("subject_id", "outcome_start_date") %>% 
    filter(!is.na(outcome_start_date)) %>% 
    rename("cohort_start_date" = "outcome_start_date")
  cdm$working_participants <- cdm$working_participants %>% 
    addDemographics(cdm) %>% 
    flagCohortPresence(cdm = cdm,
                       targetCohortTable = table1_conditions_table,
                       window = c(-Inf, 0), 
                       nameStyle = "{cohort_name}") %>% 
    flagCohortPresence(cdm = cdm,
                       targetCohortTable = table1_medications_table,
                       window = c(-365, -1), 
                       nameStyle = "{cohort_name}") 
  
  
  working_participants<- cdm$working_participants %>% 
    collect() 
  
  working_table <- bind_rows(
    working_participants %>% 
      summarise(val = as.character(n())) %>% 
      mutate(var="N"),
    working_participants %>% 
      summarise(val = as.character(median(age)))  %>% 
      mutate(var="Median age"),
    working_participants%>% 
      filter(sex == "Male") %>% 
      summarise(val = as.character(n()))  %>% 
      mutate(var="N male"))
  
  f_names <- colnames(cdm$working_participants)
  f_names <- str_subset(f_names, paste(
    "subject_id", "cohort_start_date", "age", "sex", "prior_history",
    "future_observation", sep="|"), negate = TRUE)
  
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
    rename(!!outcomes$outcome_cohort_name[i]:="val")
  
}

# combine into single table
for(i in 1:(length(characteristics)-1)){
  characteristics[[1]] <- characteristics[[1]] %>% 
    left_join(characteristics[[i+1]])
}

table_characteristics <- characteristics[[1]] %>% 
  relocate("var")

write_csv(
  table_one, 
  here("results", paste0("table_characteristics", cdmName(cdm), ".csv"))
)

  