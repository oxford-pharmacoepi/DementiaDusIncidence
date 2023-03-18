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



  