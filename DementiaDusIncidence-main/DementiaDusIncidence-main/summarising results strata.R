library(ggplot2)
inc_strata$analysis_settings<-inc_strata$analysis_settings %>% 
  left_join(outcomeCohortDefinitionSet %>% 
              select("cohortId", "cohortName") %>% 
              mutate(cohortId=as.character(cohortId)) %>%       
              rename("cohort_id_outcome"="cohortId") %>%       
              rename("outcome_name"="cohortName"),
            by="cohort_id_outcome")

plot_data<-inc_strata$incidence_estimates %>%
  left_join(inc_strata$analysis_settings %>%
              select("incidence_analysis_id", "cohort_id_denominator_pop","cohort_id_outcome",
                     "outcome_name")) %>% 
  left_join(dpop_strata$denominator_settings  %>%       
  rename("cohort_id_denominator_pop"="cohort_definition_id"))

# plot overall
plot_data %>%
  filter(sex_strata=="Both") %>%
  filter(age_strata=="40;150") %>% 
  ggplot(aes(colour=outcome_name))+
  facet_grid(outcome_name~ .)+
  geom_line(aes(calendar_year,ir_100000_pys)) +
  theme_bw()+
  theme(legend.position = "none")
ggsave("strata_overall.png")

# plot by sex
plot_data %>%
  filter(age_strata=="40;150") %>% 
  filter(sex_strata!="Both") %>%
  ggplot(aes(colour=outcome_name))+
  facet_grid(outcome_name~ sex_strata)+
  geom_line(aes(calendar_year,ir_100000_pys)) +
  theme_bw()+
  theme(legend.position = "none")
ggsave("strata_sex.png")

# plot by age 
plot_data %>%
  filter(age_strata!="40;150") %>% 
  filter(sex_strata=="Both") %>%
  ggplot(aes(colour=outcome_name))+
  facet_grid(outcome_name~ age_strata)+
  geom_line(aes(calendar_year,ir_100000_pys)) +
  theme_bw()+
  theme(legend.position = "none")
ggsave("strata_age.png")

# plot by age and sex
plot_data %>%  
  filter(age_strata!="40;150") %>% 
  filter(sex_strata!="Both") %>%
  ggplot(aes(colour=outcome_name))+
  facet_grid(age_strata~ sex_strata)+
  geom_line(aes(calendar_year,ir_100000_pys)) +
  theme_bw()+
  theme(legend.position = "none")
ggsave("strata_age_sex.png")
