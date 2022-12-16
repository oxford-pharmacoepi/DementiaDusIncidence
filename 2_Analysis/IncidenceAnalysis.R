# add code for getting denominator and estimating incidence below

# dementia population  -----
print(paste0("- Getting denominator: dementia population"))
info(logger, "- Getting denominator: dementia population")
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  strataTable = strata_table_name ,
  strataCohortId = 1 ,
  ageGroup =list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365,
  verbose = TRUE
)

#cdm$denominator %>% tally() # to check numbers in denominator population
#attrition(cdm$denominator) # to grab the attrition

print(paste0("- Got denominator: dementia population"))
info(logger, "- Got denominator: dementia population")


# Estimate incidence -------
print(paste0("- Getting incidence: dementia population"))
info(logger, "- Getting incidence: dementia population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5
)


print(paste0("- Got incidence: dementia population"))
info(logger, "- Got incidence: dementia population")


# Get the results ----------------
print(paste0("- Gathering incidence results: dementia population"))
info(logger, "- Gathering incidence results: dementia population")

#dplyr::glimpse(study_results$incidence_estimates)

study_results<- gatherIncidencePrevalenceResults(list(inc),
                             outcomeCohortId = outcome_cohorts$cohortId,
                             outcomeCohortName = outcome_cohorts$cohortName,
                             databaseName = db.name)

print(paste0("- Got incidence results: dementia population"))
info(logger, "- Got incidence results: dementia population")

# Export the results -----
print(paste0("- Exporting incidence results: dementia population"))
info(logger, "- Exporting incidence results: dementia population")

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults"),
                                 outputFolder=here::here("Results", db.name))


print(paste0("- Exported incidence results: dementia population"))
info(logger, "- Exported incidence results: dementia population")

#study_results $incidence_estimates %>% dplyr::filter(outcome_cohort_id==1)

###########################################
# plot the results for whole population
inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &&
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot <- as.data.frame(inc_yrs_plot)

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  #ggtitle("Incidence rates of anti dementia medications \nin Patients with a diagnosis of dementia") +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) 

plotname <- paste0("IncidenceRatesWholePop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()

###########################################
# plot the results stratified by gender

inc_yrs_plot1 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 &&
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot1 <- as.data.frame(inc_yrs_plot1)

plotGender <- inc_yrs_plot1 %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  #ggtitle("Incidence rates of anti dementia medications \nin Patients with a diagnosis of dementia") +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) 

plotGender <- plotGender + facet_wrap(~denominator_sex) +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()



###########################################
# plot the results stratified by age

inc_yrs_plot2 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9 &&
           denominator_sex == "Both"
         
         ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot2 <- as.data.frame(inc_yrs_plot2)

plotAge <- inc_yrs_plot2 %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  #ggtitle("Incidence rates of anti dementia medications \nin Patients with a diagnosis of dementia") +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAge <- plotAge + facet_wrap(~denominator_age_group) +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAge, newpage = FALSE)
dev.off()


###############################################
# dementia incidence in general population

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  #strataTable = strata_table_name ,
  #strataCohortId = 1 ,
  ageGroup =list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365,
  verbose = TRUE
)


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = strata_table_name,
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5
)


study_resultsDEM<- gatherIncidencePrevalenceResults(list(inc),
                                                 outcomeCohortId = strata_cohorts$cohortId,
                                                 outcomeCohortName = strata_cohorts$cohortName,
                                                 databaseName = db.name)


exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults4Dementia"),
                                 outputFolder=here::here("Results", db.name))


###########################################
# plot the results for whole population
inc_yrs_plot <- study_resultsDEM$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &&
           denominator_age_group == "40;150")


inc_yrs_plot <- as.data.frame(inc_yrs_plot)


plotAll <- inc_yrs_plot %>%
  ggplot(aes(x = time, y = ir_100000_pys,
             ymin = ir_100000_pys_95CI_lower,
             ymax = ir_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + 
  geom_line() +
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  geom_errorbar(width=0) +
  #scale_colour_discrete(labels=c('Donepezil', 'Galantamine','Memantine' ,'Rivastigmine')) +
  #scale_y_continuous(limits = c(0, NA)) +
  #scale_x_continuous(breaks = seq(min(inc_yrs_plot$time), max(inc_yrs_plot$time), by = 1)) +
  ggtitle("Incidence rates of dementia") +
  #labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
        
  ) 


plotname <- paste0("IncidenceRatesDementia", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()
