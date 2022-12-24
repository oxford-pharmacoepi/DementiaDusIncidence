# code for getting denominator and estimating incidence below

#######################################################
# incidence of drugs within a dementia population
#######################################################

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
print(paste0("- Getting drug incidence: dementia population"))
info(logger, "- Getting drug incidence: dementia population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  outcomeCohortId = outcome_cohorts$cohortId,
  outcomeCohortName = outcome_cohorts$cohortName,
  interval = "years",
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  minCellCount = 5
)


print(paste0("- Got drug incidence: dementia population"))
info(logger, "- Got drug incidence: dementia population")


# Get the results ----------------
print(paste0("- Gathering drug incidence results: dementia population"))
info(logger, "- Gathering drug incidence results: dementia population")

#dplyr::glimpse(study_results$incidence_estimates)

study_results<- gatherIncidencePrevalenceResults(cdm = cdm, 
                                                 list(inc),
                                                 databaseName = db.name)

print(paste0("- Got drug incidence results: dementia population"))
info(logger, "- Got drug incidence results: dementia population")

# Export the results -----
print(paste0("- Exporting drug incidence results: dementia population"))
info(logger, "- Exporting drug incidence results: dementia population")

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults"),
                                 outputFolder=here::here("Results", db.name))


print(paste0("- Exported drug incidence results: dementia population"))
info(logger, "- Exported drug incidence results: dementia population")

print(paste0("- Plotting drug incidence results: dementia population"))
info(logger, "- Plotting drug incidence results: dementia population")

###########################################
# plot the results fordrugs in  whole population in dementia strata
inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) 

plotname <- paste0("1DrugIncidenceRatesWholePop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()

###########################################
# plot the results stratified by gender

inc_yrs_plot1 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%  
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender <- inc_yrs_plot1 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) 

plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("1DrugIncidenceRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()



###########################################
# plot the results stratified by age

inc_yrs_plot2 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(`40;64` = "40-64 Years", 
            `65;79` = "65-79 Years",
            `80;150` = "80+ Years")

plotAge <- inc_yrs_plot2 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("1DrugIncidenceRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAge, newpage = FALSE)
dev.off()


###########################################
# plot the results stratified by age AND gender

inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "40;150" &
           denominator_sex != "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAgeGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("1DrugIncidenceRatesAgeGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 7)
print(plotAgeGender, newpage = FALSE)
dev.off()


print(paste0("- Plotted drug incidence results: dementia population"))
info(logger, "- Plotted drug incidence results: dementia population")


if (db.name != "SIDIAP") {
#######################################################
# incidence of drugs within a general population
#######################################################

print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
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


print(paste0("- Got denominator: general population"))
info(logger, "- Got denominator: general population")



# Estimate incidence -------
print(paste0("- Getting drug incidence: general population"))
info(logger, "- Getting drug incidence: general population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  outcomeCohortId = outcome_cohorts$cohortId,
  outcomeCohortName = outcome_cohorts$cohortName,
  interval = "years",
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  minCellCount = 5
)

print(paste0("- Got drug incidence: general population"))
info(logger, "- Got drug incidence: general population")


print(paste0("- Gathering drug incidence results: general population"))
info(logger, "- Gathering drug incidence results: general population")

study_resultsDrugGeneralPop<- gatherIncidencePrevalenceResults(cdm = cdm,
                                                               list(inc),
                                                               databaseName = db.name)

print(paste0("- Gathered drug incidence results: general population"))
info(logger, "- Gathered drug incidence results: general population")


print(paste0("- Export drug incidence results: general population"))
info(logger, "- Export drug incidence results: general population")

exportIncidencePrevalenceResults(result=study_resultsDrugGeneralPop,
                                 zipName= paste0(db.name, "IPResults4DementiaDrugGenPop"),
                                 outputFolder=here::here("Results", db.name))

print(paste0("- Exported drug incidence results: general population"))
info(logger, "- Exported drug incidence results: general population")

print(paste0("- Plotting drug incidence results: general population"))
info(logger, "- Plotting drug incidence results: general population")

###########################################
# plot the results for drug incidence in general population
inc_yrs_plot <- study_resultsDrugGeneralPop$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) 

plotname <- paste0("2DrugIncidenceRatesWholePopDrugsGenPop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()

###########################################
# plot the results stratified by gender

inc_yrs_plot1 <- study_resultsDrugGeneralPop$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 &
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender <- inc_yrs_plot1 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("2DrugIncidenceRatesGenderDrugsGenPop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()


###########################################
# plot the results stratified by age

inc_yrs_plot2 <- study_resultsDrugGeneralPop$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9 &
           denominator_sex == "Both"
         
  ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAge <- inc_yrs_plot2 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("2DrugIncidenceRatesAgeDrugsGenPop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAge, newpage = FALSE)
dev.off()


###########################################
# plot the results stratified by age AND gender

inc_yrs_plot <- study_resultsDrugGeneralPop$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "40;150" &
           denominator_sex != "Both"
         
  ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAgeGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("2DrugIncidenceRatesAgeGenderDrugGenPop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 7)
print(plotAgeGender, newpage = FALSE)
dev.off()


print(paste0("- Plotted drug incidence results: general population"))
info(logger, "- Plotted drug incidence results: general population")


###############################################
# dementia incidence in general population
################################################

# get denominator population already run in previous
# cdm$denominator <- generateDenominatorCohortSet(
#   cdm = cdm,
#   startDate = as.Date("2007-01-01"),
#   ageGroup =list(
#     c(40, 150),
#     c(40, 64),
#     c(65, 79),
#     c(80, 150)
#   ),
#   sex = c("Male", "Female", "Both"),
#   daysPriorHistory = 365,
#   verbose = TRUE
# )


# Estimate incidence -------
print(paste0("- Getting dementia incidence: general population"))
info(logger, "- Getting dementia incidence: general population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = strata_table_name,
  outcomeCohortId = outcome_cohorts$cohortId,
  outcomeCohortName = outcome_cohorts$cohortName,
  interval = "years",
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  minCellCount = 5
)


print(paste0("- Got dementia incidence: general population"))
info(logger, "- Got dementia incidence: general population")


print(paste0("- Gathering dementia incidence: general population"))
info(logger, "- Gathering dementia incidence: general population")


study_resultsDEM<- gatherIncidencePrevalenceResults(cdm = cdm, 
                                                    list(inc),
                                                    databaseName = db.name)

print(paste0("- Gathered dementia incidence: general population"))
info(logger, "- Gathered dementia incidence: general population")


print(paste0("- Exporting dementia incidence: general population"))
info(logger, "- Exporting dementia incidence: general population")

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults4Dementia"),
                                 outputFolder=here::here("Results", db.name))

print(paste0("- Exported dementia incidence: general population"))
info(logger, "- Exported dementia incidence: general population")


print(paste0("- Plotting dementia incidence: general population"))
info(logger, "- Plotting dementia incidence: general population")

###########################################
# plot the results for whole population for dementia incidence
inc_yrs_plot3 <- study_resultsDEM$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 & denominator_age_group == "40;150") %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- inc_yrs_plot3 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Dementia Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position = "none") 

plotname <- paste0("3DementiaIncidenceRatesWholePop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()


###########################################
# plot the results for by gender for dementia incidence
inc_yrs_plot4 <- study_resultsDEM$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 & denominator_age_group == "40;150") %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender2 <- inc_yrs_plot4 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Dementia Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position = "none") 

plotGender2 <- plotGender2 + facet_wrap(~denominator_sex, scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))


plotname <- paste0("3DementiaIncidenceRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotGender2, newpage = FALSE)
dev.off()

###########################################
# plot the results stratified by age

inc_yrs_plot5 <-study_resultsDEM$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9 &
           denominator_sex == "Both") %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAge2 <- inc_yrs_plot5 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Dementia Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position = "none"
  ) 

plotAge2 <- plotAge2 + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("3DementiaIncidenceRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAge2, newpage = FALSE)
dev.off()


###########################################
# plot the results stratified by age AND gender
inc_yrs_plot6 <-study_resultsDEM$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "40;150"  &
           denominator_sex != "Both") %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAgeGender2 <- inc_yrs_plot6 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Dementia Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position = "none"
  ) 

plotAgeGender2 <- plotAgeGender2 + facet_grid(denominator_sex ~ denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("3DementiaIncidenceRatesAgeGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAgeGender2, newpage = FALSE)
dev.off()

print(paste0("- Plotted dementia incidence: general population"))
info(logger, "- Plotted dementia incidence: general population")
}
