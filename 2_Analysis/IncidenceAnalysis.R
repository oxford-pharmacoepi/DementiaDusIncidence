# code for getting denominator and estimating incidence below

#######################################################
# incidence of drugs within a dementia population
#######################################################

# dementia population  -----
print(paste0("- Getting denominator: dementia population"))
info(logger, "- Getting denominator: dementia population")

# change the start date for different databases
if (db.name == "SIDIAP") {
  
  startdate =  as.Date("2010-01-01")
  enddate = as.Date("2020-12-31")
  
  } else {
    
    startdate =  as.Date("2007-01-01")
    enddate = as.Date("2019-12-31")
    
  }

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(startdate, enddate),
  strataTable = strata_table_name ,
  strataCohortId = 1 ,
  ageGroup =list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365
)

#cohortCount(cdm$denominator)

print(paste0("- Got denominator: dementia population"))
info(logger, "- Got denominator: dementia population")


# Estimate incidence -------
print(paste0("- Getting drug incidence: dementia population"))
info(logger, "- Getting drug incidence: dementia population")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  outcomeCohortId = outcome_cohorts$cohort_definition_id,
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5
)


print(paste0("- Got drug incidence: dementia population"))
info(logger, "- Got drug incidence: dementia population")

# Export the results -----
print(paste0("- Exporting drug incidence results: dementia population"))
info(logger, "- Exporting drug incidence results: dementia population")

exportIncidencePrevalenceResults(
  resultList = list("incidence" = inc),
  zipName = paste0(db.name, "IPResults"),
  outputFolder = here::here("Results", db.name)
)

print(paste0("- Exported drug incidence results: dementia population"))
info(logger, "- Exported drug incidence results: dementia population")


print(paste0("- Extracting patient characteristics: dementia population"))
info(logger, "- Extracting patient characteristics: dementia population")

#table_one_analysis 
if (table_one_analysis == TRUE) {
  
source(here("2_Analysis","Table1.R"))
  
} else {

print("Not running table 1 characterisation") }

print(paste0("- Extracted patient characteristics: dementia population"))
info(logger, "- Extracted patient characteristics: dementia population")


print(paste0("- Plotting drug incidence results: dementia population"))
info(logger, "- Plotting drug incidence results: dementia population")

###########################################
# plot the results for drugs in  whole population in dementia strata
inc_yrs_plot <- inc %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 & analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Calender year") + 
  ylab("Incidence rate per 100000 person-years") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
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

inc_yrs_plot1 <- inc %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter((denominator_cohort_id == 1 | denominator_cohort_id == 2) & analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%  
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender <- inc_yrs_plot1 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Calender year") + 
  ylab("Incidence rate per 100000 person-years") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
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

inc_yrs_plot2 <- inc %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter((denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) & analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(`40 to 64` = "40-64 Years", 
            `65 to 79` = "65-79 Years",
            `80 to 150` = "80+ Years")

plotAge <- inc_yrs_plot2 %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Calender year") + 
  ylab("Incidence rate per 100000 person-years") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
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

inc_yrs_plot <- inc %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter((denominator_age_group != "40 to 150" &
           denominator_sex != "Both") & analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(`40 to 64` = "40-64 Years", 
               `65 to 79` = "65-79 Years",
               `80 to 150` = "80+ Years")

plotAgeGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Calender year") + 
  ylab("Incidence rate per 100000 person-years") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
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



# if (grepl("CPRD", db.name) == TRUE) {
#   
#   
# #######################################################
# # period prevalence of drugs within a dementia population
# #######################################################  
# 
# prev_period <- estimatePeriodPrevalence(
#     cdm = cdm,
#     denominatorTable = "denominator",
#     outcomeLookbackDays = 0, 
#     outcomeTable = outcome_table_name,
#     interval = "years" ,
#     completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
#     fullContribution = FALSE , # individuals only required to be present for one day in interval
#     minCellCount = 5,
#     verbose = TRUE
#   )
#   
# study_resultsPrevDrug <- gatherIncidencePrevalenceResults(cdm = cdm,
#                                                                list(prev_period))
# 
# 
# exportIncidencePrevalenceResults(result=study_resultsPrevDrug,
#                                  zipName= paste0(db.name, "PPResultsDrugsDemPop"),
#                                  outputFolder=here::here("Results", db.name))
# 
# 
# 
# # plot the results fordrugs in  whole population in dementia strata
# pp_yrs_plot <- study_resultsPrevDrug[[1]] %>%  
#   filter(denominator_cohort_id == 3 ) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotAll <- pp_yrs_plot %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")) 
# 
# plotname <- paste0("4DrugPeriodPrevWholePop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotAll, newpage = FALSE)
# dev.off()
# 
# ###########################################
# # plot the results stratified by gender
# 
# prev_yrs_plot1 <- study_resultsPrevDrug[[1]]  %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_cohort_id == 1 | denominator_cohort_id == 2)  %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%  
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotGender <- prev_yrs_plot1 %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("4DrugPeriodPrevGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 10, height = 5)
# print(plotGender, newpage = FALSE)
# dev.off()
# 
# 
# 
# ###########################################
# # plot the results stratified by age
# 
# prev_yrs_plot2 <- study_resultsPrevDrug[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAge <- prev_yrs_plot2 %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("4DrugPeriodPrevAge", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAge, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age AND gender
# 
# prev_yrs_plot <- study_resultsPrevDrug[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_age_group != "40;150" &
#             denominator_sex != "Both")  %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAgeGender <- prev_yrs_plot %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("4DrugPeriodPrevAgeGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 7)
# print(plotAgeGender, newpage = FALSE)
# dev.off()
# 
# 
# #######################################################
# # incidence of drugs within a dementia/Drug population
# #######################################################
# # dementia strata definition includes those with either a dementia diagnosis OR drug
# 
# cdm$denominator1 <- generateDenominatorCohortSet(
#   cdm = cdm,
#   startDate = startdate,
#   endDate = enddate ,
#   strataTable = strata_table_name1 ,
#   strataCohortId = 1 ,
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
# 
# inc_new <- estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator1",
#   outcomeTable = outcome_table_name,
#   outcomeCohortId = outcome_cohorts$cohort_definition_id,
#   interval = c("years", "overall"),
#   outcomeWashout = NULL,
#   repeatedEvents = FALSE,
#   minCellCount = 5,
#   verbose = TRUE
# )
# 
# study_results2 <- gatherIncidencePrevalenceResults(cdm = cdm, 
#                                                  list(inc_new))
# 
# exportIncidencePrevalenceResults(result=study_results2,
#                                  zipName= paste0(db.name, "IPResultsNewDemDef"),
#                                  outputFolder=here::here("Results", db.name))
# 
# 
# ###########################################
# # plot the results fordrugs in  whole population in dementia strata
# inc_yrs_plot <- study_results2[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_cohort_id == 3 & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotAll <- inc_yrs_plot %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")) 
# 
# plotname <- paste0("5DrugIncidenceRatesWholePop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotAll, newpage = FALSE)
# dev.off()
# 
# ###########################################
# # plot the results stratified by gender
# 
# inc_yrs_plot1 <- study_results2[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 1 | denominator_cohort_id == 2) & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%  
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotGender <- inc_yrs_plot1 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("5DrugIncidenceRatesGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 10, height = 5)
# print(plotGender, newpage = FALSE)
# dev.off()
# 
# 
# 
# ###########################################
# # plot the results stratified by age
# 
# inc_yrs_plot2 <- study_results2[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAge <- inc_yrs_plot2 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("5DrugIncidenceRatesAge", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAge, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age AND gender
# 
# inc_yrs_plot <- study_results2[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_age_group != "40;150" &
#             denominator_sex != "Both") & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAgeGender <- inc_yrs_plot %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("5DrugIncidenceRatesAgeGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 7)
# print(plotAgeGender, newpage = FALSE)
# dev.off()
# 
# #######################################################
# # incidence of drugs within a general population
# #######################################################
# 
# print(paste0("- Getting denominator: general population"))
# info(logger, "- Getting denominator: general population")
# 
# cdm$denominatorall <- generateDenominatorCohortSet(
#   cdm = cdm,
#   startDate = startdate,
#   endDate = enddate ,
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
# 
# 
# print(paste0("- Got denominator: general population"))
# info(logger, "- Got denominator: general population")
# 
# 
# 
# # Estimate incidence -------
# print(paste0("- Getting drug incidence: general population"))
# info(logger, "- Getting drug incidence: general population")
# 
# inc1 <- estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominatorall",
#   outcomeTable = outcome_table_name,
#   interval = c("years", "overall"),
#   outcomeWashout = NULL,
#   repeatedEvents = FALSE,
#   minCellCount = 5,
#   verbose = TRUE
# )
# 
# print(paste0("- Got drug incidence: general population"))
# info(logger, "- Got drug incidence: general population")
# 
# 
# print(paste0("- Gathering drug incidence results: general population"))
# info(logger, "- Gathering drug incidence results: general population")
# 
# study_resultsDrugGeneralPop<- gatherIncidencePrevalenceResults(cdm = cdm,
#                                                                list(inc1))
# 
# print(paste0("- Gathered drug incidence results: general population"))
# info(logger, "- Gathered drug incidence results: general population")
# 
# 
# print(paste0("- Export drug incidence results: general population"))
# info(logger, "- Export drug incidence results: general population")
# 
# exportIncidencePrevalenceResults(result=study_resultsDrugGeneralPop,
#                                  zipName= paste0(db.name, "IPResults4DementiaDrugGenPop"),
#                                  outputFolder=here::here("Results", db.name))
# 
# print(paste0("- Exported drug incidence results: general population"))
# info(logger, "- Exported drug incidence results: general population")
# 
# print(paste0("- Plotting drug incidence results: general population"))
# info(logger, "- Plotting drug incidence results: general population")
# 
# ###########################################
# # plot the results for drug incidence in general population
# inc_yrs_plot <- study_resultsDrugGeneralPop[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 3 &
#            denominator_age_group == "40;150") & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotAll <- inc_yrs_plot %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")) 
# 
# plotname <- paste0("2DrugIncidenceRatesWholePopDrugsGenPop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotAll, newpage = FALSE)
# dev.off()
# 
# ###########################################
# # plot the results stratified by gender
# 
# inc_yrs_plot1 <- study_resultsDrugGeneralPop[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 1 | denominator_cohort_id == 2 &
#            denominator_age_group == "40;150") & analysis_interval == "years") %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotGender <- inc_yrs_plot1 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("2DrugIncidenceRatesGenderDrugsGenPop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 10, height = 5)
# print(plotGender, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age
# 
# inc_yrs_plot2 <- study_resultsDrugGeneralPop[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) &
#            denominator_sex == "Both" & analysis_interval == "years"
#          
#   ) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAge <- inc_yrs_plot2 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("2DrugIncidenceRatesAgeDrugsGenPop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAge, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age AND gender
# 
# inc_yrs_plot <- study_resultsDrugGeneralPop[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_age_group != "40;150" &
#            denominator_sex != "Both") & analysis_interval == "years"
#          
#   ) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAgeGender <- inc_yrs_plot %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia Medications") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("2DrugIncidenceRatesAgeGenderDrugGenPop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 7)
# print(plotAgeGender, newpage = FALSE)
# dev.off()
# 
# 
# print(paste0("- Plotted drug incidence results: general population"))
# info(logger, "- Plotted drug incidence results: general population")
# 
# 
# ###############################################
# # dementia incidence in general population
# ################################################
# 
# # get denominator population === already run in previous analysis === 
# 
# 
# # Estimate incidence -------
# print(paste0("- Getting dementia incidence: general population"))
# info(logger, "- Getting dementia incidence: general population")
# 
# inc2 <- estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominatorall",
#   outcomeTable = strata_table_name,
#   interval = "years",
#   outcomeWashout = NULL,
#   repeatedEvents = FALSE,
#   minCellCount = 5,
#   verbose = TRUE
# )
# 
# 
# print(paste0("- Got dementia incidence: general population"))
# info(logger, "- Got dementia incidence: general population")
# 
# 
# print(paste0("- Gathering dementia incidence: general population"))
# info(logger, "- Gathering dementia incidence: general population")
# 
# 
# study_resultsDEM<- gatherIncidencePrevalenceResults(cdm = cdm, 
#                                                     list(inc2))
# 
# print(paste0("- Gathered dementia incidence: general population"))
# info(logger, "- Gathered dementia incidence: general population")
# 
# 
# print(paste0("- Exporting dementia incidence: general population"))
# info(logger, "- Exporting dementia incidence: general population")
# 
# exportIncidencePrevalenceResults(result=study_resultsDEM,
#                                  zipName= paste0(db.name, "IPResults4Dementia"),
#                                  outputFolder=here::here("Results", db.name))
# 
# print(paste0("- Exported dementia incidence: general population"))
# info(logger, "- Exported dementia incidence: general population")
# 
# 
# print(paste0("- Plotting dementia incidence: general population"))
# info(logger, "- Plotting dementia incidence: general population")
# 
# ###########################################
# # plot the results for whole population for dementia incidence
# inc_yrs_plot3 <- study_resultsDEM[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 3 & denominator_age_group == "40;150") & analysis_interval == "years") %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotAll <- inc_yrs_plot3 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Dementia incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.position = "none") 
# 
# plotname <- paste0("3DementiaIncidenceRatesWholePop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotAll, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results for by gender for dementia incidence
# inc_yrs_plot4 <- study_resultsDEM[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 1 | denominator_cohort_id == 2 & denominator_age_group == "40;150") & analysis_interval == "years") %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotGender2 <- inc_yrs_plot4 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Dementia incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.position = "none") 
# 
# plotGender2 <- plotGender2 + facet_wrap(~denominator_sex, scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# 
# plotname <- paste0("3DementiaIncidenceRatesGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotGender2, newpage = FALSE)
# dev.off()
# 
# ###########################################
# # plot the results stratified by age
# 
# inc_yrs_plot5 <-study_resultsDEM[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9 &
#            denominator_sex == "Both") & analysis_interval == "years") %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAge2 <- inc_yrs_plot5 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Dementia incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.position = "none"
#   ) 
# 
# plotAge2 <- plotAge2 + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("3DementiaIncidenceRatesAge", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAge2, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age AND gender
# inc_yrs_plot6 <-study_resultsDEM[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter((denominator_age_group != "40;150"  &
#            denominator_sex != "Both") & analysis_interval == "years") %>%
#   mutate(time = format(incidence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAgeGender2 <- inc_yrs_plot6 %>%
#   ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Dementia incidence rate per 100000 person-years") +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.position = "none"
#   ) 
# 
# plotAgeGender2 <- plotAgeGender2 + facet_grid(denominator_sex ~ denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("3DementiaIncidenceRatesAgeGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAgeGender2, newpage = FALSE)
# dev.off()
# 
# print(paste0("- Plotted dementia incidence: general population"))
# info(logger, "- Plotted dementia incidence: general population")
# 
# 
# ### prevalence of dementia in general population
# print(paste0("- Getting dementia prevalence: general population"))
# info(logger, "- Plotted dementia prevalence: general population")
# 
# prev_periodDEM <- estimatePeriodPrevalence(
#   cdm = cdm,
#   denominatorTable = "denominatorall",
#   outcomeTable = strata_table_name,
#   outcomeLookbackDays = 0, 
#   interval = "years" ,
#   completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
#   fullContribution = FALSE , # individuals only required to be present for one day in interval
#   minCellCount = 5,
#   verbose = TRUE
# )
# 
# study_resultsPrevDem <- gatherIncidencePrevalenceResults(cdm = cdm,
#                                                           list(prev_periodDEM))
# 
# 
# exportIncidencePrevalenceResults(result=study_resultsPrevDem,
#                                  zipName= paste0(db.name, "PPResultsDemPop"),
#                                  outputFolder=here::here("Results", db.name))
# 
# 
# 
# # plot the results fordrugs 
# pp_yrs_plot <- study_resultsPrevDem[[1]] %>%  
#   filter(denominator_cohort_id == 3 ) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotAll <- pp_yrs_plot %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         axis.line = element_line(colour = "black", size = 0.6) ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")) 
# 
# plotname <- paste0("6DemPeriodPrevWholePop", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 7, height = 5)
# print(plotAll, newpage = FALSE)
# dev.off()
# 
# ###########################################
# # plot the results stratified by gender
# 
# prev_yrs_plot1 <- study_resultsPrevDem[[1]]  %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_cohort_id == 1 | denominator_cohort_id == 2)  %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%  
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# plotGender <- prev_yrs_plot1 %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("6DemPeriodPrevGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 10, height = 5)
# print(plotGender, newpage = FALSE)
# dev.off()
# 
# 
# 
# ###########################################
# # plot the results stratified by age
# 
# prev_yrs_plot2 <- study_resultsPrevDem[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAge <- prev_yrs_plot2 %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("6DemPeriodPrevAge", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 5)
# print(plotAge, newpage = FALSE)
# dev.off()
# 
# 
# ###########################################
# # plot the results stratified by age AND gender
# 
# prev_yrs_plot <- study_resultsPrevDem[[1]] %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
#   filter(denominator_age_group != "40;150" &
#            denominator_sex != "Both")  %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) %>%
#   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "anyAntiDementiaDrugUser", "Any Dementia Drug")) %>%
#   mutate(time = format(prevalence_start_date, format="%Y")) %>%
#   as.data.frame()
# 
# agelabels <- c(`40;64` = "40-64 Years", 
#                `65;79` = "65-79 Years",
#                `80;150` = "80+ Years")
# 
# plotAgeGender <- prev_yrs_plot %>%
#   ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
#   geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
#   geom_line(color = "black", size = 0.25) +
#   geom_point(size = 2.5) + 
#   xlab("Calender year") + 
#   ylab("Prevalence") +
#   scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
#   scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) + #blue, #red, #lightblue, #green # purple
#   scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF")) +
#   labs(colour = "Dementia") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), 
#         panel.background = element_blank() ,
#         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
#         legend.key = element_rect(fill = "transparent", colour = "transparent")
#   ) 
# 
# plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
#   theme(strip.background = element_rect(colour="black", fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"))
# 
# plotname <- paste0("6DemPeriodPrevAgeGender", db.name,".pdf")
# 
# pdf(here("Results",db.name, plotname),
#     width = 15, height = 7)
# print(plotAgeGender, newpage = FALSE)
# dev.off()
# 
# 
# }
