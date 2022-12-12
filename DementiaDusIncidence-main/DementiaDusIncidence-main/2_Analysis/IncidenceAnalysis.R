# add code for getting denominator and estimating incidence below

# general population  -----
print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  ageGroups =list(
    c(40, 150),
    c(40, 64),
    c(65, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365
)
print(paste0("- Got denominator: general population"))
info(logger, "- Got denominator: general population")
