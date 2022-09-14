# Manage project dependencies ------
# install.packages("renv") # if not already installed, install renv from CRAN
remotes::install_github("darwin-eu/IncidencePrevalence")

# Load packages ------
# load r packages
library(SqlRender)
library(DatabaseConnector)
library(CirceR)
library(CohortGenerator)
library(IncidencePrevalence)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(stringr)

# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "ouput, we can use: here("output")
# but this file path could be set to somewhere else
output.folder<-here("Results", db.name)

# Specify databaseConnector connection details -----
# database connection details
connectionDetails <- createConnectionDetails(dbms = "...",
                                             server = "...",
                                             user = "...",
                                             password = "...",
                                             port = "....",
                                             pathToDriver ="...")


# Specify DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(drv = "...", 
                dbname = "...",
                port = "...",
                host = "...", 
                user = "...",
                password = "...")
# eg for postgres 
# db <- dbConnect(RPostgres::Postgres(), dbname = server_dbi, port = port, host = host, user = user,
#                 password = password)


# Set database details -----

# your sql dialect used with the OHDSI SqlRender package
# eg postgresql, redshift, etc
# see https://ohdsi.github.io/SqlRender/articles/UsingSqlRender.html for more details
targetDialect <-"..."

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"..."

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 
outcome_table_stem<-"..."

# check database connections ----
# to check whether the DatabaseConnector connection details are correct, 
# running the next three lines should give you a count of your person table
conn <- connect(connectionDetails)
querySql(conn,paste0("SELECT COUNT(*) FROM ", cdm_database_schema, ".person"))
disconnect(conn)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
tbl(db, sql(paste0("SELECT * FROM ",cdm_database_schema, ".person"))) %>% 
  tally()

# Run the study ------
create_outcome_cohorts<-TRUE # set to false if already instantiated
create_strata_cohorts<-TRUE # set to false if already instantiated
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share


# view results
# you can take a quick look at incidence rate estimates
# by uncommenting and runnng the below

# library(ggplot2)
# plot_data<-inc$incidence_estimates %>% 
#   left_join(inc$analysis_settings %>% 
#               select("incidence_analysis_id","cohort_id_outcome",
#                      "outcome_name"))
# 
# plot_data %>% 
#   ggplot(aes(colour=outcome_name))+
#   facet_grid(outcome_name~ .)+
#   geom_point(aes(calendar_year,ir_100000_pys))
