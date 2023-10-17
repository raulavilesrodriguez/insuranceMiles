library(config)
library(RPostgreSQL)
library(DBI)
library(shiny)
library(dplyr)
library(lubridate)
library(glue)
library(leaflet)


#Read the database connection parameters from the config.yml
config_file <- "config.yml"
config <- config::get(file = config_file)

# Create a database connection
db_connection <- dbConnect(RPostgres::Postgres(),
                           host = config$host,
                           port = config$port,
                           dbname = config$dbname,
                           user = config$user,
                           password = config$password)








