# Load Libraries
library("shiny")
library("shinydashboard")
library("ggplot2")
library("magrittr")
library("plotly")
library("tidyverse")
library("DBI")
library("RSQLite")
library("feather")
library("mvtnorm")
library("glue")
library("rlang")
library("DT")
library("purrr")

# Source Relevant Code
walk(list.files("../code", full.names = TRUE), .f = function(x) {source(x)})
