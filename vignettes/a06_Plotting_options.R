## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ----message= FALSE, warning=FALSE, echo=FALSE--------------------------------
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(duckdb)
library(knitr)
library(IncidencePrevalence)

