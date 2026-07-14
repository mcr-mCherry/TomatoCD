# -----------------------------------------------------------------------------
# TomatoCD — global application bootstrap (runs once per worker at boot)
# Released under the MIT License. See LICENSE in the repository root.
#
# Manuscript: NCOMMS-26-056367-T (Nature Communications, under review)
# Role:       library loading, configuration lookups, and dataset references
#             consumed by every Shiny session (server.R / ui.R).
# Maps to:
#   - Methods § Shiny application / Data sources
#   - docs/Code_Availability_Statement.md
#   - docs/deployment.md
#
# Production runtime:
#   rocker/shiny:latest (R 4.4.x, Shiny 1.14.0)
# Static assets referenced (config.R / EXTERNAL_URLS):
#   - data/, jbrowse2/, www/, gene_search/
# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(data.table)

options(shiny.maxRequestSize = 50*1024^2)  
options(stringsAsFactors = FALSE)  


BASE_DIR <- getwd()
INPUT_DIR <- file.path(BASE_DIR, "input")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
OUTPUT_DATA_DIR <- file.path(OUTPUT_DIR, "data")
OUTPUT_PLOTS_DIR <- file.path(OUTPUT_DIR, "plots")
OUTPUT_REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")
OUTPUT_LOGS_DIR <- file.path(OUTPUT_DIR, "logs")
WWW_DIR <- file.path(BASE_DIR, "www")


create_directories <- function() {
  dirs <- c(INPUT_DIR, OUTPUT_DIR, OUTPUT_DATA_DIR, OUTPUT_PLOTS_DIR, 
            OUTPUT_REPORTS_DIR, OUTPUT_LOGS_DIR, WWW_DIR)
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
}


create_directories()


APP_CACHE <- new.env(parent = emptyenv())

get_cached_data <- function(key, loader, force_refresh = FALSE) {
  if (!force_refresh && exists(key, envir = APP_CACHE, inherits = FALSE)) {
    return(get(key, envir = APP_CACHE, inherits = FALSE))
  }

  value <- loader()
  assign(key, value, envir = APP_CACHE)
  value
}

clear_cached_data <- function(key = NULL) {
  if (is.null(key)) {
    rm(list = ls(envir = APP_CACHE, all.names = TRUE), envir = APP_CACHE)
  } else if (exists(key, envir = APP_CACHE, inherits = FALSE)) {
    rm(list = key, envir = APP_CACHE)
  }
}


welcome_message <- "Welcome to the Tomato Functional Genomics Database!"


load_example_data <- function() {
  return(mtcars) 
}

tomato_species <- c(
  "Solanum lycopersicum",
  "Solanum pennellii",
  "Solanum pimpinellifolium",
  "Solanum habrochaites",
  "Solanum chilense"
)

module_descriptions <- list(
  network = "Analyze gene-gene interaction networks",
  protein_binding = "Explore protein-DNA interaction sites",
  motif = "Discover regulatory DNA motifs",
  mc_preference = "Study DNA methylation patterns",
  gene_annotation = "Access comprehensive gene annotations",
  expression = "Visualize gene expression profiles"
)


