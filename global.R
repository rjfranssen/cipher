# ciphers
# Ref: Shiny tags: https://shiny.rstudio.com/articles/tag-glossary.html
# Ref; shifting: https://stackoverflow.com/questions/59171164/shifting-strings-using-r-functions
# Ref: dashboardlayouts with navbarpage: https://stackoverflow.com/questions/59517901/how-to-insert-valuebox-inside-navbarpage-layout

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(stringr)
library(sqldf)
library(plotly)

# library(BiocManager)
# options(repos = BiocManager::repositories())
# library(Biostrings) # Biostrings is not on CRAN but on Bioconductor. Run below to install it.
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("Biostrings")

# Setup
alphabet <- data.frame(alpha = LETTERS, num = 0:25)

