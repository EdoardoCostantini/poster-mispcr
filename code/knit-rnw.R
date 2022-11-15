# Project:   paper-template
# Objective: Initialize the environment and knit any main document
# Author:    Edoardo Costantini
# Created:   2022-10-13
# Modified:  2022-11-14
# Notes: 

# Parse argument from compile script
args = commandArgs(trailingOnly = TRUE)

# Packages ---------------------------------------------------------------------

library(knitr)
library(mice)
library(ggplot2) # for plots
library(dplyr)   # for plot pipe-lines
library(tidyr)   # for reshaping data for tables
library(patchwork) # for adding plots to each other easily 
library(english) # for translating numbers to words
library(kableExtra) # for tables

# Source plotting functions
source("./code/functions.R")

# Data -------------------------------------------------------------------------

results <- readRDS("./data/20220827-094950-run-lisa-9945538-9944296-9943298-main-res.rds")

# Pre-processing for plots -----------------------------------------------------

# Make NPCs as factor
results$npcs_f <- results$npcs_text <- factor(results$npcs)

# Make text npcs
levels(results$npcs_text) <- english::english(as.numeric(levels(results$npcs_f)))

# Make a version of CIC that is more readable
results$CIC <- round(results$coverage - 0.95, 3)

# Round the coverage
results$coverage <- round(results$coverage, 2)

# Change the names of the methods
levels(results$method) <- c("MI-PCR", "MI-SPCR", "MI-PLSR", "MI-PCovR", "MI-QP", "MI-AM", "MI-ALL", "CC", "Full data")

# Quality indicators -----------------------------------------------------------

# PRB
results$PRB_quality <- factor(
    results$PRB < 10,
    levels = c(TRUE, FALSE),
    labels = c("PRB < 10", "PRB > 10")
)

# Coverage
results$CIC_quality <- factor(
    results$coverage > 0.9,
    levels = c(TRUE, FALSE),
    labels = c("CIC > 0.9", "CIC < 0.9")
)

# Comprehensive performance (for CIW)
results$CIW_quality <- factor(
    results$PRB < 10 & results$coverage > 0.9,
    levels = c(TRUE, FALSE),
    labels = c("PRB < 10; CIC > 0.9", "other")
)

# Knitting ---------------------------------------------------------------------

# Compose name of Rnw based on input
Rnw_file <- paste0(args, ".Rnw")

# Knit file
knit(Rnw_file)