# Project:   paper-template
# Objective: Initialize the environment and knit any main document
# Author:    Edoardo Costantini
# Created:   2022-10-13
# Modified:  2022-11-29
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
library(qrcode) # for qr code generation

# Source plotting functions
source("./code/functions.R")

# Data -------------------------------------------------------------------------

results <- readRDS("./data/20220827-094950-run-lisa-9945538-9944296-9943298-main-res.rds")
pcovr_fix <- readRDS("./data/20221126-121849-pcovr-correct-alpha-tuning-pc-main-res.rds")

# Remove all pcovr from main
results <- results[results$method != "pcovr", ]

# Remove all other methods from pcovr results
pcovr_fix <- pcovr_fix[pcovr_fix$method == "pcovr", ]

# Append PCovR from new results
results <- rbind(results, pcovr_fix)

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

# Adjust order of levels
results$method <- factor(results$method, levels = c("MI-PCR", "MI-SPCR", "MI-PCovR", "MI-PLSR", "MI-QP", "MI-AM", "MI-ALL", "CC", "Full data"))

# Make lv a factor with levels
results$nla <- factor(
    results$nla,
    levels = sort(unique(results$nla)),
    labels = c("2 latent vars", "10 latent variables", "50 latent variables")
)

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