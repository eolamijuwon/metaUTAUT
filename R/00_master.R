
################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# MASTER SCRIPT
#
################################################################################

# This is a master script that calls other scripts one-by-one
# to replicate the figure discussed in the paper. 


## Step 1: Package Installation -------------------------
source ("R/01_packages_install.R")

## Step 2: Data Wrangling/Exploratory and Analysis ------
source ("R/02_exploratory analysis.R")

## Step 3: Measurement Model ----------------------------
source ("R/03_measurement model.R")

## Step 4: Structural Model ----------------------------
source ("R/04_structural_model.R")


## Step 5: Supplementary Analysis ----------------------------
source ("R/05_supplementary_analysis.R")
