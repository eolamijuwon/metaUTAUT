################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# PACKAGES INSTALLATION
#
################################################################################

# these are the required packages
pkgs <- c("broom",
          "showtext",
          "extrafont",

          "showtext",
          "scales",
          
          "kableExtra",
          "summarytools",
          "compareGroups",
          "tidyverse",
          "mosaic",
          "readstata13", 
          
          "psych",
          "psy",
          "lavaan",
          "semTools",
          "DiagrammeRsvg",
          "DiagrammeR",
          "semPlot",
          "rsvg",
          
          "ggpubr",
          "survey",
          "lavaan.survey"
)

# replaced w pacman, basically the same
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

library(pacman)
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}


p_load(pkgs, character.only = TRUE)
rm(pkgs)

