########################################
###         Cleaning Survey total    ###
###           03/11/2024             ###
########################################
#rm(list = ls()) #borrar el ambiente y las tablas

#Librerias
pacman::p_load(tidyverse,haven,ggplot2,car, arrow, readxl, openxlsx)

#### 0- Set directions
if (grepl("brigittecastaneda", getwd())) {
  path <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/"
  inputs <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/Inputs/"
  outputs <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/Outputs/"
} else if (grepl("k.castaneda", getwd())) {
  path <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/"
  inputs <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/Inputs/"
  outputs <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/Outputs/"
}


getwd()

#### 1 load database
setwd(inputs)
Pilot_survey_n200 <- read_csv("Pilot_survey_n200.csv")

#### 2- question Q5 municipality ------
#Cleaning process of the municipality columns is different for the pilot and for the final survey results.
