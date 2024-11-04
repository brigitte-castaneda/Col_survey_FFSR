########################################
###         Cleaning Survey 200n     ###
###           03/11/2024             ###
########################################
#rm(list = ls()) #borrar el ambiente y las tablas

#Librerias
pacman::p_load(tidyverse,haven,ggplot2,car, arrow, readxl, openxlsx, dplyr)

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

#### 2.1 - for pilot database ####
# Define the columns of municipalities to iterate
columns <- grep("^dq5p1r", names(Pilot_survey_n200), value = TRUE)
# Loop to see unique values for each column
for (col in columns) {
  unique_values <- unique(Pilot_survey_n200[[col]])
  print(paste("unique values for column", col, ":"))
  print(unique_values)
}
# Loop to display the frequency table only for columns where unique_count equals 2
for (col in columns) {
  unique_count <- length(unique(Pilot_survey_n200[[col]]))
  
  # Condición para filtrar columnas donde unique_count sea igual a 2
  if (unique_count == 2) {
    value_counts <- table(Pilot_survey_n200[[col]])
    print(paste("Tabla de frecuencias para la columna", col, ":"))
    print(value_counts)
  }
}
#Create a new table excluding the columns in the columns list
Pilot_survey_n200_sin_mun <- Pilot_survey_n200[, !(names(Pilot_survey_n200) %in% columns)]

#### 2.2 - for final database ####
# Define the columns of municipalities to iterate
#columns <- grep("^dq5p1r", names(Pilot_survey_n200), value = TRUE)
# Crear la nueva columna 'Municipio_Seleccionado' solo considerando las columnas deseadas
#Pilot_survey_n200 <- Pilot_survey_n200 %>%
#  rowwise() %>%
#  mutate(Municipio_Seleccionado = coalesce(!!!lapply(across(all_of(columns)), ~ ifelse(grepl("NO TO:", .), NA, .)))) %>%
#  ungroup()


#### 3- reordering and creating new columns ------
#reorder
Pilot_survey_n200_sin_mun <- Pilot_survey_n200_sin_mun[,c(5,7:9,17:20,22:26,39:40,42:46,48:113,117:135,6,10:14,21,27,41,47,29:37,114:116,1:4,15:16,28,38,137:195)]

#### q23 as dummy variable  ----------
columns <- colnames(Pilot_survey_n200_sin_mun[,c(24:38)])
# Loop to replace "NO TO:" with "" in each column
for (col in columns) {
  Pilot_survey_n200_sin_mun[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200_sin_mun[[col]])==T,         # Check if it starts with "NO TO:"
    0,      # If so, remove "NO TO:"
    1                          # Otherwise, keep the original value
  )
}
Pilot_survey_n200_sin_mun[columns]


#### q26, q27, q28 as dummy variable  ----------
columns <- colnames(Pilot_survey_n200_sin_mun)[c(41:60)]
# Loop to replace "NO TO:" with "" in each column
for (col in columns) {
  Pilot_survey_n200_sin_mun[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200_sin_mun[[col]])==T,         # Check if it starts with "NO TO:"
    0,      # If so, remove "NO TO:"
   1                          # Otherwise, keep the original value
  )
}
Pilot_survey_n200_sin_mun[columns]

#### q32 as dummy variable  ----------
columns <- colnames(Pilot_survey_n200_sin_mun)[c(64:76)]
# Loop to replace "NO TO:" with "" in each column
for (col in columns) {
  Pilot_survey_n200_sin_mun[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200_sin_mun[[col]])==T,         # Check if it starts with "NO TO:"
    0,      # If so, remove "NO TO:"
    1                          # Otherwise, keep the original value
  )
}
Pilot_survey_n200_sin_mun[columns]


#### creating variables of treatment and control as dummy variable  ----------
columns <- colnames(Pilot_survey_n200_sin_mun)[c(116:124)]
# Loop to replace "NO TO:" with "" in each column
for (col in columns) {
  Pilot_survey_n200_sin_mun[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200_sin_mun[[col]])==T,         # Check if it starts with "NO TO:"
    0,      # If so, remove "NO TO:"
    1                          # Otherwise, keep the original value
  )
}
Pilot_survey_n200_sin_mun[columns]


#### check
columns <- colnames(Pilot_survey_n200_sin_mun)[c(21:23)]
for (col in columns) {
  unique_values <- unique(Pilot_survey_n200_sin_mun[[col]])
  print(paste("unique values for column", col, ":"))
  print(unique_values)
}



#### 4- renaming as Qnaming rcode xlsx ------
column_names <- c("edad", "gnro", "etnia", "dept", "prsns", "edu", "trbjo", "ingrso", "grp_ingrso", "estrto", "moto", 
                  "moto_dias", "carro", "carro_dias", "ccnr", "attn", "cc_info", "cc_preocup", "cc_futuro", "cc_econ", 
                  "cc_imp_co2", "cc_imp_pers", "cc_imp_equit", "pol_prtds_col_hum", "pol_prtds_liga", "pol_prtds_cende", 
                  "pol_prtds_lib", "pol_prtds_cons", "pol_prtds_verdox", "pol_prtds_unpat", "pol_prtds_camra", 
                  "pol_prtds_mira", "pol_prtds_U", "pol_prtds_averd", "pol_prtds_pactoh", "pol_prtds_notie", 
                  "pol_prtds_otro", "pol_prtds_nodic", "pol_pres", "izq_der", "prop_eschr_labrl", "prop_eschr_pensl", 
                  "prop_eschr_fepc", "prop_eschr_salud", "prop_eschr_paz", "prop_eschr_energ", "prop_eschr_ning", 
                  "prop_entd_labrl", "prop_entd_pensl", "prop_entd_fepc", "prop_entd_salud", "prop_entd_paz", 
                  "prop_entd_energ", "prop_entd_ning", "prop_acrd_labrl", "prop_acrd_pensl", "prop_acrd_fepc", 
                  "prop_acrd_salud", "prop_acrd_paz", "prop_acrd_energ", "pais_gnrl", "pais_dmcrc", "pais_econ", 
                  "info_fnte_pernal", "info_fnte_perreg", "info_fnte_radnal", "info_fnte_radlol", "info_fnte_pscall", 
                  "info_fnte_tvnal", "info_fnte_tvreg", "info_fnte_web", "info_fnte_redscl", "info_fnte_vlls", 
                  "info_fnte_afic", "info_fnte_voz", "info_fnte_nose", "ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "gas_super", 
                  "dsl_super", "benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto", "frst_a", "frst_b", 
                  "frst_c", "frst_d", "ffsr_mnt", "ffsr_prcl", "ffsr_complet", "rr_lmpsm", "rr_pobre", "rr_afctds", 
                  "rr_deuda", "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst", "rr_mas", "rr_mas_si", "sesgo", 
                  "mod_age", "mod__reg", "mod__quoreg", "mod__expreg", "mod__mncp", "mod__urbn", "mod__incm", 
                  "mod__motor", "mod__ckfuel", "cc_econ_otro", "T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", 
                  "C_no_frst", "tcode", "tcop", "tpct", "record", "date", "stat", "smpl", "q5na1", "q5na2", "vlog", 
                  "tchang", "pgtm", "pgtm1", "pgtm2", "pgtm3", "pgtm4", "pgtm5nw", "pgtm5", "pgtm6", "pgtm7", "pgtm8", 
                  "pgtm9", "pgtm10", "pgtm11", "pgtm12", "pgtm13", "pgtm14", "pgtmt", "pgtm15", "pgtm16", "pgtm17", 
                  "pgtm18", "pgtm19", "pgtm20", "pgtm21", "pgtm22", "pgtm23", "pgtm24", "pgtm25", "pgtm26", "pgtm27", 
                  "pgtm28", "pgtm29", "pgtm30", "pgtm31", "pgtm32", "pgtm33", "pgtm34", "pgtm35", "pgtm36", "pgtm37", 
                  "pgtm38", "pgtm39", "pgtm40", "pgtm41", "pgtm41b", "pgtmexp", "pgtmintra", "pgtm42a", "pgtmintrb", 
                  "pgtmintrc", "pgtmintrd", "pgtm43", "pgtm44", "pgtm45", "pgtm46", "pgtm47", "pgtm48", "pgtm49", 
                  "pgtmend")


colnames(Pilot_survey_n200_sin_mun) <- column_names

## date format 
Pilot_survey_n200_sin_mun$date <- as.POSIXct(Pilot_survey_n200_sin_mun$date  / 1000, origin="1960-01-01", tz="UTC")  


#### 5- re-coding outcome variables  ------
glimpse(Pilot_survey_n200_sin_mun[,c(91:102)])

#### 5.1 - outcomes var unconditional ffsr_
# Load columns 91 to 93
original_data <- Pilot_survey_n200_sin_mun[, c(91:93)]

# Recoding for 5-scale ("recode_5")
for (i in 1:ncol(original_data)) {
  columna = names(original_data)[i]
  col_name <- paste0(columna, "_recode_5")
  temp = ifelse(original_data[[columna]] == "Muy de acuerdo", 5, 
                ifelse(original_data[[columna]] == "Algo de acuerdo", 4, 
                       ifelse(original_data[[columna]] == "Ni de acuerdo ni desacuerdo", 3, 
                              ifelse(original_data[[columna]] == "Algo en desacuerdo", 2, 1))))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Recoding for 3-scale ("recode_3")
for (i in 1:ncol(original_data)) {
  columna = names(original_data)[i]
  col_name <- paste0(columna, "_recode_3")
  temp = ifelse(original_data[[columna]] %in% c("Muy de acuerdo", "Algo de acuerdo"), 2, 
                ifelse(original_data[[columna]] == "Ni de acuerdo ni desacuerdo", 1, 0))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2A")
for (i in 1:ncol(original_data)) {
  columna = names(original_data)[i]
  col_name <- paste0(columna, "_recode_2A")
  temp = ifelse(original_data[[columna]] %in% c("Muy de acuerdo", "Algo de acuerdo"), 1, 
                ifelse(original_data[[columna]] == "Ni de acuerdo ni desacuerdo", NA, 0))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2B")
for (i in 1:ncol(original_data)) {
  columna = names(original_data)[i]
  col_name <- paste0(columna, "_recode_2B")
  temp = ifelse(original_data[[columna]] %in% c("Muy de acuerdo", "Algo de acuerdo"), 1, 0)
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2C")
for (i in 1:ncol(original_data)) {
  columna = names(original_data)[i]
  col_name <- paste0(columna, "_recode_2C")
  temp = ifelse(original_data[[columna]] %in% c("Muy de acuerdo", "Algo de acuerdo", "Ni de acuerdo ni desacuerdo"), 1, 0)
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}


#### 5.2 -- outcomes var conditional rr_
# Load columns 94 to 102
original_data_2 <- Pilot_survey_n200_sin_mun[, c(94:102)]

# Create new columns for each recoding with the specified names
# Recoding for 5-scale ("recode_5")
for (i in 1:ncol(original_data_2)) {
  columna = names(original_data_2)[i]
  col_name <- paste0(columna, "_recode_5")
  temp = ifelse(original_data_2[[columna]] == "Apoyaría MUCHO la política", 5, 
                ifelse(original_data_2[[columna]] == "Apoyaría ALGO la política", 4, 
                       ifelse(original_data_2[[columna]] == "INDECISO/A", 3, 
                              ifelse(original_data_2[[columna]] == "Apoyaría MUY POCO la política", 2, 1))))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Recoding for 3-scale ("recode_3")
for (i in 1:ncol(original_data_2)) {
  columna = names(original_data_2)[i]
  col_name <- paste0(columna, "_recode_3")
  temp = ifelse(original_data_2[[columna]] %in% c("Apoyaría MUCHO la política", "Apoyaría ALGO la política"), 2, 
                ifelse(original_data_2[[columna]] == "INDECISO/A", 1, 0))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2A")
for (i in 1:ncol(original_data_2)) {
  columna = names(original_data_2)[i]
  col_name <- paste0(columna, "_recode_2A")
  temp = ifelse(original_data_2[[columna]] %in% c("Apoyaría MUCHO la política", "Apoyaría ALGO la política"), 1, 
                ifelse(original_data_2[[columna]] == "INDECISO/A", NA, 0))
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2B")
for (i in 1:ncol(original_data_2)) {
  columna = names(original_data_2)[i]
  col_name <- paste0(columna, "_recode_2B")
  temp = ifelse(original_data_2[[columna]] %in% c("Apoyaría MUCHO la política", "Apoyaría ALGO la política"), 1, 0)
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}

# Binary recoding ("recode_2C")
for (i in 1:ncol(original_data_2)) {
  columna = names(original_data_2)[i]
  col_name <- paste0(columna, "_recode_2C")
  temp = ifelse(original_data_2[[columna]] %in% c("Apoyaría MUCHO la política", "Apoyaría ALGO la política", "INDECISO/A"), 1, 0)
  Pilot_survey_n200_sin_mun[[col_name]] = temp
}


## reordering
Pilot_survey_n200_sin_mun <- Pilot_survey_n200_sin_mun[,c(1:123,195:254,124:194)]

#### 6- Export the data frame to a CSV file
setwd(outputs)
write.csv(Pilot_survey_n200_sin_mun, file = "PilotCOL_cleaned.csv", row.names = FALSE)

# Export multiple data frames to an Excel file
write.xlsx(Pilot_survey_n200_sin_mun, file = "PilotCOL_cleaned.xlsx")



