########################################
###         Cleaning Survey 200n     ###
###           04/11/2024             ###
########################################
rm(list = ls()) # Clear the environment and tables

# Libraries
pacman::p_load(tidyverse, haven, ggplot2, car, arrow, readxl, openxlsx, dplyr, readr)

#### 0- Set file paths
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

#### 0- Load database ----
setwd(inputs)
Pilot_survey_n200 <- read_csv("Pilot_survey_n200.csv")

#### 1- Initial data review -----
head(Pilot_survey_n200)
str(Pilot_survey_n200)

## Count unique values in each column (for categorical variables)
sapply(Pilot_survey_n200, function(x) length(unique(x)))

## Count missing values by column
colSums(is.na(Pilot_survey_n200)) # No missing values

### 1.1 Group municipalities (question q5) ------
# Identify columns starting with "dq5p1r"
columns <- grep("^dq5p1r", names(Pilot_survey_n200), value = TRUE)

# Loop to see unique values for each selected column
for (col in columns) {
  cat("\nUnique values for column:", col, "\n")
  print(table(Pilot_survey_n200[[col]]))
}

# Create new 'q5' column considering only selected columns
Pilot_survey_n200 <- Pilot_survey_n200 %>%
  rowwise() %>%
  mutate(q5 = first(na.omit(sapply(across(all_of(columns)), function(x) {
    ifelse(grepl("^NO TO:", x), NA, x)
  })))) %>%
  ungroup()

# Exclude columns listed in "columns"
Pilot_survey_n200 <- Pilot_survey_n200[, !(names(Pilot_survey_n200) %in% columns)]

#### 2- Reordering and renaming columns ------
## Reorder table as follows:
# socio-dem q1:q11
# VEHICLE USE q12:q15. (for final table q12:q23)
# cooking q16 (q24)
# Attention q17 (q25)
# CLIMATE q18:q22 (q26:q30)
# POLITICAL (support/non-supporter) q23 (q31)
# POLITICAL q24:q29, q30, q31 (q32:q34, q39, q40)
# TRUST IN GOV *not included in the pilot* (q38)
# NEWS q32 (q41)
# SUBSIDY KNOWLEDGE q33:q38 (q42:q47)
# FAIRNESS TO ME q39:q41 (q48:q51)
# PROTEST SALIENCE *not included in the pilot* (q52:q53)
# Attention check *not included in the pilot* (q54)
# FIRST STAGE q42 (q55)
# STATUS QUO q43 (q56)
# UNCONDITIONAL - PARTIAL q44 (q57)
# UNCONDITIONAL - COMPLETE q45 (q58)
# CONDITIONAL q46 (q59)
# TREATMENT/CONTROLS "^HCELLSr"
# TIME PER QUESTION "pagetime"

Pilot_survey_n200 <- Pilot_survey_n200[, c(5,7:9,196,17:20,22:26,39:40,42:46,48:113,117:135,29:37,114:116,1:4,28,38,137:195)]
# This line should be adjusted to the new table

## 2.1 Rename columns according to "Qnaming rcode xlsx" 
column_names_pilot <- c("edad", "gnro", "etnia", "dept", "mpio", "prsns", "edu", "trbjo", "ingrso", "grp_ingrso", "estrto", "moto", 
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
                        "T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", 
                        "C_no_frst", "tcode", "tcop", "tpct", "record", "date", "stat", "smpl", "vlog", 
                        "tchang", "pgtm", "pgtm1", "pgtm2", "pgtm3", "pgtm4", "pgtm5nw", "pgtm5", "pgtm6", "pgtm7", "pgtm8", 
                        "pgtm9", "pgtm10", "pgtm11", "pgtm12", "pgtm13", "pgtm14", "pgtmt", "pgtm15", "pgtm16", "pgtm17", 
                        "pgtm18", "pgtm19", "pgtm20", "pgtm21", "pgtm22", "pgtm23", "pgtm24", "pgtm25", "pgtm26", "pgtm27", 
                        "pgtm28", "pgtm29", "pgtm30", "pgtm31", "pgtm32", "pgtm33", "pgtm34", "pgtm35", "pgtm36", "pgtm37", 
                        "pgtm38", "pgtm39", "pgtm40", "pgtm41", "pgtm41b", "pgtmexp

", "pgtmintra", "pgtm42a", "pgtmintrb", 
                        "pgtmintrc", "pgtmintrd", "pgtm43", "pgtm44", "pgtm45", "pgtm46", "pgtm47", "pgtm48", "pgtm49", 
                        "pgtmend")

colnames(Pilot_survey_n200) <- column_names_pilot 

# Adjust this line to the new table, including the following names:
# "carro_combus", "precio_combus", "transpub", "transpub_dias", "taxi", "taxi_dias", "bici", "bici_dias" (q16:q23)
# "pais_confianza" (q38)
# "protestas" (q52)
# "protestas_lid" (q53)
# "attn2" (q54)

#### 3- Variable format adjustments --------

## 3.1 SOCIO DEM q1:q11 variables ----
# 'grp_ingrso'
Pilot_survey_n200$grp_ingrso <- sub(".*: ", "", Pilot_survey_n200$grp_ingrso)
# 'estrto'
Pilot_survey_n200$estrto <- sub("Estrato ", "", Pilot_survey_n200$estrto)

## 3.2 VEHICLE USE q12:q15. (for final table q12:q23)  ----
# Convert vehicle usage days to numeric
convert_to_numeric <- function(x) {
  case_when(
    x == "1 día a la semana" ~ 1,
    x == "2 días a la semana" ~ 2,
    x == "3 días a la semana" ~ 3,
    x == "4 días a la semana" ~ 4,
    x == "5 días a la semana" ~ 5,
    x == "6 días a la semana" ~ 6,
    x == "7 días a la semana" ~ 7,
    TRUE ~ NA_real_  # Keep NAs
  )
}

# Apply the function to the columns of interest
Pilot_survey_n200 <- Pilot_survey_n200 %>%
  mutate(
    moto_dias = convert_to_numeric(moto_dias),
    carro_dias = convert_to_numeric(carro_dias),
    days_per_week = pmax(moto_dias, carro_dias, na.rm = TRUE)  # Take the maximum between moto and car
  )

## For final table use this:
# PilotCOL_cleaned <- PilotCOL_cleaned %>%
#   mutate(
#     moto_dias = convert_to_numeric(moto_dias),
#     carro_dias = convert_to_numeric(carro_dias),
#     transpub_dias = convert_to_numeric(transpub_dias),
#     taxi_dias = convert_to_numeric(taxi_dias),
#     bici_dias = convert_to_numeric(bici_dias),
#     days_per_week = pmax(moto_dias, carro_dias, transpub_dias, taxi_dias, bici_dias, na.rm = TRUE)  # Take the maximum
#   )

# Loop to transform each vehicle into a dummy
columns <- c("moto", "carro")
## for final table:
# columns <- c("moto", "carro", "transpub", "taxi", "bici")

for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(Pilot_survey_n200[[col]] == "Si", 1, 0)
}

# 3.3 COOKING q16 (q24) ----
Pilot_survey_n200 <- Pilot_survey_n200 %>%
  mutate(ccnr = case_when(
    ccnr == "Gas propano (en cilindro o pipeta)" ~ "Propane gas",
    ccnr == "Gas natural conectado a red pública" ~ "Natural gas",
    ccnr == "Electricidad" ~ "Electricity",
    ccnr == "Carbón mineral" ~ "Coal",
    ccnr == "Keroseno, petróleo, gasolina, cocinol, alcohol" ~ "Other fuels",
    ccnr == "Leña, madera, carbón de leña" ~ "Firewood",
    TRUE ~ ccnr  # Keep any other value unchanged
  ))

# 3.4 CLIMATE q18:q22 (q26:q30) ----
columns <- c("cc_info", "cc_preocup")
# Loop to replace values, keeping only numbers and converting to numeric
for (col in columns) {
  Pilot_survey_n200[[col]] <- as.numeric(sub(" -.*", "", Pilot_survey_n200[[col]]))
}

# Replace unique values in 'cc_futuro' with shorter phrases
Pilot_survey_n200 <- Pilot_survey_n200 %>%
  mutate(cc_futuro = case_when(
    cc_futuro == "... es un problema urgente del que tenemos que ocuparnos hoy" ~ "Urgent today",
    cc_futuro == "... todavía no es un problema urgente, pero lo será en el futuro" ~ "Urgent in future",
    cc_futuro == "... es un problema urgente, pero no hay nada que hacer, es demasiado tarde para actuar" ~ "Urgent but too late",
    cc_futuro == "No estoy seguro/a" ~ "Not sure",
    cc_futuro == "... nunca será un problema del que sea necesario ocuparse" ~ "Never a problem",
    TRUE ~ cc_futuro
  ))

# Replace unique values in 'cc_econ' with shorter phrases
Pilot_survey_n200 <- Pilot_survey_n200 %>%
  mutate(cc_econ = case_when(
    cc_econ == "Hay que dar la misma prioridad tanto al cambio climático como al crecimiento económico" ~ "Equal priority",
    cc_econ == "Hay que darle prioridad a la lucha contra el cambio climático, sin importar sus consecuencias negativas en el crecimien" ~ "Priority to climate",
    cc_econ == "Otra:" ~ "Other",
    cc_econ == "Hay que dar prioridad al crecimiento económico sin importar sus consecuencias negativas en la lucha contra el cambio cl" ~ "Priority to economy",
    cc_econ == "No estoy seguro(a)" ~ "Not sure",
    TRUE ~ cc_econ
  ))

# Replace values in 'cc_imp' keeping only the number and converting to numeric
columns <- c("cc_imp_co2", "cc_imp_pers", "cc_imp_equit")
for (col in columns) {
  Pilot_survey_n200[[col]] <- as.numeric(sub(".*([0-9])$", "\\1", Pilot_survey_n200[[col]]))
}

# 3.5 POLITICAL (support/non-supporter) q23 (q31) ----
# Replace political parties as dummy variables
columns <- grep("^pol_prtds_", names(Pilot_survey_n200), value = TRUE)
for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200[[col]]) == TRUE,  # Check if it starts with "NO TO:"
    0,  # Remove "NO TO:"
    1  # Otherwise, keep the original value
  )
}
Pilot_survey_n200[columns]

# 3.6 POLITICAL q24:q32 (q32:q36) ----
# Replace values in 'izq_der' to keep only the number and convert to numeric
Pilot_survey_n200$izq_der <- as.numeric(sub(".*([0-9])$", "\\1", Pilot_survey_n200$izq_der))

# Government reforms as dummy variables
columns <- grep("^prop_eschr_|^prop_entd_|^prop_acrd_", names(Pilot_survey_n200), value = TRUE)
for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200[[col]]) == TRUE,  # Check if it starts with "NO TO:"
    0,  # Remove "NO TO:"
    1  # Otherwise, keep the original value
  )
}

# Loop to extract the leading number and convert to numeric
columns <- c("pais_dmcrc", "pais_econ")
for (col in columns) {
  Pilot_survey_n200[[col]] <- as.numeric(sub("^([0-9]).*", "\\1", Pilot_survey_n200[[col]]))
}

# 3.7 TRUST IN GOV *not included in the pilot* (q38) ----
# Assuming we have several columns that start with "pais_confianza_" according to each response option
# Assuming the responses are as follows:
# 1. Nada de confianza
# 2. Poca confianza
# 3. Ni confianza ni desconfianza
# 4. Algo de confianza
# 5. Mucha confianza
# We create a loop to extract the numbers
# columns <- grep("^pais_confianza_", names(Pilot_survey_n200), value = TRUE)
# for (col in columns) {
#   Pilot_survey_n200[[col]] <- as.numeric(sub("^([0-9]).*", "\\1", Pilot_survey_n200[[col]]))
# }
# If there are no numbers, leave it as it is

# 3.8 NEWS q32 (q41) ----
# Media as dummy variable
columns <- grep("^info_fnte_", names(Pilot_survey_n200), value = TRUE)
for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(
    grepl("^NO TO:", Pilot_survey_n200[[col]])==T,         # Check if it starts with "NO TO:"
    0,      # If so, remove "NO TO:"
    1                          # Otherwise, keep the original value
  )
}

# 3.9 SUBSIDY KNOWLEDGE q33:q38 (q42:q47) ----
# Transform columns to 1, 0, or NA
columns <- c("ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "gas_super", "dsl_super")
for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(Pilot_survey_n200[[col]] %in% c("Si", "Sí"), 1,
                                     ifelse(Pilot_survey_n200[[col]] == "No", 0, NA))
}

# 3.10 FAIRNESS TO ME q39:q41 (q48:q51) ----
columns <- c("benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto")
# Loop to extract only the number and convert to numeric
for (col in columns) {
  Pilot_survey_n200[[col]] <- as.numeric(sub("^([0-9]).*", "\\1", Pilot_survey_n200[[col]]))
}

# 3.11 PROTEST SALIENCE *not included in the pilot* (q52:q53) ----
# Pilot_survey_n200$protestas <- ifelse(Pilot_survey_n200$protestas %in% c("Si", "Sí"), 1,
#                                    ifelse(Pilot_survey_n200$protestas == "No", 0, NA))

# 3.12 TREATMENT/CONTROLS "^HCELLSr" ----
columns <- c("T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", "C_no_frst")
# Loop to transform values into a dummy (1 if different from "NO TO:", 0 otherwise)
for (col in columns) {
  Pilot_survey_n200[[col]] <- ifelse(grepl("^NO TO:", Pilot_survey_n200[[col]]), 0, 1)
}

# TIME PER QUESTION "pagetime"
## Date format
Pilot_survey_n200$date <- as.POSIXct(Pilot_survey_n200$date / 1000, origin = "1960-01-01", tz = "UTC")

#### 4- Transformations outcome variables ------
# 4.1 TRUST IN GOV *not included in the pilot* (q38) ----
# Assuming we have several columns that start with "pais_confianza_" according to each response option
# Assuming the responses are as follows:
# Nada de confianza
# Poca confianza
# Ni confianza ni desconfianza
# Algo de confianza
# Mucha confianza
# We create a loop to extract the numbers
# columns <- grep("^pais_confianza_", names(Pilot_survey_n200), value = TRUE)
# for (col in columns) {
#   Pilot_survey_n200[[col]] <- as.numeric(sub("^([0-9]).*", "\\1", Pilot_survey_n200[[col]]))
# }
# If there are no numbers, leave it as is

# 4.2 STATUS QUO, UNCONDITIONAL - PARTIAL, UNCONDITIONAL - COMPLETE ----
# Define the columns to transform
columns <- c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")

#### 4.2.1 Binary recoding ("recode_2") "Muy de acuerdo", "Algo de acuerdo" = 1, 0 other ----
# Loop to create new binary columns
for (col in c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")) {
  # Define the name of the new column
  col_name <- paste0(col, "_recode_2")
  
  # Recode responses to binary
  Pilot_survey_n200[[col_name]] <- ifelse(Pilot_survey_n200[[col]] %in% c("Muy de acuerdo", "Algo de acuerdo"), 1, 0)
}

# Check the first rows of the new columns
head(Pilot_survey_n200[c("ffsr_mnt_recode_2", "ffsr_prcl_recode_2", "ffsr_complet_recode_2")])

##### 4.2.2 Binary recoding extrem ("recode_2extrem") "Muy de acuerdo" = 1, "Muy en desacuerdo" = 0 ---------
# Loop to create new columns capturing extreme agreement/disagreement
for (col in c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")) {
  # Define the name of the new column
  col_name <- paste0(col, "_recode_2extrem")
  
  # Recode responses for extreme agreement/disagreement
  Pilot_survey_n200[[col_name]] <- ifelse(Pilot_survey_n200[[col]] == "Muy de acuerdo", 1,
                                          ifelse(Pilot_survey_n200[[col]] == "Muy en desacuerdo", 0, NA))
}

# Check the first rows of the new columns
head(Pilot_survey_n200[c("ffsr_mnt_recode_2extrem", "ffsr_prcl_recode_2extrem", "ffsr_complet_recode_2extrem")])

##### 4.2.3 Recoding for 3-scale ("recode_3") -------
# Loop to create new columns with a 3-point Likert recoding
for (col in c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")) {
  # Define the name of the new column
  col_name <- paste0(col, "_recode_3")
  
  # Recode responses to a 3-point Likert scale
  temp <- ifelse(Pilot_survey_n200[[col]] %in% c("Muy de acuerdo", "Algo de acuerdo"), 2,
                 ifelse(Pilot_survey_n200[[col]] == "Ni de acuerdo ni desacuerdo", 1, 0))
  
  # Assign the recoded values to the new column
  Pilot_survey_n200[[col_name]] <- temp
}

# Check the first rows of the new columns
head(Pilot_survey_n200[c("ffsr_mnt_recode_3", "ffsr_prcl_recode_3", "ffsr_complet_recode_3")])

# 4.3 CONDITIONAL ----
# Define the columns to transform
# for the pilot:
columns <- c("rr_lmpsm", "rr_pobre", "rr_afctds", "rr_deuda", "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst")
## for the final table add "rr_impuesto"
# columns <- c("rr_lmpsm", "rr_pobre", "rr_afctds", "rr_deuda", "rr_impuesto", "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst")

#### 4.3.1 Binary recoding ("recode_2") "Apoyaría MUCHO la política", "Apoyaría ALGO la política" = 1, 0 other ----
# Loop to create new binary columns
for (col in columns) {
  # Define the name of the new column with the "_recode_2" suffix
  col_name <- paste0(col, "_recode_2")
  # Recode responses as binary
  Pilot_survey_n200[[col_name]] <- ifelse(Pilot_survey_n200[[col]] %in% c("Apoyaría MUCHO la política", "Apoyaría ALGO la política"), 1, 0)
}
# Check the first rows of the new columns for extreme recoding
head(Pilot_survey_n200[grep("_recode_2$", names(Pilot_survey_n200), value = TRUE)])

# 4.3.2 Binary recoding for extreme options ("recode_2extrem")
columns <- c("rr_lmpsm", "rr_pobre", "rr_afctds", "rr_deuda", "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst")
for (col in columns) {
  col_name <- paste0(col, "_recode_2extrem")
  Pilot_survey_n200[[col_name]] <- ifelse(Pilot_survey_n200[[col]] == "Apoyaría MUCHO la política", 1,
                                          ifelse(Pilot_survey_n200[[col]] == "Apoyaría NADA la política", 0, NA))
}

# Check the first rows of the new columns for extreme recoding
head(Pilot_survey_n200[grep("_recode_2extrem$", names(Pilot_survey_n200), value = TRUE)])

# 4.3.3 Recoding for 3-scale ("recode_3")
for (col in columns) {
  col_name <- paste0(col, "_recode_3")
  Pilot_survey_n200[[col_name]] <- ifelse(Pilot_survey_n200[[col]] %in% c("Apoyaría MUCHO la política", "Apoyaría

 ALGO la política"), 2,
                                          ifelse(Pilot_survey_n200[[col]] == "INDECISO/A", 1, 0))
}

# Check the first rows of the new columns for 3-point Likert recoding
head(Pilot_survey_n200[grep("_recode_3$", names(Pilot_survey_n200), value = TRUE)])


View(Pilot_survey_n200[, -c(116:183)])

#### 5- Export the data frame -----
### to a CSV file
setwd(outputs)
write.csv(Pilot_survey_n200[, -c(116:183)], file = "PilotCOL_cleaned.csv", row.names = FALSE) # without time pages

# Export multiple data frames to an Excel file
write.xlsx(Pilot_survey_n200[, -c(116:183)], file = "PilotCOL_cleaned.xlsx")
```