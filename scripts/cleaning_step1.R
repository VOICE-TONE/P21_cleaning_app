##################################
#### cleaning original version ####

# source cleaning functions
source("cleaning_functions.R")

# source cleaning script
source("cleaning_p21_HH_V3_script.R")


######### Saving ########
data_clean %>%  writexl::write_xlsx(., path=paste("UNHCR_P21_HH_V3", unique(data_clean$country)[1], "DAT_CLEAN_V1.1.xlsx", sep = "_"))



data_clean_fr <- data_clean