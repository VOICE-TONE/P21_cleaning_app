########### Dealing with data by type ########
### Variables with completely missing data
dat_empty <- data_clean %>%
  select(all_of(names(data_clean)[colSums(is.na(data_clean))==nrow(data_clean)])) %>% names()

#############################################
### Dealing with all date and time variables
# Automatically detecting and Selecting data variable
dat_date <- data_clean %>%
  select(!all_of(dat_empty)) %>%
  select_if(\(x) if_else(class(try({lubridate::is.Date(as.Date(as.character(na.omit(x))))}, silent = TRUE))=="try-error",FALSE,TRUE)) %>%
  names()

# reformating dates
#data_clean <- data_clean %>% mutate(across(.cols = all_of(dat_date), .fns = ~as.Date(.x, "%Y-%m-%d")))

#############################################
### selecting all numeric and double variables
dat_num <- data_clean %>%
  select(!all_of(c(dat_empty, dat_date))) %>%
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:digit:]") & !str_detect(na.omit(x),pattern="[:alpha:]"))|is.double(x)|is.numeric(x)|is.integer(x)) %>% names()


#############################################
### identifying freetext variables

dat_freetext <- (form_V3 %>% filter(tolower(type)=="text"))$name

#############################################
### Selecting all character variables excluding empty variables first
dat_char <- data_clean %>%
  select(!all_of(c(dat_empty, dat_date, dat_num))) %>%
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:alpha:][:digit:]|[:digit:][:alpha:]|[:alpha:]"))|is.character(x)) %>% names()
