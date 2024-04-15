language <- "Français (fr)"

## Updating the bar. 
n=10
incProgress(2/n, detail = "Fetching kobo data")

# Error Handeling on empty uid or other issues
tryCatch(expr = {
  
  # get language
  lang_dat <- kobo_lang(uid)
  
  lang_dat <- lang_dat[str_detect(tolower(lang_dat), "fran")]
  
  #loading p21 V3 data
  data_raw <- kobo_data(uid, lang = lang_dat)
  
  #get kobo form
  form_V3_original <- kobo_form(uid)
  
},warning=function(w){
  print(paste("Warning: Review the step before",w))
  
},error=function(e){
  if(uid==""){
    print(paste("The uid is empty. No kobo data could be imported for uid:", uid))
    stop("unable to import data")
  }else{
    print(paste("Something went wrong. no data could be imported for the uid:",uid, "Double check the uid.", e))
    stop("unable to import data")
  }
},finally = {
  print("Execution completed.")
})



## Updating the bar
incProgress(5/n, detail = "data fetched")


#
form_V3 <- form_V3_original



######################################
# This step is executed due to some issue found in Kobo
# where exported data includes columns from previously discarded version
# removing previous versions columns

data_raw <- data_raw %>% 
  relocate(`_submission_time`, .before = `_submitted_by`) %>%  
  select(start:`_submitted_by`)




## Removing system columns
data_raw <- data_raw %>% 
  select(!starts_with("_") | c(`_id`, `_submission_time`))


### for KII
if(srv=="KII" & "interviewer_sex" %in% names(data_raw) & "interviewee_sex" %in% names(data_raw)){
  
  data_raw <- data_raw %>%
    mutate(interviewee_sex=if_else(is.na(interviewee_sex),interviewer_sex, interviewee_sex)) %>%
    select(!interviewer_sex)
  
}


### Cleaning the data
# filtering correct data

if(("test" %in% names(data_raw)) & (ctry!="CM" & srv=="HH"))
  data_clean <- data_raw |>
    filter(tolower(test)=="real", tolower(consent)=="true") |> # removing test observation and non-consentant
    filter(tolower(respondent_is_major)=="true")

if(("is_test" %in% names(data_raw)) & (ctry=="CM" | srv=="KII"))
  data_clean <- data_raw |>
  filter(tolower(is_test)=="real", tolower(consent)=="true") |> # removing test observation and non-consentant
  filter(tolower(respondent_is_major)=="true")


######
if(ctry!="CM")
  data_clean <- data_clean |>
    rename_with(~str_replace(.x, "interviewer", "interviewee"), contains("interviewer") & !contains("interviewer_id")) # This variable is renamed to keep all freetext starting with other_ consistent
 
###### 
data_clean <- data_clean |>
  mutate(across(.cols = where(is.character)& starts_with("other_"), ~str_squish(str_trim(.x))))

## Updating the bar. 
incProgress(6/n, detail = "Preprocessing data")


## correcting names in the form

form_V3$name <- str_replace_all(form_V3$name, "interviewer", "interviewee")

## interviewee vs interviewer
if("interviewee_id" %in% form_V3$name)
  str_replace_all(form_V3$name, "interviewee_id", "interviewer_id")


## Updating the bar. 
incProgress(7/n, detail = "Preprocessing data")


# copying
data_clean <<- data_clean

form_V3_original <<- form_V3_original

form_V3 <<- form_V3
