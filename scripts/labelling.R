data_clean_fr <- data_clean

#################################
######## French version #########

# Fixing issues with form

form_V3_original <- form_V3_original %>% mutate(lang=stringr::str_replace_all(tolower(lang), "français (fr)" ,"Francais (fr)"))

########################
# converting multichoices
data_clean_fr <- val_labels_sm_from_form_(x=data_clean_fr, form=form_V3_original, lang = "Francais (fr)")

## updating progres bar
n=10
incProgress(6/n, detail = "MCQ completed")


#### Labelling single choice variables

data_clean_fr <- to_factor(data_clean_fr)


## Verifying unlabelled columns
all_vars_to_label <- form_V3_original$name[form_V3_original$type %in% c("select_one", "select_multiple")]

all_vars_to_label <- all_vars_to_label[tolower(all_vars_to_label) %in% tolower(names(data_clean))]


for(x in all_vars_to_label){
  
  if(isTRUE(all.equal(data_clean_fr[[x]], data_clean[[x]]))){
    
    print(paste(x, "has is not labelled. Labelling", x))
    
    data_clean_fr[[x]] <- stri_replace_all_regex(data_clean_fr[[x]],
                                                 paste0("\\b", form_V3_original$choices[form_V3_original$name==x][[1]]$value_name, "\\b"),
                                                 form_V3_original$choices[form_V3_original$name==x][[1]]$value_label,
                                                 vectorize_all = FALSE)
    
    print(paste(x, "labelling completed"))
    
    
  }
  
}

#data_clean_fr <- val_labels_so_from_form_(x=data_clean_fr, form=form_V3_original, lang = "Francais (fr)")





## updating progres bar
incProgress(7/n, detail = "Single Choice Completed")


######################
### Names to french names

names_tbl <- tibble(name=names(data_clean_fr))


names_tbl <- names_tbl %>%
  left_join(
    (form_V3 %>%
       select(name, label, type)), by="name", relationship = "many-to-many") %>%
  mutate(type=if_else(!is.na(label) & is.na(type), name, type),
         type=if_else(tolower(type) %in% c("text", "note"),"text", type))

#names_tbl$label <- zoo::na.locf(names_tbl$label)

# setting labels to names for names startsing with "_" which are system variables
names_tbl <- names_tbl %>% mutate(label=if_else(grepl("^_|^(start|end|today)$", name), name, label))

# setting names for dummy variables
names_tbl <- names_tbl %>%
  mutate(
    type=zoo::na.locf(type),
    dummy=if_else(is.na(label)& type %in% c("select_multiple", "geopoint"), 1,0),
    label=zoo::na.locf(label))


# finalizing dummy variables name
mcq_vars <- names_tbl$name[names_tbl$type=="select_multiple"&names_tbl$dummy==0]

# matching variables starting with mcq_vars_

names_tbl <- names_tbl %>%
  mutate(choice=
           if_else(
             grepl(paste0("^",mcq_vars,"_", collapse = "|"), name),
             str_replace(name, paste0(mcq_vars,"_", collapse = "|"), ""),
             name))




##############################
### converting value of labels
if(ctry!="CM")
  form_V3 <- form_V3 %>% filter(lang=="Francais (fr)")

if(ctry=="CM")
  form_V3 <- form_V3 %>% filter( lang == "Français (fr)")



## counting the number of choices of each variable
form_V3$length <- as.numeric(lapply(form_V3$choices, nrow))

# adding original names
form_V3$choices2 <- lapply(form_V3$name, \(x) if(form_V3$length[form_V3$name==x]>0) as_tibble(mutate(form_V3$choices[form_V3$name==x][[1]],original=x)))

# creating the name variable binding original name and choice
form_V3$choices2 <- lapply(form_V3$name, \(x) if(form_V3$length[form_V3$name==x]>0) as_tibble(mutate(form_V3$choices2[form_V3$name==x][[1]],name=paste(original, value_name, sep = "_"))))

all_choices <- bind_rows(form_V3$choices2)

# filtering choices that are relevant

all_choices <- all_choices[all_choices$name %in% names_tbl$name, c("name", "value_label")]


### joining to the original names table

names_tbl <- names_tbl %>% left_join(all_choices, by="name", relationship = "many-to-many")

### filter only relevant names to the french version

names_tbl <- names_tbl %>% filter(!duplicated(name))


### Newly create choices
names_tbl <- names_tbl %>% mutate(new_variables= if_else(dummy==1 & type=="select_multiple" & is.na(value_label), 1, 0),
                                  value_label= if_else(dummy==1 & type %in% c("select_multiple", "geopoint") & is.na(value_label), str_to_title(str_replace_all(name, "_", " ")), value_label))


### converting variable names
names_tbl <- names_tbl %>% mutate(new_names = if_else(dummy==1 & type %in% c("select_multiple", "geopoint"), paste(label, value_label, sep = "/"), label))


##############################################
### Applying the variable names to the french dataframe

if(all(names(data_clean_fr) %in% names_tbl$name)){
  
  names(data_clean_fr) <- names_tbl$new_names[names_tbl$name %in% names(data_clean_fr)]
}


data_clean_fr <<- data_clean_fr