## variables of interest
free_text_vars <- form_V3_original$name[form_V3_original$type %in% c("text", "note")]



## removing system variables
data_clean <- data_clean |> 
  select(names(data_clean)[!names(data_clean) %in% c("deviceid", "interviewer_id", "phonenumber", "uuid", "instanceID", "rootUuid", "deprecatedID")])

### Cleaning the data
data_clean_copy1 <- data_clean

########### Dealing with data by type ########
### Variables with completely missing data
dat_empty <<- data_clean %>% 
  select(all_of(names(data_clean)[colSums(is.na(data_clean))==nrow(data_clean)])) %>% names()

#############################################
### Dealing with all date and time variables
# Automatically detecting and Selecting data variable
dat_date <<- data_clean %>% 
  select(!all_of(dat_empty)) %>% 
  select_if(\(x) if_else(class(try({lubridate::is.Date(as.Date(as.character(na.omit(x))))}, silent = TRUE))=="try-error",FALSE,TRUE)) %>% 
  names()

# reformating dates  
data_clean <- data_clean %>% mutate(across(.cols = all_of(dat_date), .fns = ~as.Date(.x, "%Y-%m-%d")))

#############################################
### selecting all numeric and double variables 
dat_num <<- data_clean %>% 
  select(!all_of(c(dat_empty, dat_date))) %>% 
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:digit:]") & !str_detect(na.omit(x),pattern="[:alpha:]"))|is.double(x)|is.numeric(x)|is.integer(x)) %>% names()



## Dealing with numerical variables

### Selecting all character variables excluding empty variables first
dat_char <- data_clean %>% 
  select(!all_of(c(dat_empty, dat_date, dat_num))) %>% 
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:alpha:][:digit:]|[:digit:][:alpha:]|[:alpha:]"))|is.character(x)) %>% 
  names()


#########################################################
##### Cleaning Free text according to the protocol ######


gane_keywords_regex <- "gane|non(.+)etatique|armee|arme(.+)non(.+)etatique|boko(.+)haram|eigs|etat(.+)islamique|gsim|groupe(.+)soutien(.+)islam|aqmi|gspc|salafiste|mma|mouvement(.+)arme(.+)azawad|mnla|mouvement(.+)national(.+)liberation(.+)azawad|mujao|mouvement(.+)djihad|djihad"

gani_keywords_regex <- "gani|hani|arme(.+)non(.+)identifie"

procedure_related_regex <- c("difficile|comment obtenir|pas facile|impossible|complique|etranger|pas chez moi|pas (.+) pays|procedure|competen|authori|pu faire|fuir(.+)laiss")

document_lost_regex <- "(carte|piece|doc|acte|affair)(.+)(perdu|brule|cherche|brusque|precipite|abandon|egare|reste)|(perdu|brule|cherche|brusque|precipite|abandon)(.+)(carte|piece|doc|acte|affair)|perdu|reste"

manque_moyen_regex <- "financ|argent|manque moyen|payer|moyen"

force_displace_regex <- paste0(c("fui|depart|parti|deplace|temps|oubli)(.+)(lais|brusq|precipit|force|recuper|prendr|subit|crise)"), collapse = "|")

local_ngo_regex <- paste0("action(.+)(humanitaire|social|croix rouge|ong)", collapse = "|")

village_elders_regex <- paste0(c("(conseille|chef|ancien|ain?|ancien|notable)(.+)(village|coutum)"), collapse = "|")

community_leaders_regex <- paste0(c("(leader|responsabl|representant|chef)(.+)(communautaire|local|coutum|groupement|femme|homme|groupement|canton)"), collapse = "|") ## Should pastors and priest be consider community leaders or not?

religious_leaders_regex <- paste0(c("pasteur|pretre|imam|religieu)"), collapse = "|")

local_authority_regex <- paste0(c("maire|mairie|elu|prefect|commune|(service|structure)(.+)(social|etatique|communautair|local|locaux)"), collapse = "|")

public_security_forces_regex <- paste0(c("polic|gendarme|militair"), collapse = "|")

family_friends_relatives_regex <- paste0(c("mari|epou|cousin|oncl|tante|famill|amis|amies"), collapse = "|")

no_teacher_regex <- paste0(c("(pas|sans|auncun|absen|depar|quitt)(.+)(enseignant|instruct(.+)|personel)", "(enseignant|instructeur|instructrice|personel)(.+)(pas|sans|auncun|absen|depar|quitt)"), collapse = "|")

handicap_list_regex <- paste0(c("handicap|infirme|mental|aveugl|sourd|muet"),collapse = "|")

apprentissage_list_regex <- paste0(c("appren(.+)metier|coutur|macon|menuiserie|apprenti|elevag|eleveur|peche|metier"),collapse = "|")

insecurity_list_regex <- paste0(c("ins.cur.|s.curit.|braquag|bandit|vol."),collapse = "|")


####################################
#### Recoding the data ########

#################################
#Reordering variables

#################################
### Suppressing unused variable###

# confirm whether the GPS coordinates should be left or not

data_clean <- data_clean %>% mutate(across(all_of(c(names(data_clean)[names(data_clean) %in% c(free_text_vars)])), \(x) NA))

#################################
### Saving to RIDL #############
### Will investigate this later on


### Specific to Chad

if(any(str_detect("TCD|TD", unique(data_clean$country)))){
  
  data_clean <- data_clean %>% 
    dplyr::mutate(country="TD") %>%
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD014|TD014", "TD14"))) %>% 
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD017|TD017", "TD17"))) %>% 
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD01|TD01", "TD07")))
  
}


data_clean_final <- data_clean