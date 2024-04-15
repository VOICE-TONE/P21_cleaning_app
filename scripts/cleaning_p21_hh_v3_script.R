

## removing system variables
data_clean <- data_clean |> 
  select(names(data_clean)[!names(data_clean) %in% c("deviceid", "interviewer_id", "today", "phonenumber", "uuid", "instanceID", "rootUuid", "deprecatedID")])


# free text variables extraction
free_text_vars <- form_V3_original$name[form_V3_original$type %in% c("text", "note")]


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
data_clean <- data_clean %>% mutate(across(.cols = all_of(dat_date), .fns = ~as.Date(.x, "%Y-%m-%d")))

#############################################
### selecting all numeric and double variables 
dat_num <- data_clean %>% 
  select(!all_of(c(dat_empty, dat_date))) %>% 
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:digit:]") & !str_detect(na.omit(x),pattern="[:alpha:]"))|is.double(x)|is.numeric(x)|is.integer(x)) %>% names()



## Dealing with numerical variables

### doubles
data_clean <- data_clean |>  mutate(across(all_of(dat_num[!dat_num %in% dat_date])&where(is.double), \(x) as.double(as.character(x)))) 
### integers
data_clean <- data_clean |>  mutate(across(all_of(dat_num[!dat_num %in% dat_date])&where(is.integer), \(x) as.integer(as.character(x))))


### Selecting all character variables excluding empty variables first
dat_char <- data_clean %>% 
  select(!all_of(c(dat_empty, dat_date, dat_num))) %>% 
  select_if(\(x) all(str_detect(na.omit(x),pattern="[:alpha:][:digit:]|[:digit:][:alpha:]|[:alpha:]"))|is.character(x)) %>% names()


#########################################################
##### Cleaning Free text according to the protocol ######


gane_keywords_regex <- "gane|non(.+)etatique|armee|arme(.+)non(.+)etatique|boko(.+)haram|eigs|etat(.+)islamique|gsim|groupe(.+)soutien(.+)islam|aqmi|gspc|salafiste|mma|mouvement(.+)arme(.+)azawad|mnla|mouvement(.+)national(.+)liberation(.+)azawad|mujao|mouvement(.+)djihad|djihad"

gani_keywords_regex <- "gani|hani|arme(.+)non(.+)identif"

procedure_related_regex <- c("difficile|comment obtenir|pas facile|impossible|complique|etranger|pas chez moi|pas(.+)pays|procedure|competen|authori|pu faire|fuir(.+)laiss")

document_lost_regex <- "(carte|piece|doc|acte|affair)(.+)(cherche|brusque|precipite|abandon|egare|reste)|(cherche|brusque|precipite|abandon)(.+)(carte|piece|doc|acte|affair)|perdu|reste"


manque_moyen_regex <- "financ|argent|manque moyen|payer|moyen"

force_displace_regex <- paste0(c("fui|depart|parti|deplace|temps|oubli)(.+)(lais|brusq|precipit|force|recuper|prendr|subit|crise)"), collapse = "|")

local_ngo_regex <- paste0("action(.+)(humanitaire|social|croix rouge|ong)", collapse = "|")

village_elders_regex <- paste0(c("(conseille|chef|ancien|ain?|ancien|notable)(.+)(village|coutum)"), collapse = "|")

community_leaders_regex <- paste0(c("(leader|responsabl)(.+)(communautaire|local|coutum)"), collapse = "|") ## Should pastors and priest be consider community leaders or not?

religious_leaders_regex <- paste0(c("pasteur|pretre|imam|religieu)"), collapse = "|")

local_authority_regex <- paste0(c("maire|mairie|elu|prefect|commune|(service|structure)(.+)(social|etatique|communautair|local|locaux)"), collapse = "|")

public_security_forces_regex <- paste0(c("polic|gendarme|militair"), collapse = "|")

family_friends_relatives_regex <- paste0(c("mari|epou|cousin|oncl|tante|famill|amis|amies"), collapse = "|")

no_teacher_regex <- paste0(c("(pas|sans|auncun|absen|depar|quitt)(.+)(enseignant|instruct(.+)|personel)", "(enseignant|instructeur|instructrice|personel)(.+)(pas|sans|auncun|absen|depar|quitt)"), collapse = "|")

handicap_list_regex <- paste0(c("handicap|infirme|mental|aveugl|sourd|muet"),collapse = "|")

apprentissage_list_regex <- paste0(c("appren(.+)metier|coutur|macon|menuiserie|apprenti|elevag|eleveur|peche|metier"),collapse = "|")

insecurity_list_regex <- paste0(c("ins.cur.|s.curit.|braquag|bandit|vol."),collapse = "|")


## labelling index

## updating Progres Bar
n=10
incProgress(2/n, detail = "Freetext data")


#### Separating the GPS coordinates ########

if("other_safety" %in% names(data_clean))
  data_clean <- data_clean |> 
    mutate(
      
      ###### Safety
      safety_presence_armes_etatiques = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), gane_keywords_regex),1,as.numeric(safety_presence_armes_etatiques)),
      safety_presence_armes_nonidentifies = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), gani_keywords_regex ),1,if_else(is.na(other_safety),NA,0)), # New option Created for Hommes arm?s or groupes arm?s non-identifi?s"),
      safety_proximity = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "frontiere"),1,as.numeric(safety_proximity)),
      safety_destruction = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "destruction"),1,as.numeric(safety_destruction)),
      safety_attack = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "attaque"),1,as.numeric(safety_attack)),
      safety_kidnapping = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "enlevement"),1,as.numeric(safety_kidnapping)),
      safety_tensions_intercommunity = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "(menace|mefiance)(.+)(population|village|communaut)|envahissement|presence(.+)pdi"),1,as.numeric(safety_tensions_intercommunity)),
      safety_extorsion = if_else(!is.na(other_safety)&str_detect(tolower(iconv(other_safety, "UTF-8", "ascii//TRANSLIT")), "extortion|pillage|braqu"),1,as.numeric(safety_extorsion))
      )


if("other_why_no_idnational" %in% names(data_clean))
  data_clean <- data_clean |> mutate(
    # ###### other_why_no_idnational
    why_no_idnational_cost = if_else(!is.na(other_why_no_idnational)&str_detect(tolower(iconv(other_why_no_idnational, "UTF-8", "ascii//TRANSLIT")), paste0(c("financier|argent|manque moyen|payer|coute"), collapse = "|")),1,as.numeric(why_no_idnational_cost)),  #new option
    why_no_idnational_requested = if_else(!is.na(other_why_no_idnational)&str_detect(tolower(iconv(other_why_no_idnational, "UTF-8", "ascii//TRANSLIT")), paste0(c("attend|(jamais|pas)(.+)(sorti|pret|dispo|etabli)"), collapse = "|")),1,as.numeric(why_no_idnational_requested)),
    why_no_idnational_procedure_related = if_else(!is.na(other_why_no_idnational)&str_detect(tolower(iconv(other_why_no_idnational, "UTF-8", "ascii//TRANSLIT")), paste0(procedure_related_regex, collapse = "|")),1,if_else(is.na(other_why_no_idnational),NA,0)), # New Option created based on discussion with P21 team why_no_idnational_procedure_related
    why_no_idnational_lost = if_else(!is.na(other_why_no_idnational)&str_detect(tolower(iconv(other_why_no_idnational, "UTF-8", "ascii//TRANSLIT")), paste0(c(document_lost_regex), collapse = "|")),1,as.numeric(why_no_idnational_lost)),
    why_no_idnational_burnt = if_else(!is.na(other_why_no_idnational)&str_detect(tolower(iconv(other_why_no_idnational, "UTF-8", "ascii//TRANSLIT")), paste0("perdu|brule", collapse = "|")),1,as.numeric(why_no_idnational_burnt))
    )
  
if("other_why_no_idnational" %in% names(data_clean))
  data_clean <- data_clean |> mutate(

    ###### border_cross_why
    border_cross_why_reason_family = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("famille"), collapse = "|")),1,as.numeric(border_cross_why_reason_family)),
    border_cross_why_medical_reasons = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("sante", "malad", "soin", "medical", "docteur", "hopital", "infirmerie"), collapse = "|")),1,as.numeric(border_cross_why_medical_reasons)),
    border_cross_why_reason_economic = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("(achat|approvision|provision|faire|raison)(.+)(vivre|nourriture|achat|provision|marche|economique)|provision|marche|provision|economique"), collapse = "|")),1,as.numeric(border_cross_why_reason_economic)),
    border_cross_why_reason_violence = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("violence|attaq"), collapse = "|")),1,as.numeric(border_cross_why_reason_violence)),
    border_cross_why_reason_stigma = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("stigma"), collapse = "|")),1,as.numeric(border_cross_why_reason_stigma)),
    border_cross_why_natural_catastrophy = if_else(!is.na(other_border_cross_why)&str_detect(tolower(iconv(other_border_cross_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("catastroph"), collapse = "|")),1,as.numeric(border_cross_why_natural_catastrophy)), # Typo in the name reaon and not reaston
    

    ###### female_help_who
    female_help_who_community_leaders = if_else(!is.na(other_female_help_who)&str_detect(tolower(iconv(other_female_help_who, "UTF-8", "ascii//TRANSLIT")), paste0(c(community_leaders_regex), collapse = "|")),1,if_else(is.na(other_female_help_who),NA,0)), # new option: many female seems ask help from community leaders
    female_help_who_authority = if_else(!is.na(other_female_help_who)&str_detect(tolower(iconv(other_female_help_who, "UTF-8", "ascii//TRANSLIT")), paste0(c(local_authority_regex), collapse = "|")),1,female_help_who_authority),

    ###### child_school_whynot
    child_school_whynot_prolonged_school_dropout = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0("abandon|enfant(.+)refus(.)(.+)(.)cole|d(.)localise|n\\?ai(.+)plus(.+)((.)cole|scolaris)", collapse = "|")),1,child_school_whynot_prolonged_school_dropout),
    child_school_whynot_school_insecurity = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("ferm(.+)((.)tablissement|(.)cole)(.+)(attaq|s(.)curit)|attaq|ins(.)curite|ferm", insecurity_list_regex), collapse = "|")),1,child_school_whynot_school_insecurity),
    child_school_whynot_school_distance = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("loin|distanc"), collapse = "|")),1,child_school_whynot_school_distance), 
    child_school_whynot_school_finance = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("manque_moyen_regex"), collapse = "|")),1,child_school_whynot_school_finance), 
    child_school_whynot_school_no_school = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("(pas|manqu)(.+)((.)cole|infrastruct|infrastruct(.+)scolair)"), collapse = "|")),1,if_else(is.na(other_child_school_whynot),NA,0)), # New option
    child_school_whynot_school_child_work = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("enfant(.+)(mine|site|or)|(mine|site)(.+)or", "travail|apprenti|commerc|vendeu|metier|ouvrier|mandie|orpaill"), collapse = "|")),1,child_school_whynot_school_child_work), 
    child_school_whynot_school_space = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), no_teacher_regex),1,child_school_whynot_school_space), 
    child_school_whynot_school_destroyed = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("((.)cole|infrastruc|(.)tablissement)(.+)(detruit|brul|endommag)"), collapse = "|")),1,child_school_whynot_school_destroyed), 
    child_school_whynot_no_id_docs = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("(pas|aucun|sans|manqu)|(doc|extrait|acte naiss|certifica naiss)", document_lost_regex), collapse = "|")),1,if_else(is.na(other_child_school_whynot),NA,0)), # New option 
    child_school_whynot_not_applicable = if_else(!is.na(other_child_school_whynot)&str_detect(tolower(iconv(other_child_school_whynot, "UTF-8", "ascii//TRANSLIT")), paste0(c("(vacance|conge|n\\?ont|pas|jamais|bas)(.)ge|(fr(.)quente|scolaris|(.)cole)|pas(.+)enfant(.+)(.)cole|non|bas|ont(.+)(.)ge"), collapse = "|")),1,if_else(is.na(other_child_school_whynot),NA,0)), # New option
    
    # ###### school_closed_why
    school_closed_why_armedgroup_destruction = if_else(!is.na(other_school_closed_why)&str_detect(tolower(iconv(other_school_closed_why, "UTF-8", "ascii//TRANSLIT")), paste0("((.)cole|infrastruc|(.)tablissement)(.+)(detruit|brul)", collapse = "|")),1,school_closed_why_armedgroup_destruction),
    school_closed_why_no_teacher = if_else(!is.na(other_school_closed_why)&str_detect(tolower(iconv(other_school_closed_why, "UTF-8", "ascii//TRANSLIT")), paste0(c(no_teacher_regex), collapse = "|")),1,school_closed_why_no_teacher),
    school_closed_why_occupation_displaced_persons = if_else(!is.na(other_school_closed_why)&str_detect(tolower(iconv(other_school_closed_why, "UTF-8", "ascii//TRANSLIT")), paste0(c("site(.+)(personnes deplac|refugie|idp|nouveau(.+)arriv)"), collapse = "|")),1,school_closed_why_occupation_displaced_persons),
    
    ###### school_alternative_type
    school_alternative_type_alphabetisation = if_else(!is.na(school_alternative_type)&str_detect(tolower(iconv(school_alternative_type, "UTF-8", "ascii//TRANSLIT")), paste0(c("appren(.+)(lire|ecrir)"), collapse = "|")),1,school_alternative_type_alphabetisation),
    school_alternative_type_language = if_else(!is.na(school_alternative_type)&str_detect(tolower(iconv(school_alternative_type, "UTF-8", "ascii//TRANSLIT")), paste0(c("cours(.+)(rattrapag|soutien)|rattrapag"), collapse = "|")),1,school_alternative_type_language),
    school_alternative_type_accelerated_prog = if_else(!is.na(school_alternative_type)&str_detect(tolower(iconv(school_alternative_type, "UTF-8", "ascii//TRANSLIT")), paste0("(appren|cours)(.+)langu", collapse = "|")),1,school_alternative_type_accelerated_prog),
    school_alternative_type_competency = if_else(!is.na(school_alternative_type)&str_detect(tolower(iconv(school_alternative_type, "UTF-8", "ascii//TRANSLIT")), paste0(c(handicap_list_regex), collapse = "|")),1,school_alternative_type_competency),
    school_alternative_type_supplementary_prog = if_else(!is.na(school_alternative_type)&str_detect(tolower(iconv(school_alternative_type, "UTF-8", "ascii//TRANSLIT")), paste0(c(apprentissage_list_regex), collapse = "|")),1,school_alternative_type_supplementary_prog)
    
  )



## updating Progres Bar
incProgress(7/n, detail = "Freetext completed")

#################################
#Reordering variables
#
if("other_safety" %in% names(data_clean))
  data_clean <- data_clean |> 
    relocate(c(starts_with("safety_"), other_safety), .after="safety")

if("other_why_no_idnational" %in% names(data_clean))
  data_clean <- data_clean |> 
    relocate(c(starts_with("why_no_idnational_"), other_why_no_idnational), .after="why_no_idnational")
  


#################################
### Suppressing unused variable###

# confirm whether the GPS coordinates should be left or not

data_clean <<- data_clean %>% mutate(across(all_of(c(names(data_clean)[names(data_clean) %in% c(free_text_vars)])), \(x) NA))


#data_clean <- data_clean %>% mutate(across(contains("gps"), \(x) NA))

### Specific to Chad

if(any(str_detect("TCD|TD", unique(data_clean$country)))){
  
  data_clean <<- data_clean %>% 
    dplyr::mutate(country="TD") %>%
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD014|TD014", "TD14"))) %>% 
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD017|TD017", "TD17"))) %>% 
    dplyr::mutate(across(c(admin_level_1, admin_level_2, admin_level_3), \(x) stringr::str_replace_all(x, "TCD01|TD01", "TD07")))
  
}


## updating Progres Bar
incProgress(8/n, detail = "Fine-tuning")

data_clean_final <- data_clean