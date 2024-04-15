library(httr)
library(plyr)
library(lubridate)
library(ISOweek)
library(tidyverse)


# keys
Sys.setenv("RIDL_API_KEY" = read_rds("tokens/ridl_token.rds"))



url = "https://ridl.unhcr.org/api/3/action/"

url2 = "https://ridl.unhcr.org/"

url_hlp_lst = paste0(url,  "help_show")

url_grp_lst = paste0(url,  "group_list")

endpoint_pkg_src <- "package_search"

endpoint_pkg_show <- "package_show"

endpoint_pkg_lst <- "package_list"

endpoint_rsc_show <- "resource_show"


endpoint_rsc_src <- "resource_search"

endpoint_dataset<- "dataset"

endpoint_data_container <- "data-container"

end_point <- endpoint_dataset

query <- list(id="dedi-hcr-enquete-pdm-menages-nature-livelihood", format='json')

ds_name <- "dedi-hcr-enquete-pdm-menages-nature-livelihood"

cs_name <- "bfa-cbi"

query=list(rows=1000, start=0)

## get data container info

resp_cont <- GET(paste0(url2,"api/3/action/package_search"), query=query,add_headers(accept = 'application/json', 'X-CKAN-API-KEY'=Sys.getenv("RIDL_API_KEY")))

## type o response
httr::http_type(resp_cont)

## parsing content
resp_content <- content(resp_cont, as="text")

# To JSON
resp_json <- fromJSON(resp_content)

# to Dataframe
resp_df <- do.call("rbind.fill", lapply(resp_json, as.data.frame))




########################
########################
## Function to list all dataset

rckan_dataset_count <- function(base_url='https://ridl.unhcr.org/', api_key=Sys.getenv("RIDL_API_KEY")){
  #'This function read the dataset list from the CKAN instance
  #'composing url
  new_url = paste0(base_url, "api/3/action/package_search")
  
  # set query
  query=list(rows=1, start=1)
  
  resp_cont <- GET(url=new_url, query=query, add_headers(accept = 'application/json', 'X-CKAN-API-KEY'=api_key))
  
  ## checking errors
  if(isTRUE(httr::http_error(resp_cont))){
    
    print(paste("The following error occured during the data extraction. Please verify all your parameters and retry"))
    
  }
  
  ## parsing content
  resp_content <- content(resp_cont, as="text")
  
  # To JSON
  resp_json <- fromJSON(resp_content)
  
  count <- resp_json$result$count
  
  # df <- df %>% relocate(id, kobo_asset_id, name, short_title)
  
  return(count)
  
}



########################
########################
## Extracting full datasets list
rckan_dataset_list <- function(base_url='https://ridl.unhcr.org/', api_key=Sys.getenv("RIDL_API_KEY")){
  #'This function read the dataset list from the CKAN instance
  #'
  new_url = paste0(base_url, "api/3/action/package_search")
  
  # getting the number of datasets
  n_dat=rckan_dataset_count()
  
  
  ################
  if(n_dat>1000){
    
    start_id <- 1
    
    
    df_list <- list()
    
    ################
    while(start_id < n_dat){
      
      print(paste("Extracting data starting from id: ", start_id))
      
      # building the query based on the start index
      query=list(rows=1000, start=start_id)
      
      # list index calculation
      index <- ceiling(start_id/1000)
      
      ## Extracting the data
      resp_cont <- GET(url=new_url, query=query, add_headers(accept = 'application/json', 'X-CKAN-API-KEY'=api_key))
      
      ## checking errors
      if(isTRUE(httr::http_error(resp_cont))){
        
        print(paste("The following error occured during the data extraction. Please verify all your parameters and retry"))
        
      }
      
      ## parsing content
      resp_content <- content(resp_cont, as="text")
      
      # To JSON
      resp_json <- fromJSON(resp_content)
      
      # dataframe extraction
      df_list[[index]] <- resp_json$result$results 
      
      
      ## updating the parameters
      start_id = start_id + 1000
      
      print(paste("Extraction from record",start_id+1000, "to completed"))
      
    }
    
    # compiling the final dataframe result
    df <- bind_rows(df_list) %>% relocate(id, kobo_asset_id, name, short_title)
    
  } else{
    
    ############# if count < 30000
    # update query
    query=list(rows=n_data, start=start_id)
    
    # else
    ## checking errors
    if(isTRUE(httr::http_error(resp_cont))){
      
      print(paste("The following error occured during the data extraction. Please verify all your parameters and retry"))
      
    }
    
    ## parsing content
    resp_content <- content(resp_cont, as="text")
    
    # To JSON
    resp_json <- fromJSON(resp_content)
    
    # dataframe extraction
    df <- resp_json$result$results %>% relocate(id, kobo_asset_id, name, short_title)
    
    
  }
  
  
  df <- df %>% relocate(id, kobo_asset_id, name, short_title)
  
  return(df)
  
}



# getting dataset list
datasets_list <- rckan_dataset_list(base_url='https://ridl.unhcr.org/')



#################################
#################################
## Downloading a specific dataset





query <- list(q="pdm")

# Getting info from RIDL
resp_data <- GET(paste0(url, "api/3/action/help_show/"), path=end_point, query=query, add_headers(accept = 'application/json', 'X-CKAN-API-KEY'=Sys.getenv("RIDL_API_KEY")), accept_json())

## checking the type
httr::http_type(resp_data)

## checking errors
if(isTRUE(httr::http_error(resp_data))){
  
  print(paste("The following error occured during the data extraction. Please verify all your parameters and retry"))
  
}
## parsing content
resp_content <- content(resp_data, as="text")


df <- jsonlite::fromJSON(resp_content)

df <- paste0("{",df$result,"}")

df <- jsonlite::fromJSON(df$result)

df <- do.call("rbind.fill", lapply(df, as.data.frame))


## extracting values to a list of dataframe
df1 <- lapply(resp_data$results, \(x) unique(as_tibble(rlist::list.cbind(x), .name_repair="minimal")))


# converting to dataframe
df <- data.table::rbindlist(df1, fill = T)





##### List a dataset resources
##
##
##

rckan_dataset_resource_list <- function(base_url = "https://ridl.unhcr.org/", api_key=Sys.getenv("RIDL_API_KEY"), dataset_id=NULL){
  
  # Define the URL and headers
  new_url2 = paste0(base_url, "api/3/action/package_show")
  
  
  headers <- c(
    "Content-Type" = 'application/json',
    'X-CKAN-API-KEY' = api_key
  )
  
  
  # Make the POST request
  resp_data <- GET(new_url2, query=list(id=dataset_id), add_headers(.headers = headers), accept_json())
  

  ## parsing content
  resp_content <- content(resp_data, as="text")
  
  
  ## formatting to datafame
  df <- jsonlite::fromJSON(resp_content)
  
  
  ### Extracting the data
  resources_list <- df$result$resources  %>% select(id, format, file_type, description, created, last_modified) 
  
  
  return(resources_list)
  
}





##### patching resources
##
##
##

rckan_resource_update <- function(base_url = "https://ridl.unhcr.org/", api_key=Sys.getenv("RIDL_API_KEY"), resource_id=NULL, file_path=NULL){
  
  # Define the URL and headers
  new_url2 = paste0(base_url, "api/3/action/resource_patch")
  
  
  headers <- c(
    "Content-Type" = "multipart/form-data",
    'X-CKAN-API-KEY' = api_key
  )
  
  # Define the payload data
  payload <- list(
    id = resource_id, # resource ID
    upload = upload_file(file_path)
  )
  
  # Make the POST request
  response <- POST(new_url2, body = payload, encode = "multipart", add_headers(.headers=headers))
  
  # Print the response
  print(response)
  
  
}








# Define the URL and headers
new_url2 = paste0("https://ridl.unhcr.org/", "api/3/action/package_show")


headers <- c(
  "Content-Type" = 'application/json',
  'X-CKAN-API-KEY' = Sys.getenv("RIDL_API_KEY")
)


# Make the POST request
resp_data <- GET(new_url2, query=list(id="ner_p21_household_questionnaire_v3_niger"), add_headers(.headers = headers), accept_json())



## parsing content
resp_content <- content(resp_data, as="text")


df <- jsonlite::fromJSON(resp_content)


### Extracting the data

resources_list <- df$result$resources  %>% select(id, format, file_type, description, created, last_modified) 


### Example of extraction
resources_ist <- rckan_dataset_resource_list(dataset_id = "ner_p21_household_questionnaire_v3_niger")



### Saving RIDL list

ridl_ids_df <- datasets_list %>% select(id, name) %>% filter(str_detect(name, "p21(.*)v3")) %>% mutate(country=str_sub(name, 1,3), srv_type=if_else(str_detect(name, "household"), "HH", "KII")) %>% relocate(srv_type, country, name, id)

# ctry_ids_list <- ridl_ids_df %>% select(country, id) %>% deframe() %>% as.list()
# 
# ctry_names_list <- ridl_ids_df %>% select(country, name) %>% deframe() %>% as.list()
# 
# ctry_srv_type_list <- ridl_ids_df %>% select(country, srv_type) %>% deframe() %>% as.list()



########################
#### Creating the ID list
ridl_ids_list <- list()

for(c in ridl_ids_df$country){
  
  ridl_ids_list[[c]] <- list(names=list(HH=ridl_ids_df$name[ridl_ids_df$country==c & ridl_ids_df$srv_type=="HH"],
                             KII=ridl_ids_df$name[ridl_ids_df$country==c & ridl_ids_df$srv_type=="KII"]),
                             ids=list(HH=ridl_ids_df$id[ridl_ids_df$country==c & ridl_ids_df$srv_type=="HH"],
                                      KII=ridl_ids_df$id[ridl_ids_df$country==c & ridl_ids_df$srv_type=="KII"]))
  
}

ridl_ids_list$ner$names$HH <- "ner_p21_household_questionnaire_v3_niger"
ridl_ids_list$ner$ids$HH <- "a9b5153d-54f1-4083-8182-3e205c165fd9"


saveRDS(ridl_ids_list, "data/ridl_ids_list.rds")


## ridl ids df
ridl_ids_df <- ridl_ids_df %>% 
  mutate(pat=if_else(str_detect(name, "household"), paste("HH_V3",toupper(str_sub(name, 1,3)), sep = "_"), paste("KII_V3",toupper(str_sub(name, 1,3)), sep = "_")))

## files list
files_list <- list.files("output/", full.names = T)


ridl_ids_df <- ridl_ids_df %>% add_column(file=sapply(ridl_ids_df$pat, \(x) files_list[str_detect(files_list, x)]) %>% as.character())


## resources list

resources_list  <- lapply(files$ridl_name, \(x) rckan_dataset_resource_list(dataset_id = x))

resources_labelled <- lapply(resources_list, \(x) x %>% filter(file_type=="microdata" & format=="XLSX" & str_detect(tolower(description), "french|label")))

resources_english <- lapply(resources_list, \(x) x %>% filter(file_type=="microdata" & format=="XLSX" & str_detect(tolower(description), "clean") & !str_detect(tolower(description), "french|label")))

resources_labelled_ids <- bind_rows(resources_labelled) %>% select(id) %>% unlist() %>% as.character()

resources_english_ids <- bind_rows(resources_english) %>% select(id) %>% unlist() %>% as.character()
