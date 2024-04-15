
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
rckan_dataset_full_list <- function(base_url='https://ridl.unhcr.org/', api_key=Sys.getenv("RIDL_API_KEY")){
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


### FIltered list based on pattern
rckan_dataset_list <- function(base_url='https://ridl.unhcr.org/', pattern=NULL, kobo_id=NULL){
  
  if(!is.null(kobo_id)){
    
    ds_list <- rckan_dataset_full_list(base_url=base_url) %>% 
      dplyr::filter(kobo_asset_id==kobo_id)
    
  }else if(!is.null(pattern)){
    
    ds_list <- rckan_dataset_full_list(base_url=base_url) %>% 
      dplyr::filter(stringr::str_detect(tolower(name), tolower(pattern)))
    
  }else{
    
    ds_list <- rckan_dataset_full_list(base_url=base_url)
    
  }
  
  return(ds_list)
  
}



#################################
#################################
## Downloading a specific dataset

# ##### List a dataset resources
# ##
# ##
# ##

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
  resources_list <- df$result$resources %>% select(id, format, file_type, name, description ,created, last_modified) 
  
  
  return(resources_list)
  
}





##### patching resources
##
##
##

rckan_resource_update <- function(base_url = "https://ridl.unhcr.org/", api_key=Sys.getenv("RIDL_API_KEY"), resource_id=NULL, file_path=NULL, desc=NULL){
  
  # Define the URL and headers
  new_url2 = paste0(base_url, "api/3/action/resource_patch")
  
  
  headers <- c(
    "Content-Type" = "multipart/form-data",
    'X-CKAN-API-KEY' = api_key
  )
  
  # Define the payload data
  payload <- list(
    id = resource_id, # resource ID,
    description=if(!is.null(desc)){desc},
    upload = upload_file(file_path)
  )
  
  # Make the POST request
  response <- POST(new_url2, body = payload, encode = "multipart", add_headers(.headers=headers))
  
  # Print the response
  print(response)
  
  
}


