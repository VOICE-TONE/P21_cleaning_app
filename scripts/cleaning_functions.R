library(jsonlite)
library(httr)

############################################
#### Functions

# Function1 : renaming dataframes
rename_df <- function(df, clean_names){
  
  names(df) <- clean_names
  
  return(df)
}


#Function2 : removing empty rows and columns
remove_empty_rows_cols <- function(df){
  
  # removing empty columns
  df <- df[,colSums(is.na(df))!=nrow(df)]
  
  # removing empty rows
  df <- df[rowSums(is.na(df))!=ncol(df),]
  
  return(df)
}


######### Relabeling #############
# Function 3 : to relabeling values from list of df
relabeling_values_df <- function(df,value_labels_df){
  
  for(nam in names(df)){
    
    if(nam %in% unique(value_labels_df$var_clean)){

        # extract corresponding values
        # 
        #'notice that values need to be sorted in the same order as labels for the replacement to work
        #'Using the dataset as source is a problem as the order may differ and it will destroy the entire statistics
        #'hence hashing-out the below line and using the benefit.
        #input_values <- str_squish(names(table(df[,nam])))
        input_values <- value_labels_df$name[tolower(value_labels_df$var_clean)==tolower(nam)]
        
        input_values <- paste0("\\b", input_values, "\\b")

        #input_values_regex <- paste0(input_values)
        
        # notice that labels need to be sorted in the same order as values for the replacement to work
        # extract corresponding labels
        input_labels <- value_labels_df$label[tolower(value_labels_df$var_clean)==tolower(nam)]

        input_labels <- paste0(input_labels,";")
        
        
        # relabling the variable
        print(paste("Labelling values of :", nam))
      
        ## Single choice
        
        #str_regex <- stringi::stri_replace_all_regex(str_squish(str_trim(tolower(eval(expr = parse(text=paste0("df$",nam)))))), "[:punct:]", "")
        #str_regex <- str_squish(str_trim(tolower(eval(expr = parse(text=paste0("df$",nam))))))
        
        df[,nam] <- stringi::stri_replace_all_regex(str= tolower(eval(expr = parse(text=paste0("df$",nam)))), pattern =tolower(input_values), replacement = input_labels, vectorize_all = FALSE)
        
      }
    
  }
  
  return(df)
}



##### Relabling 2 for kobo data
relabeling_values_kobo_df <- function(df,value_labels_df, lang=c("french", "english")){
  
  print("filtering start")
  value_labels_df$len <- lapply(value_labels_df$choices, nrow)
  
  value_labels_df <- value_labels_df %>% filter(len > 0)
  
  print("filtering complete")
  
  for(nam in value_labels_df$name){
    
    if(nam %in% names(df)){
      
      print(paste("starting on", nam))
      
      choices_df <- value_labels_df$choices[value_labels_df$name==nam]
      
      print("getting values")
      input_values <- choices_df[[1]]$value_name[str_detect(tolower(choices_df[[1]]$value_lang), tolower(lang))]
      
      input_values <- paste0("\\b", input_values, "\\b")
      
      # notice that labels need to be sorted in the same order as values for the replacement to work
      # extract corresponding labels
      
      print("getting labels")
      input_labels <- choices_df[[1]]$value_label[str_detect(tolower(choices_df[[1]]$value_lang), tolower(lang))]
      
      input_labels <- paste0(input_labels,";")
      
      
      # relabling the variable
      print(paste("Labelling values of :", nam))
      
      ## Single choice
      
      #str_regex <- stringi::stri_replace_all_regex(str_squish(str_trim(tolower(eval(expr = parse(text=paste0("df$",nam)))))), "[:punct:]", "")
      #str_regex <- str_squish(str_trim(tolower(eval(expr = parse(text=paste0("df$",nam))))))
      
      df[,nam] <- stringi::stri_replace_all_regex(str= tolower(eval(expr = parse(text=paste0("df$","`",nam,"`")))), pattern =tolower(input_values), replacement = input_labels, vectorize_all = FALSE)
      
    }
    
  }
  
  return(df)
}



#########################################
# Function 4 : Relabling variable
relabeling_variables_df <- function(df, variable_labels_df){
  
  for(nam in names(df)){
    
    if(nam %in% unique(variable_labels_df$clean_names)){
      
        # labeling the variable
        print(paste("Labelling:", nam))
        
        labelled::var_label(df[,nam]) <- unique(variable_labels_df$labels[variable_labels_df$clean_names==nam])
        
      }else{
        print("Nothing to do. please check parameters")
      }
      

  }
  
  return(df)
}



# Function 5 : Relabling variable from Kobo data
relabeling_variables_kobo_df <- function(df, form_data){
  
  for(nam in names(df)){
    
    if(nam %in% unique(form_data$name)){
      
      # labeling the variable
      print(paste("Labelling:", nam))
      
      labelled::var_label(df[,nam]) <- unique(form_data$label[form_data$name==nam])
      
    }else{
      print("Nothing to do. please check parameters")
    }
    
    
  }
  
  return(df)
}





###################################
#####
##### RIDL Functions
#####
###################################
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
    query=list(rows=n_dat, start=start_id)
    
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
  resources_list <- df$result$resources  %>% select(id, format, file_type, name, description, created, last_modified) 
  
  
  return(resources_list)
  
}





##### patching resources
##
##
##

rckan_resource_update <- function(resource_id=NULL, file_path=NULL, base_url = "https://ridl.unhcr.org/", api_key=Sys.getenv("RIDL_API_KEY")){
  
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
  print(response$status_code)
  
  
}





####################################################
#### MCQ variables list - rellabing

### Function to switch MCQ
val_labels_sm_from_form_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 .data$type %in% "select_multiple")
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form |>
      filter(.data$name %in% nm) |>
      distinct(.data$name, .data$choices) |>
      group_by(.data$name) |>
      mutate(choices = list(distinct(bind_rows(choices)))) |>
      ungroup()
    choices <- form$choices
    names(choices) <- form$name
    choices <- lapply(nm, function(n) {
      ch <- choices[[n]]
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch
    })
    names(choices) <- nm
    for (n in nm)
      x[[n]] <- stri_replace_all_regex(x[[n]],
                                       paste0("\\b", choices[[n]]$value_name, "\\b"),
                                       choices[[n]]$value_label,
                                       vectorize_all = FALSE)
  } else {
    x
  }
  x
}




######

val_labels_so_from_form_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 .data$type %in% "select_one")
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form |>
      filter(.data$name %in% nm) |>
      distinct(.data$name, .data$choices) |>
      group_by(.data$name) |>
      mutate(choices = list(distinct(bind_rows(choices)))) |>
      ungroup()
    choices <- form$choices
    names(choices) <- form$name
    choices <- lapply(nm, function(n) {
      ch <- choices[[n]]
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch
    })
    names(choices) <- nm
    for (n in nm)
      x[[n]] <- stri_replace_all_regex(x[[n]],
                                       paste0("\\b", choices[[n]]$value_name, "\\b"),
                                       choices[[n]]$value_label,
                                       vectorize_all = FALSE)
  } else {
    x
  }
  x
}



### Function to edit datatable in SHiny App
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}
