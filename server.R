#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rm(list = ls())

library(shiny)
library(robotoolbox) ## remotes::install_github("dickoa/robotoolbox"))
library(ridl)
library(tidyverse)
library(labelled)
library(readxl)
library(janitor)
library(lubridate)
library(unhcrthemes)
library(stringi)
library(jsonlite)
library(DT)

## loading essential functions
source("scripts/cleaning_functions.R")
source("scripts/ridl_api_functions.R")

### Read RIDL ids
ridl_ids_list <- readRDS("input/ridl_ids_list.rds")
kobo_ids_list <- readRDS("input/kobo_form_ids.rds")


# Define server logic required to draw a histogram
function(input, output, session) {

    
    ###############################
   #### Checking if a token exists
    ridl_key_check <- reactive({
      
      ridl_token <- list.files("tokens/", full.names = T, pattern = "ridl(.*)token(.*).rds")
      
      if(purrr::is_empty(ridl_token)){
        ridl_ui <- passwordInput(inputId = "ridl_key_id", label = "Enter Your RIDL API Key:")
        
      }else{
        
        ridl_ui <- tags$div(id = "ridl_key_msg", class="key", "RIDL Key Found")
        
      }
      
      return(ridl_ui)
      

    })
    
    
    ################
    #### Kobo key
    kobo_key_check <- reactive({
      
      kobo_token <- list.files("tokens/", full.names = T, pattern = "kobo(.*)token(.*).rds")
      
      if(purrr::is_empty(kobo_token)){
        
        kobo_ui <- passwordInput(inputId = "kobo_key_id", label = "Enter Your Kobo API Key:")
        

      }else{
        
        kobo_ui <- tags$div(id = "kobo_key_msg", class="key", "Kobo Key Found")
        
        
      }
      
      return(kobo_ui)
      
      
    })
    
    ###################################################
    ######## output keys entry fields #################
    ### return kobo_ui and ridl_ui
    output$kobo_key <- renderUI(kobo_key_check())
    output$ridl_key <- renderUI(ridl_key_check())
    output$msg <- renderText(key_messages())
    
    
    
    ##################################################
    ############### Keys Reading #####################
    ### Reading KOBO keys
    read_kobo_key <- reactive({
      
      kobo_token <- list.files("tokens/", full.names = T, pattern = "kobo(.*)token(.*).rds")
      
      if(!purrr::is_empty(kobo_token)){
        
        kobo_key_read <- readRDS(kobo_token)
        
      }else{
        
        kobo_key_read <- input$kobo_key_id
        
      }
      
    })
    
    
    ### Reading RIDL KEY
    read_ridl_key <- reactive({
      
      ridl_token <- list.files("tokens/", full.names = T, pattern = "ridl(.*)token(.*).rds")
      
      if(!purrr::is_empty(ridl_token)){
        
        ridl_key_read <- readRDS(ridl_token)
        
      }else{
        
        ridl_key_read <- input$ridl_key_msg
        
      }
      
    })
    
    
    
    
    
    
   
    ################
    #### Country list 
    ctry_list <- eventReactive(
      
      input$survey_name,
      
      {
      
      if(input$survey_name=="P21"){
        
        ctry_lst_ui <- selectInput(inputId="country", choices = c("None"="None","Burkina Faso"="BF","Cameroon"="CM","Chad"="TD","Mali"="ML", "Niger"="NER"),selected = "None", label = "Select a Country")

 
      }else if(input$survey_name=="BM"){
        
        ctry_lst_ui <- selectInput(inputId="country", choices = c("None"="None","Benin"="BJ","Côte d'Ivoire"="CI"), selected = "None", label = "Select a Country")
        
      }else{
        
        ctry_lst_ui <- selectInput(inputId="country", choices = c("None"="None"), selected = "None", label = "Select a Country")
        
      }
      
      return(ctry_lst_ui)
      
      
    })
    
    
    ## Return ctry_list
    output$ctr_list <- renderUI(ctry_list())
    
    
    
    
    
    #### New version control
    observeEvent(

      input$find_data, {
        
        withProgress(message = paste("Loading" , input$sys_select ,"Dataset List"), value = 0, {
        n<-10

        incProgress(2/n, detail = "connected")
        
        if(input$sys_select=="RIDL"){
          
          ds_list <- rckan_dataset_list() %>% select(id, kobo_asset_id, name, title, short_title, metadata_created, metadata_modified)
          
        }else if(input$sys_select=="KOBO"){
          
          ds_list <- kobo_asset_list() 
          
        } else{
          
          NULL
        }
        
        
        
        ##  Adding check boxes
        # ds_list <- cbind(dat0, bool = FALSE)
        ds_list <- cbind(
          check = shinyInput(shiny::checkboxInput, len=nrow(ds_list), id="checkb", width='10px'),
          ds_list
        )
        
        
        
        js <- c(
          "$('[id^=checkb]').on('click', function(){",
          "  var id = this.getAttribute('id');",
          "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
          "  var value = $(this).prop('checked');",
          "  var info = [{row: i, col: 3, value: value}];",
          "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
          "})"
        )
        
        
        ## Making data Editable in table
        Dat <- reactiveVal(ds_list)
        
        
        incProgress(7/n, detail = "Loaded")
        
        # ds_list <- ds_list %>% filter(str_detect(tolower(name), "p21"))
        
        
        
        ### Rendering output
        output$data_list <- DT::renderDataTable({
          
          DT::datatable(
          
          ds_list,
          
          filter = 'top',
          
          rownames = TRUE,
          escape = FALSE,
          editable = list(target = "cell", disable = list(columns = 3)),
          selection = "none",
          callback = JS(js), # Calling Editable Function
          
          
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            'Table 2: ', htmltools::em('Dataset List')
          ),

          extensions = 'Buttons',

          options = list(
            fixedColumns = TRUE,
            columnDefs = list(list(visible=TRUE, targets = names(ds_list)),
                              list(width = '10px', targets = 1)),
            autoWidth = TRUE,
            ordering = TRUE,
            scrollX = TRUE,
            scrollY = TRUE,
            dom = 'Bftsp',
            pageLength = 10,
            buttons = c('copy', 'csv', 'excel','pdf','print'),
            autoWidth = TRUE)

          
          )}, server = FALSE)
          
          
          ###################
          ## Return editable
          observeEvent(input[["dtable_cell_edit"]], {
            
            info <- input[["dtable_cell_edit"]] # this input contains the info of the edit
            
            print(info)
            Dat(editData(Dat(), info))
          })
          
          output[["reactiveDF"]] <- renderPrint({ 
            Dat()
          })
          
          
        
        incProgress(10/n, detail = "Completed")
        
        
        
        })

    })
    
    
  
    
    
    
    #################
    ### Read specific country data
    ctry_data <- reactive({
      
      if(is.null(input$load_data))
        return(NULL)
      
      withProgress(message = paste("Donwloading", input$country, input$survey_type, "data:"), value = 0, {
        
      if(input$load_data){
        
        # print(input$data)
        
        ########################
        ## Extracting survey ID
        
        # progress bar steps
        n<-10

        ctry <<- input$country
        
        srv <<- input$survey_type
        
        nam <<- input$survey_name
        
        uid <<- kobo_ids_list[[input$survey_name]][[input$survey_type]][[input$country]]
        
        # print("Parameters setting complete")

 
        #######################
        ## Setting up the system
        Sys.setenv("KOBOTOOLBOX_TOKEN"=read_kobo_key())
        
        kobo_setup(url="https://kobo.unhcr.org", token = read_kobo_key())
        
        ## Updating the bar. 
        incProgress(1/n, detail = "Connected to Kobo")
        
        ## Getting data
        print("Extracting from Kobo")
        source("scripts/getting_data.R", local = FALSE)
        
        ## Updating the bar. 
        incProgress(8/n, detail = "Loading data")
        
        
        
        ## Save file
        
        saveRDS(data_clean, paste0("data/",input$country,"_",input$survey_type,"_", today(tzone="UTC"),"_raw.rds"))
        
        return(list(data_clean=data_clean, form_V3=form_V3, form_V3_original=form_V3_original))
        
        
      } ## End of if Statement
        

        incProgress(10/n, detail = "Completed")
        
        Sys.sleep(5)
        
      }) ## End of Progress Bar
      
    
      
    }) ## End of Reactive object
    
    
    
    
    #################
    ### Cleaning Data
    
    ### Manual Cleaning
    observeEvent(
      
      input$manual_cleaning,
      
      {
      
        
        # insert tabPanel
        insertTab(
          
          inputId = "maintab",
          
          tab = tabPanel(title="Manual Data Cleaning Tool"),
          
          target = "Upload Files",
          
          position = "after"
          
          )
        
        
        # select the tabpanel
        updateTabsetPanel(inputId = "maintab", selected = "Manual Data Cleaning Tool")
        
        # shinyjs::toggleState(id = "manual_cleaning")
        # 
        # shinyjs::disable("manual_cleaning")
        
        insertUI(
          selector = "#variables",

          where = "beforeEnd",

          ui=tags$div(
            
            selectInput(inputId = "sel_var", label = "Variable", choices = names(ctry_data()$data_clean))
            
            )
          )
        
        

    })
    
    ### Auto Cleaning
    observeEvent(
      
      input$clean_data,
      
                 {
                   
                   ## Starting the Progress Bare
                   withProgress(message = paste("Cleaning", input$country, input$survey_type, "data:"), value = 0,{
                     
                     # labelling index
                     n=10
                     incProgress(1/n, detail = "Preparing")
                     
                     updateTabsetPanel(session,inputId = "cleaning_steps", selected = 2)
                     
                     data_clean <- ctry_data()$data_clean
                     form_V3 <- ctry_data()$form_V3
                     form_V3_original <- ctry_data()$form_V3_original
                     
                     # source cleaning script
                     if(input$survey_type=="HH")
                       source("scripts/cleaning_p21_HH_V3_script.R", local = TRUE)
                     
                     if(input$survey_type=="KII")
                       source("scripts/cleaning_p21_KII_script.R", local = TRUE)
                     
                     
                     ## updating Progres Bar
                     incProgress(9/n, detail = "Preparing output")
                     
                     ######### Saving ########
                     data_clean %>%  writexl::write_xlsx(., path=paste("output/UNHCR",nam,srv,"V3", ctry,"DAT_CLEAN_V1.1.xlsx", sep = "_"))
                     
                     
                     
                     ############################
                     ##### Rendering Clean data
                     observeEvent(
                       input$clean_data,
                       
                       {
                         
                         updateTabsetPanel(inputId = "maintab", selected = "Clean Data")
                         
                         # data formatted
                         output$cleaned_data <- DT::renderDataTable(DT::datatable(
                           
                           data_clean,
                           
                           filter = 'top',
                           
                           caption = htmltools::tags$caption(
                             style = 'caption-side: bottom; text-align: center;',
                             'Table 2: ', htmltools::em('Clean Data')
                           ),
                           extensions = 'Buttons',
                           
                           options = list(
                             fixedColumns = TRUE,
                             columnDefs = list(list(visible=TRUE, targets = names(data_clean))),
                             autoWidth = TRUE,
                             ordering = TRUE,
                             scrollX = TRUE,
                             scrollY = TRUE,
                             dom = 'Bftsp',
                             buttons = c('copy', 'csv', 'excel','pdf','print')
                           ))
                         )
                         
                         
                       })
                     
                     
                     ## updating Progres Bar
                     incProgress(10/n, detail = "Completed")
                     Sys.sleep(5)
                     
                     
                   
                   }) ## End of Progres bar  

                 })
    
 
    
    
    
    #################
    ### Labelling Data
    
    observeEvent(input$label_data,
                 
                 {
                   
                   updateTabsetPanel(inputId = "maintab", selected = "Labelled Data")
                   
                   
                   ## Starting the Progress Bare
                   withProgress(message = paste("Labelling", input$country, input$survey_type, "data:"), value = 0,{
                     
                    n=10

                    incProgress(1/n, detail = "Reading clean data")
                    
                    data_clean <- openxlsx::read.xlsx(paste("output/UNHCR", nam, srv,"V3",ctry,"DAT_CLEAN_V1.1.xlsx", sep = "_"))
                   
                  
                   # source cleaning script
                     source("scripts/labelling.R", local = TRUE)
                   
                    ######### Saving ########
                   data_clean_fr %>%  writexl::write_xlsx(., path=paste("output/UNHCR",nam, srv,"V3",ctry,"DAT_CLEAN_FR_V1.1.xlsx", sep = "_"))
                   
                   ## Updating the bar. 
                   incProgress(10/n, detail = "Completed")
                   Sys.sleep(5)
                   
                   
                   }) ## End of progress bar
                   
                   
                   ###########################
                   ## Output of labelled data
                   # data formatted
                   output$labelled_data <- DT::renderDataTable(DT::datatable(
                     
                     data_clean_fr,
                     
                     filter = 'top',
                     
                     caption = htmltools::tags$caption(
                       style = 'caption-side: bottom; text-align: center;',
                       'Table 2: ', htmltools::em('Labelled Data')
                     ),
                     extensions = 'Buttons',
                     
                     options = list(
                       fixedColumns = TRUE,
                       columnDefs = list(list(visible=TRUE, targets = names(data_clean_fr))),
                       autoWidth = TRUE,
                       ordering = TRUE,
                       scrollX = TRUE,
                       scrollY = TRUE,
                       dom = 'Bftsp',
                       buttons = c('copy', 'csv', 'excel','pdf','print')
                     ))
                   )
    
                   
                 }) ## End of Observe event
    

    
    #################
    ### Upload Data
    observeEvent(input$upload_data,
                 
                 {
                   
                   updateTabsetPanel(inputId = "maintab", selected = "Upload Files")
                   

                   ## Starting the Progress Bar
                   withProgress(message = paste("Labelling", input$country, nam, input$survey_type, "data:"), value = 0,{
                     
                     n=10
                     
                     incProgress(1/n, detail = "Loading Resources")
                     
                   Sys.setenv("RIDL_API_KEY"=read_ridl_key())

                   file_name <- list.files(path = "output/", pattern = paste0("(.*)", nam, "(.*)", srv,"(.*)",ctry,"(.*)"), full.names = T)
                   
                   file_name_labelled <- file_name[str_detect(file_name, "FR")]
                   
                   file_name <- file_name[!str_detect(file_name, "FR")]
                   
                   
                   ### selected id
                   ds_id <- ridl_ids_list[[nam]][[tolower(ctry)]][["ids"]][[srv]]
                   ds_nam <- ridl_ids_list[[nam]][[tolower(ctry)]][["names"]][[srv]]
                   
                   ## getting the resources list from RIDL
                   resources_list  <- rckan_dataset_resource_list(dataset_id = ds_id, api_key = Sys.getenv("RIDL_API_KEY"))
                  
                   ## Extracting the non-labelled version id
                   resource_id_english <- resources_list %>% 
                     filter(file_type=="microdata" & format=="XLSX" & str_detect(tolower(name), "clean") & !str_detect(tolower(name), "french|label|fr"))
                   
                   ## Extracting the labelled version id
                   resource_id_labelled <- resources_list %>% 
                     filter(file_type=="microdata" & format=="XLSX" & str_detect(tolower(name), "clean") & str_detect(tolower(name), "french|label|fr"))
                   
                   
                   ##################################
                   ## uploading
                   ## Updating Progress Bar
                   incProgress(2/n, detail = "Uploading English Version")
                   
                   
                   # uploading english version
                   if(!purrr::is_empty(resource_id_english) & !purrr::is_empty(file_name))
                     eng_status = rckan_resource_update(resource_id = resource_id_english$id, file_path = file_name, 
                                                        base_url="https://ridl.unhcr.org/", api_key = read_ridl_key())
                   
                   ## Updating Progress Bar
                   incProgress(4/n, detail = if_else(as.numeric(eng_status)==200, "Upload successful", "Upload Failed"))
                   

                   ## Updating Progress Bar
                   incProgress(5/n, detail = "Uploading Labelled Version")
                   
                   # Uploading labelled version
                   if(!purrr::is_empty(resource_id_labelled) & !purrr::is_empty(file_name_labelled))
                     label_status = rckan_resource_update(resource_id = resource_id_labelled$id, file_path = file_name_labelled,
                                                          base_url="https://ridl.unhcr.org/", api_key = read_ridl_key())
                   
                   ## Updating Progress Bar
                   incProgress(7/n, detail = if_else(as.numeric(label_status)==200, "Upload successful", "Upload Failed"))
                   
                   # shinyjs::alert("Completed to upload")
                   
                   df_map <- tibble(ridl_name=ds_nam,
                                    ridl_dataset_id=ds_id,
                                    local=file_name,
                                    ridl_resource_id=resource_id_english$id,
                                    status=if_else(as.numeric(eng_status)==200, "Successful", "Failed")) %>%
                             add_row(ridl_name=ds_nam,
                                     ridl_dataset_id=ds_id,
                                     local=file_name_labelled,
                                     ridl_resource_id=resource_id_labelled$id,
                                     status=if_else(as.numeric(label_status)==200, "Successful", "Failed"))

                   df_map[["Select"]] <- paste0('<input type="checkbox" name="id_selected" value=',1:nrow(df_map),' checked>')

                   df_map[["_id"]] <- paste0("row_", seq(nrow(df_map)))

                   df_map <- df_map %>% relocate(Select, `_id`)


                   callback <- c(
                     sprintf("table.on('click', 'td:nth-child(%d)', function(){",
                             which(names(df_map) == "Select")),
                     "  var checkbox = $(this).children()[0];",
                     "  var $row = $(this).closest('tr');",
                     "  if(checkbox.checked){",
                     "    $row.removeClass('excluded');",
                     "  }else{",
                     "    $row.addClass('excluded');",
                     "  }",
                     "  var excludedRows = [];",
                     "  table.$('tr').each(function(i, row){",
                     "    if($(this).hasClass('excluded')){",
                     "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
                     "    }",
                     "  });",
                     "  Shiny.setInputValue('excludedRows', excludedRows);",
                     "});"
                   )



                   ## Updating Progress Bar
                   incProgress(8/n, detail = "Preparing the report")
                   Sys.sleep(5)
                   

                   #####################################
                   ### output of the data upload process
                   output$ridl_loc_map <- DT::renderDT({

                     DT::datatable(

                       df_map,

                       selection = "multiple",
                       options = list(pageLength = 5,
                                      lengthChange = FALSE,
                                      rowId = JS(sprintf("function(data){return data[%d];}",
                                                         ncol(df_map)-1)),
                                      columnDefs = list( # hide the '_id' column
                                        list(visible = FALSE, targets = ncol(df_map)-1)
                                      )
                       ),
                       rownames = FALSE,
                       escape = FALSE,
                       callback = JS(callback)
                     )
                   }, server = FALSE)


                   ### second output
                   output$excludedRows <- renderPrint({
                     input[["excludedRows"]]
                     
                   }) ## End of Data Table output
                   
                   
                   ## Updating Progress Bar
                   incProgress(10/n, detail = "Preparing the report")
                   Sys.sleep(5)
                   

                   }) # End of Progress Bar

                   
                 }) ## End of Observe event
    
    
    
    
    
    observeEvent(input$reset_all,
      
      {
        
        withProgress(message = "Removing Files:", value = 0,{
          
          # listing files to remove
          data_files <- list.files(path = "data/", pattern = "xlsx$|rds$", full.names = T)
          output_files <- list.files(path = "output/", pattern = "xlsx$|rds$", full.names = T)
          
          all_files <- c(data_files, output_files)
          
          n=length(all_files)
          
          for(i in n){
            
            incProgress(i/n, detail = "Removing all temporary files")
            
            file.remove(all_files[i])

          }
          
            incProgress(n/n, detail = "Complete.")
            Sys.sleep(5)
           
        })
        
      
    })
    
    
    
    ###################################################
    #####
    ####  OUTPUT
    ####
    ###################################################
    
  
    
  ##### Rendering loaded data
  observeEvent(
    input$load_data,
    
    {
    
      
      # render clean_data button status
      
      output$clean_df <- renderPrint(input$clean_data)
    
      # data formatted
      output$data <- DT::renderDataTable(DT::datatable(
        
        #data_filter() %>% dplyr::select(!matches("\\.status")),
        
        ctry_data()$data_clean,
        
        filter = 'top',
        
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          'Table 2: ', htmltools::em('Raw Data')
        ),
        extensions = 'Buttons',
        
        options = list(
          fixedColumns = TRUE,
          columnDefs = list(list(visible=TRUE, targets = names(ctry_data()$data_clean))),
          autoWidth = TRUE,
          ordering = TRUE,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'Bftsp',
          buttons = c('copy', 'csv', 'excel','pdf','print')
        ))
      )
      
    
  })

    

## Last line
}