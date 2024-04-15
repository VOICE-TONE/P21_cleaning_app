#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
require(ridl)


shinydashboard::dashboardPage(skin = "yellow",


                              # Header
                              shinydashboard::dashboardHeader(title = "P21 Data Cleaning"),

                              # Side panel
                              shinydashboard::dashboardSidebar(
                                width = "15%",

                                # Fluid rows

                                fluidRow(align = 'center',

                                         conditionalPanel(
                                           condition = "!input.connect",

                                           actionButton(inputId="start", label = "Click to Start")

                                         ),

                                         conditionalPanel(
                                           condition ={"input.start"},

                                           #textOutput(outputId='msg'),
                                           uiOutput(outputId='ridl_key'),
                                           uiOutput(outputId='kobo_key'),

                                           #textOutput("kobo_out"),
                                           tags$head(tags$style(type="text/css", "#ridl_key_id {width: 80%; background-color:#E6E6E6}")),
                                           tags$head(tags$style(type="text/css", "#kobo_key_id {width: 80%; background-color:#E6E6E6}")),
                                           tags$head(tags$style(type="text/css", "#kobo_key_msg {width: 80%; background-color:#E6E6E6; color:#000000}")),
                                           tags$head(tags$style(type="text/css", "#ridl_key_msg {width: 80%; background-color:#E6E6E6; color:#000000}")),
                                           p(),
                                           actionButton(inputId="connect", label = "Load Keys")

                                         ),


                                         p(),
                                         p(),
                                         p(),
                                         
                                         ##############################
                                         ### Div for data loading steps
                                         tags$div(id='div_framed',
                                                  
                                                  h4("List Datasets"),
                                                  

                                                  tags$div(id='div_version',
                                                           
                                                           radioButtons(inputId="sys_select", choices = c("RIDL"="RIDL", "KOBO"="KOBO"), label = "Select System", selected = "KOBO", inline = TRUE),
                                                           
                                                           textInput(inputId="search_data", label = "Search Data"),
                                                           
                                                           actionButton(inputId="find_data", label = "Find")

                                                           
                                                  ),
                                                  
                                                 # tags$head(tags$style(type="text/css", "#div_version {width: 100%; border-color:#FFFFFF; border-width:0px; border-style: solid; text-align: left}")),

                                                  
                                         ),
                                         tags$head(tags$style(type="text/css", "#div_framed {width: 80%; border-color:#E6E6E6; border-width:thin; border-style: solid;}")),
                                         
                                         p(),
                                         p(),
                                         p(),


                                         ##############################
                                         ### Div for data loading steps
                                         tags$div(id='div_framed',

                                                  h4("Data Loading"),

                                                  tags$div(id='div_buttons',

                                                    radioButtons(inputId="survey_name", choices = c("P21"="P21","Border Monitoring"="BM"), label = "Survey Name", selected = "P21", inline = TRUE),
                                                    
                                                    ## Return country list base on survey select
                                                    uiOutput("ctr_list"),
                                                    
                                                    
                                                    radioButtons(inputId="survey_type", choices = c("Household"="HH", "Key Informant"="KII"), label = "Survey Type", selected = "HH", inline = TRUE),

                                                  ),
                                                  tags$head(tags$style(type="text/css", "#div_buttons {width: 100%; border-color:#FFFFFF; border-width:0px; border-style: solid; text-align: left}")),

                                                  actionButton(inputId="load_data", label = "Load Data")


                                         ),
                                         tags$head(tags$style(type="text/css", "#div_framed {width: 80%; border-color:#E6E6E6; border-width:thin; border-style: solid;}")),

                                         # choices = as.list(c("BFA", "CMR", "TCD", "CIV", "MLI", "NER"))



                                         p(),
                                         p(),
                                         p(),
                                         p(),

                                         ##############################
                                         ### Div for data cleaning steps
                                         tags$div(id='div_framed',

                                                  h4("Data Cleaning"),
                                                  actionButton(inputId="clean_data", label = "Auto Cleaning"),
                                                  actionButton(inputId="manual_cleaning", label = "Manual Cleaning"),
                                                  actionButton(inputId="label_data", label = "Label Data"),


                                         ),
                                         tags$head(tags$style(type="text/css", "#div_framed {width: 80%; border-color:#E6E6E6; border-width:thin; border-style: solid}")),
                                         tags$head(tags$script(src = "scripts/messagehandler.js")),

                                         p(),
                                         p(),
                                         p(),
                                         p(),

                                         ##############################
                                         ### Div for data upload into RIDL
                                         tags$div(id='div_framed',

                                                  h4("Upload to RIDL"),

                                                  fluidRow(align='center',

                                                           actionButton(inputId="upload_data", label = "Upload to RIDL")

                                                  )


                                         ),
                                         tags$head(tags$style(type="text/css", "#div_framed {width: 80%; border-color:#E6E6E6; border-width:thin; border-style: solid}")),

                                         p(),
                                         p(),
                                         p(),
                                         p(),

                                         ##############################
                                         ### Div for data upload into RIDL
                                         tags$div(id='div_framed',

                                                  h4("Reset App"),

                                                  fluidRow(align='center',

                                                           actionButton(inputId="reset_all", label = "Reset All")

                                                  )


                                         ),
                                         tags$head(tags$style(type="text/css", "#div_framed {width: 80%; border-color:#E6E6E6; border-width:thin; border-style: solid}"))

                                )
                              ),



                              shinydashboard::dashboardBody(

                                fluidRow(align = 'center',

                                        tabsetPanel(id='maintab',
                                                    
                                                tabPanel("Data List",
                                                             
                                                             conditionalPanel(
                                                               "input.find_data",
                                                               
                                                               DT::dataTableOutput('data_list')
                                                               
                                                             )
                                                             
                                                    ),
                                                    

                                                  tabPanel("Raw Data",

                                                             conditionalPanel(
                                                               "input.connect",

                                                               DT::dataTableOutput('data')

                                                             )

                                                    ),

                                                    tabPanel("Clean Data",

                                                             conditionalPanel(
                                                               "input.connect",

                                                               DT::dataTableOutput('cleaned_data')

                                                             )

                                                    ),

                                                    tabPanel("Labelled Data",

                                                             conditionalPanel(
                                                               "input.label_data",

                                                               DT::DTOutput('labelled_data')

                                                             )


                                                    ),


                                                    tabPanel(title="Upload Files",

                                                             conditionalPanel(
                                                               "input.upload_data",

                                                               DT::DTOutput('ridl_loc_map')

                                                             )


                                                    )





                                        )


                                )
                              )


)
