source("./helpers.R")

ncores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(ncores-1) # For Vtreat
doParallel::registerDoParallel() # For Caret 




##############
### HEADER ###
##############

header <- dashboardHeaderPlus(
    
    title = tagList(
        span(class = "logo-lg", "Oxygen ML")
        ,img(src = "https://image.flaticon.com/icons/svg/119/119593.svg")
    )
    ,enable_rightsidebar = F
    
)

###############
### SIDEBAR ###
###############

sidebar <- dashboardSidebar(
    
    sidebarMenu(
        id = "tabs"
        ,menuItem(
          text= "Data"
          ,tabName = "data"
          ,icon = icon("database")
        )
        ,menuItem(
            text= "Stats"
            ,tabName = "stats"
            ,icon = icon("poll-h")
        )
        ,menuItem(
            text = "EDA"
            ,tabName = "eda"
            ,icon = icon("paint-brush")
        )
        ,menuItem(
          text = "Feature Importance"
          ,tabName = "fi"
          ,icon = icon("star")
        )
        ,menuItem(
            text = "ML"
            ,tabName = "ml"
            ,icon = icon("rocket")
        )
        
    )
)


############
### BODY ###
############

body <- dashboardBody(

    
    tabItems(
        
        
        ##############
        ### PAGE I ###
        ##############
        
        tabItem(
            tabName = "data"
            ,fluidRow(
              column(
                width = 6
                ,flipBox(
                  id = 1,
                  main_img = "https://image.flaticon.com/icons/svg/888/888900.svg",
                  header_img = "https://image.flaticon.com/icons/svg/119/119598.svg",
                  front_title = "Conventional Files",
                  front_btn_text = "GO",
                  br(),
                  br(),
                  br(),
                  "Available formats are: CSV, TXT, XLS, XLSX",
                  br(),
                  br(),
                  br(),
                  progressBar(id="p1", value = 1, striped = T, title = "Train Not Uploaded", status = "danger"),
                  br(),
                  progressBar(id="t1", value = 1, striped = T, title = "Test Not Uploaded (optional)", status = "danger"),
                  back_content = tagList(
                      br(),
                      box(
                        title = "Load Data"
                        ,status = "success"
                        ,solidHeader = T
                        ,width = 12
                        ,fileInput(
                          inputId = "file"                                                ### 1. INPUT: file ###
                          ,"Train"
                        )
                        ,fileInput(
                          inputId = "file_test"                                                ### 17. INPUT: file_test ###
                          ,"Test"
                        )
                    )
                  )
                )
                ,br()
                ,flipBox(
                  id = 3,
                  main_img = "https://image.flaticon.com/icons/svg/3439/3439047.svg",
                  header_img = "https://image.flaticon.com/icons/svg/119/119579.svg",
                  front_title = "SQL",
                  front_btn_text = "GO",
                  br(),
                  br(),
                  br(),
                  "Connect to MySQL.",
                  br(),
                  br(),
                  br(),
                  progressBar(id="p3", value = 1, striped = T, title = "Train Not Uploaded", status = "danger"),
                  br(),
                  progressBar(id="t3", value = 1, striped = T, title = "Test Not Uploaded (optional)", status = "danger"),
                  back_content = tagList(
                    column(
                      width = 6,
                      align = "center",
                      textInputAddon(
                        inputId = "host_sql",
                        label = "Host",
                        addon = icon("home"),
                        placeholder = "localhost"
                      )
                      ,textInputAddon(
                        inputId = "port_sql",
                        label = "Port",
                        addon = icon("ship"),
                        placeholder = "3306"
                      )
                      ,textInputAddon(
                        inputId = "db_sql",
                        label = "Database",
                        addon = icon("database"),
                        placeholder = "Sales"
                      )
                      ,textAreaInput(
                        inputId = "query_sql_t",
                        label = "Train",
                        placeholder = "SELECT * FROM clients"
                      )
                      ,actionButton(
                        inputId = "run_sql_tr",
                        label = "Query"
                      )
                    )
                    ,column(
                      width = 6
                      ,textInputAddon(
                        inputId = "user_sql",
                        label = "User",
                        addon = icon("user"),
                        placeholder = "username"
                      )
                      ,passwordInputAddon(
                        inputId = "pw_sql",
                        label = "Password",
                        addon = icon("key"),
                        placeholder = "*******"
                      )
                      ,textInputAddon( 
                        inputId = "driver_sql",
                        label = "Driver",
                        addon = icon("dharmachakra"),
                        value = "MySQL"
                      )
                      ,textAreaInput(
                        inputId = "query_sql_tst",
                        label = "Query Test",
                        placeholder = "SELECT * FROM clients_test"
                      )
                      ,actionButton(
                        inputId = "run_sql_tst",
                        label = "Query"
                      )
                    )
                  
                  )
                )
              )
              ,column(
                width = 6
                ,flipBox(
                  id = 2,
                  main_img = "https://image.flaticon.com/icons/svg/281/281778.svg",
                  header_img = "https://image.flaticon.com/icons/svg/119/119594.svg",
                  front_title = "Google Sheets",
                  front_btn_text = "GO",
                  br(),
                  br(),
                  br(),
                  "Introduce the spread sheet URL or the sheet ID (for encoding).",
                  br(),
                  br(),
                  br(),
                  progressBar(id="p2", value = 1, striped = T, title = "Train Not Uploaded", status = "danger"),
                  br(),
                  progressBar(id="t2", value = 1, striped = T, title = "Test Not Uploaded (optional)", status = "danger"),
                  back_content = tagList(
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      br(),
                      br(),
                      boxPlus(
                        width = 12
                        ,title = "URL"
                        ,status = "info"
                        ,solidHeader = T
                        ,closable = F
                        ,textInputAddon( 
                          inputId = "train_g",
                          label = "Train",
                          value = "",
                          placeholder = "https://docs.google.com/spreadsheets/...",
                          addon = icon("link")
                        ),
                        textInputAddon(
                          inputId = "test_g",
                          label = "Test",
                          value = "",
                          placeholder = "https://docs.google.com/spreadsheets/...",
                          addon = icon("link")
                        )
                      )
                    )
                  )
                )
                ,br()
                ,flipBox(
                  id = 4,
                  main_img = "https://www.flaticon.es/svg/static/icons/svg/1051/1051275.svg",
                  header_img = "https://image.flaticon.com/icons/svg/119/119597.svg",
                  front_title = "Github",
                  front_btn_text = "GO",
                  br(),
                  br(),
                  br(),
                  "Load data from github links.",
                  br(),
                  br(),
                  br(),
                  progressBar(id="p4", value = 1, striped = T, title = "Train Not Uploaded", status = "danger"),
                  br(),
                  progressBar(id="t4", value = 1, striped = T, title = "Test Not Uploaded (optional)", status = "danger"),
                  back_content = tagList(
                    column(
                      width = 12,
                      align = "center",
                      br(),
                      br(),
                      br(),
                      boxPlus(
                        width = 12
                        ,title = "URL"
                        ,status = "danger"
                        ,solidHeader = T
                        ,closable = F
                        ,textInputAddon( 
                          inputId = "train_git",
                          label = "Train",
                          value = "",
                          placeholder = "https://raw.githubusercontent.com/r4msi/LSTM_Ozone/master/ozone.csv",
                          addon = icon("link")
                        ),
                        textInputAddon(
                          inputId = "test_git",
                          label = "Test",
                          value = "",
                          placeholder = "https://raw.githubusercontent.com/r4msi/LSTM_Ozone/master/ozone.csv",
                          addon = icon("link")
                        )
                      )
                      
                    
                    )
                      
                  )
                )
              )
            )
        )
    
        
        ,tabItem(
            tags$head(tags$style(HTML("
                                #final_text {
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                }
                                
                                "))),
            
            tabName = "stats"
            # Hay que hacer fluid Rows para que todo quede ordenado
            ,fluidRow(
                valueBoxOutput(
                    outputId = "rows"
                    ,width = 2
                )
                ,valueBoxOutput(
                    outputId = "columns"
                    ,width = 2
                )
                ,valueBoxOutput(
                    outputId = "missing"
                    ,width = 2
                )
                ,valueBoxOutput(
                    outputId = "numeric"
                    ,width = 2
                )
                ,valueBoxOutput(
                    outputId = "character"
                    ,width = 2
                )
                ,valueBoxOutput(
                    outputId = "dates"
                    ,width = 2
                )
            )
            
            ,fluidRow(
              
                boxPlus(
                    title = ""
                    ,status = "success"
                    ,solidHeader = F
                    ,width = 12
                    ,closable = F
                    ,enable_label = T
                    ,label_text = "Change Train's Variables Class"
                    ,label_status = "success"
                    ,enable_dropdown = TRUE
                    ,dropdown_menu = dropdownItemList(
                        radioButtons(
                          inputId = "variable_class"                             ### 2. INPUT: variable_class ###
                          ,label = "Select Class"
                          ,choices = c("Numeric", "Character", "Dates")
                        )
                    )
                    ,column(
                      width = 12
                      ,align = "center"
                      ,tableOutput("input_file") ### OUTPUT ###
                    )
                )
                ,uiOutput(
                  outputId = "table_test"
                )
            )
            
            ,fluidRow(
                boxPlus( 
                    title="Expand to view Data"
                    ,width = 12
                    ,solidHeader = TRUE
                    ,status = "info"
                    ,collapsible = TRUE
                    ,collapsed = TRUE
                    ,closable = FALSE
                    ,dataTableOutput("input_file_html")                         ### 2. OUTPUT: input_file_html"
                )
            )
        )
        
        ###############
        ### PAGE II ###
        ###############
        
        ,tabItem(
            
            tabName = "eda"
            
            ,fluidRow(
                boxPlus(
                    title = "Input"
                    ,width = 12
                    ,collapsible = TRUE
                    ,closable = FALSE
                    ,status = "info"
                    ,column(
                        align = "center"
                        ,width = 12 
                        ,selectInput(
                            inputId = "tttarget"                                       ### 3. INPUT: tttarget ###
                            ,label = "Select Target"
                            ,choices = "Upload Data First" # Needs to be populated.
                        )
                    )
                    ,boxPlus(
                        title = "Expand for Report"
                        ,solidHeader = TRUE
                        ,collapsible = TRUE
                        ,collapsed = TRUE
                        ,closable = FALSE
                        ,width = 12
                        ,status = "success"
                        ,column( # Center buttom
                            align="center"
                            ,width = 12 # 12 porque esta respecto a Box.             ### 4. OUTPUT: report ###
                            ,downloadButton(
                                outputId = "report"
                                ,label = "Create Report"
                            )

                        )
                    )
                )
            )
            
            ,fluidRow(
                boxPlus(
                    title = "Missing Values"
                    ,width = 6
                    ,status = "danger"
                    ,closable = F
                    ,collapsible = T
                    ,plotlyOutput(
                        outputId = "na"                                          ### 5. OUTPUT: NA ###
                        
                    ) %>% withSpinner()
                )
                ,boxPlus(
                    title = "Interactive Target"
                    ,width = 6
                    ,status = "success"
                    ,closable = F
                    ,collapsible = T
                    ,plotlyOutput(
                        outputId = "tttarget_plotly"                                 ### 6. OUTPUT: tttarget_plotly ###
                    ) %>% withSpinner()
                )
            )
            
            
            ,fluidRow(
                tabBox(
                    title = "Numerical"
                    ,width = 6
                    ,tabPanel(
                        title = "Histograms"
                        ,plotOutput(
                            outputId = "histograms"                             ### 7. OUTPUT: histograms ###
                            ,height = "auto"
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        title ="Bivariate Box-Plots"
                        ,plotOutput(
                          outputId = "boxplots"                               ### 8. OUTPUT: boxplots ###
                          ,height = "auto"
                        ) %>% withSpinner()
                    )
                    # ,tabPanel(
                    #     "Scatter-Plots"
                    #     ,jqui_resizable(
                    #         plotOutput(
                    #             outputId = "scatterplots"                         ### 9. OUTPUT: scatterplots ###
                    #             ,height = 600
                    #         )
                    #     ) 
                    # )
                )
                ,tabBox(
                    title = "Categorical"
                    ,width = 6
                    ,tabPanel(
                        "Bar-Chart"
                        ,plotOutput(
                            outputId = "bars"                                  ### 5. OUTPUT: bars ###
                            ,height = "auto"
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        "Bar-Bivariate"
                        ,plotOutput(
                            outputId = "bars_bi"                               #### 11. OUTPUT: bars_bi ###
                            ,height = "auto"
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        "Cardinality"
                        ,plotOutput(
                            outputId = "cardinality"                           ### 12. OUTPUT: cardinality ###
                            ,height = "auto"
                        ) %>% withSpinner()
                    )
                )
            )
            
            ,fluidRow(
                boxPlus(
                    title = "Correlations"
                    ,width = 12
                    ,status = "warning"
                    ,closable = F
                    ,collapsible = T
                    ,collapsed = F
                    ,plotOutput(
                        outputId = "corr"                               #### 13. OUTPUT: corr ###
                        ,height = 1400
                    ) %>% withSpinner()
                )
            )
        )
        
        ################
        ### PAGE III ###
        ################
        
        ,tabItem(
            
            tabName = "ml"
            
            ,fluidRow(
                column(
                    width = 4
                    ,boxPlus(
                        title = "Strategy"
                        ,closable = F
                        ,status = "success"
                        ,width = 12
                        ,uiOutput(
                            outputId = "value_tttarget"
                        )
                        ,boxPlus(
                          title = "Validation"
                          ,closable = F
                          ,solidHeader = TRUE
                          ,status = "warning"
                          ,width = 12
                          ,collapsible = T
                          ,collapsed = F
                          ,column(
                            width = 12
                            ,align = "center"
                            ,sliderInput(                                                     ### 14. INPUT: split ###
                              inputId = "split"
                              ,min = .6
                              ,max = .9
                              ,value = .8
                              ,step = .05
                              ,label = "Train-Test Split"
                            )
                            ,sliderInput(                                                       ### 15. INPUT: cv ###
                              inputId = "cv"
                              ,label = "Cross-Validation"
                              ,min = 3
                              ,max = 10
                              ,step = 1
                              ,value = 3
                            )
                          )
                        )
                        ,boxPlus(
                            title = "Preprocess"
                            ,closable = F
                            ,solidHeader = TRUE
                            ,status = "primary"
                            ,width = 12
                            ,collapsible = T
                            ,collapsed = F
                            ,column(
                                width = 12
                                ,align = "center"
                                ,h5("Random Forest Imputation")
                                ,switchInput(
                                    inputId = "missranger",
                                    onStatus = "primary", 
                                    offStatus = "warning",
                                    label = '<i class="fas fa-question-circle"></i>'
                                )
                                ,h5("Outliers Treatment")
                                ,switchInput(
                                    inputId = "outliers",
                                    onStatus = "primary", 
                                    offStatus = "warning",
                                    label = '<i class="fas fa-pastafarianism"></i>'
                                )
                                ,actionButton(                                             ### 14. INPUT vtreat ###
                                    inputId = "vtreat"
                                    ,label = "Bayesian Treatments"
                                )
                            )
                        )
                        ,boxPlus(
                          title = "Models"
                          ,closable = F
                          ,solidHeader = TRUE
                          ,status = "info"
                          ,width = 12
                          ,collapsible = T
                          ,collapsed = T
                          ,column(
                            width = 12
                            ,align = "center"
                            ,pickerInput(                                                        ### 18. INPUT: pick_model ###
                              inputId = "pick_model"
                              ,label = "Select Models"
                              ,choices = c(
                                "XGB", "Lasso", "Ridge", "ElasticNet",
                                "LinearModel", "RandomForest",
                                "SVM","KNN"
                              )
                              ,multiple = T
                              ,selected = c("RandomForest","ElasticNet")
                              ,options = list(
                                `actions-box` = TRUE,
                                `select-all-text` = "Select All"
                              )
                              
                            )
                            ,actionButton(
                              inputId = "process_train"                                        ### 16. INPUT: process_train ###
                              ,label = "Porcessing"
                            )
                          )
                        
                        )
                        ,boxPlus(
                            title = "Predict"
                            ,closable = F
                            ,solidHeader = TRUE
                            ,status = "success"
                            ,width = 12
                            ,collapsible = T
                            ,collapsed = T
                            ,column(
                                width = 12
                                ,align = "center"
                                ,selectInput(                                                        ### 18. INPUT: select_model ###
                                    inputId = "select_model"
                                    ,label = "Select Model"
                                    ,choices = c(
                                      "XGB", "Lasso", "Ridge", "ElasticNet",
                                      "LinearModel", "RandomForest",
                                      "SVM","KNN"
                                    )
                                
                                )
                                ,switchInput(
                                  inputId = "tune_lenght",
                                  onStatus = "primary", 
                                  offStatus = "warning",
                                  value = FALSE,
                                  label = '<i class="fas fa-wrench"></i>'
                                )
                                ,actionButton(
                                    inputId = "predict"
                                    ,label = "Predict"
                                )
                                ,downloadButton(
                                    outputId = "download_predict"                                           ### 19. INPUT: download_predict ###
                                    ,label = "Download Predictions"
                                    ,style = "color: white; background-color: #222D32"
                                )
                            )
                            
                        )
                        
                    )
                )
                ,column(
                    width = 8
                    ,boxPlus(
                        title = "MODEL RESULTS"
                        ,width = 12
                        ,solidHeader = T
                        ,status = "primary"
                        ,closable = F
                        ,plotlyOutput(
                            outputId = "cv_model_results"                                              ### 20. OUTPUT: cv_model_results ###
                        )
                    )
                    ,boxPlus(
                        title = "TEST RESULTS"
                        ,width = 12
                        ,solidHeader = T
                        ,closable = F
                        ,status = "success"
                        ,plotlyOutput(
                            outputId = "test_models_results"                                                  ### 21. OUTPUT: test_models_results ###
                        )
                    )
                )
            )
        )    
        ################
        ### PAGE  IV ###
        ################
        ,tabItem(
          tabName = "fi"
          ,fluidRow(
            column(
              width = 12
              ,boxPlus(
                title = "Feature Importance"
                ,status = "info"
                ,closable = F
                ,width = 6
                ,plotlyOutput(
                  outputId = "ranger_importance"
                ) %>% withSpinner()
              )
              ,boxPlus(
                title = "Outliers"
                ,status = "warning"
                ,closable = F
                ,width = 6
                ,plotOutput(
                  outputId = "ranger_outliers"
                ) %>% withSpinner()
              )
            )
          )
            
        )
        
    )
    
)


##########
### UI ###
##########



ui <- dashboardPagePlus(
    enable_preloader = T
    ,loading_duration = 1
    ,header
    ,sidebar
    ,body
    ,skin = "black"
)


##############
### SERVER ###
##############

server <- function(input, output, session) {
    
    showModal(
      modalDialog(
        title = "Welcome!",
        easyClose = TRUE,
        size = "l",
        footer = boxPlus(
          title = "Workflow",
          status = "info",
          width = 12,
          collapsible = T,
          closable = F,
          timelineBlock(
            timelineEnd(color = "danger"),
            timelineLabel("First", color = "teal"),
            timelineItem(
              title = "Load Data",
              icon = "database",
              color = "olive",
              "Upload txt/csv/excel files, google sheets or connect to SQL."
            ),
            timelineLabel("Second", color = "orange"),
            timelineItem(
              title = "Explore the basics stats",
              icon = "poll-h",
              color = "maroon",
              "Data dimensions, maximum, minimum, mean, cardinality... is the data integrity correct?",
            ),
            timelineLabel("Third", color = "blue"),
            timelineItem(
              title = "Explore the EDA",
              icon = "paint-brush",
              color = "navy",
              "Observe variables' distribution, correlations, missing values for a better data understaning."
            ),
            timelineLabel("Fourth", color = "green"),
            timelineItem(
              title = "Observe Outliers & Feature Importance",
              icon = "star",
              color = "fuchsia",
              "Observes the variables that reduced the error the most in a random forest and the atypical observations."
            ),
            timelineLabel("Fifth", color = "yellow"),
            timelineItem(
              title = "Preprocess & Model & Predict",
              icon = "rocket",
              color = "purple",
              "Impute missing values, transform variables, create new bayesian variables and perform feature selection.
                      Compare different algorithms using cross validation: Shrinkage methods, support vector machines, boosting, ensemble learning...
                      Select the best model & tune it up. If you have test data, make predictions, otherwise, save the model.
                      "
            ),
            timelineStart(color = "gray")
          )
          ,modalButton("Got it!")
        )
      )
    )
  
    options(shiny.maxRequestSize=30*524^3) # 35 MB
    
    # Conventional data #
    filedata <- reactive({

        infile <- input$file
        infile2 <- input$train_g
        infile3 <- input$train_git
        
        # User has not uploaded a file yet
        if (is.null(infile) & infile2=="" & infile3==""){
            return(NULL)
        }

        if (!is.null(infile)) {

          if (grepl(".csv|.txt", infile$datapath) == TRUE) {

            return(fread(
              infile$datapath,
              integer64 = "numeric",
              data.table = F,
              na.strings = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else if (grepl(".xlsx|.xls", infile$datapath) == TRUE) {

            return(read_excel(
              infile$datapath,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
              )
            )

          } else {

            show_alert(
              title = "The file must be csv/txt/xlsx/xls",
              type = "warning"
            )

            return(NULL)

          }

        }

        if (infile2 != "") {

          if (!is.null(infile)) {

            show_alert(
              title = "Reload: Other data is loaded.",
              type = "warning"
            )

          } else if (grepl("spreadsheet", infile2) == TRUE) {

            gs4_auth(cache = FALSE)

            return(read_sheet(
              infile2,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else {

            show_alert(
              title = "Must be a spreadsheet",
              type = "warning"
            )

            return(NULL)

          }

        }

        if (infile3 != "") {

          if (grepl(".csv|.txt", infile3) == TRUE) {

            return(fread(
              infile3,
              integer64 = "numeric",
              data.table = F,
              na.strings =  c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else if (grepl(".xlsx|.xls", infile3) == TRUE) {

            return(read_excel(
              infile3,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else {

            show_alert(
              title = "The file must be csv/txt/xlsx/xls",
              type = "warning"
            )

            return(NULL)

          }

        }

    })

    filedata_sql <- reactive({
 
      if (input$run_sql_tr==0) {
        return(NULL)
      }

      con <- try(dbConnect(
        RMySQL::MySQL() # Construct SQL driver
        ,dbname = input$db_sql
        ,host = input$host_sql
        ,port = as.numeric(input$port_sql)
        ,user = input$user_sql
        ,password = input$pw_sql
      ))

      if (class(con)[1]=="try-error") {

        show_alert(
          title = "Warning",
          text = "Incorrect Query",
          type = "warning"
        )

        return(NULL)

      } else {

        con <- dbConnect(
          RMySQL::MySQL() # Construct SQL driver
          ,dbname = input$db_sql
          ,host = input$host_sql
          ,port = as.numeric(input$port_sql)
          ,user = input$user_sql
          ,password = input$pw_sql
        )

      }

      dbGetQuery(
        con,
        input$query_sql_t
      )

    })

    trigger_sql <- observeEvent(input$run_sql_tr,{
    
      filedata_sql()

    })

    filetest <- reactive({

        infile <- input$file_test
        infile2 <- input$test_g
        infile3 <- input$test_git
        # User has not uploaded a file yet
    
        if (is.null(infile) & infile2=="" & infile3==""){
          return(NULL)
        }

        if (!is.null(infile)) {

          if (grepl(".csv|.txt", infile$datapath) == TRUE) {

            return(fread(
              infile$datapath,
              integer64 = "numeric",
              data.table = F,
              na.strings =  c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else if (grepl(".xlsx|.xls", infile$datapath) == TRUE) {

            return(read_excel(
              infile$datapath,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else {

            show_alert(
              title = "The file must be csv/txt/xlsx/xls",
              type = "warning"
            )

            return(NULL)

          }

        }

        if (infile2 != "") {

          if (!is.null(infile)) {

            show_alert(
              title = "Reload: Other data is loaded.",
              type = "warning"
            )

          } else if (grepl("spreadsheet", infile2) == TRUE) {

            gs4_auth(cache = FALSE)
            return(read_sheet(
              infile2,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else {

            show_alert(
              title = "Must be a spreadsheet",
              type = "warning"
            )

            return(NULL)

          }

        }

        if (infile3 != "") {

          if (grepl(".csv|.txt", infile3) == TRUE) {

            return(fread(
              infile3,
              integer64 = "numeric",
              data.table = F,
              na.strings = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else if (grepl(".xlsx|.xls", infile3) == TRUE) {

            return(read_excel(
              infile3,
              na = c("", " ", "NA", "N/A", "n/a", "unknown", "Unknown", "missing", "Missing", "na", "null", "NULL", "Na", "nA", "?")
            ))

          } else {

            show_alert(
              title = "The file must be csv/txt/xlsx/xls",
              type = "warning"
            )

            return(NULL)

          }

        }

    })

    # tttarget + SELECTING OPTIMAL tttargetS
    observe({
        
        df <- filedata()
        z <- filedata_sql()
        
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)){
            return(NULL)
        }

        nzv <- nearZeroVar(df,freqCut = 95/5, uniqueCut = 5)

        if (length(nzv)>0) {
          df <- df[,-nzv]
        }

        unique_char <- sapply(Filter(is.character, df),function(x){ # Not advisable to plot high cardinality variables
            length(unique(x))>5
        })
        char_exclude <- names(which(unique_char==TRUE))
        id <- grepl("id|ID|Id|iD",names(df)) # It does not make sense to add ID as tttarget
        if (sum(id) > 0) {
            char_exclude <- c(char_exclude, names(df)[id])
        }

        char_exclude <- c(char_exclude, names(df)[sapply(df, function(x){sum(is.na(x))})>0]) # Variables with NA cannot be tttargets

        var = select(df,-all_of(char_exclude)) %>% names()
        updateSelectInput(session,"tttarget", choices =  var) ### 3. INPUT: tttarget ###
    })

    # OBSERVE MODEL FOR PREDICTION
    observe({
      

      df <- filedata()

      z <- filedata_sql()

      if (!is.null(z)) {
        df <- z
      }
      if (is.null(df)){
        return(NULL)
      }

      updateSelectInput(session,"select_model", choices =  input$pick_model)
      updateProgressBar(session, id="p1", value = 100, title = "Train Loaded", status = "success")
      updateProgressBar(session, id="p2", value = 100, title = "Train Loaded", status = "success")
      updateProgressBar(session, id="p3", value = 100, title = "Train Loaded", status = "success")
      updateProgressBar(session, id="p4", value = 100, title = "Train Loaded", status = "success")

    })

    observe({
      

      df <- filetest()

      # z <- filedata_sql_test()

      # if (!is.null(z)) {
      #   df <- z
      # }

      if (is.null(df)){
        return(NULL)
      }

      updateProgressBar(session, id="t1", value = 100, title = "Test Loaded", status = "success")
      updateProgressBar(session, id="t2", value = 100, title = "Test Loaded", status = "success")
      updateProgressBar(session, id="t3", value = 100, title = "Test Loaded", status = "success")
      updateProgressBar(session, id="t4", value = 100, title = "Test Loaded", status = "success")

    })

    vtreat <- eventReactive(input$vtreat,{

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(NULL)
        }

        test <- filetest()
        
        # df$prop_missings<-apply(is.na(df),1,mean)
        # 
        # if (is.null(test))
        # test$prop_missings<-apply(is.na(test),1,mean)

        # df <- lapply(df,function(x){
        #
        #     ifelse(x %in% c("","NA","N/A","Unknown","unknown","missing", "Missing", "?", "na", " ", "n/a"), NA, x)
        #
        # })
        #
        # df <- df %>% as.data.frame()
        #
        # test <- lapply(test,function(x){
        #
        #   ifelse(x %in% c("","NA","N/A","Unknown","unknown","missing", "Missing", "?", "na", " ", "n/a"), NA, x)
        #
        # })
        
        
        #test <- test %>% as.data.frame()

        df$tttarget <- df[,input$tttarget]
        df[,input$tttarget] <- NULL

        tttargetLevels <- length(unique(df[,"tttarget"]))
        
        print(tttargetLevels)

        # r <- recipe(tttarget~., data = df) %>%
        #   step_YeoJohnson(all_numeric(),-all_outcomes())
        #
        # df <- r %>% prep %>% juice() %>% as.data.frame()
        #
        # test <- r %>% prep %>% bake(test) %>% as.data.frame()


        if (tttargetLevels > 5) {

            set.seed(123)
            inTrain <- createDataPartition(df$tttarget, p = input$split, list = FALSE)
            training <- df[ inTrain,]
            testing <- df[-inTrain,]

            if (input$missranger == TRUE) {

              l <- missVtreatRanger(training,testing,test,"tttarget")
              training <- l[[1]]
              testing <- l[[2]]
              test <- l[[3]]

            }

            list <- names(select(training,-tttarget)) # training
            set.seed(123)
            treatments <- vtreat::mkCrossFrameNExperiment(
                training,
                varlist = list,
                outcomename = "tttarget",
                collarProb = .03, doCollar = input$outliers,
                parallelCluster = parallelCluster
            ) 
            df_treat <- treatments$crossFrame
            df_treat_fs <- select(df_treat,-tttarget)[,treatments$treatments$scoreFrame$recommended]
            df_treat_fs$tttarget <- df_treat[,"tttarget"]

        } else if (tttargetLevels <= 5) {

            df$tttarget <- df$tttarget %>% factor()
            set.seed(123)
            inTrain <- createDataPartition(df$tttarget, p = input$split, list = FALSE)
            training <- df[ inTrain,]
            testing <- df[-inTrain,]

            if (input$missranger == TRUE) {

              l <- missVtreatRanger(training, testing, test,"tttarget")
              training <- l[[1]]
              testing <- l[[2]]
              test <- l[[3]]

            }

            list <- names(select(df,-tttarget))
            set.seed(123)
            treatments <- vtreat::mkCrossFrameCExperiment(
                training,
                varlist = list,
                outcomename = "tttarget",
                outcometarget = df[,"tttarget"][[2]],
                collarProb = .03, doCollar = input$outliers,
                parallelCluster = parallelCluster
            )
            df_treat <- treatments$crossFrame
            df_treat_fs <- select(df_treat,-tttarget)[,treatments$treatments$scoreFrame$recommended]
            df_treat_fs$tttarget <- df_treat[,"tttarget"]

        }

        testing_treat <- vtreat::prepare(treatments$treatments, testing, doCollar = input$outliers)
        
        return(list(
            treatments = treatments,
            test = test,
            df_treat_fs = df_treat_fs,
            testing_treat = testing_treat,
            tttargetLevels = tttargetLevels,
            training = training,
            testing = testing
        ))

    })

    observe_upload_preprocess <- observe({                           ### Warning if data it's not upload ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df) & input$vtreat>0) {

            return(
                show_alert(
                    title = "Upload Data First!",
                    type = "warning"
                )
            )
        }

    })
    

    activate_Preprocess <- observeEvent(input$vtreat,{                                      ### Activate preprocess ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(NULL)
        }

        if (!is.null(df)) {

            showModal(modalDialog("Preprocessing" ,fade = TRUE, size = "l", footer = "5 - 60 sec"))
            vtreat()$treatments
            vtreat()$df_treat_fs
            vtreat()$tttargetLevels
            vtreat()$testing_treat
            removeModal()
            show_alert(
                title = "Success",
                text = "All in order",
                type = "success"
            )
        }
    })
    
    # observe({
    #   
    #   df <- filedata()
    #   if (input$tttarget!="Upload Data First") {
    #     
    #     try(
    #       if(class(length(unique(df[,input$tttarget]))) != "numeric") {
    #         stop("New Data")
    #       } else { 
    #         tt <- length(unique(df[,input$tttarget])) 
    #         if (tt > 2 & tt <= 5) {
    #           updatePickerInput(session,"pick_model", choices =  c("XGB", "RandomForest", "SVM", "KNN"), selected = c("RandomForest", "XGB"))
    #         }
    #       }
    #     )
    #     
    #     # if (tt > 2 & tt <= 5) {
    #     #   updatePickerInput(session,"pick_model", choices =  c("XGB", "RandomForest", "SVM", "KNN"), selected = c("RandomForest", "XGB"))
    #     # }
    #   } else {
    #     return(NULL)
    #   }
    #   
    # })

    validation <- eventReactive(input$process_train,{

        df_treat_fs <- vtreat()$df_treat_fs
        tttargetLevels <- vtreat()$tttargetLevels
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }

        if (is.null(df_treat_fs)|is.null(df)) {
          show_alert(
            title = "Preprocess data first!",
            type = "warning"
          )
          stop()
        }
        showModal(modalDialog("Fitting Models" ,fade = TRUE, size = "l", footer = NULL))

        set.seed(123)
        trControl <- trainControl(
            method = "cv",
            savePredictions = "final",
            index = createFolds(df_treat_fs$tttarget, k =input$cv),
            allowParallel = TRUE,
            verboseIter = FALSE
        ) %>% withProgress()

        xgbTreeGrid <- expand.grid(
            nrounds = 50,
            max_depth = 3,
            eta = 0.3,
            gamma = 0,
            colsample_bytree = .6,
            subsample = 0.75,
            min_child_weight = 1)
        lassoGrid <- expand.grid(
            alpha =1,
            lambda = .1
        )
        ridgeGrid <- expand.grid(
            alpha =0,
            lambda = .1
        )
        elasticNetGrid <- expand.grid(
            alpha = 0.5
            ,lambda = .1
        )
        linearGrid <- expand.grid(
            alpha = 0,
            lambda = 0
        )
        if (tttargetLevels<=5) {
            rfGrid <- expand.grid(
                mtry = round(sqrt(ncol(df_treat_fs))),
                splitrule = "gini",
                min.node.size = 1)
        } else {
            rfGrid <- expand.grid(
                mtry = round(sqrt(ncol(df_treat_fs))),
                splitrule = "variance",
                min.node.size = 5)
        }
        knnGrid <- expand.grid(
            k = 5
        )
        svmGrid <- expand.grid(
            sigma = 0.09581,
            C = .5
        )


        l <- list(
          XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid, preProcess = c("YeoJohnson")),
          Lasso = caretModelSpec(method = "glmnet", tuneGrid = lassoGrid, preProcess = c("YeoJohnson")),
          Ridge = caretModelSpec(method = "glmnet", tuneGrid = ridgeGrid, preProcess = c("YeoJohnson")),
          ElasticNet = caretModelSpec(method = "glmnet", tuneGrid = elasticNetGrid, preProcess = c("YeoJohnson")),
          LinearModel = caretModelSpec(method = "glmnet", tuneGrid = linearGrid, preProcess = c("YeoJohnson")),
          RandomForest = caretModelSpec(method = "ranger", tuneGrid = rfGrid, num.trees=150, preProcess = c("YeoJohnson")),
          #Tree = caretModelSpec(method = "rpart", preProcess = c("YeoJohnson")),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid, preProcess = c("YeoJohnson","center","scale")),
          KNN   = caretModelSpec(method = "knn", tuneGrid = knnGrid,  preProcess = c("center","scale"))
        )


        l_m <- list(
          XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid, preProcess = c("YeoJohnson")),
          RandomForest     = caretModelSpec(method = "ranger", tuneGrid = rfGrid, num.trees=150, preProcess = c("YeoJohnson")),
          #Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid, preProcess = c("YeoJohnson","center","scale"))
        )


        if (tttargetLevels == 2 | tttargetLevels > 5) {

            if (length(input$pick_model) >= 2) {

              m <- sapply(l %>% names, function(x){
                x == input$pick_model
              })

              r <- which(apply(m, 2,sum) == 0) %>% names

              l[r] <- NULL

            } else {

              show_alert(
                title = "Warning",
                text = "Needed at least 2 models. Using XGB, SVM, Tree & RF",
                type = "warning"
              )

              l <- l_m
            }

            set.seed(123)
            withProgress(modelList <- caretList(
                tttarget ~ ., data = df_treat_fs,
                trControl = trControl,
                tuneList = l
            ))

        }

        else {

            if (length(input$pick_model) >= 2) {
              m <- sapply(l_m %>% names, function(x){
                x == input$pick_model
              })

              r <- which(apply(m, 2,sum) == 0) %>% names

              l_m[r] <- NULL
            } else {
              show_alert(
                title = "Warning",
                text = "Needed at least 2 models. Using XGB, SVM, Tree & RF",
                type = "warning"
              )
            }

            set.seed(123)
            modelList <- caretList(
                tttarget ~ ., data = df_treat_fs,
                trControl = trControl,
                tuneList = l_m
            )
        }

        show_alert(
            title = "Success",
            text = "All in order",
            type = "success"
        )
        removeModal()
        return(list(
            modelList = modelList,
            trControl = trControl
        ))

    })


                                #########################################################################################################################################################
                                ######################################################################## OUTPUTS ########################################################################
                                #########################################################################################################################################################

    output$rows <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº Rows")} else {nrow(df)}
            ,subtitle = "Rows"
            ,icon = icon("server")
            ,color = "blue"
        )

    })

    output$columns <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº Columns")} else {ncol(df)}
            ,subtitle = "Columns"
            ,icon = icon("columns")
            ,color = "green"
        )

    })

    output$missing <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº NA")} else {sum(is.na(df))}
            ,subtitle = "Missing Values"
            ,icon = icon("times-circle")
            ,color = "red"
        )

    })

    output$numeric <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº Num")} else {ncol(Filter(is.numeric,df))}
            ,subtitle = "Numeric"
            ,icon = icon("calculator")
            ,color = "orange"
        )

    })

    output$character <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº Char")} else {ncol(Filter(is.character,df))}
            ,subtitle = "Character"
            ,icon = icon("at")
            ,color = "teal"
        )

    })

    output$dates <- renderValueBox({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        valueBox(
            value = if (is.null(df)) {h4("Nº Dates")} else {ncol(Filter(is.Date,df))}
            ,subtitle = "Dates"
            ,icon = icon("calendar")
            ,color = "fuchsia"
        )

    })

    output$input_file <- renderTable({                                                          ### 2. INPUT: variable_class ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            df <- iris
            df$Species <- as.character(df$Species)
        }

        if (input$variable_class=="Numeric")
            skim(Filter(is.numeric, df))[,c(-3,-8,-5,-12)]
        else if (input$variable_class=="Character")
            skim(Filter(is.character, df))[,c(-4)]
        else {
            skim(Filter(is.Date, df))
        }

    })
    
    output$table_test <- renderUI({
      boxPlus(
        title = ""
        ,status = "warning"
        ,solidHeader = F
        ,width = 12
        ,closable = F
        ,enable_label = T
        ,label_text = "Change Test's Variable Class"
        ,label_status = "success"
        ,enable_dropdown = TRUE
        ,dropdown_menu = dropdownItemList(
          radioButtons(
            inputId = "variable_class_test"                             ### 2. INPUT: variable_class ###
            ,label = "Select Class"
            ,choices = c("Numeric", "Character", "Dates")
          )
        )
        ,column(
          width = 12
          ,align = "center"
          ,tableOutput("input_file_test") ### OUTPUT ###
        )
      )
    })
    
    output$input_file_test <- renderTable({                                                          ### 2. INPUT: variable_class ###
      
      df <- filetest()
      # z <- filedata_sql()
      # if (!is.null(z)) {
      #   df <- z
      # }
      if (is.null(df)) {
        return(NULL)
      }
      
      if (input$variable_class_test=="Numeric")
        skim(Filter(is.numeric, df))[,c(-3,-8,-5,-12)]
      else if (input$variable_class_test=="Character")
        skim(Filter(is.character, df))[,c(-4)]
      else {
        skim(Filter(is.Date, df))
      }
      
    })

    output$input_file_html <- renderDataTable({                                                     ### 3. OUTPUT: input_file_html ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            df <- iris
        }

        head(df,25)

    })

    # output$report <- downloadHandler(
    #   dd <- filedata(), 
    #   function(theFile){                            ### 4. Output: report ###
    # 
    #     showModal(modalDialog("Creating Report" ,fade = TRUE, size = "l", footer = "1-3 mins"))
    #     create_report(dd, report_title = "EDA")
    #     removeModal()
    # 
    # })
    
    output$report <- downloadHandler(
      
      
      filename = "report.html",
      
      content= function(file) {
        dd <- filedata()        
        
        showModal(modalDialog("Creating Report" ,fade = TRUE, size = "l", footer = "1-3 mins"))
        tempReport <- file.path(tempdir(), "report.html")
        create_report(dd) 
        file.copy("report.html", tempReport, overwrite = TRUE)
        
        render(
          tempReport,
          output_file = file,
          params = 1,
          envir = new.env(parent = globalenv())
        )
        
        removeModal()
    

   
      }

    )

    output$na <- renderPlotly({                                                                         ### 5. Output: na ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/na.RDS"))
        }

        na <- sum(is.na(df))
        
        if (na > 0) {
          
          plot_missing(
            df,
            ggtheme = theme_plex,
            missing_only = TRUE
          ) %>% ggplotly()
          
        } else {
          
          return(readRDS("rds/no_na.RDS"))
          
        }

    })

    output$tttarget_plotly <- renderPlotly({                                                      ### 6. tttarget_plotly

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/target_plotly.RDS"))
        }

        if (nrow(unique(select(df,input$tttarget)))<=5) {

            a <- df %>%
                select(input$tttarget) %>%
                set_names("tttarget") %>%
                ggplot(., aes(tttarget, fill=factor(tttarget))) + geom_bar() + labs(title = input$tttarget, x=NULL) + guides(fill=F)
            ggplotly(a)

        } else {

            a <- df %>%
                select(input$tttarget) %>%
                set_names("tttarget") %>%
                ggplot(., aes(tttarget)) + geom_histogram(fill=sample(colors,size=1), alpha=.8) + labs(title = input$tttarget, x=NULL)
            ggplotly(a)

        }

    })
    
    n_n <- function(){
      
      df <- filedata()
      z <- filedata_sql()
      
      if (!is.null(df)) {
        return(sum(sapply(df, is.numeric))*100)
      } else {
        return(6*100)
      }
      
    }

    output$histograms <- renderPlot({                                                                ### 7. OUTPUT: histograms ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/hist.RDS"))
        }
        
        if (sum(sapply(df, is.numeric)) == 0) {
          
          return(readRDS("/rds/not_num.RDS"))
          
        } else {
          
          plot_histogram(
            df,
            geom_histogram_args = list("fill" = sample(colors,size=1), "alpha"=0.8),
            ggtheme = theme_plex,
            nrow = ncol(Filter(is.numeric, df)),parallel = T
          )
          
          
        }

    },height=n_n)
    

    output$boxplots <- renderPlot({                                                                ### 8. OUTPUT: boxplots ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/boxplot.RDS"))
        }
        
        if (sum(sapply(df, is.numeric)) == 0) {
          
          
          return(readRDS("/rds/not_num_gg.RDS"))
          
        } else {
          
          
          
          plot_boxplot(
              df,
              geom_boxplot_args = list("fill" = sample(colors,size=1), "alpha"=0.8),
              ggtheme = theme_plex,
              by = input$tttarget,
              nrow = ncol(Filter(is.numeric, df)),
              parallel = T
          )
       
          
        }

    },height = n_n)

    n_cat <- function(){
      
      df <- filedata()
      z <- filedata_sql()
      
      if (!is.null(df)) {
        return(sum(sapply(df, is.character))*100)
      } else {
        return(6*100)
      }
      
    }

    output$bars <- renderPlot({                                                                ### 5. OUTPUT: bars ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/barplot.RDS"))
        }

        unique_vars <- sapply(df,function(x){

            length(unique(x))

        })

        cardinality <- which(unique_vars <= 15)
        
        if (sum(sapply(df, is.character)) == 0) {
          
          return(readRDS("rds/not_cat_gg.RDS"))
          
        } else {
          

          plot_bar(
              data = df,
              binary_as_factor = TRUE,
              maxcat = 15,
              ggtheme = theme_plex,
              nrow = ncol(df[,cardinality]), parallel = T
          )
          
          
        
        }
        
    },height=n_n)


    output$bars_bi <- renderPlot({                                                                 ### 11. OUTPUT: bars_bi ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/barplot_bi.RDS"))
        }
        
        if (sum(sapply(df, is.character)) == 0) {
          
          return(readRDS("rds/not_cat_gg.RDS"))
          
        } else if (nrow(unique(select(df,input$tttarget)))<=15) {
  
          unique_vars <- sapply(df,function(x){
              length(unique(x))
          })
          cardinality <- which(unique_vars <= 15)
          df %>%
              mutate(tttarget = factor(df[,input$tttarget])) %>%
              plot_bar_all(
                  data = .,
                  group = "tttarget",
                  binary_as_factor = T,
                  maxcat = 15,
                  ggtheme = theme_plex,
                  nrow = ncol(df[,cardinality]),
                  ncol = 3
              )
          
  
        } else {
          return(readRDS("rds/not_bi_num.RDS"))
        }
          
    },height = n_cat)

    output$cardinality <- renderPlot({                                                        ### 12. OUTPUT: cardinality ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        
        
        if (is.null(df)) {
            return(readRDS("rds/cardinality.RDS"))
        }
        
        if (sum(sapply(df, is.character)) == 0) {
          
          return(readRDS("rds/not_cat_gg.RDS.RDS"))
          
        } else {
          
          x <- inspect_cat(df)
          show_plot(x)
        
        } 

    },height = n_cat)

    output$corr <- renderPlot({                                                                 ### 13. OUTPUT: corr ###

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/corr.RDS"))
        }
        
        if (sum(sapply(df, is.numeric)) == 0) {
          
          return(readRDS("/rds/not_num_gg.RDS")) 
          
        } else {

          # nzv <- nearZeroVar(df,freqCut = 90/10, uniqueCut = 10)
          # 
          # if (length(nzv)>0) {
          #   df <- df[,-nzv]
          # }

          numeric <- ncol(df)
          
          if (numeric > 20) {
            plot_correlation(
              na.omit(df[,sample(1:ncol(df),size = 20)]),
              maxcat = 5,
              ggtheme = theme_plex
            )
          } else {
            plot_correlation(
              na.omit(df),
              maxcat = 5,
              ggtheme = theme_plex
            )
          } 
        }
    })

    output$value_tttarget <- renderUI({

        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(NULL)
        }

        a <- ""
        
        l <- length(unique(df[,input$tttarget]))

        if (l<=5) {

          a <- "Classification"
        
        } else {
          a <- "Regression"
        }

        valueBox(
          value = a
          ,icon = icon("ruler")
          ,color = ifelse(a=="Classification", "green", "aqua")
          ,subtitle = "Target Problem"
          ,width = 12

        )

    })


    output$cv_model_results <- renderPlotly({                                                                 ### 18. OUTPUT: cv_model_results ###
      
        
        df <- filedata()
        
        
        if (is.null(df) | input$process_train ==0) {
          
          return(readRDS("rds/preprocess.RDS")) 
          
        }
        modelList <- validation()$modelList

        first_model <- names(modelList)[1]
        last_model <- names(modelList)[length(modelList)]

        a <- modelList %>%
            resamples() %>%
            data.frame() %>%
            pivot_longer(first_model:last_model) %>%
            ggplot(., aes(reorder(name,value), value)) +
            geom_boxplot(color="dodgerblue") +
            coord_flip() +
            labs(title = "CV Results", y=modelList$XGB$metric, x=NULL)

        ggplotly(a)

    })

    output$test_models_results <- renderPlotly({                                                                 ### 19. OUTPUT: test_models_results ###
        df <- filedata()
        
        
        if (is.null(df) | input$process_train ==0) {
          
          return(readRDS("rds/preprocess.RDS")) 
          
        }
        modelList2 <- validation()$modelList
        testing_treat <- vtreat()$testing_treat
        treatments <- vtreat()$treatments
        
        modelList <- modelList2
        
        if (is.null(modelList)) {
          
          return(readRDS("rds/preprocess.RDS")) 
          
        }

        first_model <- names(modelList)[1]
        last_model <- names(modelList)[length(modelList)]

        tttargetLevels <- length(unique(testing_treat[,"tttarget"]))

        min <- length(modelList) + 1
        max <- min * 2

        if (tttargetLevels == 2) {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$tttarget %>% factor)
            }) %>% data.frame() %>%
                pivot_longer(first_model:last_model) %>%
                mutate(Metric="Accuracy")

            metric_results[min:max,]$Metric <- "Kappa"

        } else if (tttargetLevels > 5) {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$tttarget)
            }) %>% data.frame() %>%
                pivot_longer(first_model:last_model) %>%
                mutate(Metric="RMSE")

            metric_results <- metric_results[1:min,]

        } else {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$tttarget %>% factor)
            }) %>% data.frame() %>%
                pivot_longer(first_model:last_model) %>%
                mutate(Metric="Accuracy")

            metric_results[min:max,]$Metric <- "Kappa"

        }

        b <- metric_results %>%
            ggplot(.,aes(reorder(name,value),value)) +
                geom_col(fill=sample(colors,1)) +
                facet_wrap(~Metric) +
                coord_flip() +labs(y=NULL,x=NULL)

        ggplotly(b)

    })

    show_pred <- observeEvent(input$predict,{

      test <- filetest()


      if (is.null(test)) {
        show_alert(
          title = "Upload Test First!",
          type = "warning"
        )
        return(stop)

      } else if (input$process_train >0) {
        showModal(modalDialog("Computing predictions." ,fade = TRUE, size = "l", footer = NULL))
        pred()$pred
        removeModal()
      }

    })

    pred <- eventReactive(input$predict,{

        # t t
        treatments1 <- vtreat()$treatments
        test <- vtreat()$test
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        df_treat_fs <- vtreat()$df_treat_fs
        modelList <- validation()$modelList
        ll <- validation()$ll
        training <- vtreat()$training
        testing <- vtreat()$testing
        tttargetLevels <- vtreat()$tttargetLevels



        if (is.null(treatments1) | is.null(test)) {
          return(NULL)

          }
        #showModal(modalDialog("Computing predictions." ,fade = TRUE, size = "l", footer = NULL))
        # if (input$predict > 0) {
        #     showModal(modalDialog("Predicting" ,fade = TRUE, size = "l", footer = "Less than 15 sec"))
        #     removeModal()
        # }
        #
        train_total <- rbind(
          training,
          testing
        )

        trControl <- trainControl(
          method = "cv",
          savePredictions = "final",
          search = "random",
          index = createFolds(train_total$tttarget, k =3),
          allowParallel = TRUE,
          verboseIter = FALSE
        )

        # trControl <- trainControl(
        #   method = "cv",
        #   savePredictions = "final",
        #   search = "random",
        #   index = createFolds(df_treat_fs$tttarget, k =3),
        #   allowParallel = TRUE,
        #   verboseIter = FALSE
        # )


        if (tttargetLevels > 5) {

          list <- names(select(train_total,-tttarget)) # training
          set.seed(123)
          treatments <- vtreat::mkCrossFrameNExperiment(
            train_total,
            varlist = list,
            outcomename = "tttarget",
            collarProb = .03, doCollar = input$outliers,
            parallelCluster = parallelCluster
          )
          df_treat <- treatments$crossFrame
          df_treat_fs <- select(df_treat,-tttarget)[,treatments$treatments$scoreFrame$recommended]
          df_treat_fs$tttarget <- df_treat[,"tttarget"]

        } else if (tttargetLevels <= 5) {

          list <- names(select(train_total,-tttarget))
          set.seed(123)
          treatments <- vtreat::mkCrossFrameCExperiment(
            train_total,
            varlist = list,
            outcomename = "tttarget",
            outcometarget = train_total[,"tttarget"][[2]],
            collarProb = .03, doCollar = input$outliers
          )
          df_treat <- treatments$crossFrame
          df_treat_fs <- select(df_treat,-tttarget)[,treatments$treatments$scoreFrame$recommended]
          df_treat_fs$tttarget <- df_treat[,"tttarget"]

        }

        ll <- list(
          XGB    = caretModelSpec(method = "xgbTree"),
          Lasso = caretModelSpec(method = "glmnet"),
          Ridge = caretModelSpec(method = "glmnet"),
          ElasticNet = caretModelSpec(method = "glmnet"),
          LinearModel = caretModelSpec(method = "glmnet", tuneGrid = expand.grid(
            alpha = 0,
            lambda = 0
          )),
          RandomForest = caretModelSpec(method = "ranger"),
          #Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial"),
          KNN   = caretModelSpec(method = "knn",  preProcess = c("center","scale"))
        )

        m <- sapply(ll %>% names, function(x){
          x != input$select_model
        })

        ll[m] <- NULL

        test_treated <- vtreat::prepare(treatments$treatments, test, doCollar = input$outliers)

        i=1
        
      

        # place where model predictions are stored
        solution.table <- data.frame(id = 1:nrow(test_treated))

        if (input$tune_lenght == TRUE) {

          set.seed(123)
          modelList <- caretList(
            tttarget ~ ., data = df_treat_fs,
            trControl = trControl,
            tuneList = ll,
            tuneLength = 30
          )
          
          if (modelList[[1]]$method %in% c("ranger", "xgbTree")) {
  
            for (i in 1:9){
    
              set.seed(i)
              model <- train(
                tttarget ~ .,
                data = df_treat_fs,
                trControl = trainControl(method="none"),
                tuneGrid = modelList[[1]]$bestTune,
                method = modelList[[1]]$method
              )
    
              solution.table[,i] <- predict(model, test_treated)
              
    
            }
    
    
    
            if (tttargetLevels > 5) {
    
              solution.table$mean <- rowMeans(solution.table[,-1])
    
              pred <- solution.table$mean
    
            } else {
    
              solution.table.count<-apply(solution.table, MARGIN=1, table)
    
              # Create vector where my solution will be stored
              predict.combined<-vector()
    
              # Identify category with more frequency (votes) per row.
              for (x in 1:nrow(test_treated)) {
                predict.combined[x]<-names(which.max(solution.table.count[[x]]))
              }
    
              pred <- predict.combined
    
            } 
            
          } else {
              
              set.seed(i)
              model <- train(
                tttarget ~ .,
                data = df_treat_fs,
                trControl = trainControl(method="none"),
                tuneGrid = modelList[[1]]$bestTune,
                method = modelList[[1]]$method
              )
              
              pred <- predict(model, test_treated)
              
          }
          
        } else {
          
          set.seed(i)
          model <- train(
            tttarget ~ .,
            data = df_treat_fs,
            trControl = trainControl(method="none"),
            method = modelList[[1]]$method
          )
          
          pred <- predict(model, test_treated)
          
        }

        #removeModal()
        return(list(pred=pred))

    })

    

    output$download_predict <- downloadHandler(
        filename = function() {
            paste("prediction", "csv", sep=".")
        },
        content = function(files) {
            write.csv(pred()$pred, files, row.names = F)
        }
    )
    
    ### Para hacer solo un ranger ### +50% speed
    
    
    rf_var_imp <- eventReactive(input$tabs,{
      if (input$tabs=="fi") {
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if(is.null(df)) {
          return(NULL)
        }
        
        nzv <- nearZeroVar(df,freqCut = 95/5, uniqueCut = 5)
        
        if (length(nzv)>0) {
          df <- df[,-nzv]
        }
        
        df[, "tttarget"] <- df[,input$tttarget]
        df[,input$tttarget] <- NULL
        
        df <- recipe(tttarget~., data = df) %>% 
          step_integer(all_nominal()) %>%
          recipes::prep() %>% 
          juice()
        
        importance_measure <- "impurity"
        
        split_rule <- "gini"
        
        if (length(unique(df$tttarget))>5) {
          importance_measure <- "permutation"
          split_rule <- "variance"
        }
        
        names(df) <- make.names(names(df))
        
        
        isna <- sapply(df, function(x, df) {
          sum(is.na(x))/nrow(df)
        },df)
        
        l_na <- which(isna>.3) %>% length()
        
        if (l_na>0) {
          
          remove <- which(isna>.3) %>% names()
          
          df <- select(df, -all_of(remove))
          
        }
        
        rf_var <- ranger(
          tttarget~.
          ,data = na.omit(df)
          ,num.trees = 50
          ,importance = importance_measure
        )

      }
      return(list(
        rf = rf_var,
        df = df)
      )
      
    })

    output$ranger_importance <- renderPlotly({
      
      rf_var_imp2 <- rf_var_imp()$rf
      
      if (is.null(rf_var_imp2)) {

        return(readRDS("rds/rfi.RDS"))

      }
      
   
      imp <- as.data.frame(ranger::importance(rf_var_imp2))
      imp$Variable <- rownames(imp)
      names(imp)[1] <- c("Importance")
      rownames(imp) <- NULL

      g <- ggplot(imp, aes(reorder(Variable, Importance), Importance, fill=Importance)) +
        geom_col() +
        coord_flip() +
        labs(x=NULL)

      ggplotly(g)

    })

    output$ranger_outliers <- renderPlot({

      rf_var_imp2 <- rf_var_imp()$rf
      df <- rf_var_imp()$df
      
      
      if(is.null(rf_var_imp2)) {
        return(readRDS("rds/outliers_gg.RDS"))
      }
      
      
      tmp1 <-  na.omit(df) %>%
        mutate(
          Predicted = predict(rf_var_imp2, na.omit(df))[[1]],
          Residual = tttarget - Predicted,
          out = abs(Residual) > quantile(abs(Residual), .9975,na.rm = T),
          Id = 1:nrow(na.omit(df))
        ) %>%
        select(Id, tttarget, Predicted, Residual, out)

      p1 <- ggplot(tmp1, aes(x = Predicted, y = Residual, col = out, label = Id)) +
        geom_point(size = 2, alpha = .7, show.legend = FALSE) +
        # geom_rug(sides = "r", outside = TRUE) +
        # coord_cartesian(clip = "off") +
        scale_color_viridis_d(end = .7, direction = -1) +
        ggrepel::geom_label_repel(
          data = filter(tmp1, out == 1), show.legend = FALSE)

      p2 <- ggplot(tmp1, aes(x = Residual)) +
        geom_histogram() +
        theme_void() +
        coord_flip()


      p1 + p2 + plot_layout(nrow = 1, widths = c(3, 1))


    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
