source("./helpers.R")

# library(doParallel)
# doParallel::registerDoParallel()
#doParallel::stopImplicitCluster()


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
        menuItem(
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
            text = "ML"
            ,tabName = "ml"
            ,icon = icon("rocket")
        )
        ,menuItem(
          text = "Feature Importance"
          ,tabName = "fi"
          ,icon = icon("star")
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
                width = 4
                ,boxPlus(
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
                      color = "black",
                      "Observe variables' distribution, correlations, missing values for a better data understaning."
                    ),
                    timelineLabel("Fourth", color = "green"),
                    timelineItem(
                      title = "Preprocess & Model & Predict",
                      icon = "rocket",
                      color = "yellow",
                      "Impute missing values, transform variables, create new bayesian variables and perform feature selection.
                      Compare different algorithms using cross validation: Shrinkage methods, support vector machines, boosting, ensemble learning...
                      Select the best model & tune it up. If you have test data, make predictions, otherwise, save the model.
                      "
                    ),
                    timelineLabel("Fifth", color = "purple"),
                    timelineItem(
                      title = "Observe Outliers & Feature Importance",
                      icon = "star",
                      color = "navy",
                      "Observes the variables that reduced the error the most in a random forest and the atypical observations."
                    ),
                    timelineStart(color = "gray")
                  )
                )
              )
              ,column(
                width = 4
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
                  progressBar(id="p1", value = 1, striped = T, title = "Not Uploaded", status = "danger"),
                  br(),
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
                  progressBar(id="p3", value = 1, striped = T, title = "Not Uploaded", status = "danger"),
                  br(),
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
                      ,textInputAddon( 
                        inputId = "pw_sql",
                        label = "Password",
                        addon = icon("key"),
                        placeholder = "******"
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
                width = 4
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
                  progressBar(id="p2", value = 1, striped = T, title = "Not Uploaded", status = "danger"),
                  br(),
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
                  main_img = "https://image.flaticon.com/icons/svg/1051/1051275.svg",
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
                  progressBar(id="p4", value = 1, striped = T, title = "Not Uploaded", status = "danger"),
                  br(),
                  back_content = tagList(
                    column(
                      width = 12,
                      align = "center")
                      
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
                    ,label_text = "Change Variable Class"
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
                            inputId = "target"                                       ### 3. INPUT: target ###
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
                    ,plotOutput(
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
                        outputId = "target_plotly"                                 ### 6. OUTPUT: taRget_plotly ###
                    ) %>% withSpinner()
                )
            )
            
            
            ,fluidRow(
                tabBox(
                    title = "Numerical"
                    ,width = 6
                    ,tabPanel(
                        title = "Histograms"
                        ,jqui_resizable(
                            plotOutput(
                                outputId = "histograms"                             ### 7. OUTPUT: histograms ###
                                ,height = 600
                            ) 
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        "Bivariate Box-Plots"
                        ,jqui_resizable(
                            plotOutput(
                                outputId = "boxplots"                               ### 8. OUTPUT: boxplots ###
                                ,height = 600
                            )
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
                        ,jqui_resizable(
                            plotOutput(
                                outputId = "bars"                                  ### 10. OUTPUT: bars ###
                                ,height = 600
                            ) 
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        "Bar-Bivariate"
                        ,jqui_resizable(
                            plotOutput(
                                outputId = "bars_bi"                               #### 11. OUTPUT: bars_bi ###
                                ,height = 600
                            ) 
                        ) %>% withSpinner()
                    )
                    ,tabPanel(
                        "Cardinality"
                        ,jqui_resizable(
                            plotOutput(
                                outputId = "cardinality"                           ### 12. OUTPUT: cardinality ###
                                ,height = 600
                            )
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
                    ,jqui_resizable(
                      plotOutput(
                        outputId = "corr"                               #### 13. OUTPUT: corr ###
                        ,height = 1200
                    )) %>% withSpinner()
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
                            outputId = "value_target"
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
                                "LinearModel", "RandomForest", "Tree",
                                "SVM","KNN"
                              )
                              ,multiple = T
                              ,selected = c("XGB","ElasticNet")
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
                                      "LinearModel", "RandomForest", "Tree",
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
                        ,plotlyOutput(
                            outputId = "cv_model_results"                                              ### 20. OUTPUT: cv_model_results ###
                        )
                    )
                    ,boxPlus(
                        title = "TEST RESULTS"
                        ,width = 12
                        ,solidHeader = T
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
                title = "Feaure Importance"
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
    header
    ,sidebar
    ,body
    ,skin = "black"
)


##############
### SERVER ###
##############

server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=30*1024^2) # 35 MB
    
    # Conventional data #
    filedata <- reactive({
      
        infile <- input$file
        infile2 <- input$train_g
        # User has not uploaded a file yet
        if (is.null(infile) & infile2==""){
            return(NULL)
        }
        
        if (!is.null(infile)) {
          
          if (grepl(".csv|.txt", infile$datapath) == TRUE) {
            
            return(fread(infile$datapath, integer64 = "numeric",data.table = F)) # datapath es lo que devuelve input$file
            
          } else if (grepl(".xlsx|.xls", infile$datapath) == TRUE) {
            
            return(read_excel(infile$datapath))
            
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
            return(read_sheet(infile2,na = c("", " ", "NA", "na", "null", "NULL", "Na"),))
            
          } else {
            
            show_alert(
              title = "Must be a spreadsheet",
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
    
    triger_sql <- observeEvent(input$run_sql_tr,{
      
      filedata_sql()
      
    })
    
    filetest <- reactive({
      
        infile <- input$file_test
        infile2 <- input$test_g
        # User has not uploaded a file yet
        if (is.null(infile) & infile2==""){
          return(NULL)
        }
        
        if (!is.null(infile)) {
          
          if (grepl(".csv|.txt", infile$datapath) == TRUE) {
            
            return(fread(infile$datapath, integer64 = "numeric",data.table = F)) # datapath es lo que devuelve input$file
            
          } else if (grepl(".xlsx|.xls", infile$datapath) == TRUE) {
            
            return(read_excel(infile$datapath))
            
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
            return(read_sheet(infile2,na = c("", " ", "NA", "na", "null", "NULL", "Na")))
            
          } else {
            
            show_alert(
              title = "Must be a spreadsheet",
              type = "warning"
            )
            
            return(NULL)
            
          }
          
        }
        
    })
    
    # TARGET + SELECTING OPTIMAL TARGETS
    observe({
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)){
            return(NULL)
        }
        
        unique_char <- sapply(Filter(is.character, df),function(x){ # Not advisable to plot high cardinality variables
            length(unique(x))>=10
        })
        char_exclude <- names(which(unique_char==TRUE))
        id <- grepl("id|ID|Id|iD",names(df)) # It does not make sense to add ID as target
        if (sum(id) > 0) {
            char_exclude <- c(char_exclude, names(df)[id])
        }
        
        char_exclude <- c(char_exclude, names(df)[sapply(df, function(x){sum(is.na(x))})>0]) # Variables with NA cannot be targets
        
            
        var = select(df,-all_of(char_exclude)) %>% names()
        updateSelectInput(session,"target", choices =  var) ### 3. INPUT: target ###
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
      updateProgressBar(session, id="p1", value = 100, title = "Loaded", status = "success")
      updateProgressBar(session, id="p2", value = 100, title = "Loaded", status = "success")
      updateProgressBar(session, id="p3", value = 100, title = "Loaded", status = "success")
      updateProgressBar(session, id="p4", value = 100, title = "Loaded", status = "success")
      
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
        
        
        df <- lapply(df,function(x){
            
            ifelse(x %in% c("","NA","N/A","Unknown","unknown","missing", "Missing", "?", "na", " ", "n/a"), NA, x)
            
        }) 
        
        df <- df %>% as.data.frame()
        
        test <- lapply(test,function(x){

          ifelse(x %in% c("","NA","N/A","Unknown","unknown","missing", "Missing", "?", "na", " ", "n/a"), NA, x)

        }) 
        
        test <- test %>% as.data.frame()
        
        df$target <- df[,input$target]
        df[,input$target] <- NULL
        
        targetLevels <- length(unique(df[,"target"]))
        
        # r <- recipe(target~., data = df) %>% 
        #   step_YeoJohnson(all_numeric(),-all_outcomes()) 
        # 
        # df <- r %>% prep %>% juice() %>% as.data.frame()
        # 
        # test <- r %>% prep %>% bake(test) %>% as.data.frame()
        
        
        if (targetLevels > 10) {
            
            set.seed(123)
            inTrain <- createDataPartition(df$target, p = input$split, list = FALSE)
            training <- df[ inTrain,]
            testing <- df[-inTrain,]
            
            if (input$missranger == TRUE) {
              
              l <- missVtreatRanger(training,testing,test,"target")
              training <- l[[1]]
              testing <- l[[2]]
              test <- l[[3]]
              
            }
            
            list <- names(select(training,-target)) # training
            set.seed(123)
            treatments <- vtreat::mkCrossFrameNExperiment(
                training,
                varlist = list,
                outcomename = "target",
                collarProb = .03, doCollar = input$outliers
            ) %>% withProgress()
            df_treat <- treatments$crossFrame
            df_treat_fs <- select(df_treat,-target)[,treatments$treatments$scoreFrame$recommended]
            df_treat_fs$Target <- df_treat[,"target"]
            
        } else if (targetLevels <= 10) {
            
            df$target <- df$target %>% factor()
            set.seed(123)
            inTrain <- createDataPartition(df$target, p = input$split, list = FALSE)
            training <- df[ inTrain,]
            testing <- df[-inTrain,]
            
            if (input$missranger == TRUE) {
              
              l <- missVtreatRanger(training, testing, test,"target")
              training <- l[[1]]
              testing <- l[[2]]
              test <- l[[3]]
              
            }
            
            list <- names(select(df,-target)) 
            set.seed(123)
            treatments <- vtreat::mkCrossFrameCExperiment(
                training,
                varlist = list,
                outcomename = "target",
                outcometarget = df[,"target"][[2]],
                collarProb = .03, doCollar = input$outliers
            )%>% withProgress()
            df_treat <- treatments$crossFrame
            df_treat_fs <- select(df_treat,-target)[,treatments$treatments$scoreFrame$recommended]
            df_treat_fs$Target <- df_treat[,"target"]
            
        }
        
        testing_treat <- vtreat::prepare(treatments$treatments, testing, doCollar = input$outliers)
        
        return(list(
            treatments = treatments,
            test = test,
            df_treat_fs = df_treat_fs,
            testing_treat = testing_treat,
            targetLevels = targetLevels,
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
            vtreat()$targetLevels
            vtreat()$testing_treat
            removeModal()
            show_alert(
                title = "Success",
                text = "All in order",
                type = "success"
            )
        }
    })
  
    validation <- eventReactive(input$process_train,{

        df_treat_fs <- vtreat()$df_treat_fs
        targetLevels <- vtreat()$targetLevels
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
        
        set.seed(123)
        trControl <- trainControl(
            method = "cv",
            savePredictions = "final",
            index = createFolds(df_treat_fs$Target, k =input$cv),
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
        if (targetLevels<=10) {
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
          Lasso = caretModelSpec(method = "glmnet", tuneGrid = lassoGrid),
          Ridge = caretModelSpec(method = "glmnet", tuneGrid = ridgeGrid, preProcess = c("YeoJohnson")),
          ElasticNet = caretModelSpec(method = "glmnet", tuneGrid = elasticNetGrid),
          LinearModel = caretModelSpec(method = "glmnet", tuneGrid = linearGrid),
          RandomForest = caretModelSpec(method = "ranger", tuneGrid = rfGrid),
          Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid, preProcess = c("center","scale")),
          KNN   = caretModelSpec(method = "knn", tuneGrid = knnGrid,  preProcess = c("center","scale"))
        )%>% withProgress()
        

        l_m <- list(
          XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
          RandomForest     = caretModelSpec(method = "ranger", tuneGrid = rfGrid, num.trees=150),
          Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid, preProcess = c("center","scale"))
        )%>% withProgress()

      
        if (targetLevels == 2 | targetLevels > 10) {
            
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
                Target ~ ., data = df_treat_fs,
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
                Target ~ ., data = df_treat_fs,
                trControl = trControl,
                tuneList = l_m
            )
        }
        
        show_alert(
            title = "Success",
            text = "All in order",
            type = "success"
        )
   
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
            skim(Filter(is.numeric, df))[,c(-3,-8,-10,-12)] 
        else if (input$variable_class=="Character")
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
    
    output$report <- downloadHandler(dd <- filedata(), function(theFile){                            ### 4. Output: report ###
        
        showModal(modalDialog("Creating Report" ,fade = TRUE, size = "l", footer = "1-3 mins"))
        create_report(dd)
        removeModal()
        
    })
    
    output$na <- renderPlot({                                                                         ### 5. Output: na ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/na.RDS"))
        }
        
        na <- lapply(df,function(x){
            
            ifelse(x %in% c("","NA","N/A","Unknown","unknown","missing", "Missing", "?", "na", " "), NA, x)
            
        })
        
        na <- na %>% as.data.frame()
        
        plot_missing(
            na, 
            ggtheme = theme_plex, 
            missing_only = TRUE
        )
        
    }) 
    
    output$target_plotly <- renderPlotly({                                                      ### 6. target_plotly
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) { 
            return(readRDS("rds/target_plotly.RDS"))
        }
        
        if (nrow(unique(select(df,input$target)))<=10) {
            
            a <- df %>% 
                select(input$target) %>% 
                set_names("Target") %>% 
                ggplot(., aes(Target, fill=factor(Target))) + geom_bar() + labs(title = input$target) + guides(fill=F)
            ggplotly(a)
                
        } else {
            
            a <- df %>% 
                select(input$target) %>% 
                set_names("Target") %>% 
                ggplot(., aes(Target)) + geom_histogram(fill=sample(colors,size=1), alpha=.8) + labs(title = input$target)
            ggplotly(a)
            
        }
        
    })
    
    output$histograms <- renderPlot({                                                                ### 7. OUTPUT: histograms ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/hist.RDS"))
        }
        
        plot_histogram(
            df, 
            geom_histogram_args = list("fill" = sample(colors,size=1), "alpha"=0.8),
            ggtheme = theme_plex,
            nrow = ncol(Filter(is.numeric, df))
        ) 
        
    })
    
    output$boxplots <- renderPlot({                                                                ### 8. OUTPUT: boxplots ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/boxplot.RDS"))
        }
            
        plot_boxplot(
            df, 
            geom_boxplot_args = list("fill" = sample(colors,size=1), "alpha"=0.8),
            ggtheme = theme_plex,
            by = input$target,
            nrow = ncol(Filter(is.numeric, df))
            
        ) 
        
    })
    
    # output$scatterplots <- renderPlot({                                                                ### 9. OUTPUT: Scatterplots ###
    #     
    #     df <- filedata()
    #     if (is.null(df)) {
    #         return(NULL)
    #     }
    #     
    #     plot_scatterplot(
    #         df, 
    #         geom_point_args = list("color" = sample(colors,size=1), "alpha"=0.8),
    #         # sampled_rows = 10000,
    #         #ggtheme = theme_plex,
    #         title = "Bivariate Scatterplot",
    #         by = input$target,
    #         nrow = ncol(Filter(is.numeric, df))
    #         
    #     ) 
    #     
    # })
    
    output$bars <- renderPlot({                                                                ### 10. OUTPUT: bars ###
        
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
        
        plot_bar(
            data = df,
            binary_as_factor = TRUE,
            maxcat = 15,
            ggtheme = theme_plex,
            nrow = ncol(df[,cardinality])
        )
        
        
    })
    
    
    output$bars_bi <- renderPlot({                                                                 ### 11. OUTPUT: bars_bi ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/barplot_bi.RDS"))
        }
        
        if (nrow(unique(select(df,input$target)))<=15) {
            
            unique_vars <- sapply(df,function(x){
                
                length(unique(x))
                
            })
            
            cardinality <- which(unique_vars <= 15)
            
            df %>% 
                mutate(Target = factor(df[,input$target])) %>% 
                plot_bar_all(
                    data = .,
                    group = "Target",
                    binary_as_factor = T,
                    maxcat = 15,
                    ggtheme = theme_plex,
                    nrow = ncol(df[,cardinality]),
                    ncol = 3
                )
            
        } else {
            ggplot(df) + labs(title="Target must be Categorical")
        }
    })
    
    output$cardinality <- renderPlot({                                                        ### 12. OUTPUT: cardinality ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/cardinality.RDS"))
        }
        
        x <- inspect_cat(df)
        show_plot(x)
        
    })
    
    output$corr <- renderPlot({                                                                 ### 13. OUTPUT: corr ###
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(readRDS("rds/corr.RDS"))
        }
        
        numeric <- ncol(df) 
        
        if (numeric > 40) {
          plot_correlation(
            na.omit(df[,sample(1:ncol(df),size = 40)]),
            maxcat = 10,
            ggtheme = theme_plex
          ) 
        } else {
          plot_correlation(
            na.omit(df),
            maxcat = 10,
            ggtheme = theme_plex
          ) 
        }
      
        
        
    })
    
    output$value_target <- renderUI({
        
        df <- filedata()
        z <- filedata_sql()
        if (!is.null(z)) {
          df <- z
        }
        if (is.null(df)) {
            return(NULL)
        }
        
        a <- ""
        
        if (length(unique(df[,input$target]))<=5) {
            
            a <- "Classification"
            
        } else {a <- "Regression"}
        
        info_card(
            title="Target Problem"
            ,value = a
            ,sub_value = NULL
            ,sub_icon = NULL
            ,bg_color  = ifelse(a=="Classification", "info", "warning")
            ,sub_text_color = NULL
            ,main_icon = "ruler"
        )
        
    })

    
    output$cv_model_results <- renderPlotly({                                                                 ### 18. OUTPUT: cv_model_results ###
        
          
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

        modelList2 <- validation()$modelList
        testing_treat <- vtreat()$testing_treat
        treatments <- vtreat()$treatments
        
        modelList <- modelList2
        
        first_model <- names(modelList)[1]
        last_model <- names(modelList)[length(modelList)]

        targetLevels <- length(unique(testing_treat[,"target"]))
        
        min <- length(input$pick_model) + 1
        max <- min * 2

        if (targetLevels == 2) {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$target %>% factor)
            }) %>% data.frame() %>%
                pivot_longer(first_model:last_model) %>%
                mutate(Metric="Accuracy")
           
            metric_results[min:max,]$Metric <- "Kappa"

        } else if (targetLevels > 10) {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$target)
            }) %>% data.frame() %>%
                pivot_longer(first_model:last_model) %>%
                mutate(Metric="RMSE")

            metric_results <- metric_results[1:min,]

        } else {

            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing_treat)
                results <- postResample(y_pred, testing_treat$target %>% factor)
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
    
    show_pred <- observe({

      test <- filetest()
      

      if (input$predict>0 & is.null(test)) {
        show_alert(
          title = "Upload Test First!",
          type = "warning"
        )
        return(stop)

      } else if (input$predict>0 & input$process_train >0) {
        showModal(modalDialog("Computing predictions." ,fade = TRUE, size = "l", footer = NULL))
        pred()$pred
        Sys.sleep(2)
        removeModal()
      }

    })
    
    pred <- eventReactive(input$predict,{
    
        # t t
        treatments <- vtreat()$treatments
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
        targetLevels <- vtreat()$targetLevels
        
    
        
        if (is.null(treatments) | is.null(test)) {
          return(NULL)
        
          }
        
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
          index = createFolds(train_total$target, k =3),
          allowParallel = TRUE,
          verboseIter = FALSE
        )
        
        # trControl <- trainControl(
        #   method = "cv",
        #   savePredictions = "final",
        #   search = "random",
        #   index = createFolds(df_treat_fs$Target, k =3),
        #   allowParallel = TRUE,
        #   verboseIter = FALSE
        # )
        
        
        if (targetLevels > 10) {

          list <- names(select(train_total,-target)) # training
          set.seed(123)
          treatments <- vtreat::mkCrossFrameNExperiment(
            train_total,
            varlist = list,
            outcomename = "target",
            collarProb = .03, doCollar = input$outliers
          )
          df_treat <- treatments$crossFrame
          df_treat_fs <- select(df_treat,-target)[,treatments$treatments$scoreFrame$recommended]
          df_treat_fs$Target <- df_treat[,"target"]

        } else if (targetLevels <= 10) {

          list <- names(select(train_total,-target))
          set.seed(123)
          treatments <- vtreat::mkCrossFrameCExperiment(
            train_total,
            varlist = list,
            outcomename = "target",
            outcometarget = train_total[,"target"][[2]],
            collarProb = .03, doCollar = input$outliers
          )
          df_treat <- treatments$crossFrame
          df_treat_fs <- select(df_treat,-target)[,treatments$treatments$scoreFrame$recommended]
          df_treat_fs$Target <- df_treat[,"target"]

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
          Tree = caretModelSpec(method = "rpart"),
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
        
     
         
        set.seed(123)
        modelList <- caretList(
          Target ~ ., data = df_treat_fs,
          trControl = trControl,
          tuneList = ll,
          tuneLength = 30
        )
 
        for (i in 1:9){
          
          set.seed(i)
          model <- train(
            Target ~ ., 
            data = df_treat_fs,
            trControl = trainControl(method="none"),
            tuneGrid = modelList[[1]]$bestTune,
            method = modelList[[1]]$method
          )
   
          solution.table[,i] <- predict(model, test_treated)

          
        }
        
    
    
        if (targetLevels > 10) {
          
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
            
            # pred <- predict(modelList[input$select_model], test_treated)
          
            
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
    
    output$ranger_importance <- renderPlotly({
      
      df <- filedata()
      z <- filedata_sql()
      if (!is.null(z)) {
        df <- z
      }
      if(is.null(df)) {
        return(NULL)
      }
      
      df[, "Target"] <- df[,input$target]
      df[,input$target] <- NULL
      
      importance_measure <- "impurity"
      
      split_rule <- "gini"
      
      if (length(unique(df$Target))>10) {
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
      
      rf_var_imp <- ranger(
        Target~.
        ,data = na.omit(df)
        ,num.trees = 50
        ,importance = importance_measure
      )
      
      imp <- as.data.frame(ranger::importance(rf_var_imp))
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
      
      df <- filedata()
      z <- filedata_sql()
      if (!is.null(z)) {
        df <- z
      }
      if(is.null(df)) {
        return(NULL)
      }
      
      df[, "Target"] <- df[,input$target]
      df[,input$target] <- NULL
      
      importance_measure <- "impurity"
      
      split_rule <- "gini"
      
      if (length(unique(df$Target))>10) {
        importance_measure <- "permutation"
        split_rule <- "variance"
      }
      
      names(df) <- make.names(names(df))
      
      
      isna <- sapply(df, function(x, df) {
        sum(is.na(x))/nrow(df)
      },df)
      
      l_na <- which(isna>.2) %>% length()
      
      if (l_na>0) {
        
        remove <- which(isna>.2) %>% names()
        
        df <- select(df, -all_of(remove))
        
      }
      
      rf_var_imp <- ranger(
        Target~.
        ,data = na.omit(df)
        ,num.trees = 50
        ,importance = importance_measure
      )
      
      tmp1 <-  na.omit(df) %>% 
        mutate(
          Predicted = predict(rf_var_imp, na.omit(df))[[1]],
          Residual = Target - Predicted,
          out = abs(Residual) > quantile(abs(Residual), .9975),
          Id = 1:nrow(na.omit(df))  
        ) %>% 
        select(Id, Target, Predicted, Residual, out)
  
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
