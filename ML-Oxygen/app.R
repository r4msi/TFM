source("./helpers.R")

library(doParallel)
doParallel::registerDoParallel()


##############
### HEADER ###
##############


header <- dashboardHeaderPlus(
    
    title = tagList(
        span(class = "logo-lg", "ML Oxygen 0.1.0")
        ,img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")
    )
    ,enable_rightsidebar = T
    
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
            text = "EDA"
            ,tabName = "eda"
            ,icon = icon("dashboard")
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
            tags$head(tags$style(HTML("
                                #final_text {
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                }
                                "))),
            
            tabName = "data"
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
                box(
                    title = "Load Data"
                    ,status = "success"
                    ,solidHeader = T
                    ,width = 2
                    ,fileInput(
                        inputId = "file"                                                ### 1. INPUT: file ###
                        ,"Train"
                    )
                    ,fileInput(
                      inputId = "file_test"                                                ### 17. INPUT: file_test ###
                      ,"Test"
                    )
                )
                ,boxPlus(
                    title = ""
                    ,status = "success"
                    ,solidHeader = F
                    ,width = 10
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
    
    # Para poder cambiar de data hay que hacerlo reactivo. ### 1. INPUT: file ###
    filedata <- reactive({
        infile <- input$file
        # User has not uploaded a file yet
        if (is.null(infile)){
            return(NULL)
        }
        fread(infile$datapath, integer64 = "numeric",data.table = F) # datapath es lo que devuelve input$file
    })
    
    filetest <- reactive({
        infile <- input$file_test
        # User has not uploaded a file yet
        if (is.null(infile)){
            return(NULL)
        }
        fread(infile$datapath, integer64 = "numeric",data.table = F) 
    })
    
    # TARGET + SELECTING OPTIMAL TARGETS
    observe({
        df <- filedata()
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
      updateSelectInput(session,"select_model", choices =  input$pick_model) 
    })
    
    vtreat <- eventReactive(input$vtreat,{
        
        df <- filedata()
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
        
        r <- recipe(target~., data = df) %>% 
          step_YeoJohnson(all_numeric(),-all_outcomes()) 
        
        df <- r %>% prep %>% juice() %>% as.data.frame()
        
        test <- r %>% prep %>% bake(test) %>% as.data.frame()
        
        
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
            )
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
            )
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
    
    # observe_validation <- observeEvent(input$process_train,{                           ### Warning if data it's not upload ###
    #     
    #     df <- filedata()
    #     t <- vtreat()$treatments
    #     
    #     if (is.null(df) & input$process_train >0) {
    #         
    #         return(
    #             show_alert(
    #                 title = "Upload Data First!",
    #                 type = "warning"
    #             )
    #         )
    #     } else if (!is.null(df) & is.null(t) & input$process_train >0 ){
    #         
    #         return(
    #             show_alert(
    #                 title = "Preprocess Data First!",
    #                 type = "warning"
    #             )
    #         )
    #         
    #     }
    #     
    # })
    
    warning <- observe({
      
      treatments <- vtreat()$treatments
      
      if (is.null(treatments)&input$process_train>0) {
        
        return(
          show_alert(
            title = "Preprocess data first!",
            type = "warning"
          )
        )
      }
      
    })
    
    
    validation <- eventReactive(input$process_train,{
      
        df <- filedata()
        if (is.null(df)) {
            return(NULL)
        }
        
        df_treat_fs <- vtreat()$df_treat_fs
        targetLevels <- vtreat()$targetLevels
        
        showModal(modalDialog("Fitting Models" ,fade = TRUE, size = "l", footer = "1-30 mins"))
        
        set.seed(123)
        trControl <- trainControl(
            method = "cv",
            savePredictions = "final",
            index = createFolds(df_treat_fs$Target, k =input$cv),
            allowParallel = TRUE,
            verboseIter = FALSE
        )

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
          XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
          Lasso = caretModelSpec(method = "glmnet", tuneGrid = lassoGrid),
          Ridge = caretModelSpec(method = "glmnet", tuneGrid = ridgeGrid),
          ElasticNet = caretModelSpec(method = "glmnet", tuneGrid = elasticNetGrid),
          LinearModel = caretModelSpec(method = "glmnet", tuneGrid = linearGrid),
          RandomForest = caretModelSpec(method = "ranger", tuneGrid = rfGrid),
          Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid),
          KNN   = caretModelSpec(method = "knn", tuneGrid = knnGrid,  preProcess = c("center","scale"))
        )
        

        l_m <- list(
          XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
          RandomForest     = caretModelSpec(method = "ranger", tuneGrid = rfGrid, num.trees=150),
          Tree = caretModelSpec(method = "rpart"),
          SVM = caretModelSpec(method = "svmRadial", tune_grid = svmGrid, preProcess = c("center","scale"))
        )

      
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
            modelList <- caretList(
                Target ~ ., data = df_treat_fs,
                trControl = trControl,
                tuneList = l
            )
        
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
        
        removeModal()
        
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
        
        valueBox(
            value = if (is.null(df)) {h4("Nº Rows")} else {nrow(df)}
            ,subtitle = "Rows"
            ,icon = icon("server")
            ,color = "blue"
        )
        
    })
    
    output$columns <- renderValueBox({
        
        df <- filedata()
        
        valueBox(
            value = if (is.null(df)) {h4("Nº Columns")} else {ncol(df)}
            ,subtitle = "Columns"
            ,icon = icon("columns")
            ,color = "green"
        )
        
    })
    
    output$missing <- renderValueBox({
        
        df <- filedata()
        
        valueBox(
            value = if (is.null(df)) {h4("Nº NA")} else {sum(is.na(df))}
            ,subtitle = "Missing Values"
            ,icon = icon("times-circle")
            ,color = "red"
        )
        
    })
    
    output$numeric <- renderValueBox({
        
        df <- filedata()
        
        valueBox(
            value = if (is.null(df)) {h4("Nº Num")} else {ncol(Filter(is.numeric,df))}
            ,subtitle = "Numeric"
            ,icon = icon("calculator")
            ,color = "orange"
        )
        
    })
    
    output$character <- renderValueBox({
        
        df <- filedata()
        
        valueBox(
            value = if (is.null(df)) {h4("Nº Char")} else {ncol(Filter(is.character,df))} 
            ,subtitle = "Character"
            ,icon = icon("at")
            ,color = "teal"
        )
        
    })
    
    output$dates <- renderValueBox({
        
        df <- filedata()
        
        valueBox(
            value = if (is.null(df)) {h4("Nº Dates")} else {ncol(Filter(is.Date,df))}
            ,subtitle = "Dates"
            ,icon = icon("calendar")
            ,color = "fuchsia"
        )
        
    })
    
    output$input_file <- renderTable({                                                          ### 2. INPUT: variable_class ###
        
        df <- filedata()
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
        if (is.null(df)) {
            return(readRDS("rds/cardinality.RDS"))
        }
        
        x <- inspect_cat(df)
        show_plot(x)
        
    })
    
    output$corr <- renderPlot({                                                                 ### 13. OUTPUT: corr ###
        
        df <- filedata()
        if (is.null(df)) {
            return(readRDS("rds/corr.RDS"))
        }
        
        plot_correlation(
            na.omit(df),
            maxcat = 10,
            ggtheme = theme_plex
        ) 
        
        
    })
    
    output$value_target <- renderUI({
        
        df <- filedata()
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
        
        return(
          show_alert(
            title = "Upload Test First!",
            type = "warning"
          )
        )
        
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
        df_treat_fs <- vtreat()$df_treat_fs
        modelList <- validation()$modelList
        ll <- validation()$ll
        training <- vtreat()$training
        testing <- vtreat()$testing
        targetLevels <- vtreat()$targetLevels
        
    
        
        if (is.null(treatments) | is.null(test)) {return(NULL)}
        
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

    
}

# Run the application 
shinyApp(ui = ui, server = server)
