source("./helpers.R")
library(doParallel)

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
                        ,"Upload the file"
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
                    ,plotOutput(
                        outputId = "corr"                               #### 13. OUTPUT: corr ###
                        ,height = 1200
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
                            outputId = "value_target"
                        )
                        ,boxPlus(
                            title = "Auto-Bayesian-Processing"
                            ,closable = F
                            ,solidHeader = TRUE
                            ,status = "warning"
                            ,width = 12
                            ,collapsible = T
                            ,collapsed = F
                            ,column(
                                width = 12
                                ,align = "center"
                                ,radioButtons(
                                    inputId = "rb"
                                    ,label = "Treatment"
                                    ,choices = c("One-Sample", "Cross-Validation-Bayes")
                                )
                            )
                        )
                        ,boxPlus(
                            title = "Validation"
                            ,closable = F
                            ,solidHeader = TRUE
                            ,status = "primary"
                            ,width = 12
                            ,collapsible = T
                            ,collapsed = T
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
                                ,actionButton(
                                    inputId = "process_train"                                        ### 16. INPUT: process_train ###
                                    ,label = "Porcessing"
                                )
                            )
                        )
                        ,boxPlus(
                            title = "Upload Test"
                            ,closable = F
                            ,solidHeader = TRUE
                            ,status = "info"
                            ,width = 12
                            ,collapsible = T
                            ,collapsed = T
                            ,column(
                                width = 12
                                ,align = "center"
                                ,fileInput(
                                    inputId = "file_test"                                                ### 17. INPUT: file_test ###
                                    ,"Test"
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
                                        "LinearModel", "RandomForest", "KNN"
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
            length(unique(x))>=15
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
    
    
    
    validation <- eventReactive(input$process_train,{

        showModal(modalDialog("Fitting Models" ,fade = TRUE, size = "l", footer = "1-10 mins"))
        df <- filedata()
        if (is.null(df)) {
            return(NULL)
        }
        
        if (length(unique(df[,input$target]))<=5) {
            
            df[,input$target] <- factor(df[,input$target])
            
            list <- names(select(df,-input$target)) 
            treatments <- vtreat::designTreatmentsC(df, varlist = list, outcomename = input$target, outcometarget = df[,input$target][1])
            df_treat <- vtreat::prepare(treatments, df)
            df_treat_fs <- df_treat[,treatments$scoreFrame$recommended] # Variables recomendadas.
            df_treat_fs$Target <- df_treat[,input$target]
            
            df_treat_fs$Target <- make.names(df_treat_fs$Target)
            
        } else {
            list <- names(select(df,-input$target)) 
            treatments <- vtreat::designTreatmentsN(df, varlist = list, outcomename = input$target)
            df_treat <- vtreat::prepare(treatments, df)
            df_treat_fs <- df_treat[,treatments$scoreFrame$recommended] # Variables recomendadas.
            df_treat_fs$Target <- df_treat[,input$target]
        }
        
        inTrain <- createDataPartition(y = df_treat_fs$Target, p = input$split, list = FALSE)
        training <- df_treat_fs[ inTrain,]
        testing <- df_treat_fs[-inTrain,]
        
        
        trControl <- trainControl(
            method = "cv",
            savePredictions = "final",
            index = createFolds(training$Target, k =input$cv), 
            allowParallel = TRUE,
            verboseIter = FALSE
        )
        
        xgbTreeGrid <- expand.grid(
            nrounds = 150,
            max_depth = 8,
            eta = 0.02,
            gamma = 0.05,
            colsample_bytree = c(.3),
            subsample = 0.7,
            min_child_weight = 5)
        lassoGrid <- expand.grid(
            alpha =1,
            lambda = .1
        )
        ridgeGrid <- expand.grid(
            alpha =0,
            lambda = .1
        )
        elasticNetGrid <- expand.grid(
            alpha = .5
            ,lambda = .1
        )
        linearGrid <- expand.grid(
            alpha = 0,
            lambda = 0
        )
        if (length(unique(df[,input$target]))<=15) {
            rfGrid <- expand.grid(
                mtry = round(sqrt(ncol(training))),
                splitrule = "gini",
                min.node.size = 1)
        } else {
            rfGrid <- expand.grid(
                mtry = round(sqrt(ncol(training))),
                splitrule = "variance",
                min.node.size = 5)
        }
        knnGrid <- expand.grid(
            k = 3
        )
        
        if (length(unique(df[,input$target]))==2 | length(unique(df[,input$target])) > 15 ) {
        
            modelList <- caretList(
                Target ~ ., data = training,
                trControl = trControl,
                tuneList = list(
                    XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
                    Lasso = caretModelSpec(method = "glmnet", tuneGrid = lassoGrid),
                    Ridge = caretModelSpec(method = "glmnet", tuneGrid = ridgeGrid),
                    ElasticNet = caretModelSpec(method = "glmnet", tuneGrid = elasticNetGrid),
                    LinearModel = caretModelSpec(method = "glmnet", tuneGrid = linearGrid),
                    RandomForest     = caretModelSpec(method = "ranger", tuneGrid = rfGrid),
                    KNN   = caretModelSpec(method = "knn", tuneGrid = knnGrid)
                )
            )
        
        }
        
        else {
            modelList <- caretList(
                Target ~ ., data = training,
                trControl = trControl,
                tuneList = list(
                    XGB    = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
                    RandomForest     = caretModelSpec(method = "ranger", tuneGrid = rfGrid),
                    KNN   = caretModelSpec(method = "knn", tuneGrid = knnGrid)
                )
            )
        }
        
        removeModal()
        #doParallel::stopImplicitCluster()
        return(list(
            treatments = treatments,
            modelList = modelList,
            testing = testing
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
        
        plot_missing(
            df, 
            #ggtheme = theme_plex, 
            title = "NA", 
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
            #ggtheme = theme_plex,
            title = "Numerical Distributions",
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
            #ggtheme = theme_plex,
            title = "Bivariate Numerical Boxplot",
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
            #ggtheme = theme_plex,
            title = "Bar Plot Univariate",
            nrow = ncol(df[,cardinality])
        )
        
        
    })
    
    
    output$bars_bi <- renderPlot({                                                                 ### 11. OUTPUT: bars_bi ###
        
        df <- filedata()
        if (is.null(df)) {
            return(readRDS("rds/barplot_bi.RDS.RDS"))
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
                    title = "Bar Plot Bivariate",
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
            na.omit(df)
            #ggtheme = theme_plex
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
        
        
        a <- modelList %>% 
            resamples() %>% 
            data.frame() %>% 
            pivot_longer(XGB:KNN) %>% 
            ggplot(., aes(reorder(name,value), value)) + 
            geom_boxplot(color="dodgerblue") + 
            coord_flip() + 
            labs(title = "CV Results", y=modelList$XGB$metric, x=NULL) 
        
        ggplotly(a)
        
    })
    
    output$test_models_results <- renderPlotly({                                                                 ### 19. OUTPUT: test_models_results ###
        
        modelList <- validation()$modelList
        testing <- validation()$testing
        
        if (length(unique(testing$Target))<=5) {
        
            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing)
                results <- postResample(y_pred, testing$Target %>% factor)
            }) %>% data.frame() %>% 
                pivot_longer(XGB:KNN) %>% 
                mutate(Metric="Accuracy")
            
            metric_results[8:14,]$Metric <- "Kappa"
        
        } else {
            metric_results <- sapply(modelList, function(x){
                y_pred <- predict(x,testing)
                results <- postResample(y_pred, testing$Target)
            }) %>% data.frame() %>% 
                pivot_longer(XGB:KNN) %>% 
                mutate(Metric="RMSE")
            
            metric_results <- metric_results[1:7,]
          
        }
        
        b <- metric_results %>% 
            ggplot(.,aes(reorder(name,value),value)) + 
                geom_col(fill=sample(colors,1)) + 
                facet_wrap(~Metric) + 
                coord_flip() +labs(y=NULL,x=NULL)
        
        ggplotly(b)
        
    })
    
    pred <- eventReactive(input$predict,{
        
        treatments <- validation()$treatments
      
        df <- filedata()
        modelList <- validation()$modelList
    
        
        if (is.null(treatments) | is.null(test)) {return(NULL)}
        
        
        showModal(modalDialog("Predicting" ,fade = TRUE, size = "l", footer = "Less than 30"))
        
        Sys.sleep(5)
        test_treated <- prepare(treatments, test)
        pred <- predict(modelList$RandomForest, test_treated)
        
        removeModal()
        
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
