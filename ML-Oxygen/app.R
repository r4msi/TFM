library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(data.table)
library(skimr)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggthemes)
library(dplyr)
library(purrr)
library(shinyjqui)
library(inspectdf)

# theme_plex <- theme_set(ggthemes::theme_fivethirtyeight() + theme(plot.title = element_text(hjust = 0.5)))

colors <- c("dodgerblue", "coral", "salmon", "#00AFBB", "#FFDB6D", "44015FF", "55C667FF", "74D055FF")
source("./helpers.R")

##############
### HEADER ###
##############

header <- dashboardHeaderPlus(
    
    title = tagList(
        span(class = "logo-lg", "ML Oxygen 0.0.3")
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
            ,tabName = "Machine Learning"
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
                    title = "Statistics"
                    ,status = "success"
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
                    ,tableOutput("input_file") ### OUTPUT ###
              
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
    
    
    # TAGET + SELECTING OPTIMAL TARGETS
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
        var = select(df,-all_of(char_exclude)) %>% names()
        updateSelectInput(session,"target", choices =  var) ### 3. INPUT: target ###
    })
    
    
    
    
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
            return(data.frame(Message = "Browse your data!"))
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
            return(NULL)
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
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
