library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders) 
library(shinymanager)
library(data.table)
library(skimr)
library(DataExplorer)
library(ggrepel)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggthemes)
library(dplyr)
library(tidyr)
library(purrr)
library(inspectdf)
library(caret)
library(caretEnsemble)
library(vtreat)
library(stringr)
library(ranger)
library(xgboost)
library(glmnet)
library(patchwork)
library(e1071 )
library(shinyWidgets)
library(recipes)
library(readxl)
library(RMySQL)
library(googlesheets4)
library(htmltools)
library(kernlab)
library(rmarkdown)
library(recipes)
library(parallel)


#rf_var_imp <- NULL
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}
# 
# 
# credentials <- data.frame(
#   user = c("R4msesII", "1"),
#   password = c("ramses", "2"),
#   stringsAsFactors = FALSE
# )


`%AND%` <- function (x, y) {
  if (!is.null(x) && !anyNA(x))
    if (!is.null(y) && !anyNA(y))
      return(y)
  return(NULL)
}
passwordInputAddon <- function (inputId, label, value = "", placeholder = NULL, addon, width = NULL)
{
  value <- shiny::restoreInput(id = inputId, default = value)
  htmltools::tags$div(
    class = "form-group shiny-input-container",
    label %AND% htmltools::tags$label(label, `for` = inputId),
    style = if (!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), ";"),
    htmltools::tags$div(
      style = "margin-bottom: 5px;", class="input-group",
      addon %AND% htmltools::tags$span(class="input-group-addon", addon),
      htmltools::tags$input(
        id = inputId, type = "password", class = "form-control",
        value = value, placeholder = placeholder
      )
    )
  )
}

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

theme_plex <- theme_set(theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))
#theme_set(theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))

colors <- c("dodgerblue", "coral", "salmon", "yellow")

plot_bar_all <- function(data, group, with = NULL, maxcat = 50, order_bar = TRUE, binary_as_factor = TRUE, title = NULL, ggtheme = theme_gray(), theme_config = list(), nrow = 4L, ncol = 4L, parallel = FALSE, position=FALSE) {
  require(DataExplorer)
  require(ggplot2)
  frequency <- measure <- variable <- value <- NULL
  if (!is.data.table(data)) data <- data.table(data)
  split_data <- split_columns(data, binary_as_factor = binary_as_factor)
  if (split_data$num_discrete == 0) stop("No discrete features found!")
  discrete <- split_data$discrete
  ind <- DataExplorer:::.ignoreCat(discrete, maxcat = maxcat)
  if (length(ind)) {
    message(length(ind), " columns ignored with more than ", maxcat, " categories.\n", paste0(names(ind), ": ", ind, " categories\n"))
    drop_columns(discrete, names(ind))
    if (length(discrete) == 0) stop("Note: All discrete features ignored! Nothing to plot!")
  }
  feature_names <- names(discrete)
  if (is.null(with)) {
    dt <- discrete[, list(frequency = .N), by = feature_names]
  } else {
    if (is.factor(data[[with]])) {
      measure_var <- suppressWarnings(as.numeric(levels(data[[with]]))[data[[with]]])
    } else if (is.character(data[[with]])) {
      measure_var <- as.numeric(data[[with]])
    } else {
      measure_var <- data[[with]]
    }
    if (all(is.na(measure_var))) stop("Failed to convert `", with, "` to continuous!")
    if (with %in% names(discrete)) drop_columns(discrete, with)
    tmp_dt <- data.table(discrete, "measure" = measure_var)
    dt <- tmp_dt[, list(frequency = sum(measure, na.rm = TRUE)), by = feature_names]
  }
  dt2 <- suppressWarnings(melt.data.table(dt, id.vars = c(group, "frequency"), measure.vars = setdiff(feature_names, group))) # This line is updated
  layout <- DataExplorer:::.getPageLayout(nrow, ncol, ncol(discrete))
  plot_list <- DataExplorer:::.lapply(
    parallel = parallel,
    X = layout,
    FUN = function(x) {
      if (order_bar) {
        base_plot <- ggplot(dt2[variable %in% feature_names[x]], aes(x = reorder(value, frequency), y = frequency))
      } else {
        base_plot <- ggplot(dt2[variable %in% feature_names[x]], aes(x = value, y = frequency))
      }
      if (position == TRUE) {
        base_plot +
          geom_bar(stat = "identity", aes_string(fill = group), position = "fill") + # This line is updated
          coord_flip() +
          xlab("") + ylab(ifelse(is.null(with), "Frequency", toTitleCase(with)))
      } else {
        base_plot +
          geom_bar(stat = "identity", aes_string(fill = group)) + # This line is updated
          coord_flip() +
          xlab("") + ylab(ifelse(is.null(with), "Frequency", toTitleCase(with)))
      }
    }
  )
  class(plot_list) <- c("multiple", class(plot_list))
  plotDataExplorer(
    plot_obj = plot_list,
    page_layout = layout,
    title = title,
    ggtheme = ggtheme,
    theme_config = theme_config,
    facet_wrap_args = list(
      "facet" = ~ variable,
      "nrow" = nrow,
      "ncol" = ncol,
      "scales" = "free"
    )
  )
}


missVtreatRanger <- function(train, val, test=NULL, originalTarget) {
  
  tr <- train %>% 
    select(-originalTarget)
  
  vl <- val %>% 
    select(-originalTarget)
  
  # UNION
  if (is.null(test)) {
    data <- rbind(
      tr,
      vl
    )
  } else {
    data <- rbind(
      tr,
      vl,
      test
    )
  }
  
  # NZV
  
  nzv <- nearZeroVar(data,freqCut = 90/10, uniqueCut = 10)
  
  if (length(nzv)>0) {
    data <- data[,-nzv]
  }
  
  isna <- sapply(data, function(x, data) {
    sum(is.na(x))/nrow(data)
  },data)
  
  l_na <- which(isna>.3) %>% length()
  
  if (l_na>0) {
    
    remove <- which(isna>.3) %>% names()
    
    data <- select(data, -all_of(remove))
    
  }
  
  unique_char <- sapply(Filter(is.character, data),function(x){ # Not advisable to plot high cardinality variables
    length(unique(x))>=10
  })
  char_exclude <- names(which(unique_char==TRUE))
  
  select(data,-all_of(char_exclude))
  
  data <- select(data,-all_of(char_exclude))
  
  
  isnaVars <- sapply(data, function(x) {
    any(is.na(x))
  })
  
  isnaVars <- data[,isnaVars] %>% names
  
  
  data_impute <- data 
  
  vars <- length(isnaVars)
  
  
  for (i in 1:vars) {
    
    missTarget <- isnaVars[i] 
    
    missTarget %>% print()
    
    targetMissLevels <- data_impute[ , missTarget] %>% unique() %>% length()
    
    
    if (targetMissLevels < 10) {
      
      data_impute[ , missTarget] <- ifelse(
        is.na(data_impute[, missTarget]), 
        Mode(data_impute[, missTarget]),
        data_impute[, missTarget]
      )
      
      listMiss <- select(data_impute, -missTarget) %>% names()
      
      set.seed(123)
      treatments <- vtreat::mkCrossFrameCExperiment(
        data_impute,
        varlist = listMiss,
        outcomename = missTarget, 
        outcometarget = data[ ,missTarget][[1]],
        verbose = T
      )
      
      df_treat <- treatments$crossFrame
      df_treat_fs <- select(df_treat,-missTarget)[,treatments$treatments$scoreFrame$recommended]
      df_treat_fs$Target <- as.factor(df_treat[,missTarget])
      
      
    } else {
      
      
      data_impute[ , missTarget] <- ifelse(
        is.na(data_impute[, missTarget]), 
        median(data_impute[, missTarget],na.rm = T),
        data_impute[, missTarget]
      )
      
      
      listMiss <- select(data_impute, -missTarget) %>% names()
      
      set.seed(123)
      treatments <- vtreat::mkCrossFrameNExperiment(
        data_impute,
        varlist = listMiss,
        outcomename = missTarget,
        verbose = F
      )
      
      df_treat <- treatments$crossFrame
      df_treat_fs <- select(df_treat,-missTarget)[,treatments$treatments$scoreFrame$recommended]
      df_treat_fs$Target <- df_treat[,missTarget]
      
      
    }
    
    for (i in 1:nrow(data)) {
      if (is.na(data[i, missTarget])) {
        df_treat_fs[i, "Target"] = NA
      }
    }
    
    # if (targetMissLevels == 2) {
    #   f <- "binomial"
    # } else if (targetMissLevels >2 & targetMissLevels <10){
    #   f <- "multinomial"
    # } else {
    #   f <- "gaussian"
    # }
    
    
    i=1
    
    # place where model predictions are stored
    solution.table<-data.frame(id = 1:nrow(df_treat_fs))
    
    for (i in 1:10){ # I begin with 2 because these values will indicate column position in data.frame 'solution.table'
      
      model <- ranger(
        data = df_treat_fs %>% na.omit(),
        formula = Target~.,
        num.trees = 150,
        max.depth = 14,
        seed = i
      )

      solution.table[,i]<-predict(model, df_treat_fs)[[1]]
      
      solution.table %>% print()
    
    }
    
    if (targetMissLevels > 10) {
    
      solution.table$mean <- rowMeans(solution.table[,-1])
      
      varCleaned <- solution.table$mean
    
    } else {
      
      solution.table.count<-apply(solution.table, MARGIN=1, table)
      
      # Create vector where my solution will be stored
      predict.combined<-vector()
      
      # Identify category with more frequency (votes) per row.
      for (x in 1:nrow(df_treat_fs)) {
        predict.combined[x]<-names(which.max(solution.table.count[[x]]))
      }
      
      varCleaned <- predict.combined
      
    }
    
    
    # Recuperando los originales que no eran NA
    for (i in 1:nrow(data)) {
      if (!is.na(data[i, missTarget])) {
        varCleaned[i] = data[i, missTarget]
      }
    }
    
    trRows <- nrow(tr)
    vlRows <- nrow(vl)
    if(!is.null(test)) {
      testRows <- nrow(test)
    }
    
    bad_var <- paste(missTarget,"bad",sep = "_")
    tr[, bad_var] <- ifelse(data[1:trRows, missTarget] %>% is.na(), 1,0)    
    tr[, missTarget] <- varCleaned[1:trRows]
    
    vl[, bad_var] <- ifelse(data[(trRows+1):(trRows+vlRows), missTarget] %>% is.na(), 1,0) 
    vl[, missTarget] <- varCleaned[(trRows+1):(trRows+vlRows)]
    
    if (!is.null(test)) {
      test[, bad_var] <- ifelse(data[(trRows+vlRows+1):(data %>% nrow()), missTarget] %>% is.na(), 1,0) 
      test[, missTarget] <- varCleaned[(trRows+vlRows+1):(data %>% nrow())]
    }
    
  }
  
  # col <- ncol(tr)
  
  # for (i in 1:col) {
  # 
  #   c <- sapply(tr, class)
  #   class(tr[,i]) <- c[[i]]
  # 
  # }
  # 
  # col <- ncol(vl)
  # 
  # for (i in 1:col) {
  # 
  #   c <- sapply(vl, class)
  #   class(vl[,i]) <- c[[i]]
  # 
  # }
  
  tr <- cbind(tr, train %>% select(originalTarget))
  vl <- cbind(vl, val %>% select(originalTarget))
  
  return(list(tr, vl, test))
  
}

