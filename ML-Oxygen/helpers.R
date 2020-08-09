colors <- c("dodgerblue", "coral", "salmon", "#00AFBB", "#FFDB6D", "44015FF", "55C667FF", "74D055FF")

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

target_encode <- function(target_encoding, data, factor = FALSE) {
  
  data <- as.data.frame(data)
  
  if (factor == TRUE) {
    
    data[, names(target_encoding)] <- lapply(data[, names(target_encoding)], factor)
    
  }
  
  for (i in 1:length(target_encoding)) {
    
    for (j in 1:length(target_encoding[[i]])) {
      
      levels(data[,which(names(data) == names(target_encoding)[i])])[levels(data[,which(names(data) == names(target_encoding)[i])])==target_encoding[[i]][[j,1]]] = target_encoding[[i]][[j,2]]
      
    }
  }
  
  data[, names(target_encoding)] = apply(data[, names(target_encoding)], 2, function(x) as.numeric(as.character(x)))
  data[, names(target_encoding)] = apply(data[, names(target_encoding)], 2, function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))
  
  data
  
}

cat_names <- function(data) {
  numeric_names <- names(which(sapply(data, class) == "factor" | sapply(data, class) == "character" | sapply(data, class) == "ordered"))
  numeric_names
}