#' @title Summary Statistics for factors of interest grouped by other factors of interest
#' @description Produce summary statistics on the factor of interest to be summarized on each group defined and specified by the grouping factor of interest
#' @param data is the dataset in use
#' @param group_factor is the variable/column to be grouped of interest in the data
#' @param summarize_factor is the variable/column to be summarized with different summary statistics
#'
#' @return the numeric minimum, first quartile (Q1), median, third quartile (Q3), maximum, range, and interquartile range (IQR), and standard deviation of the factor of interest to be summarized for all groups defined by the grouping factor of interest
#'
#' @examples
#' library(tidyverse)
#' library(datateachr)
#' neighbourhood_diameter_summary <- stat_summary(vancouver_trees, "neighbourhood_name", "diameter")
#' @export

stat_summary <- function(data, group_factor, summarize_factor) {
  if(!is.character(data[[group_factor]]) || is.factor(data[[group_factor]])) {
    stop("I am so sorry, but this function only works for character or factor input for the factor to be grouped
         \n",
         "You have provided the group_factor object of class: ", class(data[[group_factor]])[1]
    )
  }
  if (!is.numeric(data[[summarize_factor]])){
    stop("I am so sorry, but this function only works for numeric input for the factor to be summarized
         \n",
         "You have provided the summarize_factor object of class: ", class(data[[summarize_factor]])[1]
    )
  }
  data tidyverse::%>%
    dplyr::group_by(.data[[group_factor]], na.rm=T) %>%
    dplyr::summarize(Mean = mean(.data[[summarize_factor]], na.rm=T),
              Minimum = min(.data[[summarize_factor]], na.rm=T),
              Q1 = quantile(.data[[summarize_factor]], 0.25, na.rm=T),
              Median = median(.data[[summarize_factor]], na.rm=T),
              Q3 = quantile(.data[[summarize_factor]], 0.75, na.rm=T),
              Maximum = max(.data[[summarize_factor]], na.rm=T),
              Range = range(.data[[summarize_factor]], na.rm=T),
              IQR = IQR(.data[[summarize_factor]], na.rm=T),
              Standard_deviation = sd(.data[[summarize_factor]], na.rm = T))}

