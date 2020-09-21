
#' Calculate Shewhart rules (generic)
#'
#' @aliases lims_shewhart_default
#' @export
#' @rdname lims_shewhart
lims_shewhart <- function(x, ...){
  UseMethod("lims_shewhart")
}

#' Calculate Shewhart rules (lims_data object)
#' 
#' @aliases lims_shewhart_default
#' @export
#' @rdname lims_shewhart
#' @method lims_shewhart lims_data
lims_shewhart.lims_data <- function(x, entrycol = "ENTRY", ...){
  lims_shewhart.default(x = x[[entrycol]], ...)
  
}

#' Calculate Shewhart rules (default)
#'
#' @param x lims_data object containing all info to make a shewhart plot
#' @param entrycol the column name in the dataset that is used as value column for the results
#' @param center fixed mean value, or calculated when set to NULL
#' @param sd fixed standard deviation, or calculated when set to NULL
#' @param rules definition of rules ??? ADD MORE EXPLANATION ???
#' @param ... other variables passed on
#' @rdname lims_shewhart
#' @method lims_shewhart default
#' @return dataset with the violations for all the 8 shewhart rules
#' @export
lims_shewhart.default <- function(x, center = NULL, sd = NULL, rules=lims_shewhart_rules, ...){
  if (is.null(center)) center <- mean(x)
  if (is.null(sd)) sd <- sd(x)
  
  plotdata <- lims_shewhart_rules(x, center, sd, runs = c(1,9,6,14,3,5,15,8))
  plotdata <- cbind(Nr = 1:length(x), plotdata)
  plotdata <- as.data.frame(plotdata)
  plotdata$RVIOL <- as.character(paste0(plotdata$rule01,plotdata$rule02,plotdata$rule03,plotdata$rule04,
                                        plotdata$rule05,plotdata$rule06,plotdata$rule07,plotdata$rule08))
  plotdata
}

