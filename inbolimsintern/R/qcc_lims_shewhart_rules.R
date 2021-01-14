

#' Rule1: point outside 3sigma
#'
#' @param x numeric vactor of lab results
#' @param lcl_3s lower limit for violation
#' @param ucl_3s upper limit for violation
#' @param run should always 1
#'
#' @return TRUE if rule violated
#' @export
qcc_rule01 <- function(x, lcl_3s, ucl_3s, run = 1){
  if (run != 1) stop("run for shewhart rule 01 must be exactly 1")
  x <= lcl_3s | x >= ucl_3s
}

#' Rule2: 9 consecutive points on the same side of the center
#'
#' @param x numeric vector of lab results
#' @param center center line off which the side is calculated
#' @param run how many times the same side before rule violation is triggered
#' @importFrom zoo rollapply
#' @return TRUE if rule violation
#' @export
qcc_rule02 <- function(x, center, run = 9){
  if (length(x) < run) {
    return(rep(FALSE, length(x)))
  } else {
    centered_x <- x - center
    base_data <- zoo::rollapply(centered_x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(y), na.rm = TRUE)) >= run))
  }
}

#'Rule3: 6 consecutive points steadily increasing or decreasing
#'
#' @param x numeric vector of lab results
#' @param run how long monotonous increase or decrease before rule violation
#'
#' @return TRUE if rule violation
#' @export
qcc_rule03 <- function(x, run = 6){
  if (length(x) <= run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run + 1, FUN = function(x) x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(c(tail(y, -1) - head(y, -1))), na.rm = TRUE)) >= (run)))
  }
}

#'Rule4: 14 consecutive points alternating up and down
#'
#' @param x numeric vector of lab results
#' @param run number of alternating up and down before rule violation
#'
#' @return TRUE if rule violation
#' @export
qcc_rule04 <- function(x, run = 14){
  if (length(x) <= run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run + 1, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    apply(base_data, 1, FUN = function(z) {
      tmp <- sign(tail(z, -1) - head(z, -1))
      tmp[is.na(tmp)] <- 0
      return(all(tmp == rep(c(-1,1),100)[1:run]) | (all(tmp == rep(c(1,-1),100)[1:run])))
    })
  }
}

#'Rule5: 2 out of 3 consecutive points more than 2 sigma in the same direction
#'
#' @param x numeric vector of lab results
#' @param lcl_2s 2-sigma lower limit
#' @param ucl_2s 2-sigma upper limit
#' @param run (run - 1) violations on run more than 2 sigma in the same direction
#' @importFrom zoo rollapply
#' @return TRUE if rule violated
#' @export
qcc_rule05 <- function(x, lcl_2s, ucl_2s, run = 3){
  if (length(x) < run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_2s, na.rm = TRUE)
      v_low <- sum(y <= lcl_2s, na.rm = TRUE)
      (v_low >= run - 1) | (v_upp >= run - 1)
    }) )
  }
}

#'Rule6: 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction
#'
#' @param x numeric vector of lab results
#' @param lcl_1s 1-sigma lower limit
#' @param ucl_1s 1-sigma upper limit
#' @param run violation if (run - 1) of run results meet the criterion
#' @return TRUE if rule violation
#' @export
qcc_rule06 <- function(x, lcl_1s, ucl_1s, run = 5){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_1s, na.rm = TRUE)
      v_low <- sum(y <= lcl_1s, na.rm = TRUE)
      (v_low >= run-1) | (v_upp >= run-1)
    }))
  }
}

#'Rule7: 15 consecutive points are within 1 sigma of the center line
#'
#' @param x numeric vector of lab results
#' @param lcl_1s lower 1-sigma limit
#' @param ucl_1s upper 1-sigma limit
#' @param run how many that meet the criterion before violation is triggered
#' @return TRUE if rule violation
#' @export
qcc_rule07 <- function(x, lcl_1s, ucl_1s, run = 15){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum(y <= ucl_1s & y >= lcl_1s, na.rm = TRUE) >= run
    }))
  }
}

#'Rule8: 8 consecutive points on either side of the center line not within 1 sigma
#'
#' @param x numeric vector of lab results
#' @param lcl_1s  1-sigma lower limit
#' @param ucl_1s  1-sigma upper limit
#' @param run how many before violation is triggerd
#' @return TRUE if violation
#' @export
#'
qcc_rule08 <- function(x, lcl_1s, ucl_1s, run = 8){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum((y >= ucl_1s) | (y <= lcl_1s), na.rm = TRUE) >= run
    }))
  }

}

#' Wrapper function that calculates all 8 Shewhart rules for lab results
#'
#' @param x numeric vector of lab results
#' @param center mean value around which all rules are checked
#' @param sd standard deviation used as basis for the rule violations
#' @param runs vector of 8 elements that contain the run before violation is triggerd for all 8 rules. The first should always be exactly 1, the rest larger than 1.
#' @param ... not used
#'
#' @return matrix containing the value of the lab result, with all 6-sigma stats (-3s, -s, -1s, center, +1s, +2s, +3s) and all 8 rule violations as a TRUE/FALSE vector
#' @export
lims_shewhart_rules <- function(x, center, sd, runs = c(1,9,6,14,3,5,15,8), ...){
  lcl_3s <- center - 3*sd
  lcl_2s <- center - 2*sd
  lcl_1s <- center - 1*sd
  ucl_1s <- center + 1*sd
  ucl_2s <- center + 2*sd
  ucl_3s <- center + 3*sd
  rule01 <- qcc_rule01(x, lcl_3s, ucl_3s, run = runs[1])
  rule02 <- qcc_rule02(x, center = center, run = runs[2])
  rule03 <- qcc_rule03(x, run = runs[3])
  rule04 <- qcc_rule04(x, run = runs[4])
  rule05 <- qcc_rule05(x, lcl_2s, ucl_2s, run = runs[5])
  rule06 <- qcc_rule06(x, lcl_1s, ucl_1s, run = runs[6])
  rule07 <- qcc_rule07(x, lcl_1s, ucl_1s, run = runs[7])
  rule08 <- qcc_rule08(x, lcl_1s, ucl_1s, run = runs[8])
  cbind(value=x, lcl_3s, lcl_2s, lcl_1s, center, ucl_1s, ucl_2s, ucl_3s,
        rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08)
}

