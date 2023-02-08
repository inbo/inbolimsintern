
#' Elementaire testen (t-test en variantie test)
#'
#' @param data data waarin de data voor de t-test en F-test te vinden zijn
#' @param valuecol kolomnaam voor de waarden
#' @param grpcol kolomnaam voor de groepen
#' @param label identificatielabel voor resultaten
#'
#' @return data.frame met resultaten t-test en var.test
#' @export
#'
elc_htest <- function(data, valuecol = "WAARDE", grpcol = "grp", label = "" ) {
  x <- data[, valuecol]
  g <- data[, grpcol]
  x1 <- x[g == 1]
  x2 <- x[g != 1]
  n1 <- length(x1); n2 = length(x2)
  avg1 <- mean(x1, na.rm = TRUE); avg2 = mean(x2, nr.rm = TRUE)
  sdv1 <- sd(  x1, na.rm = TRUE); sdv2 = sd(  x2, na.rm = TRUE)

  ttest <- try(t.test(x1, x2))
  if (class(ttest) != "try-error") {
    tval_t <- ttest$statistic
    pval_t <- ttest$p.value
    lcl_t <-  ttest$conf.int[1]
    ucl_t <-  ttest$conf.int[2]
  } else {
    tval_t <- NA
    pval_t <- NA
    lcl_t <-  NA
    ucl_t <-  NA
  }
  vtest <- try(var.test(x1, x2))
  if (class(vtest) != "try-error") {
    fval_v <- vtest$statistic
    pval_v <- vtest$p.value
    lcl_v <-  vtest$conf.int[1]
    ucl_v <-  vtest$conf.int[2]
  } else {
    fval_v <- NA
    pval_v <- NA
    lcl_v <-  NA
    ucl_v <-  NA
  }
  bind_rows(
    data.frame(label = label, test = "stats",
               n1 = n1, n2 = n2,
               avg1 = avg1, avg2 = avg2,
               sdv1 = sdv1, sdv2 = sdv2),
    data.frame(label = label, test = "t.test",
               stat = tval_t, pval = pval_t,
               lcl = lcl_t, ucl = ucl_t),
    data.frame(label = label, test = "var.test",
               stat = fval_v, pval = pval_v,
               lcl = lcl_v, ucl = ucl_v))
}

