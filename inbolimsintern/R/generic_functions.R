#' Krijg basisstatistieken
#'
#' @param x vector with results
#' @param keuze character vector with stats to be calculated (n, nas, mean, sd, se, median, min, max, q.025, q.975,q.25,q.75
#' )
#'
#' @return
#' named vector with the statistics
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' get_base_stats(x, keuze = c("n", "mean", "sd"))
get_base_stats <- function(x, keuze = c("n", "nas", "mean", "sd")) {
  nas <- sum(is.na(x))
  n <- length(x)
  sd <- sd(x, na.rm = TRUE)
  se <- sd/sqrt(n)
  minimum <- min(x)
  maximum <- max(x)
  q.025 <- quantile(x, prob = 0.025)
  q.975 <- quantile(x, prob = 0.975)
  q.25 <- quantile(x, prob = 0.25)
  q.75 <- quantile(x, prob = 0.75)
  avg <- mean(x, na.rm = TRUE)
  rv <- c(n = n, nas = nas, mean = avg, sd = sd, se = se, min = minimum, max = maximum,
          q.025 = q.025, q.975 = q.975, q.25 = q.25, q.75 = q.75)
  rv[keuze]
}

#######################################################################

#' Verwijder waarden buiten een vooropgesteld aantal sigmas
#'
#' @param x vector of numerical values
#' @param nsigma numeric value containing the n-sigma values should be removed
#'
#' @return vector without outliers
#' @export
#'
#' @examples
#' x <- rnorm(100000)
#' length(remove_outliers(x))
remove_outliers <- function(x, nsigma = 3) {
  x <- x[!is.na(x)]
  repeat {
    avg <- mean(x)
    sd <- sd(x)
    if(is.na(sd)) return(x)
    rv <- x[(x >= avg - 3 * sd) & (x <= avg + 3 * sd)]

    if (length(rv) == length(x)) break
    x <- rv
  }
  rv
}


#' Get current datetime as character variable
#'
#' @return character string yyyymmdd_hhmmss
#' @export
datetime_text <- function(){
  tm <- Sys.time()
  gsub(' ', '_', gsub(':', '', gsub('-', '', tm)))
}

#' Verkrijg de bestandsextensie
#'
#' @param file de naam van de file waarvoor de extensie gezocht wordt. De extensie is alles wat na het laatste '.' komt
#' @return character string met de extensie
get_file_extension <- function(file) {
  occ <- max(gregexpr(pattern = "\\.", file)[[1]])
  if (occ > 0) {
    return(substring(file, occ+1))
  } else {
    return('')
  }
}

