

#' Convert ascii characters to their ascii number
#' @param x vector of strings
#' @param simplify argument as used in the sapply function
#'
#' @return integer numbers of the strings. One entry per letter, one column per string
#' @export
#'
asc_to_int <- function(x, simplify = FALSE) {
  sapply(x, function(x) strtoi(charToRaw(x), 16L), simplify = simplify)
}

#' Convert integers to their ascii symbols
#'
#' @param x integer value to be converted to the corresponding ascii symbol
#'
#' @return list with ascii numbers for each character
#' @export
#'
int_to_asc <- function(x) {
  sapply(x, function(x) rawToChar(as.raw(x)))
}

#' Remove special characters from string
#'
#' @param x vector of strings containing special characters
#' @param keep_codes ascii-codes to be kept. By default 38:128 and 181
#' @param replace 2-kolom data.frame, in which the character in the first column is replaced by the character in the second column
#'
#' @return ascii vector without special characters
#' @export
#'
convert_to_simple_ascii <- function(x, keep_codes = c(38:128, 181), replace = data.frame(181, 181)) {
  lst <- asc_to_int(x, simplify = FALSE)
  unname(sapply(lst, function(x){
    tmp <- x[x %in% keep_codes]
    for (i in 1:nrow(replace)){
      tmp[tmp == replace[i,1]] <- replace[i,2]
    }
    rv <- paste(int_to_asc(tmp), collapse = "")
    rv
  }))
}
