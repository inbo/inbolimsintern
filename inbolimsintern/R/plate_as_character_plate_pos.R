#' Converteer numerieke positie op een plaat een letter-cijfer codering
#'
#' @param pos positie op de plaat
#' @param lanes aantal kolommen op de plaat (op 96-well plaat is dit meestel 8)
#'
#' @return character vector met posities in het formaat A1, B2, C12
#' @export
#'
#' @examples
#' as_character_plate_pos(c(1,37,38,96))
as_character_plate_pos <- function(pos, lanes = 8) {
  base <- 1 + (pos - 1) %/% lanes
  rest <- pos %% lanes
  rest[rest == 0] <- lanes
  rest <- LETTERS[rest]
  paste0(rest, base)
}