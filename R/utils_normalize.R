#' Normalizes with z score
#' @details Use as.character if x is a factor
#' @param x Input numerical variable
#' @return Normalized vector x
#' @examples
#' as_mdy("2-1-2014")
#' \dontrun{
#' as_mdy("2-1-2014") %>% month(label = TRUE)
#' as_mdy("2-1-2014") %>% week()
#' as_mdy("2-1-2014") %>% wday(label = TRUE)
#' }
#' @noRd
zscore <- function(x) {
  
  x <- (mean(x)-x)/sd(x)
  x
}