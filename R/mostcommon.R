
#' Return the most common occurrence
#'
#' This function often used in apply.shrink
#'
#' @param x A vector
#'
#' @export
#'
mostcommon <- function(x) {
  x1 <- sort(table(x))
  x1 <- x1[length(x1)]
  return(as.numeric(names(x1)))
}
