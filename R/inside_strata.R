#' inside.strata
#'
#' location: /net/hafkaldi/export/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData
#'
#' @param stodvar XXX
#' @param stratalist XXX
#' @param stratas XXX Defaults to the old strata (NOTE: Not in huskys original)
#'
#' @export
#'
inside.strata <- function (stodvar, stratalist = ralllist, stratas = STRATAS)
{
  stodvar$newstrata <- rep(0, nrow(stodvar))
  for (i in stratalist) {
    j <- geo::geoinside(stodvar, reg = stratas[[i]], robust = T,
                        option = 0)
    if (length(j) > 0)
      stodvar[j, "newstrata"] <- i
  }
  i <- stodvar$newstrata == 0
  if (any(i)) {
    i1 <- 1:length(i)
    i1 <- i1[i]
    cat("Strata fannst ekki fyrir stodvar", paste(i1, collapse = ","))
  }
  return(stodvar)
}
