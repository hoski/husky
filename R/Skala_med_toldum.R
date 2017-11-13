#' Skala.med.toldum
#'
#' @param lengdir XXX
#' @param numer XXX
#' @param tegund XXX
#
#' @export
#'

Skala.med.toldum <- function (lengdir, numer, tegund) {
  if (missing(numer))
    numer <- fjolst::lesa.numer(synis.id = unique(lengdir$synis.id),
                        tegund = tegund)
  numer$rat <- 1 + numer$fj.talid/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 1
  l1 <- join(lengdir, numer[, c("synis.id", "rat")], "synis.id")
  lengdir$fj.alls <- l1$fjoldi * l1$rat
  return(lengdir)
}
