#' @title XXX
#'
#' @description XXX
#'
#' location: /net/hafkaldi/export/u2/reikn/Splus5/GETALK/.RData
#'
#' @export
#'
#' @param fj1 XXX
#' @param fj2 XXX

rbind.alk <- function(fj1, fj2) {
  if (length(fj1) == 0) {
    return(fj2)
  } else {
    if (length(fj2) == 0) {
      return(fj1)
    } else {
      fj2$FjPerAldur <- rbind(fj2$FjPerAldur, fj1$FjPerAldur)
      if (!is.na(match("WtPerAldur", names(fj1))))
        fj2$WtPerAldur <- rbind(fj2$WtPerAldur, fj1$WtPerAldur)
      if (!is.na(match("KynthFjPerAldur", names(fj1))))
        fj2$KynthFjPerAldur <- rbind(fj2$KynthFjPerAldur,
                                     fj1$KynthFjPerAldur)
      if (!is.na(match("KynthWtPerAldur", names(fj1))))
        fj2$KynthWtPerAldur <- rbind(fj2$KynthWtPerAldur,
                                     fj1$KynthWtPerAldur)
      if (!is.na(match("MeanlePerAldur", names(fj1))))
        fj2$MeanlePerAldur <- rbind(fj2$MeanlePerAldur, fj1$MeanlePerAldur)
      if (!is.na(match("LengdSinnumFjPerAldur", names(fj1))))
        fj2$LengdSinnumFjPerAldur <- rbind(fj2$LengdSinnumFjPerAldur,
                                           fj1$LengdSinnumFjPerAldur)
      if (!is.na(match("Lengd2SinnumFjPerAldur", names(fj1))))
        fj2$Lengd2SinnumFjPerAldur <- rbind(fj2$Lengd2SinnumFjPerAldur,
                                            fj1$Lengd2SinnumFjPerAldur)
      attr(fj2, "Stodvar") <- rbind(attr(fj2, "Stodvar"), attr(fj1,
                                                               "Stodvar"))
    }
  }
    return(fj2)
}


