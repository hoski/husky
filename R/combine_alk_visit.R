#' @title XXX
#'
#' @description XXX
#'
#' location: /net/hafkaldi/export/u2/reikn/Splus5/GETALK/.RData
#'
#' @export
#'
#' @param fjvisit XXX
#' @param kynthvisit XXX
#' @param biovisit XXX
#' @param meanlevisit XXX
#' @param sdevvisit XXX
#' @param kynthbiovisit XXX
#' @param ar XXX
#' @param aldur XXX
#' @param rows XXX
#' @param mult XXX

combine.alk.visit <-
  function (fjvisit, kynthvisit, biovisit, meanlevisit, sdevvisit,
            kynthbiovisit, ar = 1985:2002, aldur = 1:11, rows = 22, mult = 1e+06)
  {
    utkoma <- list()
    m <- matrix(0, length(ar), length(aldur))
    dimnames(m) <- list(as.character(ar), as.character(aldur))
    fj <- kynthhlutfall <- wt <- kynthwt <- meanle <- cv <- sdev <- m
    for (i in 1:length(ar)) if (!is.null(fjvisit[[i]]))
      for (j in 1:length(aldur)) {
        fj[i, j] <- sum(fjvisit[[i]][[j]][rows, "total"]/mult)
        cv[i, j] <- sqrt(sum((fjvisit[[i]][[j]][rows, "total"] *
                                fjvisit[[i]][[j]][rows, "cv"])^2))/fj[i, j]/mult
      }
    utkoma$fj <- fj
    utkoma$cv <- cv
    if (!missing(kynthvisit)) {
      for (i in 1:length(ar)) if (!is.null(kynthvisit[[i]]))
        for (j in 1:length(aldur)) kynthhlutfall[i, j] <- sum(kynthvisit[[i]][[j]][rows,"total"]/mult)
        kynthhlutfall <- kynthhlutfall/fj
        utkoma$kynthhlutfall <- kynthhlutfall
    }
    if (!missing(biovisit)) {
      for (i in 1:length(ar)) if (!is.null(biovisit[[i]]))
        for (j in 1:length(aldur)) wt[i, j] <- sum(biovisit[[i]][[j]][rows,"total"]/mult)
        wt <- wt/fj
        utkoma$wt <- wt/1000
    }
    if (!missing(kynthbiovisit)) {
      for (i in 1:length(ar)) if (!is.null(kynthbiovisit[[i]]))
        for (j in 1:length(aldur)) kynthwt[i, j] <- sum(kynthbiovisit[[i]][[j]][rows,"total"]/mult)
        kynthwt <- kynthwt/(fj * kynthhlutfall)
        utkoma$kynthwt <- kynthwt/1000
    }
    if (!missing(meanlevisit)) {
      for (i in 1:length(ar)) if (!is.null(meanlevisit[[i]]))
        for (j in 1:length(aldur)) meanle[i, j] <- sum(meanlevisit[[i]][[j]][rows,"total"]/mult)
        meanle <- meanle/fj
        utkoma$meanle <- meanle
    }
    if (!missing(sdevvisit)) {
      for (i in 1:length(ar)) if (!is.null(sdevvisit[[i]]))
        for (j in 1:length(aldur)) sdev[i, j] <- sum(sdevvisit[[i]][[j]][rows,"total"]/mult)
        sdev <- sqrt(((sdev - meanle * meanle * fj)/fj))
        utkoma$sdev <- sdev
    }
    return(utkoma)
  }
