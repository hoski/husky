
#' Title
#'
#' @param data xxx
#' @param only.sv.1to10 xxx
#' @param ignore.latlon xxx
#' @param ignore.area0 xxx
#' @param option xxx
#'
#' @export

inside.reg.bcbreytt <-
  function(data,
           only.sv.1to10 = F,
           ignore.latlon = F,
           ignore.area0 = T,
           option = 1)
  {
    if (is.na(match("lat", names(data)))) {
      data$lat <- data$lon <- rep("NA", nrow(data))
    }
    if (!is.na(match("area", names(data)))) {
      print("warning column area exists")
      j <- match("area", names(data))
      names(data)[j] <- "oldarea"
      #return(invisible(data))
    }
    n <- nrow(data)
    data$area <- rep("NA", nrow(data))
    index <- !is.na(data$lat) & !is.na(data$lon)
    index1 <- c(1:length(index))
    index1 <- index1[index]
    if (ignore.latlon) index1 <- NULL
    if (length(index1) > 0) {
      area <-
        inside.reg.bc1breytt(data[index1, c("lat", "lon")], option = option)$area
      data$area[index1] <- area
    }
    if (is.na(match("reitur", names(data)))) return(data)

    if (ignore.latlon) {
      index1 <- c(1:nrow(data))
    } else {
      index <- (is.na(data$lat) | is.na(data$lon)) & !is.na(data$reitur)
      if (ignore.area0)
        index <- index | (data$area == 0 & !is.na(data$area))
      index1 <- c(1:length(index))
      index1 <- index1[index]
    }

    if (length(index1) > 0) {
      reitdata <- data[index1,]
      if (only.sv.1to10) {
        reitdata$area <- Reitur2Svaedi1to10(reitdata$reitur)
      } else {
        if (is.na(match("smareitur", names(reitdata)))) {
          reitdata$smareitur <- rep(0, nrow(reitdata))
        } else {
          reitdata$smareitur[is.na(reitdata$smareitur)] <-
            0
        tmp <- data.frame(geo::sr2d(reitdata$reitur * 10 + reitdata$smareitur))
        ind <- c(1:nrow(tmp))
        ind <- ind[!is.na(tmp$lat)]
        reitdata$area[ind] <-
          inside.reg.bc1breytt(tmp[ind,], option = option)$area
      }
      data$area[index1] <- reitdata$area
    }
    return(data)
    }
  }

