inside.reg.bc1breytt <- function(data,option=1)
{
  if(option==1) regs <- reg.bc
  if(option==2) regs <- reg.bc2
  if(nrow(data) > 1) {
    tmpdata <- data[, c("lat", "lon")]
  } else {
    tmpdata <- as.data.frame(data[, c("lat", "lon")])
  }
  tmpdata$area <- rep(0, nrow(tmpdata))
  i <- 1
  ind <- geoinside(tmpdata, reg = regs[[i]], option = 0, robust = F)
  if(length(ind) > 0) tmpdata[ind, "area"] <- i
  i <- 2
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 3
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 4
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 5
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 6
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 7
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 8
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 9
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 10
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 11
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 12
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0) tmpdata[j1[ind], "area"] <- i
  }
  i <- 13
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0)  tmpdata[j1[ind], "area"] <- i
  }
  i <- 14
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0)  tmpdata[j1[ind], "area"] <- i
  }
  i <- 15
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0)   tmpdata[j1[ind], "area"] <- i
  }
  i <- 16
  j <- tmpdata$area == 0
  j1 <- c(1:length(j))
  j1 <- j1[j == T]
  if(length(j1) > 0) {
    ind <- geoinside(tmpdata[j1,  ], reg = regs[[i]], option = 0,
                     robust = F)
    if(length(ind) > 0)  tmpdata[j1[ind], "area"] <- i
  }
  data$area <- tmpdata$area
  return(data)
}
