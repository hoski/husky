#' Rattach
#'
#' @param what X
#' @param pos X
#' @param ... X
#'
#' @export

Rattach <- function(what, pos, ...){
  if(file_test("-d",what))
    what<- paste(what,".RData",sep="/")
  txt <- paste("file:",what,sep="")
  if(!is.na(match(txt,search()))){
    pos1 <- match(txt,search())
    detach(pos=pos1)
    if(missing(pos))
      attach(what,pos=pos1,...)
    else attach(what,pos=pos,...)
  }
  else {
    attach(what,...)
  }
}
