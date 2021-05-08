#' how to make molecule to symbol and fullname
#'
#' @param x is the name or cid of molecule
#' @param type is the type of x
#' @return symbol and fullname
#' @export
#'
#' @examples
#' .m2t(x='FER',type='molecule')
#' .m2t(x='445858',type='cid')
#'  m2t(x=c('FER','pyrene'),type='molecule')
#'  m2t(x=c('445858','31423'),type='cid')
m2t<-function(x,type="cid")
{
  y<-lapply(x,.m2t,type=type)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.m2t<-function(x,type="cid"){
  {
    type <- match.arg(type,c("cid","molecule"))
    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="cid")
      y<-chemtarget[cid==x,][,c(1,20)]
    else
      y<-chemtarget[molecule==x,][,c(1,20)]
  }
  y<-y[!duplicated(y$fullname),]
  y[y==""]<-NA
  y
}

