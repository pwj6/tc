#' How to make herb to symbol and fullname
#'
#' @param x is the information adout herb
#' @param type is the type of the x
#'
#' @return symbol and fullname
#' @export
#'
#' @examples
#' .h2t(x='Ziziphi Spinosae Semen',type='latin')
#' .h2t(x='huangqi',type='pinyin')
#' h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin')
h2t<-function(x,type="latin")
{
  y<-lapply(x,.h2t,type=type)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.h2t<-function(x,type="latin"){
  {
    type <- match.arg(type,c("latin","pinyin","chinese"))
    if(length(x)>1)
      stop("Length of x must be 1!")
    else if(type=="pinyin")
      x<-drug[pinyin%in%tolower(x),]$latin
    else if(type=="chinese")
      x<-drug[chinese%in%x,]$latin
  }
  {
    if(length(x)==1)
      y <- drugtarget[herb==x,][,c(1,21)]
    else
      y<-NA
  }
  y<-y[!duplicated(y$fullname),]
  y[y==""]<-NA
  y
}
