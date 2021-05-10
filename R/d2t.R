#' How to make disease to target
#'
#' @param x is diseaseID or disease
#' @param type is the class of x
#' @param scorevalue is the limitation of score
#' @return target symbol and score
#' @export
#'
#' @examples
#' .d2t(x='C0009375',type='diseaseID',scorevalue=0.3)
#' d2t(x=c('C0009375','C0271979'),type='diseaseID',scorevalue=0.4)
#' .d2t(x='Schizophrenia',type='disease',scorevalue=0.3)
#' d2t(x=c('Schizophrenia','beta Thalassemia'),type='disease',scorevalue=0.4)
d2t<-function(x,type="diseaseID",scorevalue=0.3)
{
  y<-lapply(x,.d2t,type=type,scorevalue=scorevalue)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.d2t<-function(x,type="diseaseID",scorevalue=0.3){
  {
    type <- match.arg(type,c("diseaseID","disease"))
    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="diseaseID")
      y<-diseasetarget[diseaseID==x&score>=scorevalue,][,c(1,4)]
    else
      y<-diseasetarget[disease==x&score>=scorevalue,][,c(1,4)]
  }
  y
}
