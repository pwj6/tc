#' How to make disease to target
#'
#' @param x is diseaseId or diseaseName
#' @param type the class of x
#'
#' @return target symbol and score
#' @export
#'
#' @examples
#' .d2t(x='C0009375',type='diseaseId')
#' d2t(x=c('C0009375','C0271979'),type='diseaseId')
#' .d2t(x='Schizophrenia',type='diseaseName')
#' d2t(x=c('Schizophrenia','beta Thalassemia'),type='diseaseName')
d2t<-function(x,type="diseaseId")
{
  y<-lapply(x,.d2t,type=type)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.d2t<-function(x,type="diseaseId"){
  {
    type <- match.arg(type,c("diseaseId","diseaseName"))
    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="diseaseId")
      y<-disease2t[diseaseId==x,][,c(1,4)]
    else
      y<-disease2t[diseaseName==x,][,c(1,4)]
  }
  y
}
