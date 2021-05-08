#' How to make herb and disease to target
#'
#' @param hb hb is the name of the drug
#' @param disease disease is the name of the disease
#' @param hbtype hbtype is the type of the hb
#' @param diseasetype diseasetype is the type of the disease
#'
#' @return targets and other information
#' @export
#'
#' @examples
#' hd2t(hb='mahuang',disease='C0009375',hbtype='pinyin',diseasetype='diseaseId')
#' hd2t(hb='Abutili Semen',disease='Colonic Neoplasms',hbtype='latin',diseasetype='diseaseName')
hd2t <- function(hb,disease,hbtype="latin",diseasetype="diseaseId"){
  {
    hbtype <- match.arg(hbtype,c("latin","pinyin","chinese"))
    diseasetype <- match.arg(diseasetype,c("diseaseId","diseaseName"))
    if(length(hb)>1)
      stop("Length of hb must be 1!")
    else if(hbtype=="pinyin")
      hb<-drug[pinyin%in%tolower(hb),]$latin
    else if(hbtype=="chinese")
      hb<-drug[chinese%in%hb,]$latin
  }
  {
    if(length(disease)>1)
      stop("Length of disease must be 1!")
  }
  {
    if(length(hb)==1)
      y <- drugtarget[herb==hb,][,c(3,4,1,21)]
    else
      y<-NA
  }
  {
    if(length(hb)==1)
      m <- drugchem[herb==hb,][,c(2,3,4,12,14)]
    else
      m <-NA
  }
  {
    if(diseasetype=="diseaseId")
      z<-disease2t[diseaseId==disease,]
    else
      z<-disease2t[diseaseName==disease,]
  }
  y<-y[!duplicated(y$fullname),]
  y[y==""]<-NA
  m<-m[!duplicated(m$molecule),]
  n<-intersect(unique(y$symbol),unique(z$geneSymbol))
  n<-data.frame(n)
  names(n)<-"samesymbol"
  l <- list(y,z,m,n)
  l
}
