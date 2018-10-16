# Functions to read and process Web of Science citation report data.
#bib <- read.isiwok('isiwok_MAM.txt')

#' Read and process Web of Science citation report data.
#'
#' @param infile Input text file from Web of Science
#'
#' @return A list of information
#' @export
#' @examples
#' wos_file = system.file('sbegueria.txt', package = "bibliometRics")
#' bib <- read.wos(wos_file)
read.wos <- function(infile) {

  # author
  au <- read.table(infile, sep=',', nrows=1, stringsAsFactors=FALSE)
  #au <- paste(gsub('AUTHOR: \\(','',au[1]),gsub('\\)','',au[2]),sep=', ')
  au <- paste(gsub('(AUTHOR)( IDENTIFIERS)*(: \\()','',au[1]), gsub('\\)','',au[2]),sep=', ')
  au <- toupper(au)

  # reference
  re <- gsub('.txt','',rev(strsplit(infile,'/')[[1]])[1])

  # read publications
  nskip <- ifelse (read.table(infile,sep=',',nrows=1,skip=1)=='null', 5, 4)
  dat <- read.table(infile, sep=',', stringsAsFactors=FALSE, skip=nskip)
  colnames(dat) <- read.table(infile,sep=',',stringsAsFactors=FALSE,skip=nskip-1,nrows=1)
  o <- rev(order(dat[,'Publication Year']))
  dat <- dat[o,]
  start <- min(dat[,'Publication Year'])
  w <- which(colnames(dat)==start)
  dat <- dat[,c(1:21,w:ncol(dat))]

  return(list(author=au,reference=re,pubs=dat))
}
