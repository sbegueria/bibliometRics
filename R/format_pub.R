#' Format Publication data
#'
#' @param x A list with an element called  \code{pubs}, which is a
#' \code{data.frame} with columns \code{'Publication Year'} and
#' \code{'Total Citations'}.  Usually
#' from \code{\link{read.wos}}
#' @param au Author to grab publications
#' @importFrom tools toTitleCase
#' @return A character vector
#' @export
#' @examples
#' wos_file = system.file("sbegueria.txt", package = "bibliometRics")
#' bib <- read.wos(wos_file)
#' base = read.baselines()
#' au = bib$au
#' au = gsub('Ñ', 'N', au)
#' format_pub(cbind(bib$pubs,
#' citation_rank(bib,quant=base$GEOSCIENCES))[1,],
#'  au=au)
format_pub <- function(x, au) {
  # format authors
  if (substr(x['Authors'], nchar(x['Authors']), nchar(x['Authors'])) != '.') {
    x['Authors'] <- paste(x['Authors'], '.', sep = '')
  }
  auths <- strsplit(as.character(x['Authors']), ';')[[1]]
  w <-
    grep(strsplit(au, ',')[[1]][1], auths, ignore.case = TRUE)
  auths[w] <- paste('\\textbf{', auths[w], '}', sep = '')
  auths <- paste0(auths, sep = ';', collapse = '')
  auths <- substr(auths, 1, nchar(auths) - 1)
  # format citations rank
  w <- grep('rank', names(x))
  #	if (is.na(x[,w]) | x[,w]=='') {
  if (is.na(x[w])) {
    rank <- ''
  } else {
    #		rank <- paste('; $',x[,w],'$',sep='')
    rank <- paste('; $', x[w], '$', sep = '')
  }
  # format title
  title <-
    gsub('&', '\\\\&', toTitleCase(tolower(as.character(x['Title']))))
  # format source
  source <- gsub('&', '\\\\&', x['Source Title'])
  # format volume and pages
  if (is.na(x['Volume'])) {
    vol <- ''
  } else {
    vol <- paste(' ', x['Volume'], sep = '')
  }
  if (is.na(x['Issue']) | x['Issue'] == '') {
    issue <- ''
  } else {
    issue <- paste('(', x['Issue'], ')', sep = '')
  }
  if (is.na(x['Beginning Page']) | x['Beginning Page'] == '' |
      is.na(x['Ending Page']) | x['Ending Page'] == '') {
    if (is.na(x['Article Number']) | x['Article Number'] == '') {
      pages <- ''
    } else {
      pages <- paste(': ', x['Article Number'], sep = '')
    }
  } else {
    pages <-
      paste(': ', x['Beginning Page'], '--', x['Ending Page'], sep = '')
  }
  # put everything together
  string = paste(
    '\\item ',
    auths,
    ' ',
    title,
    '. ',
    '\\textit{',
    source,
    '}',
    vol,
    issue,
    pages,
    '. ',
    x['Publication Year'],
    '. ',
    '(cit: ',
    x['Total Citations'],
    rank,
    ')\n',
    sep = ''
  )
  return(string)
}
