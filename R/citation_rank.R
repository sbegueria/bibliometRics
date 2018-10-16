
#' Calculate citation ranking (quantiles)
#'
#' @param x A list with an element called  \code{pubs}, which is a
#' \code{data.frame} with columns \code{'Publication Year'} and
#' \code{'Total Citations'}.  Usually
#' from \code{\link{read.wos}}
#' @param w column indicators of which quantiles to grab
#' @param quant A \code{data.frame}? maybe
#'
#' @return A scalar number
#' @export
#'
#' @examples
#' wos_file = system.file("sbegueria.txt", package = "bibliometRics")
#' bib <- read.wos(wos_file)
#' hirsch(bib)
#' hirsch(bib, 2010)
#' egghe(bib)
#' egghe(bib, 2010)
#' base = read.baselines()
#' citation_rank(bib, quant=base$GEOSCIENCES)
citation_rank <- function(x, w = NULL, quant = NULL) {
  x <- x$pubs
  if (!is.null(w)) {
    x <- x[w, ]
  }
  r <-
    data.frame(a = x[, 'Publication Year'],
               b = x[, 'Total Citations'],
               c = '',
               stringsAsFactors = FALSE)
  r$c <-
    factor(r$c,
           levels = c(
             '>q0.9999',
             '>q0.999',
             '>q0.99',
             '>q0.9',
             '>q0.8',
             '>q0.5',
             '>q0'
           ))
  for (i in 1:nrow(r)) {
    w <- grep(r[i, 1], names(quant))
    if (length(w) == 0)
      next()
    ww <- which(r[i, 2] - quant[, w] >= 0)[1]
    if (is.na(ww)) {
      r[i, 3] <- levels(r$c)[7]
    } else {
      r[i, 3] <- levels(r$c)[ww]
    }
  }
  return(r[, 3])
}
#citation_rank(bib, c(9,10), quant)
#citation_rank(bib, quant)
