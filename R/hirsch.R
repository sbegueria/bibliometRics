
#' Calculate Hirsch's h-index or Egghe's g-index
#'
#' @param bib A list with an element called  \code{pubs}, which is a
#' \code{data.frame} with column \code{'Publication Year'}.  Usually
#' from \code{\link{read.wos}}
#' @param year Maximum year to include for calculation
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
#' ifactor(bib) # 2-year impact factor
#' ifactor(bib,2013) # 2-year impact factor, year 2013
#' ifactor(bib,n=5)  # 5-year impact factor#'
hirsch <- function(bib, year = NULL) {
  x <- bib$pubs
  if (is.null(year)) {
    #y <- max(x[,'Publication Year'])
    year <- rev(colnames(x))[1]
  }
  w <- which(x[, 'Publication Year'] <= year)
  ww <- which(colnames(x) <= year)
  if (length(ww) > 1) {
    cit <- rowSums(x[w, ww])
  } else {
    cit <- x[w, ww]
  }
  #cit <- x[,'Total Citations']
  hin <- cbind(cit[order(cit, decreasing = TRUE)], 1:length(cit))
  w <- (hin[, 1] - hin[, 2]) > 0
  if (sum(w) == 0)
    return(0)
  return(max(hin[w, 2]))
}


#' @export
#' @rdname hirsch
egghe <- function(bib, year = NULL) {
  x <- bib$pubs
  if (is.null(year)) {
    year <- max(x[, 'Publication Year'])
  }
  w <- x[, 'Publication Year'] <= year
  cit <- x[w, 'Total Citations']
  gin <- cbind(1:length(cit),
               cumsum(cit[order(cit, decreasing = TRUE)]),
               (1:length(cit)) ^ 2)
  w <- (gin[, 2] - gin[, 3]) > 0
  return(max(gin[w, 1]))
}


#' @export
#' @rdname hirsch
#' @param n number of years
ifactor <- function(bib, year = NULL, n = 2) {
  x <- bib$pubs
  all_years <- x[, 'Publication Year']
  if (is.null(year)) {
    year <- max(all_years, na.rm = TRUE) - 1
  }
  w <- which(all_years >= (year - n) & all_years < year)
  ww <- which(colnames(x) == year)
  sum(x[w, ww]) / length(w)
}
