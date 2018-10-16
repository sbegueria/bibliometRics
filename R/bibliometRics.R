



# read.scopus <- function(infile) {

	# # author
	# au <- read.table(infile,sep=',',nrows=1,stringsAsFactors=FALSE)[,2]
	# au <- substr(au,9,nchar(au))

	# # read publications
	# dat <- read.table(infile,sep=',',stringsAsFactors=FALSE,skip=7)[,c(1:7,9,12)]
	# colnames(dat) <- c('year','title','authors','issn','journal','volume','issue','cit','cittotal')

	# return()
# }
# bib <- read.scopus('scopus_SBP_win.csv')


#' Calculate Bibliometrics
#'
#' @param bib A list with an element called  \code{pubs}, which is a
#' \code{data.frame} with columns \code{'Publication Year'} and
#' \code{'Total Citations'}.  Usually
#' from \code{\link{read.wos}}
#' @param base Baseline percentages
#' @param discipline Name of Discipline, must be in column names of
#' \code{base}
#'
#' @return A \code{data.frame} of metric
#' @export
#'
#' @examples
#' wos_file = system.file("sbegueria.txt", package = "bibliometRics")
#' bib <- read.wos(wos_file)
#' bib$author = gsub('[ñÑ]', 'N', bib$author)
#' bib$pubs$Authors = gsub('[ñÑ]', 'N', bib$pubs$Authors)
#'
#' base = read.baselines()
#' discipline = 'GEOSCIENCES'
#' bibliometric(bib, base, 'GEOSCIENCES')
bibliometric <-
  function(bib,
           base = read.baselines(),
           discipline = NULL) {
    if (!is.null(discipline)) {
      w <- grep(discipline, names(base))
    } else {
      w <- 1
    }
    quant <- base[[w]]

    au <- bib$author
    dat <- bib$pubs

    # publications
    pubs <- nrow(dat)

    # publications as lead author
    leads <-
      as.character(lapply(dat[, 'Authors'], function(x) {
        strsplit(x, ';')[[1]][1]
      }))
    auu <- strsplit(au, ',')[[1]][1]
    pubs_lead <- grep(auu, leads, ignore.case = TRUE)
    #	pubs_lead <- length(grep(auu,leads,ignore.case=TRUE))

    # impact factor
    ifact2 <- round(ifactor(bib, n = 2), 2)
    ifact5 <- round(ifactor(bib, n = 5), 2)

    # citations
    cit_tot <- sum(dat[, 'Total Citations'])
    cit_max <- max(dat[, 'Total Citations'])
    cit_art <- round(mean(dat[, 'Total Citations']), 2)
    ini <- min(dat[, 'Publication Year'])
    span <- max(dat[, 'Publication Year']) - ini

    # number of pubs with n citations
    icit <- function(x, n = 10) {
      length(which(x[, 'Total Citations'] >= n))
    }
    i10 <- icit(dat)
    i25 <- icit(dat, 25)
    i50 <- icit(dat, 50)

    # h-index
    hin <- hirsch(bib)

    # g-index
    gin <- egghe(bib)

    # pubs >0.9, >0.99
    r <- citation_rank(bib, quant = quant)
    p09 <- sum(as.numeric(r) <= 4, na.rm = TRUE)
    p099 <- sum(as.numeric(r) <= 3, na.rm = TRUE)

    # pubs >0.9, as lead author
    r_lead <- citation_rank(bib, pubs_lead, quant)
    p09_lead <- sum(as.numeric(r_lead) <= 4, na.rm = TRUE)

    # i-score
    scores <-
      c(1 / (1 - 0.9999),
        1 / (1 - 0.999),
        1 / (1 - 0.99),
        1 / (1 - 0.9),
        1 / (1 - 0.8),
        1 / (1 - 0.5),
        1 / 1)
    iscore <- sum(scores[as.numeric(r)], na.rm = TRUE)

    # i-score, as lead author
    isc_lead <- sum(scores[as.numeric(r_lead)], na.rm = TRUE)

    # output
    out <- as.data.frame(t(
      c(
        name = au,
        ini,
        span,
        pubs,
        length(pubs_lead),
        round(pubs / span, 2),
        hin,
        round(hin / span, 2),
        gin,
        round(gin / span, 2),
        cit_tot,
        round(cit_tot / span, 2),
        cit_art,
        ifact2,
        ifact5,
        i10,
        i25,
        i50,
        cit_max,
        p09,
        p09_lead,
        p099,
        round(iscore, 2),
        round(isc_lead, 2)
      )
    ))
    colnames(out) <-
      c(
        'name',
        'ini',
        'years',
        'pubs',
        'lead',
        'pubs_year',
        'hin',
        'hin_year',
        'gin',
        'gin_year',
        'cit_tot',
        'cit_year',
        'cit_art',
        'ifact2',
        'ifact5',
        'i10',
        'i25',
        'i50',
        'cit_max',
        'pubs09',
        'pubs09_lead',
        'pubs099',
        'iscore',
        'iscore_lead'
      )
    return(out)
  }
#bibliometric(bib, baseline)








