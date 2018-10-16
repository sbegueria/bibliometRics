#' Plot Bibliometrics
#'
#' @param bib A list with an element called  \code{pubs}, which is a
#' \code{data.frame} with columns \code{'Publication Year'} and
#' \code{'Total Citations'}.  Usually
#' from \code{\link{read.wos}}
#' @param quant Quantile, passed to \code{\link{citation_rank}}
#'
#' @return NULL
#' @export
#'
#' @import graphics
#'
#' @examples
#' wos_file = system.file("sbegueria.txt", package = "bibliometRics")
#' bib <- read.wos(wos_file)
#' base = read.baselines()
#' biblioplot(bib, q=base$GEOSCIENCES)
#' bib$author = gsub('Ñ', 'N', bib$author)
biblioplot <- function(bib, quant) {
  opar = par(no.readonly = TRUE)
  on.exit({
    par(opar)
  })
  par(mfrow = c(1, 3))

  au <- bib$author
  dat <- bib$pubs

  # Publication range
  pubrange <- range(dat[, 'Publication Year'])

  # Leading authors
  leads <-
    as.character(lapply(dat[, 'Authors'], function(x) {
      strsplit(x, ';')[[1]][1]
    }))
  w_lead <-
    grep(strsplit(au, ',')[[1]][1], leads, ignore.case = TRUE)

  # Publications and citations
  # pubs per year
  years <- dat[, 'Publication Year']
  years <- factor(years, levels = as.character(min(years):max(years)))
  years_nolead <- years[-w_lead]
  years_lead <- years[w_lead]
  pubs <- rbind(table(years_lead), table(years_nolead))
  rownames(pubs) <- c('pubs_lead', 'pubs_nolead')
  # cumulative pubs
  pubs_cum <- t(apply(pubs, 1, cumsum))
  # citations per year
  cits_lead <-
    t(as.matrix(colSums(bib$pubs[w_lead, 22:ncol(bib$pubs)])))
  cits_nolead <-
    t(as.matrix(colSums(bib$pubs[-w_lead, 22:ncol(bib$pubs)])))
  cits <- rbind(cits_lead, cits_nolead)
  rownames(cits) <- c('cits_lead', 'cits_nolead')
  w <- colnames(cits) >= pubrange[1] & colnames(cits) <= pubrange[2]
  cits <- cits[, w]
  # cumulative citations
  cits_cum <- t(apply(cits, 1, cumsum))
  # plot
  par(mar = c(5, 4, 4, 4) + 0.1)
  ylabs <-
    pretty(c(0, max(max(
      colSums(pubs_cum)
    ), ceiling(max(
      colSums(cits_cum)
    ) / 10))))
  ylims <- c(min(ylabs), max(ylabs))
  pubbar <- barplot(pubs_cum,
                    ylim = ylims,
                    ylab = '',
                    yaxt = 'n')
  abline(h = ylabs, col = 'lightgray', lty = 'dotted')
  par(new = TRUE)
  pubbar <- barplot(pubs_cum,
                    ylim = ylims,
                    ylab = '',
                    yaxt = 'n')
  lines(x = pubbar, y = cits_cum['cits_lead', ] / 10)
  points(x = pubbar, y = cits_cum['cits_lead', ] / 10, pch = 19)
  lines(x = pubbar, y = colSums(cits_cum) / 10)
  points(
    x = pubbar,
    y = colSums(cits_cum) / 10,
    pch = 19,
    col = 'lightgray'
  )
  points(x = pubbar,
         y = colSums(cits_cum) / 10,
         pch = 21)
  axis(2)
  axis(4, at = pretty(ylims), labels = pretty(ylims) * 10)
  mtext('Pubs', 2, line = 2.5, cex = 0.75)
  mtext('Citations', 4, line = 2.5, cex = 0.75)

  # h- and g-index
  years <-
    c(min(dat[, 'Publication Year']):max(dat[, 'Publication Year']))
  hh <- NULL
  gg <- NULL
  for (y in years) {
    hh <- c(hh, hirsch(bib, y))
    gg <- c(gg, egghe(bib, y))
  }
  # plot
  ylabs <- pretty(c(0, max(max(hh), ceiling(max(
    gg
  ) / 2))))
  ylims <- c(min(ylabs), max(ylabs))
  plot(
    x = years,
    y = hh,
    type = 'n',
    ylab = '',
    xlab = '',
    ylim = ylims
  )
  abline(h = pretty(hh), col = 'lightgray', lty = 'dotted')
  lines(years, 0:{
    length(years) - 1
  }, lty = 'dashed')
  lines(x = years, y = hh)
  points(x = years, y = hh, pch = 19)
  mtext('h-index', 2, line = 2.5, cex = 0.75)
  par(new = TRUE)
  lines(x = years, y = gg / 2)
  points(
    x = years,
    y = gg / 2,
    pch = 19,
    col = 'lightgray'
  )
  points(x = years, y = gg / 2, pch = 21)
  axis(4, at = pretty(ylims), labels = pretty(ylims) * 2)
  mtext('g-index', 4, line = 2.5, cex = 0.75)

  # Excelence
  exc <-
    rbind(rev(table(
      citation_rank(bib, w_lead, quant = quant)
    )), rev(table(
      citation_rank(bib,-w_lead, quant = quant)
    )))[, -7]
  rownames(exc) <- c('pubs_lead', 'pubs_nolead')
  # plot
  barplot(exc)
  abline(h = pretty(colSums(exc)),
         col = 'lightgray',
         lty = 'dotted')
  par(new = TRUE)
  barplot(exc)
  mtext('Counts', 2, line = 2.5, cex = 0.75)
}
#biblioplot(bib)

#' @rdname biblioplot
#' @export
biblioplot2 <- function(bib) {
  opar = par(no.readonly = TRUE)
  on.exit({
    par(opar)
  })
  par(mfrow = c(1, 2))

  au <- bib$author
  dat <- bib$pubs

  # Publication range
  pubrange <- range(dat[, 'Publication Year'])

  # Leading authors
  leads <-
    as.character(lapply(dat[, 'Authors'], function(x) {
      strsplit(x, ';')[[1]][1]
    }))

  # Publications and citations
  # pubs per year
  years <- dat[, 'Publication Year']
  years <- factor(years, levels = as.character(min(years):max(years)))
  pubs <- table(years)
  # cumulative pubs
  pubs_cum <- cumsum(pubs)
  # citations per year
  cits <- t(as.matrix(colSums(bib$pubs[, 22:ncol(bib$pubs)])))
  rownames(cits) <- c('cits')
  w <- colnames(cits) >= pubrange[1] & colnames(cits) <= pubrange[2]
  cits <- cits[, w]
  # cumulative citations
  cits_cum <- cumsum(cits)
  # plot
  par(mar = c(5, 4, 4, 4) + 0.1)
  ylabs <- pretty(c(0, max(max(pubs_cum), ceiling(max(
    cits_cum
  ) / 10))))
  ylims <- c(min(ylabs), max(ylabs))
  pubbar <- barplot(pubs_cum,
                    ylim = ylims,
                    ylab = '',
                    yaxt = 'n')
  abline(h = ylabs, col = 'lightgray', lty = 'dotted')
  par(new = TRUE)
  pubbar <- barplot(pubs_cum,
                    ylim = ylims,
                    ylab = '',
                    yaxt = 'n')
  lines(x = pubbar, y = cits_cum / 10)
  points(x = pubbar, y = cits_cum / 10, pch = 19)
  axis(2)
  axis(4, at = pretty(ylims), labels = pretty(ylims) * 10)
  mtext('Pubs', 2, line = 2.5, cex = 0.75)
  mtext('Citations', 4, line = 2.5, cex = 0.75)
  title('Publications and citations')

  # h-index
  years <-
    c(min(dat[, 'Publication Year']):max(dat[, 'Publication Year']))
  hh <- NULL
  for (y in years) {
    hh <- c(hh, hirsch(bib, y))
    gg <- c(gg, egghe(bib, y))
  }
  # plot
  ylabs <- pretty(c(0, max(max(hh))))
  ylims <- c(min(ylabs), max(ylabs))
  plot(
    x = years,
    y = hh,
    type = 'n',
    ylab = '',
    xlab = '',
    ylim = ylims
  )
  abline(h = pretty(hh), col = 'lightgray', lty = 'dotted')
  lines(years, 0:{
    length(years) - 1
  }, lty = 'dashed')
  lines(x = years, y = hh)
  points(x = years, y = hh, pch = 19)
  mtext('h-index', 2, line = 2.5, cex = 0.75)
  title('h-index')
  return(invisible(NULL))
}



