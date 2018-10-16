#' Read field percentile baselines (FPB) from the Essential Science
#' Indicators database.
#'
#' @param infile Baseline percentages for each field
#'
#' @importFrom utils read.table
#' @return A \code{list} of percentages
#' @export
#'
#' @examples
#' res = read.baselines()
read.baselines <-
  function(infile = system.file("BaselinePercentiles.csv",
                                package = "bibliometRics")) {
    fpb <- read.table(
      infile,
      sep = ',',
      skip = 1,
      stringsAsFactors = FALSE,
      header = TRUE
    )
    fields <- fpb$RESEARCH.FIELDS[-grep('%|©', fpb$RESEARCH.FIELDS)]
    baselines <- list(NULL)
    for (i in 1:length(fields)) {
      w <- which(fpb$RESEARCH.FIELDS == fields[i])
      baselines[[i]] <- fpb[w + 1:6, ]
      colnames(baselines[[i]])[1] <- 'percentiles'
      names(baselines)[i] <- fields[i]
    }
    return(baselines)
  }
