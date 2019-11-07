# Summary:
# A wrapper to generate a summary of PTXQC Quality Control of LC_MSMS instrument.
# Intended to be run with local R-3.5.0 installation on User,
# on the ProtePipe folder.
# Target: F:\ProteoPipe\Proteomics_Endgame.R"
# Start in: "C:\Program Files\R\R-3.5.0\bin"
# Input arguments:
# None, start date, stop date.
# Output:
# File with pass/fail values summary table. Bar plot of summary table.
# Reference: HeLa_QC_20190823_20190823120456.raw, "Average~Overall~Quality" 0.900145967423899.

library(graphics)
devAskNewPage(ask = FALSE)

library(dplyr)

## Defaults

# Installation directory
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

dname <- dirname(thisFile())

logo <- c("-------------------------------------",
          "Proteomics Endgame v1 [November, 2019]",
          "Uppsala University",
          "Niklas Handin, et al.",
          "-------------------------------------")

currentenv <- environment()
graphics.off()

ProteomicsEndgame <- function(startDir, stopDir) {

  ## User info
  writeLines(logo)
  cat("Arguments can be start and stop directories; default is using first to last.\n")
  cat("To read in paths from a File select, copy and use 'x <- readline()' for correct format.\n")
  
  ## Parse arguments
  if (missing(startDir) || missing(stopDir)) {
    assign("dirList", dir(dname, pattern = "^[0-9]", full.names = TRUE, ignore.case = TRUE), currentenv)
    len <- length(dirList)
  }

  # Select first folder in date order
  if (missing(startDir)) {
    if(len != 0) {
      assign("startdir", dirList[[1]], currentenv)
    }
  } else assign("startdir", normalizePath(startDir, winslash = "/"), currentenv)

  # Select last folder in date order
  if (missing(stopDir)) {
    if(len != 0) {
      assign("stopdir", dirList[[len]], currentenv)
    }
  } else assign("stopdir", normalizePath(stopDir, winslash = "/"), currentenv)
  
  directories <- dirList[(grep(startdir, dirList, value = FALSE):grep(stopdir, dirList, value = FALSE))]
  
  ## Assemble list with dates & Average Overall Quality scores
  results <- lapply(directories, function(directory) {
    
    fileList <- list.files(path = directory)
    heatmapFile <- fileList[grepl("_heatmap.txt", fileList, fixed = TRUE)]
    hmTable <- read.table(paste(directory, heatmapFile, sep="/"), sep="\t", header = TRUE)
    
    date <- basename(directory)
    AOQscore <- hmTable$Average.Overall.Quality
    
    data.frame(
      date = date, 
      AOQscore = AOQscore,
      stringsAsFactors=FALSE)
  })
  results <- do.call(rbind, results)
  
  ## Plot simple bar plot with height and color accordingly
  ## We don't use the ProtePipe fail level for an unbiased comparison.
  
  barColors <- lapply(results$AOQscore, function(score) {
    if (score < 0.5) {
        sv <- 2*score   # Black value = [0,1] for score = [0.5, 1]
        return(hsv(0,1,1-sv)) # Red value is fixed
    }
    else {
        sv <- (2-2*score)   # Black value = [1,0] for score = [0.5, 1]
        return(hsv(0.3,1,1-sv)) # Green value is fixed
      }
  })

  barplot(results$AOQscore, axes=FALSE, col=as.character(barColors))

  ## Group data by year and month and make bar plots
  df <- data.frame(date = as.Date(results$date), color = unlist(barColors))
  dfGrouped <- mutate(df, year = format(date, "%Y"), month = format(date, "%m"))

  #par(mfrow = c(length(min(dfGrouped$year):max(dfGrouped$year))*12, 1))
  # par(mar=rep(0,4)) # Margins off
  # par(mfrow = c(12,1)) # With margins off
  
  # par(mfrow = c(4,1)) # With margins
  
  for (year in seq(min(dfGrouped$year):max(dfGrouped$year))) {
    dfYear <- dfGrouped[dfGrouped$year == year,]
    for (month in seq(1:12)) {
      dfMonth <- dfYear[dfYear$month == month,]
      
      # Check if data frame has columns but no observations before plotting.
      if (dim(dfMonth)[1] != 0) {
        print(dfMonth)
        dev.new()
        barplot(rep(1, length(dfMonth$color)), names.arg = dfMonth$date, axes=FALSE, col=as.character(dfMonth$color))
      }
    }
  }

  return(results)
  
} # End call