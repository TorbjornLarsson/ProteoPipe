# Summary:
# A help function with a pop up GUI to generate a year summary of PTXQC Quality Control of LC_MSMS instrument.
# Intended to be run with local R-3.5.0 installation on User,
# on the ProtePipe folder.

## Load libraries

# Load widget graphics; assumes librarys loaded to the used R
# Object references will be passed by reference frame (so is mutable) 
library(gWidgets2)
# To get the toolkit RGTk2, load library gWidgets2RGtk2 into R and accept installing GTK+:
# install.packages(c("gWidgets2RGtk2", "cairoDevice"))
# library(gWidgets2RGtk2)
options("guiToolkit"="RGtk2")
library(tcltk2)

library(graphics)
library(plyr)
library(dplyr)
library(stringi)


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

currentenv <- environment()

#ProteomicsEndgame <- function(env = parent.frame()) {
ProteomicsEndgame <- function() {
 
  ## GUI Widget
  win <<- gwindow("ProteoPipe", width=900, height=900, visible = TRUE)
  focus(win)
  gr <<- ggroup(container = win, horizontal = FALSE, visible = FALSE)
  
  # With 14 rows: 1 label, 12 months, 1 close button.
  # And 9 columns: 1 label, 8 boxes.
  # glayout needs a widget list for proper, easy attachment.
  lyt <<- glayout(homogeneous = TRUE, spacing = 10, container=gr)
  assign("widgetList", widgetList <- list(), .GlobalEnv)
  
  # First row is labels
  lyt[1,1] <<- widgetList[["w1,1"]] <<- glabel("Month  ", container = lyt)
  font(widgetList[["w1,1"]]) <<- list(family="monospace", weight="bold", size=c(15))

  lapply(n <- 1:8, function(n) {
    lyt[1,n+1] <<- widgetList[[stri_join("w1,", as.character(n+1))]] <<- glabel(n, container = lyt)
    font(widgetList[[stri_join("w1,", as.character(n+1))]]) <<- list(family="monospace", weight="bold", size=c(15))
  })

  # Then 12 month rows
  lapply(month.abb, function(month){
    n <- match(month, month.abb)
    lyt[n+1,1] <<- widgetList[[stri_join("w", as.character(n+1), ",1")]] <<- glabel(month, container = lyt)
    font(widgetList[[stri_join("w", as.character(n+1), ",1")]]) <<- list(family="monospace", weight="bold", size=c(15))

    lapply(m <- 1:8, function(m){
    lyt[n+1,m+1] <- widgetList[[stri_join("w", as.character(n+1), "," , as.character(m+1))]] <<- glabel(stri_unescape_unicode("\\u25a0"), container = lyt) # Unicode Character 'BLACK SQUARE' (U+25A0)
    font(widgetList[[stri_join("w", as.character(n+1), "," , as.character(m+1))]]) <<- list(size=c(28), foreground="gray")
    })
  })

  # Close button
  han1 <- function(..., envir = parent.frame()){
    dispose(win, ...)
  }
  lyt[14,9] <- widgetList[["close"]] <- gbutton("Close", container = lyt, handler = han1)

  ## Defaults
  thatDate <- seq.Date(Sys.Date(), length = 2, by = "-12 months")[2]

  ## Select folders based on current date
  # Assemble a data frame with file paths, years and month
  assign("dirList", dir(dname, pattern = "^[0-9]", full.names = TRUE, ignore.case = TRUE), currentenv)

  # Select first folder 12 months back
  dfGrouped <- data.frame(file_name = dirList, date = as.Date(basename(dirList)))
  assign("dfFiles", dfGrouped[dfGrouped$date >= thatDate, ], currentenv)
  
  ## Assemble list with dates & scores
  directories <- as.character(dfFiles$file_name)
  d <- lapply(directories, function(directory) {

    date <- as.Date(basename(directory))
    
    fileList <- list.files(path = directory)
    heatmapFile <- fileList[grepl("_heatmap.txt", fileList, fixed = TRUE)]
    hmTable <- read.table(paste(directory, heatmapFile, sep="/"), sep="\t", header = TRUE)
    
    # Column 1 is a file reference; if PTXQC ran on several files, a column 6 is added.
    scores <- hmTable[-c(1)] 
    if (dim(scores)[2] > 20) scores <- scores[-c(6)]

    e <- data.frame(
      date = date,
      score = scores,
      stringsAsFactors=FALSE)
    colnames(e) <- c(LETTERS[1:21])
    
    return(e)
    })
  results <- do.call(bind_rows, d)

  ## Convert the list to FAIL/PASS color scores
  d <- apply(results, 1, function(scores) {
    # Strip date
    date <- scores[1]
    scores <- scores[-c(1)]

    if (any(as.numeric(scores) < 0.25)) colorScore <- hsv(0,1,1) # Too low QC score
    else {
      score <- as.numeric(scores[[20]])
      if (score < 0.5) {
        sv <- 2*score   # Black value = [0,1] for score = [0.5, 1]
        colorScore <- hsv(0,1,1-sv) # Red value is fixed
      }
      else {
        sv <- (2-2*score)   # Black value = [1,0] for score = [0.5, 1]
        colorScore <- hsv(0.3,1,1-sv) # Green value is fixed
      }
    }

    e <- data.frame(
      date = date,
      colorScore = colorScore,
      stringsAsFactors=FALSE)
    return(e)
  })
  results <- do.call(bind_rows, d)

  ## Group data by month and color the boxex
  dfGrouped <- mutate(results, month = format(as.Date(date), "%m"))
  for (n in seq(1:12)) {
    dfMonth <- dfGrouped[as.numeric(dfGrouped$month) == n,]
    # Check if data frame has columns but no observations before plotting.
    if (dim(dfMonth)[1] != 0) {
      colorscores <- dfMonth$colorScore
      for (m in seq(1:length(colorscores))){
        assign("widgetList", widgetList, .GlobalEnv) # This works for binding
       font(widgetList[[stri_join("w", as.character(n+1), "," , as.character(m+1))]]) <<- list(size=c(28), foreground=colorscores[[m]])
      }
    }
  }
  
  ## Mark current month
  assign("widgetList", widgetList, .GlobalEnv)
  font(widgetList[[stri_join("w", as.character(as.numeric(format(Sys.Date(), "%m"))+1), ",1")]]) <<- list(weight="bold", size=c(15), style="italic")

} # End call