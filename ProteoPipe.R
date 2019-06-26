#Summary:
#A widget to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.


#ProteoPipe<-function(){

  require(stringi)

  # Load widget graphics
  require(gWidgets2)
  options("guiToolkit"="RGtk2")
  require(tcltk2)
  
  # Load Quality Control methods
  require(PTXQC)
  require(yaml)
  require(methods)

  cat("-------------------------------------\n")
  cat("ProteoPipe v1 [June, 2019]\n")
  cat("Uppsala University\n")
  cat("Niklas Handin, et al.\n")
  cat("-------------------------------------\n")
  
  dname <- ""

  #------------------Calling
  
  # GUI Widget
  win <- gwindow("ProteoPipe")
  gr <- ggroup(container = win, horizontal = FALSE)
  
  # Select folder textbox and button
  lbl_dname<- glabel("Folder to run Quality Control on: ", container = gr)
  txt_dname <- gedit(dname, container = gr)
  h1 <- function(...){
    setwd('~')
    getwd()
    dname <<- tclvalue(tkchooseDirectory())
    svalue(txt_dname) <- dname
  }
  b1 <- gbutton("Select folder", container = gr, handler = h1)
  
  # Run button
  h2 <- function(...){
    cat("Running Quality Control on", dname, "\n")
    cat("... please wait until result Time elapsed is given.\n")
    if (dname != ""){
      y <- list.files(path = dname, pattern = "([0-9A-Za-z]+)[.][y][a][m][l]")
      p <- file.path(normalizePath(dname,"/"), y[[1]]) 
      print(p)
      yaml_list_object <- yaml.load_file(p)
      r <- createReport(svalue(txt_dname), yaml_list_object)
      }
    }
  b2 <- gbutton("Start run", container = gr, handler = h2)
  
  # Quit button
  h3 <- function(...){
    dispose(win)
    cat("Done!\n")
  }
  b3 <- gbutton("Quit", container = gr, handler = h3)
  
  # Quick fix to keep running in background on target machine
  # Comment away during other testing
  Sys.sleep(1000000)
  
#} # end function