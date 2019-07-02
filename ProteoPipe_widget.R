#Summary:
#A widget to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.


ProteoPipe_widget<-function(){

  require(stringi)

  # Load widget graphics; assumes librarys loaded to the used R
  require(gWidgets2)
  # To get the toolkit RGTk2, load library gWidgets2RGtk2 into R and accept installing GTK+
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

  # Split screen
  topgr <- ggroup(container = gr, horizontal = FALSE)
  bottomgr <- ggroup(container = gr)
  leftbgr <- ggroup(container = bottomgr, horizontal = FALSE)
  rightbgr <- ggroup(container = bottomgr, horizontal = FALSE) 

  # Select folder textbox and button
  lbl_dname<- glabel("Selected txt folder to run Quality Control on: ", container = topgr)
  txt_dname <- gedit(dname, container = topgr)
  
  h1 <- function(...){
    getwd()
    dname <<- tclvalue(tkchooseDirectory())
    svalue(txt_dname) <- dname
#    dn <<- dname
  }
  b1 <- gbutton("Select folder", container = leftbgr, handler = h1)
  
  # Run button
  h2 <- function(...){
    cat("Running Quality Control on", dname, "\n")
    cat("... please wait...\n\n")
    if (dname != ""){
      y <- list.files(path = dname, pattern = "([0-9A-Za-z]+)[.][y][a][m][l]")
      yp <<- file.path(normalizePath(dname,"/"), y[[1]]) 
      cat("Generating file", yp, "\n")
      yaml_list_object <- yaml.load_file(yp)
      #y <- createReport(svalue(txt_dname), yaml_list_object)
      tryCatch(withCallingHandlers(createReport(svalue(txt_dname), yaml_list_object), 
                            warning=function(w) {
                              write(conditionMessage(w), file=warnings_log, append=TRUE)
                              invokeRestart("muffleWarning")
                                })) 
      #createReport(svalue(txt_dname), yaml_list_object)
    }
    cat("... done!\n\n")
    }
  b2 <- gbutton("Run QC", container = leftbgr, handler = h2)
  
  # html report button
  h3 <- function(...){
    report <- unlist(strsplit(basename(yp), '.yaml'))[1]
    html_file <- file.path(dirname(yp), paste(report, "_combined.html", sep = ""))
    browseURL(html_file)
  }
  b3 <- gbutton("html", container = leftbgr, handler = h3)
  
  # pdf report button
  h4 <- function(...){
    report <- unlist(strsplit(basename(yp), '.yaml'))[1]
    pdf_file <- file.path(dirname(yp), paste(report, "_combined.pdf", sep = ""))
    browseURL(pdf_file)
  }
  b4 <- gbutton("pdf", container = leftbgr, handler = h4)
  
  # # Result image
  # g1 <- gimage(filename = "heatmap.png", dirname = "C:/Users/torla438/Work Folders/Documents/QC/ProteoPipe", container = gr)
  # size(g1) <- c(960,480)
  
  # Quit button
  h5 <- function(...){
    dispose(win)
    quit(save="yes")
  }
  b4 <- gbutton("Quit", container = rightbgr, handler = h5)
  
#  return(dn)
} # end function
