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
  
  # Load html, image methods
  require(XML)
  require(base64enc)
  require(EBImage)

  cat("-------------------------------------\n")
  cat("ProteoPipe v1 [June, 2019]\n")
  cat("Uppsala University\n")
  cat("Niklas Handin, et al.\n")
  cat("-------------------------------------\n")
  
  # Default directory
  dname <- ""
  
  # Default white image put into temp file
  plot.new()
  rect(0, 0, 480, 240, border = NA)
  png(file=(tf <- tempfile(fileext = ".png")), bg = "transparent")
  dev.off()

  #------------------Calling
  
  # GUI Widget
  win <- gwindow("ProteoPipe")
  gr <- ggroup(container = win, horizontal = FALSE)

  # Split screen
  topgr <- ggroup(container = gr, horizontal = FALSE) # Used for folder select
  middlegr <- ggroup(container = gr) # Used for button groups
  bottomgr <- ggroup(container = gr) # Used for result images
  g1 <<- gimage(tf, container = bottomgr) # Image reference needs to be global in GTK
  size(g1) <- c(480, 240)
  leftbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for setup & quit buttons
  rightbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for run & results buttons
  visible(middlegr) <- TRUE # Button groups
  visible(rightbgr) <- FALSE # Run button group
  visible(bottomgr) <- TRUE # Images group
  
  # Select folder textbox and button
  lbl_dname<- glabel("Selected txt folder to run Quality Control on: ", container = topgr)
  txt_dname <- gedit(dname, container = topgr)
  
  h1 <- function(...){
    getwd()
    dname <<- tk_choose.dir()
    svalue(txt_dname) <- dname
    
    #Change visibilities
    delete(bottomgr, g1) # Previous result images
    visible(rightbgr) <- TRUE # Run button group
    visible(b1) <- FALSE #Folder button
    visible(b2) <- TRUE  #Run button
    visible(b3) <- FALSE #html button
    visible(b4) <- FALSE #pdf button
  }
  b1 <- gbutton("Select folder", container = topgr, handler = h1)
  
  # Run button
  h2 <- function(...){
      cat("Running Quality Control on", dname, "\n")
        
    if (dname != ""){
      cat("... please wait...\n\n")
      
      # Default is provided yaml object, but use a provided one if in folder,
      # else let PTXQC generate one.
      y <- list.files(path = dname, pattern = "([0-9A-Za-z]+)[.][y][a][m][l]")
      
      # Use first in list
      if (length(y) > 0) {
        yp <<- file.path(normalizePath(dname,"/"), y[[1]])
        cat("Generating file", yp, "\n")
        yaml_list_object <- yaml.load_file(yp)
      } else if(length(fname) > 0) {
        cat("Generating file", fname, "\n")
        yaml_list_object <- yaml.load_file(fname)
      }
      else {yaml_list_object <- list()}

      # Report will generate lots of warnings that we want to catch to warnings log file.
      # Call handler for each warning as they come, to reenter try/catch loop.
      tryCatch(withCallingHandlers(r <<- createReport(svalue(txt_dname), yaml_list_object), 
                          # Warning object seems to have abbreviation 'w'
                          # W is a single warning list object when run with try/catch calling handler
                          warning=function(w) {
                            # Note: conditionMessage(w) does that too
                            write(capture.output(cat("createReport() warning:", conditionMessage(w), 
                                                     "\n")), file=warnings_log, append=TRUE)
                            invokeRestart("muffleWarning")
                                }))

      cat("... QC report finalized!\n\n")
      
      # Generate result heatmap image from report files
      #bottomgr <- plot_heatmap(r, bottomgr)

      # Parse the html file and extract the heatmap node data
      doc =  htmlParse(r$report_file_HTML)
      src <-  xpathApply(doc, "//div[@id=\"heatmap\"]//img", xmlGetAttr, "src")

      # Convert the png base64 image data through a temporary file connection
      base64 <- sub("data:image/png;base64,", "", unlist(src), fixed=TRUE)
      bin <- base64decode(base64)
      fileConn<-file(tf <- tempfile(fileext = ".png"), "wb")
      writeBin(bin, fileConn)
      close(fileConn)

      # Resize the image and plot it through a temporary file
      # Resizing is hardcoded for now since that works; graphics bug?
      imraw <- readImage(tf)
      imscaled <- resize(imraw, w = 480, h = 240)
      png(file=(tf <- tempfile(fileext = ".png")), bg = "transparent")
      plot(imscaled)
      dev.off()

      g1 <<- gimage(tf) # Image reference needs to be global in GTK
      size(g1) <- c(480, 240)
      add(bottomgr, g1)


      #Change visibilities
      visible(rightbgr) <- TRUE # Button group
      visible(b1) <- TRUE #Folder button
      visible(b2) <- FALSE #Run button
      visible(b3) <- TRUE #html button
      visible(b4) <- TRUE #pdf button
      visible(b5) <- TRUE #Quit button; always visible
      visible(b6) <- TRUE  #.yaml button; always visible
    }
  }
  
  b2 <- gbutton("Run QC", container = rightbgr, handler = h2)

  # html report button
  h3 <- function(...){
    browseURL(r$report_file_HTML)
  }
  b3 <- gbutton("html", container = rightbgr, handler = h3)
  
  # pdf report button
  h4 <- function(...){
    browseURL(r$report_file_PDF)
  }
  b4 <- gbutton("pdf", container = rightbgr, handler = h4)
  
  # Quit button
  h5 <- function(...){
    dispose(win, ...)
    quit(save="yes")
  }
  b5 <- gbutton("Quit", container = leftbgr, handler = h5)
  
  # Select .yaml file button
  h6 <- function(...){
    #Return list of file, perhaps empty
    Filters <- matrix(c("yaml", ".yaml",
                        "yaml", ".yaml"),
                      2, 2, byrow = TRUE)
    fname <<- tk_choose.files(default = "", caption = "Select .yaml file",
                              multi = FALSE, filters = Filters, index = 1)
  }
  b6 <- gbutton("Select .yaml file", container = leftbgr, handler = h6)
  
  
} # end Calling

# ###################
# 
# # Generate result heatmap image from report files
# plot_heatmap <- function(r, bottomgr){
# 
#   # Parse the html file and extract the heatmap node data
#   doc =  htmlParse(r$report_file_HTML)
#   src <-  xpathApply(doc, "//div[@id=\"heatmap\"]//img", xmlGetAttr, "src")
# 
#   # Convert the png base64 image data through a temporary file connection
#   base64 <- sub("data:image/png;base64,", "", unlist(src), fixed=TRUE)
#   bin <- base64decode(base64)
#   fileConn<-file(tf <- tempfile(fileext = ".png"), "wb")
#   writeBin(bin, fileConn)
#   close(fileConn)
# 
#   # Resize the image and plot it through a temporary file
#   # Resizing is hardcoded for now since that works; graphics bug?
#   imraw <- readImage(tf)
#   imscaled <- resize(imraw, w = 480, h = 240)
#   png(file=(tf <- tempfile(fileext = ".png")), bg = "transparent")
#   plot(imscaled)
#   dev.off()
# 
#   g1 <<- gimage(tf) # Image reference needs to be global in GTK
#   size(g1) <- c(480, 240)
#   add(bottomgr, g1)
# 
#   # return()
# #  return_list <- list(bottomgr, ...)
# #  return(return_list)
#   return(bottomgr)
# 
# } # end Generating heatmap image
