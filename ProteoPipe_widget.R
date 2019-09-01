#Summary:
#A widget to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.


ProteoPipe_widget<-function(env = parent.frame()){
  
  require(stringi)

  # Load widget graphics; assumes librarys loaded to the used R
  # Object references will be passed by reference frame (so is mutable) 
  require(gWidgets2)
  # To get the toolkit RGTk2, load library gWidgets2RGtk2 into R and accept installing GTK+
  options("guiToolkit"="RGtk2")
  require(tcltk2)
  
  # Load Quality Control methods
  require(PTXQC)
  require(yaml)
  require(methods)
  
  # Load image methods
  require(EBImage)
  require(pdftools)
  
  cat("-------------------------------------\n")
  cat("ProteoPipe v1 [June, 2019]\n")
  cat("Uppsala University\n")
  cat("Niklas Handin, et al.\n")
  cat("-------------------------------------\n")
  
  # Defaults
  currentenv <- environment()
  dname <- ""

  #------------------Calling
  
  # GUI Widget
  win <- gwindow("ProteoPipe")
  gr <- ggroup(container = win, horizontal = FALSE)
  
  # Split screen
  topgr <- ggroup(container = gr, horizontal = FALSE) # Used for folder select
  middlegr <- ggroup(container = gr) # Used for button groups
  leftbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for setup & quit buttons
  rightbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for run & results buttons
  bottomgr <- ggroup(container = gr,  horizontal = FALSE, expand = T) # Used for result images
  
  img1 <- gpanedgroup(container = bottomgr)

  img1left <- ggraphics(container = img1);
  addHandlerChanged(img1left, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(imraw, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
    dev.set(img1left);plot(imscaled)
   })
  img1left <- dev.cur()
  
  img1right <- ggraphics(container = img1);
  addHandlerChanged(img1right, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(imraw, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
    dev.set(img1right);plot(imscaled)
  })
  img1right <- dev.cur()

  img2 <- gpanedgroup(container = bottomgr)
  
  img2left <- ggraphics(container = img2);
  addHandlerChanged(img2left, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(imraw, w=gWidgets2::size(img2)[1], h=gWidgets2::size(img2)[2])
    dev.set(img2left);plot(imscaled)
  })
  img2left <- dev.cur()
  
  img2right <- ggraphics(container = img2);
  addHandlerChanged(img2right, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(imraw, w=gWidgets2::size(img2)[1], h=gWidgets2::size(img2)[2])
    dev.set(img2right);plot(imscaled)
  })
  img2right <- dev.cur()
  
  visible(middlegr) <- TRUE # Button groups
  visible(rightbgr) <- FALSE # Run button group
  visible(bottomgr) <- TRUE # Images group
  
  # Select folder textbox and button
  lbl_dname<- glabel("Selected txt folder to run Quality Control on: ", container = topgr)
  txt_dname <- gedit(dname, container = topgr)

  h1 <- function(...,  envir = parent.frame()){
    getwd()
    assign("dname", tk_choose.dir(), currentenv)
    svalue(txt_dname) <- dname
    
    #Change visibilities
    visible(bottomgr) <- FALSE # Previous result images
    visible(rightbgr) <- TRUE # Run button group
    visible(b1) <- FALSE #Folder button
    visible(b2) <- TRUE  #Run button
    visible(b3) <- FALSE #html button
    visible(b4) <- FALSE #pdf button
  }
  b1 <- gbutton("Select folder", container = topgr, handler = h1)
  
  # Run button
  h2 <- function(..., envir = parent.frame()){
    cat("Running Quality Control on", dname, "\n")
    
    if (dname != ""){
      cat("... please wait...\n\n")
      
      # Default is provided yaml object, but use a provided one if in folder,
      # else let PTXQC generate one.
      file_list <- list.files(path = dname)
      yaml_list <- file_list[grepl(".yaml", file_list)]
      
      # Use first in list
      #if (length(y) > 0) {
      if (length(yaml_list) > 0) {
        yaml_path <- file.path(normalizePath(dname,"/"), yaml_list[[1]])
        cat("Generating file", yaml_path, "\n")
        yaml_list_object <- yaml.load_file(yaml_path)
      } else if(length(fname) > 0) {
        cat("Generating file", fname, "\n")
        yaml_list_object <- yaml.load_file(fname)
      }
      else {yaml_list_object <- list()}
      
      # Report will generate lots of warnings that we want to catch to warnings log file.
      # Call handler for each warning as they come, to reenter try/catch loop.
      tryCatch(withCallingHandlers(assign("r", createReport(svalue(txt_dname), yaml_list_object), envir=globalenv()),
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
      # Plot it through a temporary file
      bitmap <- pdf_render_page(r$report_file_PDF, page = 1, dpi = 300, antialias = "text")
      png::writePNG(bitmap, assign("tf", tempfile(fileext = ".png")))
      assign("imraw", readImage(tf), currentenv)

      dev.set(img1left);plot(imraw)
      png(file=(assign("tf", tempfile(fileext = ".png"), currentenv)), bg = "transparent")
      
      dev.set(img1right);plot(imraw)
      png(file=(assign("tf", tempfile(fileext = ".png"), currentenv)), bg = "transparent")

      dev.set(img2left);plot(imraw)
      png(file=(assign("tf", tempfile(fileext = ".png"), currentenv)), bg = "transparent")
      
      dev.set(img2right);plot(imraw)
      png(file=(assign("tf", tempfile(fileext = ".png"), currentenv)), bg = "transparent")
      
      #Change visibilities
      visible(bottomgr) <- TRUE # New images
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
  b3 <- gbutton("html", container = rightbgr, handler=function(...) browseURL(r$report_file_HTML))

  # pdf report button
  b4 <- gbutton("pdf", container = rightbgr, handler=function(...) browseURL(r$report_file_PDF))
  
  # Quit button
  h5 <- function(..., envir = parent.frame()){
    rm(tf)
    dev.off()
    dispose(win, ...)
    quit(save="yes")
  }
  b5 <- gbutton("Quit", container = leftbgr, handler = h5)
  
  # Select .yaml file button
  h6 <- function(..., envir = parent.frame()){
    #Return list of file, perhaps empty
    Filters <- matrix(c("yaml", ".yaml",
                        "yaml", ".yaml"),
                      2, 2, byrow = TRUE)
    assign("fname", tk_choose.files(default = "", caption = "Select .yaml file",
                                                  multi = FALSE, filters = Filters, index = 1), envir=currentenv)
  }
  b6 <- gbutton("Select .yaml file", container = leftbgr, handler = h6)
  
} # end Calling
