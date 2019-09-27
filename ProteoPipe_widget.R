#Summary:
#A widget to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.
#Use require to put package info into the log file.


ProteoPipe_widget<-function(env = parent.frame()){
  
  library(stringi)
  
  # Load widget graphics; assumes librarys loaded to the used R
  # Object references will be passed by reference frame (so is mutable) 
  library(gWidgets2)
  # To get the toolkit RGTk2, load library gWidgets2RGtk2 into R and accept installing GTK+
  options("guiToolkit"="RGtk2")
  library(tcltk2)
  
  # Load Quality Control methods
  library(PTXQC)
  library(yaml)
  library(methods)
  
  # Load image methods
  library(png)
  library(EBImage)
  library(pdftools)
  library(mzR)
  library(MSnbase)
  
  cat("-------------------------------------\n")
  cat("ProteoPipe v1 [June, 2019]\n")
  cat("Uppsala University\n")
  cat("Niklas Handin, et al.\n")
  cat("-------------------------------------\n")
  
  # Defaults
  gcinfo(FALSE)
  currentenv <- environment()
  dname <- ""

  #------------------Calling
  
  # GUI Widget
  win <- gwindow("ProteoPipe") # Cannot make this invisible and get the GTK graphics mounted correctly
  gr <- ggroup(container = win, horizontal = FALSE, visible = FALSE)

  # Split screen
  topgr <- ggroup(container = gr, horizontal = FALSE) # Used for folder select
  middlegr <- ggroup(container = gr) # Used for button groups
  leftbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for setup & quit buttons
  rightbgr <- ggroup(container = middlegr, horizontal = FALSE) # Used for run & results buttons
  bottomgr <- ggroup(container = gr,  horizontal = FALSE, expand = TRUE) # Used for result images
  
  img1 <- gpanedgroup(container = bottomgr, expand=TRUE)

  # 4 plots w/ handlers for rescaling when clicked; device handling is primitive since GTK breaks easily.
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

  img2 <- gpanedgroup(container = bottomgr, expand=TRUE)
  
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
  visible(gr) <- TRUE

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
    visible(b1) <- TRUE # Folder button; keep visible in case user selects wrong folder
    visible(b2) <- TRUE  # Run button
    visible(b3) <- FALSE # html button
    visible(b4) <- FALSE # pdf button
  }
  b1 <- gbutton("Select folder", container = topgr, handler = h1)
  
  # Run button
  h2 <- function(..., envir = parent.frame()){
    enabled(b1) <- FALSE # Folder button
    cat("Running Quality Control on", dname, "\n")
    gcinfo(TRUE) # Image files are large, we want to see the garbage collection interruptions

    if (dname != ""){
      cat("... please wait...\n\n")
      file_list <- list.files(path = dname)
      
      ## Showing raw data image
      
      # Use first raw file in list
      # --- Implement return else
      raw_list <- file_list[grepl(".raw", file_list)]
      if (length(raw_list) > 0) {
        raw_path <- file.path(normalizePath(dname,"/"), raw_list[[1]])
        cat("Found .raw file:", raw_path, "\n")
      } else {
        e <- simpleError("Found no .raw file.")
        stop(e)
      }
      
      # Use msconvert for converting to mzXML
      # Filter out MS1 spectra
      cat("Converting MS1/MS2 .raw file to MS1 .mzXML file for plotting purposes\n")
      cat("... this may take a while ...\n\n")
  
      cmd <- "C:\\R LocalData\\QC\\ProteoWizard 3.0.19246.075ea16f5 64-bit\\msconvert.exe"
      arg <- stri_join(shQuote(raw_path), "--filter", shQuote("msLevel 1"), "--mzXML -o", shQuote(normalizePath(dname)), sep=' ')
      system2(cmd, arg)
      
      # Update file list
      # Use first mzXML file in list
      # --- Implement stop w/ error else ---
      file_list <- list.files(path = dname)
      mzxml_list <- file_list[grepl(".mzXML", file_list)]
      if (length(mzxml_list) > 0) {
        mzxml_path <- file.path(normalizePath(dname,"/"), mzxml_list[[1]])
        cat("Generating raw MS1 peak image data from .mzXML file:", mzxml_path, "\n")
        cat("... this may take a while ...\n")
      } else {
        e <- simpleError("Found no .mzXML file.")
        stop(e)
      }
      
      # Use MSnbase methods to extract MS retention times and peak m/z values
      # Remove large files after use
      cat("... extracting retention times and m/z ...\n")
      basename(mzxml_path)
      ms_msl1 <- readMSData(mzxml_path, mode="onDisk")
      print(ms_msl1)
      
      ms_rt <- rtime(ms_msl1)
      ms_mz <- mz(ms_msl1)
      rm(ms_msl1)

      cat("Removing mzXML file.\n\n")
      file.remove(mzxml_path)
      
      # Plot through list function
      # To speed up, don't use plot vector graphics but bitmap in a png file in memory
      # and assign data each loop
      maxrt <- max(ms_rt)
      
      plot.new()
      plt <- dev.cur()
      png(file=(assign("tf", tempfile(fileext = ".png"))), bg = "transparent")
      plot(c(0,2000), c(0,1.1*maxrt/60), type="n", xlab="m/z", ylab="rt [min]")
      
      cat("... peak index count will follow ...\n")
      lapply(seq(8, length(ms_rt), 8), function(i, envir = parent.frame()) {
        print(i)
        assign("plt", points.default(unlist(ms_mz[[i]]), rep(ms_rt[[i]]/60, length(ms_mz[[i]])), type="p", col="green", pch='.'))
      })
      dev.off()
      dev.set(img1left);
      plot(readImage(tf))
      cat("... raw peak data done.\n")
      rm(tf)
      gcinfo(FALSE) # Quit showing the garbage business until next image

      # Remove the previous run images
      dev.set(img1right)
      plot(0,type='n',axes=FALSE,ann=FALSE)
      
      dev.set(img2left)
      plot(0,type='n',axes=FALSE,ann=FALSE)
      
      dev.set(img2right)
      plot(0,type='n',axes=FALSE,ann=FALSE)
      
      visible(bottomgr) <- TRUE # New images
      
      ## Running PTXQC
      
      # Default is provided yaml object, but use a provided one if in folder,
      # else let PTXQC generate one.
      yaml_list <- file_list[grepl(".yaml", file_list)]
      
      # Use first in list
      if (length(yaml_list) > 0) {
        yaml_path <- file.path(normalizePath(dname,"/"), yaml_list[[1]])
        cat("Found .yaml file: ", yaml_path, "\n")
        yaml_list_object <- yaml.load_file(yaml_path)
      } else if(length(fname) > 0) {
        cat("Generating .yaml file: ", fname, "\n")
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
      writePNG(bitmap, assign("tf", tempfile(fileext = ".png")))
      assign("imraw", readImage(tf), currentenv)
      rm(tf)

      dev.set(img1right);plot(imraw)
      dev.set(img2left);plot(imraw)
      dev.set(img2right);plot(imraw)

      #Change visibilities
      visible(rightbgr) <- TRUE # Button group
      enabled(b1) <- TRUE # Folder button
      visible(b2) <- FALSE # Run button
      visible(b3) <- TRUE # html button
      visible(b4) <- TRUE # pdf button
      visible(b5) <- TRUE # Quit button; always visible
      visible(b6) <- TRUE  # .yaml button; always visible
    }
  }
  
  b2 <- gbutton("Run QC", container = rightbgr, handler = h2)
  
  # html report button
  b3 <- gbutton("html", container = rightbgr, handler=function(...) browseURL(r$report_file_HTML))

  # pdf report button
  b4 <- gbutton("pdf", container = rightbgr, handler=function(...) browseURL(r$report_file_PDF))
  
  # Quit button
  h5 <- function(..., envir = parent.frame()){
    graphics.off()
    dispose(win, ...)
    quit()
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
