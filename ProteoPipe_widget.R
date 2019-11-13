#Summary:
#A widget to generate a PTXQC Quality Control of LC-MSMS instrument
#based on MaxQuant analysis.
#Use require to put package info into the log file.


ProteoPipe_widget<-function(env = parent.frame()){
  
  ## Load libraries
  # Notify operator that program is running
  system("msg * /time:10 /w ProteoPipe has started, please wait while loading libraries", wait = FALSE)
  
  library(stringi)
  library(BiocGenerics)
  
  # Load widget graphics; assumes librarys loaded to the used R
  # Object references will be passed by reference frame (so is mutable) 
  library(gWidgets2)
  # To get the toolkit RGTk2, load library gWidgets2RGtk2 into R and accept installing GTK+:
  # install.packages(c("gWidgets2RGtk2", "cairoDevice"))
  # library(gWidgets2RGtk2)
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
  
  # Defaults
  gcinfo(FALSE)
  currentenv <- environment()
  raw_list <- list()
  ref_raw <- "raw.png"
  ref_results <- "results.png"
  no_raw <- "found_no_raw_files.png"
  many_raw <- "found_many_raw_files.png"
  cat("Console texts are logged in ", text_log, "\n")
  cat("Warnings are logged in ", warnings_log, "\n")
  MQ_maxtime <- 7200 # Time in seconds; QC run takes about 1 hour, so 2 hours seems reasonable.
  mqpar_path_default <- file.path(dname, "mqpar_for_HeLa.xml", fsep="\\")
  mqpar_path <- mqpar_path_default
  yaml_path_default <- file.path(dname, "Template_QC_Hela_digests_1h_400_Niklas.yaml", fsep="\\")
  yaml_path <- yaml_path_default
  source(file.path(dname, "ProteomicsEndgame.R"))

  #------------------Calling------------------

  ## GUI Widget
  graphics.off()
  par(mar=c(1,1,1,1)) # In case of RStudio graphics bug during development
  win <- gwindow("ProteoPipe", width=960, height=960, visible = FALSE)
  focus(win)
  gr <- ggroup(container = win, horizontal = FALSE, visible = FALSE)
  
  gr1 <- ggroup(container = gr) # Used for buttons
  gr2 <- ggroup(container = gr) # Used for buttons
  gr3 <- ggroup(container = gr, horizontal = FALSE) # Used for images
  gr31 <- ggroup(container = gr3) # Label group
  gr32 <- ggroup(container = gr3) # Run group
  gr33 <- ggroup(container = gr3) # Result group
  gr34 <- ggroup(container = gr3) # Buttons group

  ## Expert mode buttons
  # Toggle expert mode
  han11 <- function(...,  envir = parent.frame()){
    # Change flag for lopp looking for .raw files 
    assign("first_wait", TRUE, currentenv)
    
    # Change visibilities and paths
    if (svalue(but11)) {
      visible(but12) <- TRUE # .raw button
      visible(but13) <- TRUE # .xml button
      visible(but14) <- TRUE # .yaml button
    } else {
      assign("mqpar_path", mqpar_path_default, currentenv)
      assign("yaml_path", yaml_path_default, currentenv)
      assign("dname", dname_default, currentenv)
      visible(but12) <- FALSE # .raw button
      visible(but13) <- FALSE # .xml button
      visible(but14) <- FALSE # .yaml button
    }
  }
  but11 <- gcheckbox("Toggle expert mode", container = gr1, use.togglebutton=TRUE, handler = han11)
  enabled(but11) <- FALSE
  
  # Choose .raw file folder
  han12 <- function(...,  envir = parent.frame()){
    selected_folder <- tk_choose.dir()
    if(!is.na(selected_folder)) assign("dname", normalizePath(selected_folder, "\\"), currentenv)
    cat("Work folder:", dname, "\n")
  }
  but12 <- gbutton(".raw folder", container = gr1, handler = han12)
  visible(but12) <- FALSE

  # Choose .xml file
  han13 <- function(...,  envir = parent.frame()){
    Filters <- matrix(c(".xml",
                        ".xml"),
                      1, 2, byrow = TRUE)
    selected_file <- tk_choose.files(default = "mqpar.xml", multi = FALSE, filter = Filters)
    if(!is.na(selected_file)) assign("mqpar_path", selected_file, currentenv)
    cat("mqpar.xml:", mqpar_path, "\n")
  }
  but13 <- gbutton(".xml", container = gr1, handler = han13)
  visible(but13) <- FALSE
  
  # Choose .yaml file
  han14 <- function(...,  envir = parent.frame()){
    Filters <- matrix(c(".yaml",
                        ".yaml"),
                      1, 2, byrow = TRUE)
    selected_file <- tk_choose.files(default = ".yaml", multi = FALSE, filter = Filters)
    if(!is.na(selected_file)) assign("yaml_path", selected_file, currentenv)
    cat(".yaml:", yaml_path, "\n")
  }
  but14 <- gbutton(".yaml", container = gr1, handler = han14)
  visible(but14) <- FALSE
  
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up
  
  ## Run button
  han2 <- function(..., envir = parent.frame()){
    
    # Change visibilities; Quit & Expert is always visible and enabled.
    enabled(but2) <- FALSE # Run button
    enabled(but3) <- FALSE # html button
    enabled(but4) <- FALSE # pdf button
    
    # Run start time; used for folder names
    run_time <- Sys.time()
    run_time_text <- paste(Sys.time())
    cat(run_time_text, "\n")
    con_temp <- file(warnings_log, open = "a", blocking = FALSE)
    writeLines(run_time_text, con_temp)
    close(con_temp)
    
    
    # Fill previous run with empty plots
    dev.set(img1left)
    plot(0,type='n', axes = FALSE, ann = FALSE)
    dev.set(img1right)
    plot(0,type='n', axes = FALSE, ann = FALSE)

    cat("Running Quality Control on", dname, "\n")
    if (length(raw_list) == 0) {
      confirmDialog("There is no .raw file here", handler = function(h,...) dispose(h$obj))
      return()
    }
    
    ## Running MaxQuant 1.6.3.4
    # MaxQuant command line interface has no method to call .raw files, and no verbose mode.
    # Raw file paths are specified in mqpar.xml; replace the *.raw path with the new.
    # Use a file connection and textfile operations for ease and speed.
    # The program is run asynchronously and can't time out. The timeout is in the R thread.
    # For control and correct text logging a temporary log file is created, merged with main log, and erased.
    
    replace_txt <- stri_join("      <string>", raw_path, "</string>")
    
    con_temp <- file(mqpar_path)
    
    txt <- readLines(con_temp)
    txt[[grep(".raw", txt, fixed = TRUE)]] <- replace_txt
    
    writeLines(txt, con_temp)
    close(con_temp)
    
    # Then we build the R pipe command and call MaxQuant from the command line.
    error_file <- file.path(dname, "error.txt", fsep="\\")
    mq_path <- file.path(dname, "MaxQuant_1.6.3.4\\MaxQuant\\bin\\MaxQuantCmd.exe")
    system2(mq_path, shQuote(mqpar_path), stdout = temp_file, stderr = error_file, wait = FALSE)
    enabled(lbl_MaxQuant) <- TRUE
    cat("MaxQuant has started.\n")
    
    ## Plotting .raw MS1 retention times and peak m/z values

    # Use msconvert for converting to mzXML
    # Filter out MS1 spectra
    cat("Converting MS1/MS2 .raw file to MS1 .mzXML file for plotting purposes\n")
    cmd <- file.path(dname, "ProteoWizard 3.0.19246.075ea16f5 64-bit\\msconvert.exe")
    arg <- stri_join(shQuote(raw_path), "--verbose", "--filter", shQuote("msLevel 1"), "--mzXML -o", shQuote(normalizePath(dname)), sep=' ')
    system2(cmd, arg)
    
    # Update file list
    # Use first mzXML file in list
    file_list <- list.files(path = dname)
    mzxml_list <- file_list[grepl(".mzXML", file_list, fixed = TRUE)]
    if (length(mzxml_list) > 0) {
      mzxml_path <- file.path(normalizePath(dname, "\\"), mzxml_list[[1]], fsep="\\")
      cat("Generating raw MS1 peak image data from .mzXML file: ", mzxml_path, "\n")
    } else {
      e <- simpleError("Found no .mzXML file.")
      system("msg * /time:200000 /w /v Pipeline error: Found no .mzXML file.") # Tell the GUI user (for 2.3 d).
      stop(e)
    }

    # Use MSnbase methods to extract MS retention times and peak m/z values
    # Remove large files after use
    cat("Extracting retention times and m/z data.\n")
    basename(mzxml_path)
    ms_msl1 <- readMSData(mzxml_path, mode="onDisk")
    
    cat("MS data object: \n")
    cat(capture.output(print(ms_msl1)), "\n")

    ms_rt <- rtime(ms_msl1)
    ms_mz <- mz(ms_msl1)
    rm(ms_msl1)

    # Plot through list function
    # To speed up, don't use plot vector graphics but bitmap in a png file in memory
    # and assign data each loop
    maxrt <- max(ms_rt)

    plot.new()
    plt <- dev.cur()
    png(file=(assign("tf", tempfile(fileext = ".png"))), bg = "transparent")
    plot(c(0,2000), c(0,1.1*maxrt/60), type="n", xlab="m/z", ylab="rt [min]")

    cat("Building spectra image.\n")
    len <- length(ms_rt)
    sink() # Don't divert this to log file. 
    lapply(seq(8, len, 8), function(i, envir = parent.frame()) {
      cat("\r", i, "/", len)
      assign("plt", points.default(unlist(ms_mz[[i]]), rep(ms_rt[[i]]/60, length(ms_mz[[i]])), type="p", col="green", pch='.'))
    })
    cat("\n")
    sink(con, append=TRUE, split=TRUE) #Continue divert stdin to log file.
    dev.off()
    dev.set(img1left);
    plot(assign("img1", readImage(tf), currentenv))
    rm(tf)
    cat("Image done, based on", len, "spectra.\n")
    cat("Waiting for MaxQuant to finish.\n")
    Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up

    ## Waiting for MaxQuant to finish.
    # After finish, append the stdin text to the main log file.
    assign("temp_log", "", envir=currentenv)
    assign("error_log", "", envir=currentenv)
    assign("wait_time", 0, envir=currentenv)

    while ((all(nzchar(temp_log)) == 0) & (all(nzchar(error_log)) == 0) & (wait_time <= MQ_maxtime)) {
      tryCatch({assign("temp_log", readLines(temp_file), envir=currentenv)},
               error = function(e) {
                 assign("wait_time", as.numeric(Sys.time() - run_time, unit="secs"), envir=currentenv)
                 Sys.sleep(60)
               })
      tryCatch({assign("error_log", readLines(error_file), envir=currentenv)},
               error = function(e) {
                 assign("wait_time", as.numeric(Sys.time() - run_time, unit="secs"), envir=currentenv)
                 Sys.sleep(60)
               })
    }
    assign("error_log", readLines(error_file), envir=currentenv)
    if ((any(nzchar(error_log)) != 0)) {
      e <- simpleError("MaxQuant threw an error.")
      system("msg * /time:200000 /w /v Pipeline error: MaxQuant threw an error. Computer may need restart to clear files at next run.") # Tell the GUI user (for 2.3 d); Quit works, but MQ may have locked the files.
      stop(e)
    }
    if (wait_time > MQ_maxtime) {
      e <- simpleError("MaxQuant timed out.")
      system("msg * /time:200000 /w /v Pipeline error: MaxQuant timed out. Computer may need restart to clear files at next run.") # Tell the GUI user (for 2.3 d); Quit works, but MQ may have locked the files.
      stop(e)
    }
    
    file.remove(temp_file)
    write(temp_log, file = text_log, append = TRUE)
    
    ## Making a Quality Control report with PTXQC    
    # QC run parameters is in a provided yaml file, else use the first in the folder,
    # else let PTXQC generate a default yaml object.
    # First copy the .xml file MaxQuant used to /txt folder for PTXQC/archive purposes
    txt_folder <- file.path(dname, "combined", "txt", fsep="\\")
    
    xml_path <- file.path(dname, "mqpar.xml", fsep="\\")
    file.copy(xml_path, txt_folder, copy.date = TRUE)
    
    yaml_list_object <- yaml.load_file(yaml_path)
    
    # Report will generate lots of warnings that we want to catch to warnings log file.
    # Call handler for each warning as they come, to reenter try/catch loop.
    tryCatch(withCallingHandlers(assign("r", createReport(txt_folder, yaml_list_object), envir=currentenv),
                                 # Warning object seems to have abbreviation 'w'
                                 # W is a single warning list object when run with try/catch calling handler
                                 warning=function(w) {
                                   # Note: conditionMessage(w) does that too
                                   write(capture.output(cat("createReport() warning:", conditionMessage(w),
                                                            "\n")), file=warnings_log, append=TRUE)
                                   invokeRestart("muffleWarning")
                                 }))
    cat("... QC report finalized!\n\n")

    # Generate result image from report files
    # Plot it through a temporary file
    dev.set(img1right)
    bitmap <- pdf_render_page(r$report_file_PDF, page = 1, dpi = 300, antialias = "text")
    writePNG(bitmap, assign("tf", tempfile(fileext = ".png")))
    plot(assign("img1r", readImage(tf), currentenv))
    rm(tf)
    
    # Generate result summary from heatmap.txt
    entries <- read.table(r$heatmap_values_file, header=TRUE, sep="\t")
    scores <- entries[-c(1)] # First entry is a file reference
    ptxqc_color <- function(scores) {
      if (any(scores < 0.25)) return(hsv(0,1,1)) # Too low QC score
      else {
        score <- scores$Average.Overall.Quality
        if (score < 0.5) {
          sv <- 2*score   # Black value = [0,1] for score = [0.5, 1]
          return(hsv(0,1,1-sv)) # Red value is fixed
        }
        else {
          sv <- (2-2*score)   # Black value = [1,0] for score = [0.5, 1]
          return(hsv(0.3,1,1-sv)) # Green value is fixed
        }
      }
    }
    font(labelimg1) <- list(size=c(72), foreground=ptxqc_color(scores))
    enabled(labelimg1) <- TRUE # Results label

    ## Creating folder labeled date
    cat("Creating result folder.\n")
    assign("folder_path", file.path(dname, format(run_time, "%Y-%m-%d %H-%M-%S")), currentenv)

    # If existing, reuse; else create
    if (!dir.exists(folder_path)) dir.create(folder_path)

    # Copy raw & report files; suppress warnings for missing PTXQC files since it depends on test
    file.copy(raw_path, folder_path)
    file.copy(mqpar_path, folder_path)
    suppressWarnings(file.copy(unlist(r), folder_path, overwrite = TRUE))
    file.copy(file.path(dname, "combined", "proc"), folder_path, recursive = TRUE)
    file.copy(file.path(dname, "combined", "txt"), folder_path, recursive = TRUE)

    ## Removing work files; suppress warnings for attempts to remove listed folders
    suppressWarnings(file.remove(unlist(r)))
    unlink(file.path(dname, "combined"), recursive = TRUE)
    raw_name <- stri_split_fixed(raw_list[[1]], '.')[[1]][1]
    unlink(file.path(dname, raw_name[[1]]), recursive = TRUE)
    file.remove(file.path(dname, stri_join(raw_name[[1]], ".raw")))
    file.remove(file.path(dname, stri_join(raw_name[[1]], ".mzXML")))
    file.remove(file.path(dname, stri_join(raw_name[[1]], ".index")))

    cat("Removed work files.\n")
    cat("All Work done!\n")
    
    stop_lines <- c(stri_join(paste(Sys.time()), "\n"), "-------------------------------------\n\n")
    cat(stop_lines)
    con_temp <- file(warnings_log, open = "a", blocking = FALSE)
    writeLines(stop_lines, con_temp)
    close(con_temp)
    
    # # Change visibilities; Quit & Expert is always visible and enabled.
    enabled(lbl_MaxQuant) <- FALSE
    enabled(but2) <- FALSE # Run button
    enabled(but3) <- TRUE # html button
    enabled(but4) <- TRUE # pdf button
    return()
  }
  but2 <- gbutton("Run QC", container = gr2, handler = han2)
  enabled(but2) <- FALSE
  lbl_MaxQuant <- glabel("MaxQuant will now run for 1-2 hours.", container = gr2)
  enabled(lbl_MaxQuant) <- FALSE
  lbl_no_raw_files <- glabel("Found no .raw files!", container = gr2)
  visible(lbl_no_raw_files) <- FALSE
  lbl_many_raw_files <- glabel("Found many .raw files!", container = gr2)
  visible(lbl_many_raw_files) <- FALSE
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up
  
  ## Labels and paned groups for rescalable GUI
  labelh1 <- glabel("   ", container = gr31)
  font(labelh1) <- list(family="monospace")
  hor1 <- gpanedgroup(container = gr31, expand = TRUE)
  labelh2 <- glabel("Raw", container = hor1)
  font(labelh2) <- list(family="monospace")
  labelh3 <- glabel("HeatMap", container = hor1)
  font(labelh3) <- list(family="monospace")
  labelh4 <- glabel("Result   ", container = gr31) # Adjusted towards centre; may want to try text format
  font(labelh4) <- list(family="monospace")
  
  labelv1 <- glabel("Run", container = gr32)
  font(labelv1) <- list(family="monospace")
  img1 <- gpanedgroup(container = gr32, expand = TRUE)
  labelimg1 <- glabel(stri_unescape_unicode("\\u25a0"), container = gr32) # Unicode Character 'BLACK SQUARE' (U+25A0)
  font(labelimg1) <- list(size=c(72), foreground="green")

  labelv2 <- glabel("Ref", container = gr33)
  font(labelv2) <- list(family="monospace")
  img2 <- gpanedgroup(container = gr33, expand = TRUE)
  labelimg2 <- glabel(stri_unescape_unicode("\\u25a0"), container = gr33) # Unicode Character 'BLACK SQUARE' (U+25A0)
  font(labelimg2) <- list(size=c(72), foreground=hsv(0.3,1, 0.8002919))
  
  labelv3 <- glabel("Report", container = gr34)
  font(labelv3) <- list(family="monospace")
  hor2 <- gpanedgroup(container = gr34, expand = TRUE)
  
  visible(win) <- TRUE # Must make this visible to get the GTK graphics mounted correctly.
  visible(gr) <- TRUE
  
  # 4 plots w/ handlers for rescaling when clicked; device handling is primitive since GTK breaks easily.
  # Since we draw empty plots, check for images before allowing rescale.
  img1left <- ggraphics(container = img1);
  addHandlerChanged(img1left, handler = function(h, ..., envir=parent.frame()){
    if (exists("img1l")) {
      imscaled <- resize(img1l, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
      dev.set(img1left);plot(imscaled)
    }
  })
  img1left <- dev.cur()
  
  img1right <- ggraphics(container = img1);
  addHandlerChanged(img1right, handler = function(h, ..., envir=parent.frame()){
    if (exists("img1r")) {
      imscaled <- resize(img1r, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
      dev.set(img1right);plot(imscaled)
    }
  })
  img1right <- dev.cur()
  
  img2left <- ggraphics(container = img2);
  img2left <- dev.cur()
  
  img2right <- ggraphics(container = img2);
  img2right <- dev.cur()
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up

  # Fill with empty plots
  dev.set(img1left)
  plot(0,type='n', axes = FALSE, ann = FALSE)
  dev.set(img1right)
  plot(0,type='n', axes = FALSE, ann = FALSE)
  dev.set(img2left)
  plot(0,type='n', axes = FALSE, ann = FALSE)
  dev.set(img2right)
  plot(0,type='n', axes = FALSE, ann = FALSE)
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up

  # Generate reference images from file
  dev.set(img2left)
  plot(assign("img2l", readImage(file.path(dname, ref_raw)), currentenv))

  dev.set(img2right)
  plot(assign("img2r", readImage(file.path(dname, ref_results)), currentenv))

  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up
  
  ## Report and Quit buttons
  gr_res <- ggroup(container = hor2) 
  but3 <- gbutton("html", container = gr_res, handler=function(...) browseURL(file.path(folder_path, basename(r$report_file_HTML))))
  but4 <- gbutton("pdf", container = gr_res, handler=function(...) browseURL(file.path(folder_path, basename(r$report_file_PDF))))
  but6 <- gbutton("1 year summary", container = gr_res, handler=function(..., envir = parent.frame()) ProteomicsEndgame())
  
  # Quit button
  gr_quit <- ggroup(container = hor2)
#  han5 <- function(..., envir = parent.frame()){
  han6 <- function(..., envir = parent.frame()){
    graphics.off()
    dispose(win, ...)
    stop_lines <- c(stri_join(paste(Sys.time()), "\n"), "-------------------------------------\n\n")
    cat(stop_lines)
    con_temp <- file(warnings_log, open = "a", blocking = FALSE)
    writeLines(stop_lines, con_temp)
    closeAllConnections() # Close sinks and connections
    console <- FALSE # Flag console closed
    quit()
  }
#  but5 <- gbutton("Quit", container = gr_quit, handler = han5)
  but6 <- gbutton("Quit", container = gr_quit, handler = han6)
  
  # Graphics mounted; Quit & Expert is always visible and enabled.
  enabled(labelimg1) <- FALSE # Results label
  enabled(but11) <- TRUE # Expert button
  enabled(but2) <- FALSE # Run button
  enabled(but3) <- FALSE # html button
  enabled(but4) <- FALSE # pdf button

  ## Loop and look for .raw files before Run.
  # Use a flag so Run button is always enabled when there is a .raw file,
  # the other two states are kept fully dynamic.
  
  assign("first_wait", TRUE, currentenv)
  
  while (TRUE) {
    file_list <- list.files(path = dname)
    
    # Use first raw file in list
    raw_list <- file_list[grepl(".raw", file_list, fixed = TRUE)]

    if ((length(raw_list) == 1) & (first_wait)) {
      assign("first_wait", FALSE, currentenv)
      assign("raw_path", file.path(normalizePath(dname, "\\"), raw_list[[1]], fsep="\\"), currentenv)
      cat("Found .raw file:", raw_path, "\n")

      # Change visibilities; Quit is always visible and enabled.
      visible(lbl_no_raw_files) <- FALSE
      visible(lbl_many_raw_files) <- FALSE
      enabled(labelimg1) <- FALSE # Results label
      enabled(but2) <- TRUE  # Run button
      enabled(but3) <- FALSE # html button
      enabled(but4) <- FALSE # pdf button
    }
    
    if (length(raw_list) == 0) {
      assign("first_wait", TRUE, currentenv)
      
      # Change visibilities; Quit is always visible and enabled.
      visible(lbl_no_raw_files) <- TRUE
      enabled(but2) <- FALSE  # Run button
    }
    
    if (length(raw_list) > 1 & (first_wait)) {
      assign("first_wait", FALSE, currentenv)

      # Change visibilities if in default mode; Quit is always visible and enabled.
      if (isFALSE(svalue(but11))) {
        visible(lbl_many_raw_files) <- TRUE
        enabled(but2) <- FALSE  # Run button
      } else {
        assign("raw_path", file.path(normalizePath(dname, "\\"), raw_list[[1]], fsep="\\"), currentenv)
        cat("Found .raw file:", raw_path, "\n")
        enabled(but2) <- TRUE  # Run button
      }
    }
    
    Sys.sleep(1) # Wait a bit before continue looping

  } # End while loop
  
} # End main call
