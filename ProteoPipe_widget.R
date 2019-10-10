#Summary:
#A widget to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.
#Use require to put package info into the log file.


ProteoPipe_widget<-function(env = parent.frame()){
  
  library(stringi)
  library(BiocGenerics)
  
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
  dname <- "C:\\Users\\torla438\\Work Folders\\Desktop\\ProteoPipe"
  ref_raw <- "raw.png"
  ref_results <- "results.png"
  cat("Console texts are logged in ", text_log, "\n")
  cat("Warnings are logged in ", warnings_log, "\n")

  #------------------Calling
  
  ## GUI Widget
  graphics.off()
  par(mar=c(1,1,1,1))
  win <- gwindow("ProteoPipe", width=960, height=960, visible = FALSE)
  gr <- ggroup(container = win, horizontal = FALSE, visible = FALSE)
  
  gr1 <- ggroup(container = gr, horizontal = FALSE) # Used for folder select
  gr2 <- ggroup(container = gr) # Used for buttons
  gr3 <- ggroup(container = gr, horizontal = FALSE) # Used for images
  gr31 <- ggroup(container = gr3) # Label group
  gr32 <- ggroup(container = gr3) # Run group
  gr33 <- ggroup(container = gr3) # Result group
  gr34 <- ggroup(container = gr3) # Buttons group

  ## Select folder textbox and button
  fr1 <- gframe("", container = gr1)
  lbl_dname<- glabel("Selected folder to run Quality Control on: ", container = fr1)
  txt_dname <- gedit(dname, container = fr1)

  han1 <- function(...,  envir = parent.frame()){
    getwd()
    selected_folder <- tk_choose.dir()
    if(!is.na(selected_folder)) assign("dname", normalizePath(selected_folder, "\\"), currentenv)
    # If no selection, use default
    svalue(txt_dname) <- dname

    ## Look for .raw files before Run.
    file_list <- list.files(path = dname)

    # Use first raw file in list
    # Else popup an info text
    raw_list <- file_list[grepl(".raw", file_list)]
    if (length(raw_list) > 0) {
      assign("raw_path", file.path(normalizePath(dname, "\\"), raw_list[[1]], fsep="\\"), currentenv)
      cat("Found .raw file:", raw_path, "\n")
    } else {
      modal_dialog <- gbasicdialog(title = "", do.buttons = FALSE)
      dgr <- ggroup(container =  modal_dialog, horizontal=FALSE)
      glabel("There is no .raw file here", container = dgr)
      Sys.sleep(1)
      dispose(modal_dialog)
    }

    #Change visibilities; Quit is always visible and enabled.
    enabled(but1) <- TRUE # Folder button; keep enabled in case user selects wrong folder
    if (length(raw_list) > 0) enabled(but2) <- TRUE  # Run button
    enabled(but3) <- FALSE # html button
    enabled(but4) <- FALSE # pdf button
  }
  but1 <- gbutton("Select folder", container = fr1, expand = FALSE, handler = han1)
  enabled(but1) <- FALSE
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up
  
  ## Run button
  han2 <- function(..., envir = parent.frame()){
    
    #Change visibilities; Quit is always visible and enabled.
    enabled(but1) <- FALSE # Folder button
    enabled(but2) <- FALSE # Run button
    enabled(but3) <- FALSE # html button
    enabled(but4) <- FALSE # pdf button
    
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

    ## Plotting .raw MS1 retention times and peak m/z values

    gcinfo(TRUE) # Image files are large, we want to see the garbage collection interruptions

    # Use msconvert for converting to mzXML
    # Filter out MS1 spectra
    cat("Converting MS1/MS2 .raw file to MS1 .mzXML file for plotting purposes\n")

    cmd <- "C:\\Users\\torla438\\Work Folders\\Desktop\\ProteoPipe\\ProteoWizard 3.0.19246.075ea16f5 64-bit\\msconvert.exe"
    arg <- stri_join(shQuote(raw_path), "--verbose", "--filter", shQuote("msLevel 1"), "--mzXML -o", shQuote(normalizePath(dname)), sep=' ')

    system2(cmd, arg)

    # Update file list
    # Use first mzXML file in list
    # --- Implement stop w/ error else ---
    file_list <- list.files(path = dname)
    mzxml_list <- file_list[grepl(".mzXML", file_list)]
    if (length(mzxml_list) > 0) {
      mzxml_path <- file.path(normalizePath(dname, "\\"), mzxml_list[[1]], fsep="\\")
      cat("Generating raw MS1 peak image data from .mzXML file:", mzxml_path, "\n")
    } else {
      e <- simpleError("Found no .mzXML file.")
      stop(e)
    }

    # Use MSnbase methods to extract MS retention times and peak m/z values
    # Remove large files after use
    cat("Extracting retention times and m/z data.\n")
    basename(mzxml_path)
    ms_msl1 <- readMSData(mzxml_path, mode="onDisk")
    print(ms_msl1)

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
    lapply(seq(8, len, 8), function(i, envir = parent.frame()) {
      cat("\r", i, "/", len)
      assign("plt", points.default(unlist(ms_mz[[i]]), rep(ms_rt[[i]]/60, length(ms_mz[[i]])), type="p", col="green", pch='.'))
    })
    cat("\n")
    dev.off()
    dev.set(img1left);
    plot(assign("im1l", readImage(tf), currentenv))
    rm(tf)
    gcinfo(FALSE) # Quit showing the garbage business until next image

    ## Running MaxQuant 1.6.3.4
    # MaxQuant command line interface has no method to call .raw files, and no verbose mode.
    # Raw file paths are specified in mqpar.xml; replace the *.raw path with the new.
    # Use a file connection and textfile operations for ease and speed.

    replace_txt <- stri_join("      <string>", raw_path, "</string>")

    mqpar_path <- "C:\\Users\\torla438\\Work Folders\\Desktop\\ProteoPipe\\mqpar.xml"
    con <- file(mqpar_path)

    txt <- readLines(con)
    txt[[grep(".raw", txt)]] <- replace_txt

    writeLines(txt, con)
    close(con)

    # Then we build the R pipe command and call MaxQuant from the command line.
    mq_path <- "C:\\Users\\torla438\\Work Folders\\Desktop\\ProteoPipe\\MaxQuant_1.6.3.4\\MaxQuant\\bin\\MaxQuantCmd.exe"
    system2(mq_path, shQuote(mqpar_path))

    ## Running PTXQC

    # Default is provided yaml object, but use a provided one if in folder,
    # else let PTXQC generate one.
    yaml_path <- file.path(Sys.getenv("USERPROFILE"), "Work Folders", "Desktop", "ProteoPipe", "Template_QC_Hela_digests_1h_400_Niklas.yaml", fsep="\\")

    yaml_list <- file_list[grepl(".yaml", file_list)]
    # Use first in list
    if (length(yaml_list) > 0) {
      yaml_path <- file.path(normalizePath(dname, "\\"), yaml_list[[1]], fsep="\\")
    }

    if(length(yaml_path) > 0) {
      yaml_list_object <- yaml.load_file(yaml_path)
      cat("Loaded .yaml file: ", yaml_path, "\n")
    }
    else {yaml_list_object <- list()}

    # Report will generate lots of warnings that we want to catch to warnings log file.
    # Call handler for each warning as they come, to reenter try/catch loop.
    txt_folder <- file.path(dname, "combined", "txt", fsep="\\")
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
    plot(assign("im1r", readImage(tf), currentenv))
    rm(tf)

    ## Creating folder labeled date
    cat("Creating result folder.\n")
    assign("folder_path", file.path(dname, Sys.Date()), currentenv)

    # If existing, reuse; else create
    if (!dir.exists(folder_path)) dir.create(folder_path)

    # Copy raw & report files; suppress warnings for missing PTXQC files since it depends on test
    file.copy(raw_path, folder_path)
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
    cat("All Work done!\n\n")

    # Change visibilities
    enabled(but1) <- TRUE # Folder button
    enabled(but2) <- FALSE # Run button
    enabled(but3) <- TRUE # html button
    enabled(but4) <- TRUE # pdf button
  }
  but2 <- gbutton("Run QC", container = gr2, handler = han2)
  enabled(but2) <- FALSE
  Sys.sleep(1) # Development documentation suggest some waiting to let the graphics catch up
  
  ## Labels and paned groups for rescalable GUI
  labelh1 <- glabel("   ", container = gr31)
  hor1 <- gpanedgroup(container = gr31, expand = TRUE)
  labelh2 <- glabel("Raw", container = hor1)
  labelh3 <- glabel("HeatMap", container = hor1)
  
  labelv1 <- glabel("Run", container = gr32)
  img1 <- gpanedgroup(container = gr32, expand = TRUE)

  labelv2 <- glabel("Ref", container = gr33)
  img2 <- gpanedgroup(container = gr33, expand = TRUE)
  
  labelv3 <- glabel("Report", container = gr34)
  hor2 <- gpanedgroup(container = gr34, expand = TRUE)
  
  visible(win) <- TRUE # Must make this visible to get the GTK graphics mounted correctly.
  visible(gr) <- TRUE
  
  # 4 plots w/ handlers for rescaling when clicked; device handling is primitive since GTK breaks easily.
  # --_ check for images before allowing rescale ---
  img1left <- ggraphics(container = img1);
  addHandlerChanged(img1left, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(img1l, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
    dev.set(img1left);plot(imscaled)
  })
  img1left <- dev.cur()
  
  img1right <- ggraphics(container = img1);
  addHandlerChanged(img1right, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(img1r, w=gWidgets2::size(img1)[1], h=gWidgets2::size(img1)[2])
    dev.set(img1right);plot(imscaled)
  })
  img1right <- dev.cur()
  
  img2left <- ggraphics(container = img2);
  addHandlerChanged(img2left, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(img2l, w=gWidgets2::size(img2)[1], h=gWidgets2::size(img2)[2])
    dev.set(img2left);plot(imscaled)
  })
  img2left <- dev.cur()
  
  img2right <- ggraphics(container = img2);
  addHandlerChanged(img2right, handler = function(h, ..., envir=parent.frame()){
    imscaled <- resize(img2r, w=gWidgets2::size(img2)[1], h=gWidgets2::size(img2)[2])
    dev.set(img2right);plot(imscaled)
  })
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
  
  # Quit button
  gr_quit <- ggroup(container = hor2)
  han5 <- function(..., envir = parent.frame()){
    graphics.off()
    dispose(win, ...)
    console <- FALSE # Flag console closed
    sink() # Close log file
    close(con)
    quit()
  }
  but5 <- gbutton("Quit", container = gr_quit, handler = han5)
  
  # Graphics mounted; Quit is always visible and enabled.
  enabled(but1) <- TRUE # Folder button
  enabled(but2) <- FALSE # Run button
  enabled(but3) <- FALSE # html button
  enabled(but4) <- FALSE # pdf button

  ## Look for .raw files before Run.
  file_list <- list.files(path = dname)

  # Use first raw file in list
  raw_list <- file_list[grepl(".raw", file_list)]
  if (length(raw_list) > 0) {
    assign("raw_path", file.path(normalizePath(dname, "\\"), raw_list[[1]], fsep="\\"), currentenv)
    cat("Found .raw file:", raw_path, "\n")
    
    #Change visibilities; Quit is always visible and enabled.
    enabled(but1) <- TRUE # Folder button; keep enabled in case user selects wrong folder
    enabled(but2) <- TRUE  # Run button
    enabled(but3) <- FALSE # html button
    enabled(but4) <- FALSE # pdf button
  }

} # end Calling
