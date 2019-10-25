# Summary:
# A widget wrapper to generate a PTXQC Quality Control of MS^2 instrument
# based on MaxQuant analysis.
# Intended to be run with local R-3.5.0 installation on User,
# and will place warnings log file on User Desktop.
# If installed to be run from a Windows shortcut,
# the shortcut properties should be
# Target: %COMSPEC% /C Rscript --vanilla "%USERPROFILE%\Work Folders\Desktop\ProteoPipe\ProteoPipe_wrapper.R"
# Start in: "C:\Program Files\R\R-3.5.0\bin"
# Input arguments:
# None
# Output:
# Console text goes to log file; runs are time stamped. A temporary log file is used for MaxQuant control, 
# then merged with the main log file.
# Errors goes to console.
# Warnings goes to separate log file; program start is time stamped.
# Garbage collection is the generic, optimized R scheduling.

## Defaults
# Installation directory
dname <- file.path(Sys.getenv("USERPROFILE"), "Work Folders", "Desktop", "ProteoPipe", fsep="\\")

tryCatch({source(file.path(dname, "ProteoPipe_widget.R"))
  # Defaults
  console <- TRUE # Do-while (repeat) console open flag
  text_log <- file.path(dname, "log.txt", fsep="\\")
  warnings_log <- file.path(dname, "warnings.txt", fsep="\\")
  temp_file <- file.path(dname, "MQtemp.txt")
  logo <- c("-------------------------------------",
            "ProteoPipe v1 [June, 2019]",
            "Uppsala University",
            "Niklas Handin, et al.",
            "-------------------------------------")
  start_time <- paste(Sys.time())
  
  # Text log
  assign("con", file(text_log, open = "a", blocking = FALSE), environment())
  sink(con, split = TRUE)
  writeLines(logo, con)
  cat(start_time, "\n")
  
  # Warnings log
  con_temp <- file(warnings_log, open = "a", blocking = FALSE)
  writeLines(logo, con_temp)
  writeLines(start_time, con_temp)
  close(con_temp)

  # Call handler for each warning as they come, to reenter try/catch loop.
  withCallingHandlers(ProteoPipe_widget(),
                      # Warning object seems to have abbreviation 'w'
                      # W is a single warning list object when run with try/catch calling handler
                      warning=function(w) {
                        # Capture text message of warning condition object, without showing in console
                        # Note: conditionMessage(w) does that too
                        write(capture.output(cat("ProteoPipe_widget() warning:", conditionMessage(w),
                                                 "\n")), file=warnings_log, append=TRUE)
                        invokeRestart("muffleWarning")
                      })

  # Keep console open in background for messaging
  repeat{
    if (!console) {}
  }
})

