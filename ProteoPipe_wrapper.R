#Summary:
#A widget wrapper to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.
#Intended to be run with local R-3.5.0 installation on User,
#and will place warnings log file on User Desktop.
#If installed to be run from a Windows shortcut,
#the shortcut properties should be
#Target: %COMSPEC% /C Rscript --vanilla "%USERPROFILE%\Work Folders\Desktop\ProteoPipe\ProteoPipe_wrapper.R"
#Start in: "C:\Program Files\R\R-3.5.0\bin"
#Input arguments:
#None
#Output:
#Errors goes to console.
#Warnings goes to log file.
#Garbage collection is the generic, optimized R scheduling.

tryCatch({source("C:/Users/torla438/Work Folders/Documents/QC/ProteoPipe/ProteoPipe_widget.R")
          console <- TRUE
          warnings_log <- file.path(normalizePath(Sys.getenv("USERPROFILE"), winslash='/'),
                                     "Work Folders", "Desktop", "ProteoPipe", "warnings.txt", fsep='/')
          cat("Warnings are logged in ", warnings_log, "\n")
          
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


