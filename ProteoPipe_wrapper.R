#Summary:
#A widget wrapper to generate a PTXQC Quality Control of MS^2 instrument
#based on MaxQuant analysis.
#Input arguments:
#None
#Output:
#Errors goes to console.
#Warnings goes to log file.

tryCatch({source("C:/Users/torla438/Work Folders/Documents/QC/ProteoPipe/ProteoPipe_Widget.R")
          warnings_log <<- file.path("C:/Users/torla438/Work Folders/Documents","warnings.txt", sep = "")
          cat("Warnings are logged in ", warnings_log, "\n")
          withCallingHandlers(ProteoPipe_widget(), 
                              warning=function(w) {
                                write(conditionMessage(w), file=warnings_log, append=TRUE)
                                invokeRestart("muffleWarning")
                              })
          while (1){}
})
