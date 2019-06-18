#test
print("Hello World!")

#txt_folder <- "F:/Working folder/TL/20190411/combined/txt"
#txt_folder <- "C:/Users/torla438/Work Folders/Documents/QC/20190411/combined"
#txt_folder <- "C:/Users/torla438/Work Folders/Documents/QC/PTXQC test/txt_20min/txt_20min"
#txt_folder <- "C:/Users/torla438/Work Folders/Documents/QC/PTXQC test/txt_100min"
txt_folder <- "C:/Users/torla438/Work Folders/Documents/QC/20190411/combined/txt"


require(PTXQC)
require(yaml)
require(methods)

#r <- createReport(txt_folder)
yaml_list_object <- yaml.load_file("C:/Users/torla438/Work Folders/Documents/QC/20190411/combined/txt/report_v0.92.6.yaml")
r <- createReport(txt_folder, yaml_list_object)

#git test