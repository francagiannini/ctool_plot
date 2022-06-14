

# take aver write the file scenarios and then run the functions 

run_in_folder <- function(i) {
  #browser()
  
# Make a folder with scenario ID name
folder_name <-
  paste(
    "C:\\Users\\au710823\\Dropbox\\Franca\\Aarhus\\ctool_plot\\",
    i[[1]],
    sep = ""
  )

dir.create(folder_name)
# set the working 

setwd(folder_name)

# Write data txt

write.table(as.data.frame(i[[2]]), 
            paste(folder_name, "\\data.txt", sep = ""), 
            sep ="\t",
            dec = ".",
            row.names = FALSE,
            quote=FALSE)

# Write input .txt

writeLines(i[[3]], paste(folder_name, "\\input.txt", sep = ""), sep = "\t")

# Make runscenarios .txt 

runscn <-
  c(paste("input", paste(folder_name, "\\input.txt", sep = ""), sep = "\t"),
    paste("data", paste(folder_name, "\\data.txt", sep = ""), sep = "\t"),
    paste("TemperatureData",
          "C:\\Users\\au710823\\OneDrive - Aarhus universitet\\ctool1st2022\\ctool_plot\\data\\temp_ask.txt",
          sep = "\t"),
    paste("outputDir", paste(folder_name, "\\", sep = ""),  sep = "\t")
  )

writeLines(runscn, paste(folder_name,
                         "\\runsenarios.txt",
                         sep = ""))


writeLines("0", paste(folder_name, 
                     "\\mode.txt", 
                     sep = ""))

# write.table(0, paste(folder_name, 
#                            "\\mode.txt", 
#                            sep = ""),  
#            sep = "\t",
#            row.names = FALSE,
#            quote=FALSE)

# copy ctool app

file.copy(
  from = "C:\\Users\\au710823\\OneDrive - Aarhus universitet\\ctool1st2022\\runs\\ctool2.3.exe",
  to = paste(folder_name,
             "\\ctool2.3.exe",
             sep = ""))

# Execute .exe in each folder

system2(paste(folder_name,
             "\\ctool2.3.exe",
             sep = ""))

# delete executable 

file.remove(from = paste(folder_name,
                         "\\ctool2.3.exe",
                         sep = ""))

setwd("C:\\Users\\au710823\\Dropbox\\Franca\\Aarhus\\ctool")

}


aver_plot <- readRDS("aver_plot.RDS")

lapply(aver_plot, run_in_folder)


setwd("C:\\Users\\au710823\\OneDrive - Aarhus universitet\\ctool1st2022")
