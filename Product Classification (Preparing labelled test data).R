file_list <- list.files("C:\\Users\\imart\\Documents\\Fuses\\Model_2\\MCAT\\Testing Files")
path <- "C:/Users/imart/Documents/Fuses/Model_2/MCAT/Testing Files/"


final_df <- data.frame(V1= character(),
                          label=character())


for (i in 1:length(file_list)) {

  
  dataframe <- read.table(paste0(path,file_list[i]), sep = "\n")
  dataframe$label <- ""
  dataframe$label <- paste0("__label__",gsub(".txt","",file_list[i])," ",dataframe$V1)
  final_df <- rbind(final_df,dataframe)
    
}


final_df$label <- tolower(final_df$label)
final_df$label <- trimws(final_df$label)


getwd()
