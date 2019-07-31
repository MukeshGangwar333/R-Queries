library(xlsx)
library(readxl)
library(formattable)
library(stringr)
library(reshape2)

#filename <- basename("C:/Users/imart/Desktop/R Folder/exacavator_brand1.xlsx") #extract the file name from the path
input_files <- list.files(path="C:/Users/imart/Desktop/R Folder/Input_Files/",pattern = ".xlsx")
output_files <- list.files(path = "C:/Users/imart/Desktop/R Folder/Output_Files/",pattern = ".xlsx")
#files <- gsub(".xlsx","",files)

result <- data.frame(TRU=as.numeric(),
                     FALS=as.numeric(),
                     "input_files[i]"=character())

colnames(result)
i <- 1

for (i in 1:length(input_files)) 
  {
  location_input <- paste0("C:/Users/imart/Desktop/R Folder/Input_Files/",input_files[i])
  df <- read_excel(location_input,sheet = 1) #read the file 
  
  df2 <- df[,c("test_data","V2","mcat1","rating1","mcat2","rating2","Existing","Item_Id")]
  df2 <- as.data.frame(df2)
  #class(df2$rating2)
  #sum(is.na(df2))   count of NA
  #df2[is.na(df2)]
  df2[df2$rating1=="NA",c("rating1","rating2")] <- ""  #removing NA and assign blanks
  df2$true_false<-ifelse(df2$mcat1==df2$Existing,"TRUE","FALSE")
  df2$greater_mean <-ifelse(df2$rating1>(mean(as.numeric(df2$rating1),na.rm = T)),"TRUE","FALSE") 
  #df2$rating1 <- as.numeric(df2$rating1)  #to convert class of rating1
  #class(df2$rating1[658])
  df3 <- df2[df2$greater_mean=="TRUE" & df2$true_false=="TRUE",]
  
  #location_output <- paste0("C:/Users/imart/Desktop/R Folder/Output_Files/",output_files[i])
  
  testing_file <- read_excel("C:/Users/imart/Desktop/R Folder/Output_Files/validation.xlsx",sheet = 1)
  testing_file <- as.data.frame(testing_file)
  check <- merge(df3,testing_file,by.x = "Item_Id",by.y = "Item_Id",all.x = T )
  check1 <- check[,c("Item_Id","cat_suggestion","mcat1","rating1")]
  #fix(check1)
  check1$TrueFalse <-ifelse(check1$cat_suggestion==check1$mcat1,"True","False") 
  a <- table(check1$TrueFalse)
  TR <- as.data.frame(as.numeric(a["True"]))
  colnames(TR)[colnames(TR)=="as.numeric(a[\"True\"])"] <- "TRU"
  FA <- as.data.frame(as.numeric(a["False"]))
  colnames(FA)[colnames(FA)=="as.numeric(a[\"False\"])"] <- "FALS"
  res <- cbind(TR,FA,input_files[i])
  result <- rbind(result,res)
 #percentage <- percent(prop.table(count))
 }