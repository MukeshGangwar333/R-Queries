library(psych)
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")
#Folder selection for reading and writing files
File1 <- choose.files()  # select excel file that needs to be processed.
MCAT_Training_PATH <- paste0("C:/Users/imart/Documents/Compressor/MCAT/")
PMCAT_Testing_PATH <- paste0("C:/Users/imart/Documents/Compressor/PMCAT/")
Kfold_PATH <- paste0("C:/Users/imart/Documents/Compressor/Kfold/")
dir.create(paste0(Kfold_PATH,"Training Files/"),showWarnings = F)
dir.create(paste0(Kfold_PATH,"Testing Files/"),showWarnings = F)




df<- readxl::read_excel(File1,sheet = 1)

varr <- names(df)
varr
#colnames(df)[names(df)=="Final Label"] <- "Label1"
#df1 <- df[,c("GOOD PMCAT","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","Label1")]
#df1 <- df[,c(1,2,8,9,15,21)]
df1 <- df[,c("PRIME_MCAT_ID","PMCAT2","PRIME_MCAT_NAME","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC","Label","Label1")]
#df1 <- df[,c(1:5,8,9)]
#fix(df)
#df2 <- df1[1:2000,]

df2 <- df1
colnames(df2)[colnames(df2)=="PC_ITEM_DESC_SMALL"] <- "New_Desc"
table(df2$PRIME_MCAT_NAME)
table(df2$PMCAT2)
#?cut
#creating a user defined function to identify bullet points description
df2$New_Desc1 <- gsub("</li li>"," ",df2$New_Desc)
df2$New_Desc1 <- gsub("</li>"," ",df2$New_Desc1)
df2$New_Desc1 <- gsub("<li>"," ",df2$New_Desc1)
df2$New_Desc1 <- gsub("<p>"," ",df2$New_Desc1)
#df2$New_Desc1 <- gsub("</b>"," ",df2$New_Desc1)
#df2$New_Desc1 <- gsub("<b>"," ",df2$New_Desc1)
df2$New_Desc1 <- gsub("<br />"," ",df2$New_Desc1)

fun_bullet <- function(x)
{
  test1 <- rm_between(x,"<ul>","</ul>",extract = T)
  test1 <- test1[[1]]
  split_data <- unlist(str_split(test1,"([<ul>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}

fun_bullet_b <- function(x)
{
  test1 <- rm_between(x,"<b>","</b>",extract = T)
  test1 <- test1[[1]]
  split_data <- unlist(str_split(test1,"([<b>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}

fun_bullet_li <- function(x)
{
  test1 <- rm_between(x,"<li>","</li>",extract = T)
  test1 <- test1[[1]]
  split_data <- unlist(str_split(test1,"([<li>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}

fun_table_data <- function(x)
{
  test1 <- rm_between(x,"<td>","</td>",extract = T)
  test1 <- test1[[1]]
  split_data <- unlist(str_split(test1,"([<td>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}



class(df2$New_Desc)

df2$New_Desc1 <- lapply(df2$New_Desc1,fun_bullet)
df2$New_Desc2 <- lapply(df2$New_Desc,fun_bullet_b)
df2$New_Desc3 <- lapply(df2$New_Desc,fun_bullet_li)
df2$New_Desc1 <- as.character(df2$New_Desc1)
df2$New_Desc2 <- as.character(df2$New_Desc2)
df2$New_Desc3 <- as.character(df2$New_Desc3)

df2$New_Desc4 <- sapply(strsplit(df2$New_Desc,":"),"[",2)
df2$New_Desc5 <- sapply(strsplit(df2$New_Desc,"-"),"[",2)
df2$New_Desc6 <- lapply(df2$New_Desc,fun_table_data)
df2$New_Desc6 <- as.character(df2$New_Desc6)

df2$New_Desc1 <- rm_white(df2$New_Desc1)
df2$New_Desc2 <- rm_white(df2$New_Desc2)
df2$New_Desc3 <- rm_white(df2$New_Desc3)
df2$New_Desc6 <- rm_white(df2$New_Desc6)

for (i in 1:nrow(df2)) {
  ifelse(df2$New_Desc1[i]=="NA",df2$New_Desc1[i] <- "",df2$New_Desc1[i] <- as.character(df2$New_Desc1[i]))
}

for (i in 1:nrow(df2)) {
  ifelse(df2$New_Desc2[i]=="NA",df2$New_Desc2[i] <- "",df2$New_Desc2[i] <- as.character(df2$New_Desc2[i]))
}

for (i in 1:nrow(df2)) {
  ifelse(df2$New_Desc3[i]=="NA",df2$New_Desc3[i] <- "",df2$New_Desc3[i] <- as.character(df2$New_Desc3[i]))
}

for (i in 1:nrow(df2)) {
  ifelse(df2$New_Desc6[i]=="NA",df2$New_Desc6[i] <- "",df2$New_Desc6[i] <- as.character(df2$New_Desc6[i]))
}
#</p p>

df2$New_Desc_final <- paste(df2$New_Desc1,df2$New_Desc2,df2$New_Desc3,df2$New_Desc4,df2$New_Desc6)
df2$New_Desc_final[1]
#NA NA
#removing NA 
df2$New_Desc_final <- gsub("\\s+ NA","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("NA NA","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("NA ","",df2$New_Desc_final)
df2$New_Desc_final <- gsub(" NA","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</p p>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<p p>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</table>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<table>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</td>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</tr>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<tr>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<td>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</br>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<br/>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<br />","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<br>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("<b>","",df2$New_Desc_final)
df2$New_Desc_final <- gsub("</b>","",df2$New_Desc_final)
#now we need to clean the data


#df_check2 <- cbind(df1[1:2000,],df2)
df_check2 <- df2
df_check2$New_Desc <- df_check2$New_Desc_final
df_check2$New_Desc <- tolower(df_check2$New_Desc)

#df_check2 <- df_check2[,-c(6:9)]
df_check2$New_Desc <- gsub(","," ",df_check2$New_Desc)
df_check2$New_Desc <- rm_white(df_check2$New_Desc)
df_check2$New_Desc <- gsub("</li li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("</li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<ol>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("</ul>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<ul>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<p>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<b>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<br />"," ",df_check2$New_Desc)
df_check2$New_Desc <- as.character(df_check2$New_Desc)
df_check2$New_Desc <- gsub("[[:punct:]]"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9] \\w+ *", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9]\\w+ *", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9]", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("\\s+"," ",str_trim(df_check2$New_Desc))
#df_check2$New_Desc <- gsub("stee","steel",str_trim(df_check2$New_Desc))
df_check2$New_Desc <- rm_white(df_check2$New_Desc) #to remove multiple white spaces with single space

for (i in 1:nrow(df_check2)) {
  ifelse(df_check2$New_Desc[i]=="na",df_check2$New_Desc[i] <- "",df_check2$New_Desc[i] <- as.character(df_check2$New_Desc[i]))
}

#removing duplicate words:
rem_dup.one <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = " ")
}

df_check2$New_Desc <- lapply(df_check2$New_Desc,rem_dup.one)
df_check2$New_Desc <- as.character(df_check2$New_Desc)
#df_check2$PC_ITEM_DESC_SMALL.1 <- tolower(df_check2$PC_ITEM_DESC_SMALL.1)


Stopwords1 <- c(" we "," are "," dealing "," quality "," manufacturers "," manufacturer "," exporters "," supplier "," dealer ",
                " good "," topmost "," business "," trusted "," finest "," offer ","offering"," involved "," provide "," reputed "," company ",
                " organization "," trader "," trading ")
#create a user defined function to remove stop words
library(tm)
Funrem_stop <- function(x)
{
  Stopwords <- c("we","are","dealing","quality","manufacturers","manufacturer","exporters","supplier","dealer",
                 "good","topmost","business","nbsp","trusted","finest","offer","offering","involved","provide","reputed","company",
                 "organization","trader","trading","inr","indian","rupees","rupee","features","specifications","material","feature","specification","materials","size")
  x <- removeWords(x,Stopwords)
  return(x)
}


df_check2$New_Desc <- lapply(df_check2$New_Desc,Funrem_stop)
length(!duplicated(df_check2$PC_ITEM_ID))
#data <- data.frame(lapply(df_check2, function(x) { gsub("na", "", x)}))
df_check2$New_Desc <- as.character(df_check2$New_Desc)
#data_unique <- df_check2[!duplicated(df_check2$PC_ITEM_ID),]
#data_unique <- dplyr::mutate(data_unique,concat_data <- paste0())
data_unique <- df_check2

data_unique$PC_ITEM_NAME1 <- tolower(data_unique$PC_ITEM_NAME)
data_unique$PC_ITEM_NAME1 <- rm_white(data_unique$PC_ITEM_NAME1)
data_unique$PC_ITEM_NAME1 <- gsub("[[:punct:]]","",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[0-9] \\w+ *", "",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[0-9]\\w+ *", "",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[[:digit:]]","",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- rm_white(data_unique$PC_ITEM_NAME1)
#data_unique$Label2 <- NULL
index <- grepl("\\d",data_unique$Label1)
#for (i in 1:nrow(data_unique)) {
# ifelse(index[i]==TRUE,data_unique$Label1[i] <- "",data_unique$Label1[i] <- as.character(data_unique$Label1[i]))
#}

for (i in 1:nrow(data_unique)) {
  ifelse(data_unique$New_Desc[i]=="na",data_unique$New_Desc[i] <- "",data_unique$New_Desc[i] <- as.character(data_unique$New_Desc[i]))
}
#fix(data_unique)

data_unique$Label1 <- gsub(","," ",data_unique$Label1)
data_unique$Label1 <- lapply(data_unique$Label1,rem_dup.one)
data_unique$Label1 <- as.character(data_unique$Label1)
data_unique$Label1 <- rm_white(data_unique$Label1)
for (i in 1:nrow(data_unique)) {
  ifelse(data_unique$Label1[i]=="na",data_unique$Label1[i] <- "",data_unique$Label1[i] <- as.character(data_unique$Label1[i]))
}
data_unique$Label1 <- gsub("[[:punct:]]","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9] \\w+ *","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9]\\w+ *","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9]","",data_unique$Label1)
#data_unique$Label1 <- gsub("[[:punct:]]","",data_unique$Label1)
data_unique$Label1 <- lapply(data_unique$Label1,rem_dup.one)
data_unique$Label1 <- as.character(data_unique$Label1)
data_unique$Label1 <- tolower(data_unique$Label1)
##removing stopeords from option_description:
# Import list of all stopwords
#stopwords = readLines('E:/Product classification-fasttext/stopwords_excavator.txt')     #Your stop words file

# Function to remove stopwords
removeWords1 <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}
#i<-2

for (i in 1:nrow(data_unique)) {
  ifelse(data_unique$Label1[i]=="na",data_unique$Label1[i] <- "",data_unique$Label1[i] <- as.character(data_unique$Label1[i]))
}


data_unique$Label1 <- gsub("[0-9]","",data_unique$Label1)
#data_unique$Label1 <- tolower(data_unique$Label1)
data_unique$concat_name <- paste(data_unique$PC_ITEM_NAME1,data_unique$Label1,data_unique$New_Desc)
data_unique$concat_name <- rm_white(data_unique$concat_name)
data_unique$concat_name <- gsub("NA","",data_unique$concat_name)
data_unique$concat_name <- tolower(data_unique$concat_name)
data_unique$concat_name <- lapply(data_unique$concat_name,rem_dup.one)
data_unique$concat_name <- as.character(data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9] \\w+ *","",data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9]\\w+ *","",data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9]","",data_unique$concat_name)
data_unique$concat_name <- gsub(":","",data_unique$concat_name)
#data_unique$PRIME_MCAT_NAME1 <- gsub(" ","_",data_unique$PRIME_MCAT_NAME)
data_unique$PRIME_MCAT_NAME1 <- gsub(" ","_",data_unique$PMCAT2)
data_unique$Fasttext_label <- paste0("__label__",data_unique$PRIME_MCAT_NAME1," ",data_unique$concat_name)
#data_unique$Fasttext_label <- paste0("__label__",data_unique$PRIME_MCAT_NAME1," ",data_unique$concat_name)
data_unique$Fasttext_test <- paste0(data_unique$concat_name)
data_unique$Fasttext_test <- rm_white(data_unique$Fasttext_test)
#write.table(data_unique$Fasttext_test,"E:/Conduit/pmcat_testing.txt",sep = "\t",col.names = F,quote = F,row.names = F)
#write.csv(data_unique,"E:/Conduit/pmcat_21K_vlookup.csv",row.names = F,quote = F)
table(data_unique$PMCAT2)
data_unique <- data_unique[order(data_unique$PMCAT2),]
sum(grepl(",",data_unique$New_Desc1)==T)
data_unique$New_Desc1 <- NULL
data_unique$New_Desc2 <- NULL
data_unique$New_Desc3 <- NULL
data_unique$New_Desc4 <- NULL
data_unique$New_Desc5 <- NULL
data_unique$New_Desc_final <- NULL
data_unique$New_Desc6 <- NULL
data_unique$FK_IM_SPEC_OPTIONS_DESC <- NULL
data_unique$FK_IM_SPEC_MASTER_DESC <- NULL
data_unique$PC_ITEM_NAME <- gsub(","," ",data_unique$PC_ITEM_NAME)
data_unique$PC_ITEM_NAME <- rm_white(data_unique$PC_ITEM_NAME)

data_unique2 <- data_unique[,c(1:5,12,13)]

#writing PMCAt_training data
dir.create(paste0(PMCAT_Testing_PATH,"PMCAT_training/"),showWarnings = F)
dir.create(paste0(PMCAT_Testing_PATH,"PMCAT_testing/"),showWarnings = F)

PMCAT_TRAINING_LOC <- paste0(PMCAT_Testing_PATH,"PMCAT_training/","pmcat_training.txt")
PMCAT_Testing_LOC_T <- paste0(PMCAT_Testing_PATH,"PMCAT_testing/","pmcat_testing.txt")


##*********Only for splitting data for testing PMCAT products on Brand and Child MCATs**************##
##**********Now we are creating training files PMCAT wise************##

data_unique <- data_unique[order(data_unique$PMCAT2),]
data_unique2 <- data_unique2[order(data_unique2$PMCAT2),]
#wriring pmcat training files to disk
write.table(data_unique$Fasttext_test,PMCAT_Testing_LOC_T,sep = "\t",row.names = F,col.names = F,quote = F)
write.table(data_unique$Fasttext_label,PMCAT_TRAINING_LOC,sep = "\t",row.names = F,col.names = F,quote = F)

#data_unique2 <- data_unique[data_unique$CHILD_MCAT!=data_unique$PRIME_MCAT_NAME,]
#out2 <- split(data_unique2,f=data_unique2$PRIME_MCAT_NAME)
out2 <- split(data_unique,f=data_unique$PMCAT2)
unique(out2[[7]]$PMCAT2)
unique(out2[[7]]$PRIME_MCAT_NAME)

for (i in 1:length(out2)) {
  print(table(out2[[i]]$PMCAT2))
  print(table(out2[[i]]$PRIME_MCAT_NAME))
}

#write.table(data_unique$Fasttext_label,"E:\\Yarn_new/pmcat_training.txt",sep = "\t",row.names = F,quote = F,col.names = F)


New_LOC <- paste0(MCAT_Training_PATH,"vlookup.csv")
write.csv(data_unique2,New_LOC,row.names = F,quote = F)

#data_with_desc <- data_unique[data_unique$New_Desc!="",]
#data_unique$Label_count <- table(data_unique$PRIME_MCAT_NAME)
#temp training data for complete pmcat_mapped products:

#*************Creating a dummy variable of 50 rows*****************#

vec1 <- "__label__Air_Compressor Air Compressor"
vec2 <- data.frame(vec1)
vec2[1:500,1] <- vec1
vec2 <- dplyr::rename(vec2,"Fasttext_label"="vec1")
final_training <- data_unique$Fasttext_label
final_training <- data.frame(final_training)
final_training <- rename(final_training,"Fasttext_label"="final_training")
final_training2 <- rbind(final_training,vec2)

testing_out <- data_unique$Fasttext_test
#write.table(testing_out,"E:/Ayush_PC/search_test_863.txt",sep = "\t",row.names = F,col.names = F,quote = F)
#write.table(final_training,"E:\\Product Classification_new/earthmoving_41K_complete_training.txt",sep = "\t",row.names = F,col.names = F,quote = F)
##vlookup_sheet
data_vlook <- data_unique2
#write.csv(data_vlook,"E:/Ayush_PC/863_vlookup.csv",row.names = F,quote = F)
#Final_data <- data_unique[,c(1:3,9)]
Final_data <- data_unique2
Final_data$PC_ITEM_NAME <- gsub(","," ",Final_data$PC_ITEM_NAME)

training_pmcat <- Final_data$Fasttext_label
#train_temp <- Final_data[-(1:600),5]
#test_vlookup <- Final_data[c(1:600),c(2,6)]
#test_temp <- Final_data[c(1:600),6]
#write.table(train_temp,"E:/Yarn-product classification/PMCAT/validation_temp/train_temp.txt",sep = "\t",row.names = F,col.names = F,quote = F)
#write.csv(test_vlookup,"E:/Yarn-product classification\\PMCAT\\validation_temp/temp_vlookup.csv",row.names = F,quote = F)
#write.table(test_temp,"E:\\Yarn-product classification\\PMCAT\\test_temp.txt",sep = "\t",row.names = F,col.names = F,quote = F)
#write.table(final_training2,"E:/Ayush_PC/backhoe_loader_brand_with_dumy.txt",sep = "\t",row.names = F,col.names = F,quote = F)
#write.csv(Final_data,"E:\\Conduit/v_lookup_sampled.csv",row.names = F,quote = F)

#Kfold cross validation:
final_df<-Final_data[sample(nrow(Final_data)),]
#write.csv(final_df,"E:\\Conduit/v_lookup_sampled21K.csv",row.names = F,quote = F)

table(final_df$PMCAT2)
KF_new_vlookup <- paste0(Kfold_PATH,"vlookup_kfold.csv")
write.csv(final_df,KF_new_vlookup,row.names = F,quote = F)
#Create 10 equally size folds
folds <- cut(seq(1,nrow(final_df)),breaks=10,labels=FALSE)
#dir.create("E:/Product_classification_YARN/Polyester Yarn/Training Files2")
#Perform 10 fold cross validation
i<-1
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final_df[testIndexes, ]
  trainData <- final_df[-testIndexes, ]
  trainData <- data.frame(trainData[,c("Fasttext_label")])
  trainLocation<-paste0(Kfold_PATH,"Training Files/",i,".txt")
  testLocation<-paste0(Kfold_PATH,"Testing Files/",i,".txt")
  write.table(trainData,trainLocation,sep = "\t",row.names = F,col.names = F,quote = F)
  testData<-data.frame(testData[,c("Fasttext_test")])
  write.table(testData,testLocation,sep = "\t" ,row.names = F,col.names = F,quote = F)
  
  #Use the test and train data partitions however you desire...
}
