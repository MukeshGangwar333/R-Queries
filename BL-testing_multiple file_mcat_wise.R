library(psych)
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")
#?qdapRegex



df<- readxl::read_excel("C:/Users/imart/Documents/Excavator-PG2/Excavator Buy Lead Data input file.xlsx",sheet = 3)


#gsub("\\.","","i am a . dggf.f")
#df1 <- df[,c(1:4,6,14,15)] 
df1 <- df
df1 <- df1[order(df1$FK_GLCAT_MCAT_ID),]
df1$testing <- df1$Label1
df1$testing <- gsub(","," ",df1$testing)
df1$testing <- rm_white(df1$testing) #to remove multiple white spaces with single space
#df1$testing <- gsub(" ","",df1$testing)
df1$testing <- gsub("[0-9] \\w+ *", "",df1$testing)
df1$testing <- gsub("[0-9]\\w+ *", "",df1$testing)
df1$testing <- gsub("\\.","",df1$testing)
df1$testing <- gsub("[[:punct:]]","",df1$testing)
df1$testing <- gsub("-","",df1$testing)
#df1$testing <- gsub(",","",df1$testing)
#df1$testing <- gsub("[:digit:]","",df1$testing)
df1$testing <- gsub("[0-9]","",df1$testing)
#df1$testing <- rm_white(df1$testing)
#index <- grepl("\\d",df1$testing)
#removing duplicate words:
rem_dup.one <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = " ")
}

df1$testing <- lapply(df1$testing,rem_dup.one)
df1$testing <- as.character(df1$testing)
df1$testing <- gsub("NA","",df1$testing)

#Function to remove Stopwords like inr, indian, rupees.
library(tm)
Funrem_stopBL <- function(x)
  {
  Stopwords <- c("inr","indian","rupees","rupee","unit","meter","kilometer","to","upto","and")
  x <- removeWords(x,Stopwords)
  return(x)
}

df1$testing <- lapply(df1$testing,Funrem_stopBL)
df1$testing <- as.character(df1$testing)

df1$ETO_OFR_TITLE1 <- tolower(df1$ETO_OFR_TITLE)
df1$ETO_OFR_TITLE1 <- gsub("[[:punct:]]"," ",df1$ETO_OFR_TITLE1)
df1$fasttextTest <- paste0(df1$ETO_OFR_TITLE1," ",df1$testing)
df1$fasttextTest <- rm_white(df1$fasttextTest)
#df1$fasttextTest <- gsub("[0-9] \\w+","",df1$fasttextTest)
#df1$fasttextTest <- gsub("[0-9]\\w+","",df1$fasttextTest)
#df1$fasttextTest <- gsub("[0-9]","",df1$fasttextTest)
df1$fasttextTest <- gsub("\\.","",df1$fasttextTest)
df1$fasttextTest <- gsub("-","",df1$fasttextTest)
df1$fasttextTest <- gsub("&","",df1$fasttextTest)
#df1$fasttextTest <- gsub("/","",df1$fasttextTest)
df1$fasttextTest <- gsub("%","",df1$fasttextTest)
#df1$fasttextTest <- gsub(" o ","",df1$fasttextTest)
a <- gsub("[0-9] \\w+ *","","Single Drum Roller 5000 Kg JCB Single Drum Roller Gravel Road JCB 5000 Kg Gravel Road")
b <- gsub("[0-9]\\w+ *","",a)
#testing_9_pmcat <- df1$fasttextTest
#write.table(testing_9_pmcat,"E:/Ayush_PC/9_pmcat_bl_test_930.txt",sep = "\t",row.names = F,col.names = F,quote = F)
df1$MCAT_NAME <- gsub(" ","_",df1$GLCAT_MCAT_NAME)
df1$PMCAT_NAME1 <- gsub(" ","_",df1$PMCAT_NAME)
df1$fasttext_label1 <- paste0("__label__",df1$PMCAT_NAME1," ",df1$fasttextTest)
df1$fasttext_label <- paste0("__label__",df1$MCAT_NAME," ",df1$fasttextTest)
df1 <- df1[order(df1$GLCAT_MCAT_NAME),c(1:4,20:24)]
#df2 <- df1[df1$FK_GLCAT_MCAT_ID!=852,]
#df3 <- df1[df1$FK_GLCAT_MCAT_ID==852,]
#df2 <- gsub(",","'",df2)
#df1 <- gsub(",","'",df1)
write.csv(df1,"C:/Users/imart/Documents/Excavator-PG2/ExcavatorBuyLeadDataoutputfile.csv",row.names = F,quote = F)

write.table(df3$fasttextTest,"C:/Users/imart/Documents/Excavatorbuyleadfasttext.txt",row.names = F,col.names = F,quote = F)
#df1 <- df1[order(df1$GLCAT_MCAT_NAME),c(1:4,20,22)]

df3 <- df1[df1$GLCAT_MCAT_NAME!=df1$PMCAT_NAME,]

write.csv(df3,"vlookup_1.csv",row.names = F,quote = F)

table(df3$PMCAT_NAME)

#Getting PMCAt wise BL data

df3 <- df3[order(df3$PMCAT_NAME),]

out2 <- split(df3,f=df3$PMCAT_NAME)

i <- 1

for (i in 1:length(out2)) {
  hold_a <- unique(out2[[i]]$PMCAT_NAME)
  a_a <- out2[[i]]$fasttext_label
  Location_Y <- paste0("C:/Users/imart/Documents/Excavator-PG2/BL/",hold_a,"bl.txt")
  write.table(a_a,Location_Y,sep = "\t",row.names = F,col.names = F,quote = F)
  
}


PMCAT_Training_complete <- df3$fasttext_label1

write.table(PMCAT_Training_complete,"C:/Users/imart/Documents/Excavator-PG2/pmcat_lebel_trainining.txt",sep = "\t",row.names = F,col.names = F,quote = F)


out <- split(df1,f=df1$GLCAT_MCAT_NAME)
j <- 1
for (j in 1:length(out)) {
  
  hold <- unique(out[[j]]$GLCAT_MCAT_NAME)
  hold2 <- unique(out[[j]]$FK_GLCAT_MCAT_ID)
  a <- out[[j]]$fasttextTest
  Location_W <- paste0("C:/Users/imart/Documents/Excavator-PG2/Bl-testing/",hold,"bl.txt")
  write.table(a,Location_W,sep = "\t",row.names = F,col.names = F,quote = F)
}



write.csv(df1,"C:/Users/imart/Documents/Excavator-PG2/vlookup_9_pmcats.csv",row.names = F,quote = F)




save.image(file = "C:/Users/imart/Documents/Excavator-PG2/Excavator_Buy_Lead.RData")
