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
dir.create("c:/Users/imart/Desktop/NPS_5subcats/KFOLD")
MCAT_Training_PATH <- paste0("C:/Users/imart/Desktop/NPS_5subcats/MCAT/")
PMCAT_Testing_PATH <- paste0("C:/Users/imart/Desktop/NPS_5subcats/PMCAT/")
Kfold_PATH <- paste0("C:/Users/imart/Desktop/NPS_5subcats/KFOLD/")
dir.create(paste0(Kfold_PATH,"Training Files/"),showWarnings = F)
dir.create(paste0(Kfold_PATH,"Testing Files/"),showWarnings = F)


df<- readxl::read_excel(File1,sheet = 1)

#df <- read_excel("C:/Users/imart/Documents/5 SUBCATS Product Classification/5Subcat.xlsx",sheet = 1)

varr <- names(df)


#colnames(df)[names(df)=="Final Label"] <- "Label1"
#df1 <- df[,c("GOOD PMCAT","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","Label1")]
#df1 <- df[,c(1,2,8,9,15,21)]
df1 <- df[,c("PRIME_MCAT_ID","FK_MCAT_TYPE_ID","PMCAT2","PMCAT ID","OTHER_MCATS","PRIME_MCAT_NAME","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","Label1","SUBCAT_NAME")]
#df1 <- df[,c(1:5,8,9)]
#fix(df)
#df2 <- df1[1:2000,]

df2 <- df1
df2 <- df2[!is.na(df2$PMCAT2),]
df2 <- df2[!df2$PMCAT2==0, ]
colnames(df2)[colnames(df2)=="PC_ITEM_DESC_SMALL"] <- "New_Desc"
MCAT_count <- data.frame(table(df2$PRIME_MCAT_NAME))
summary(MCAT_count$Freq)

Five_subcat_pmcats <- data.frame(table(df2$PMCAT2))


#PMCATs1a <-  unique(df2[,c("PRIME_MCAT_ID","PRIME_MCAT_NAME")]) 
#PMCATs1b <- unique(df2[,c("PMCAT2","PMCAT ID")]) 
#PMCATs1b <- na.omit(PMCATs1b)
#colnames(PMCATs1a)[colnames(PMCATs1a)=="PRIME_MCAT_ID"] <- "MCAT_ID"
#colnames(PMCATs1a)[colnames(PMCATs1a)=="PRIME_MCAT_NAME"] <- "MCAT_NAME"
#colnames(PMCATs1b)[colnames(PMCATs1b)=="PMCAT2"] <- "MCAT_NAME"
#colnames(PMCATs1b)[colnames(PMCATs1b)=="PMCAT ID"] <- "MCAT_ID"
#PMCATs <- unique(rbind(PMCATs1b,PMCATs1a))
#df2a <- merge(df2,PMCATs,by.x = "PMCAT2", by.y = "MCAT_NAME", all.x = T)
rm(PMCATs,PMCATs1b,PMCATs1a,df,df2)
colnames(df2a)[colnames(df2a)=="MCAT_ID"] <- "PMCAT_ID"

df3 <- df2a[,c("PRIME_MCAT_ID","PRIME_MCAT_NAME","FK_MCAT_TYPE_ID","PMCAT2","PMCAT_ID","OTHER_MCATS","PC_ITEM_ID","PC_ITEM_NAME","New_Desc","Label1","SUBCAT_NAME")]

colnames(MCAT_count) <- c("MCAT_NAME","Number_of_Products")
#freqoffreq <- data.frame(table(MCAT_count$Number_of_Products ))

###label loading

MCAT_count$multiplier <- ""

MCAT_count$multiplier <- ifelse(MCAT_count$Number_of_Products>=1500,MCAT_count$multiplier <- 0,
                         ifelse(MCAT_count$Number_of_Products>=1000 & MCAT_count$Number_of_Products < 1500,MCAT_count$multiplier <- 2,
                         ifelse(MCAT_count$Number_of_Products>=800 & MCAT_count$Number_of_Products < 1000, MCAT_count$multiplier <- 3,
                         ifelse(MCAT_count$Number_of_Products>=600 & MCAT_count$Number_of_Products < 800, MCAT_count$multiplier <- 4,
                         ifelse(MCAT_count$Number_of_Products>=400 & MCAT_count$Number_of_Products < 600, MCAT_count$multiplier <- 6,
                         ifelse(MCAT_count$Number_of_Products>=200 & MCAT_count$Number_of_Products < 400, MCAT_count$multiplier <- 10,
                         ifelse(MCAT_count$Number_of_Products>=100 & MCAT_count$Number_of_Products < 200, MCAT_count$multiplier <- 15,
                         ifelse(MCAT_count$Number_of_Products>=80 & MCAT_count$Number_of_Products < 100, MCAT_count$multiplier <- 20,
                         ifelse(MCAT_count$Number_of_Products>=60 & MCAT_count$Number_of_Products < 80, MCAT_count$multiplier <- 25,
                         ifelse(MCAT_count$Number_of_Products>=50 & MCAT_count$Number_of_Products < 60, MCAT_count$multiplier <- 30,
                         ifelse(MCAT_count$Number_of_Products>=40 & MCAT_count$Number_of_Products < 50, MCAT_count$multiplier <- 40,
                         ifelse(MCAT_count$Number_of_Products>=30 & MCAT_count$Number_of_Products < 40, MCAT_count$multiplier <- 50,
                         ifelse(MCAT_count$Number_of_Products>=25 & MCAT_count$Number_of_Products < 30, MCAT_count$multiplier <- 60,
                         ifelse(MCAT_count$Number_of_Products>=20 & MCAT_count$Number_of_Products < 25, MCAT_count$multiplier <- 65,
                         ifelse(MCAT_count$Number_of_Products==20 , MCAT_count$multiplier <- 70,
                         ifelse(MCAT_count$Number_of_Products==19 , MCAT_count$multiplier <- 70,
                         ifelse(MCAT_count$Number_of_Products==18 , MCAT_count$multiplier <- 72,      
                         ifelse(MCAT_count$Number_of_Products==17 , MCAT_count$multiplier <- 72,
                         ifelse(MCAT_count$Number_of_Products==16 , MCAT_count$multiplier <- 74,
                         ifelse(MCAT_count$Number_of_Products==15 , MCAT_count$multiplier <- 75,
                         ifelse(MCAT_count$Number_of_Products==14 , MCAT_count$multiplier <- 78,
                         ifelse(MCAT_count$Number_of_Products==13 , MCAT_count$multiplier <- 80,
                         ifelse(MCAT_count$Number_of_Products==12 , MCAT_count$multiplier <- 85,
                         ifelse(MCAT_count$Number_of_Products==11 , MCAT_count$multiplier <- 95,
                         ifelse(MCAT_count$Number_of_Products==10 , MCAT_count$multiplier <- 100,
                         ifelse(MCAT_count$Number_of_Products==9 , MCAT_count$multiplier <- 120,
                         ifelse(MCAT_count$Number_of_Products==8 , MCAT_count$multiplier <- 125,
                         ifelse(MCAT_count$Number_of_Products==7 , MCAT_count$multiplier <- 175,
                         ifelse(MCAT_count$Number_of_Products==6 , MCAT_count$multiplier <- 180,
                         ifelse(MCAT_count$Number_of_Products==5 , MCAT_count$multiplier <- 200,
                         ifelse(MCAT_count$Number_of_Products==4 , MCAT_count$multiplier <- 250,
                         ifelse(MCAT_count$Number_of_Products==3 , MCAT_count$multiplier <- 300,
                         ifelse(MCAT_count$Number_of_Products==2 , MCAT_count$multiplier <- 500,
                         ifelse(MCAT_count$Number_of_Products==1 , MCAT_count$multiplier <- 1000, ""))))))))))))))))))))))))))))))))))


MCAT_count$multiplier <-  as.numeric(MCAT_count$multiplier)

MCAT_count$MCAT_Total_Count <- ""

MCAT_count$MCAT_Total_Count <- ifelse(MCAT_count$multiplier ==0, MCAT_count$MCAT_Total_Count <- MCAT_count$Number_of_Products, MCAT_count$MCAT_Total_Count <- MCAT_count$Number_of_Products * MCAT_count$multiplier) 
                                      
mcatzer0 <- MCAT_count[(MCAT_count$multiplier==0),]
mcatzer0 <- mcatzer0$MCAT_NAME

#sum(MCAT_count$MCAT_Total_Count)
#length(unique (labelled_data$PRIME_MCAT_NAME))


labelled_data <- df3[ df3$PRIME_MCAT_NAME %in% mcatzer0, ]

k=1

for (k in 1:nrow(MCAT_count)) {

  ind <- df3[df3$PRIME_MCAT_NAME==MCAT_count$MCAT_NAME[k], ]
  data_mul <- ind[rep(seq_len(nrow(ind)), each=MCAT_count$multiplier[k] ),]
  labelled_data <- rbind(labelled_data,data_mul)
  
}


rm(data_mul,df2a,ind,mcatzer0)


df2 <- labelled_data

write.csv(df2,"df2.csv",row.names = F)
save.image("5 Subctas.RData")


colnames(Five_subcat_pmcats) <- c("PMCAT_NAME","Number of Products")
df2[is.na(df2$OTHER_MCATS),"OTHER_MCATS"] <- ""
Index_other <- df2$PMCAT2 ==df2$OTHER_MCATS
Index_other2 <- which(Index_other==TRUE)

df2[Index_other2,"OTHER_MCATS"] <- ""

PMCAT_LIST <- unique(df2$PMCAT2)

SUPER_PMCAT_LIST <- df2[df2$FK_MCAT_TYPE_ID==1,"PRIME_MCAT_NAME"]

TABLE_DF2 <- data.frame(table(df2$PRIME_MCAT_NAME))

SUPER_PMCAT_LIST <- SUPER_PMCAT_LIST[!is.na(SUPER_PMCAT_LIST$PRIME_MCAT_NAME),]

SUPER_PMCAT_LIST <- unique(SUPER_PMCAT_LIST$PRIME_MCAT_NAME)
#SUPER_PMCAT_LIST <- as.vector(SUPER_PMCAT_LIST)
PMCAT_LIST <- c(PMCAT_LIST,SUPER_PMCAT_LIST)

df2 <- df2[df2$FK_MCAT_TYPE_ID !=1,]

Index_of_PMCAT <- which(df2$OTHER_MCATS %in% PMCAT_LIST)

df2[Index_of_PMCAT,"OTHER_MCATS"] <- ""

df2[is.na(df2$Label1),"Label1"] <- ""

df2 <- df2[!is.na(df2$PRIME_MCAT_ID),]

df2[is.na(df2$PMCAT2),c("PMCAT2","PMCAT ID")] <- ""

#for (i in 1:length(PMCAT_LIST)) {
# ifelse(df2$OTHER_MCATS[i]==df2$PMCAT2[i],df2$OTHER_MCATS[i] <- "",df2$OTHER_MCATS[i] <- as.character(df2$OTHER_MCATS[i]))
#}


df2$N_STRING <- paste("__label__n_Excavator __label__n_Motor_Grader __label__n_Excavator_Parts __label__n_Backhoe_loader __label__n_Road_Roller __label__n_End_Loader __label__n_Bulldozer __label__n_Skid_Steer_Loader __label__n_Wheel_Loader __label__n_Trencher __label__n_Earthmoving_Bucket ")

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
df2$New_Desc_final <- rm_white(df2$New_Desc_final)
df2$New_Desc_final[1]
#now we need to clean the data
#now we need to clean the data


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
data_unique$concat_name <- gsub(","," ",data_unique$concat_name)

data_unique$concat_name <- rm_white(data_unique$concat_name)
data_unique$New_Desc1 <- NULL
data_unique$New_Desc2 <- NULL
data_unique$New_Desc3 <- NULL
data_unique$New_Desc4 <- NULL
data_unique$New_Desc5 <- NULL
data_unique$New_Desc_final <- NULL
data_unique$New_Desc6 <- NULL

##formating mcata and pmcat names also adding pmcat ids alongwith mcat name in label.

data_unique$PRIME_MCAT_NAME1 <- gsub(" ","_",data_unique$PRIME_MCAT_NAME)
data_unique$PRIME_MCAT_NAME1 <- paste0(data_unique$PRIME_MCAT_NAME1,"_",data_unique$`PMCAT ID`)
#data_unique[is.na(data_unique$Other_PMCAT_ID),"Other_PMCAT_ID"] <- ""
warnings()

length(!is.na(data_unique$PMCAT2))


Prime_Pmcat_index <- which(data_unique$FK_MCAT_TYPE_ID==2)


data_unique[Prime_Pmcat_index,"PMCAT2"] <- data_unique[Prime_Pmcat_index,"PRIME_MCAT_NAME"]

data_unique$PRIME_MCAT_NAME1[Prime_Pmcat_index] <- ""

data_unique$OTHER_MCATS1 <- gsub(" ","_",rm_white(data_unique$OTHER_MCATS))
data_unique$PMCAT_C <- gsub(" ","_",data_unique$PMCAT2)
data_unique$PMCAT_C[Blank_PMCAT_INDEX] <- paste0(data_unique$PMCAT_C[Blank_PMCAT_INDEX],"_",data_unique$`PMCAT ID`[Blank_PMCAT_INDEX])
data_unique$PRIME_MCAT_NAME1 <- paste0("__label__p_",data_unique$PRIME_MCAT_NAME1)
data_unique[Prime_Pmcat_index,"PRIME_MCAT_NAME1"] <- ""
Other_blank_index <- which(data_unique$OTHER_MCATS!="")
data_unique$OTHER_MCATS1[Other_blank_index] <- paste0("__label__s_",data_unique$OTHER_MCATS1[Other_blank_index],"_",data_unique$`PMCAT ID`[Other_blank_index])

Blank_PMCAT_INDEX <- which(data_unique$PMCAT2!= "")

sum(data_unique$`PMCAT ID`=="")

data_unique$PMCAT_C[Blank_PMCAT_INDEX] <- paste0("__label__pp_",data_unique$PMCAT_C[Blank_PMCAT_INDEX])
data_unique[data_unique$OTHER_MCATS1=="__label__s__","OTHER_MCATS1"] <- ""
#data_unique[data_unique$OTHER_MCATS1=="__label__s_Earthmoving_Machinery","OTHER_MCATS1"] <- ""
#data_unique[data_unique$OTHER_MCATS1=="__label__s_Earthmoving_Machinery_Parts","OTHER_MCATS1"] <- ""

data_unique$n_Rem <- gsub("__label__pp_","__label__n_",data_unique$PMCAT_C)

for (i in 1:nrow(data_unique)) {
  data_unique$N_STRING[i] <- gsub(data_unique$n_Rem[i]," ",data_unique$N_STRING[i])
}


data_unique$N_STRING <- rm_white(data_unique$N_STRING)

data_unique$N_STRING[1]

REM_PM <- data_unique$PMCAT2==data_unique$PRIME_MCAT_NAME

REM_PM_index <- which(REM_PM==TRUE)

data_unique[REM_PM_index,"PRIME_MCAT_NAME1"] <- ""



sum(data_unique$FK_MCAT_TYPE_ID==2)

data_unique2 <- data_unique[data_unique$PMCAT2!="",]


#data_unique$FASTTEXT_LABEL <- paste0(data_unique$PRIME_MCAT_NAME1," ",data_unique$OTHER_MCATS1," ",data_unique$PMCAT_C," ",data_unique$concat_name)
#data_unique$FASTTEXT_LABEL <- rm_white(data_unique$FASTTEXT_LABEL)

data_unique2$FASTTEXT_LABEL <- paste0(data_unique2$PRIME_MCAT_NAME1," ",data_unique2$OTHER_MCATS1," ",data_unique2$PMCAT_C," ",data_unique2$concat_name)

write.table(data_unique2$FASTTEXT_LABEL,"c:/Users/imart/Desktop/nps_levele_5SUBCATS_with_ids.txt",row.names = F,col.names = F,quote = F)


#data_unique$Fasttext_label <- paste0("__label__",data_unique$PRIME_MCAT_NAME1," ",data_unique$concat_name)
#data_unique$Fasttext_label <- paste0("__label__",data_unique$PRIME_MCAT_NAME1," ",data_unique$concat_name)
data_unique$Fasttext_test <- paste0(data_unique$concat_name)
data_unique$Fasttext_test <- rm_white(data_unique$Fasttext_test)
#write.table(data_unique$Fasttext_test,"E:/Conduit/pmcat_testing.txt",sep = "\t",col.names = F,quote = F,row.names = F)
#write.csv(data_unique,"E:/Conduit/pmcat_21K_vlookup.csv",row.names = F,quote = F)
table(data_unique$PMCAT2)
data_unique <- data_unique[order(data_unique$PMCAT2),]
sum(grepl(",",data_unique$New_Desc1)==T)

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

vec1 <- "__label__Backhoe_Loader backhoe loader"
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
