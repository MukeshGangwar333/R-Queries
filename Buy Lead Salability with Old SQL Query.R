options(java.parameters = "-Xmx8192m")
memory.limit(size=10240)

library(RJDBC)
library(xlsx)
library(dplyr)
library(sqldf)
library(stringr)
library(qdapRegex)
library(reshape2)
#library(wordcloud)
# load("celeb_t.RData")

#save.image(file = "Buy Lead 1 Cr data.RData")
BL_PATH <- choose.dir()
dir.create(paste0(BL_PATH,"/Train"),showWarnings = F)
dir.create(paste0(BL_PATH,"/Test"),showWarnings = F)

startDate<-Sys.Date()-30
endDate<- Sys.Date()-1
startDate <-format(startDate,format="%d-%b-%y")
endDate <-format(endDate,format="%d-%b-%y")

############################################Testing Code###############################################

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Imart/Desktop/sqldeveloper/jdbc/lib/ojdbc6.jar")
jdbcReportDB <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora9.intermesh.net:1521/STSGEN", "report", "report")


Query <- paste("select blstudy_atul.*,SECONDARY_MCAT, PRIME_MCAT_NAME, FREQUENCY, LOOKING_FOR_SUPPLIERS, PORPOSE
               from blstudy_atul, blstudy_atul2
               where blstudy_atul.ETO_OFR_DISPLAY_ID = blstudy_atul2.ETO_OFR_DISPLAY_ID")


QueryResult <- dbGetQuery(jdbcReportDB,Query)



df <- QueryResult
df1 <- df[order(df$ETO_OFR_DISPLAY_ID),]
df1$DUP_CHECK <- paste(df1$ETO_OFR_DISPLAY_ID,df1$FK_IM_SPEC_MASTER_DESC,df1$FK_IM_SPEC_OPTIONS_DESC)

df1 <- df1[!duplicated(df1$DUP_CHECK),]
df1$DUP_CHECK <- NULL
df1$OPTION_DESC_A <- ""
#write.csv(df1,"46.5_lakhs_New.csv",row.names = F,quote = F)
#df1 <- read.csv("C:/Users/imart/Documents/BL-Saleability/Bifurcation/Old Query Result/46.5 lakhs.csv")
#df1 <- df1[1500001:2000000,]

df1$OPTION_DESC_A <- ""
df1$OPTION_DESC_A[1] <- df1$FK_IM_SPEC_OPTIONS_DESC[1]
j <- 1

for (j in 1:nrow(df1)) {
  ifelse(df1$ETO_OFR_DISPLAY_ID[j+1]==df1$ETO_OFR_DISPLAY_ID[j],df1$OPTION_DESC_A[j+1] <- as.character(paste(df1$OPTION_DESC_A[j],df1$FK_IM_SPEC_OPTIONS_DESC[j+1])),df1$OPTION_DESC_A[j+1] <- as.character(df1$OPTION_DESC_A[j+1]))
}


df1$OPTION_DESC_B <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$ETO_OFR_DISPLAY_ID[i]!=df1$ETO_OFR_DISPLAY_ID[i+1],df1$OPTION_DESC_B[i] <- as.character(df1$OPTION_DESC_A[i]),df1$OPTION_DESC_B[i] <- "aaaooovvv")
}



df1 <- df1[df1$OPTION_DESC_B!="aaaooovvv",]

df1$S_NS <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$UNIQUE_SOLD[i]==1,df1$S_NS[i] <- "Sold",df1$S_NS[i] <- "Not_Sold")
}

df1$Retail_NR <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$RETAIL_FLAG[i]==1,df1$Retail_NR[i] <- "retail",df1$Retail_NR[i] <- "non_retail")
}

df1$Email_NE <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$EMAIL_FLAG[i]==1,df1$Email_NE[i] <- "email",df1$Email_NE[i] <- "no_email")
}

df1$FK_IM_SPEC_OPTIONS_DESC <- gsub("_"," ",df1$FK_IM_SPEC_OPTIONS_DESC)
#df1$ETO_OFR_TITLE <- gsub("_"," ",df1$ETO_OFR_TITLE)
#df1$GLUSR_USR_CITY <- gsub("_"," ",df1$GLUSR_USR_CITY)
df1$GLUSR_USR_CITY <- gsub("0","",df1$GLUSR_USR_CITY)
df1$PRIME_MCAT_NAME <- sub("_"," ",df1$PRIME_MCAT_NAME)
df1$SECONDARY_MCAT <- sub("_"," ",df1$SECONDARY_MCAT)
df1$PRIME_MCAT_NAME <- gsub("pmcat","",df1$PRIME_MCAT_NAME)
df1$SECONDARY_MCAT <- gsub("smcat","",df1$SECONDARY_MCAT)
df1$SECONDARY_MCAT <- rm_white(df1$SECONDARY_MCAT)
df1$PRIME_MCAT_NAME <- rm_white(df1$PRIME_MCAT_NAME)
df1$FREQUENCY <- gsub("0","",df1$FREQUENCY)
df1$PORPOSE <- gsub("0","",df1$PORPOSE)
df1$LOOKING_FOR_SUPPLIERS <- gsub("0","",df1$LOOKING_FOR_SUPPLIERS)


#Removing row items from Secondary mcats where prime_mcat=secondary_mcat

Index_MCAT <- df1$PRIME_MCAT_NAME == df1$SECONDARY_MCAT

Index_val <- which(Index_MCAT==T)

df1$SECONDARY_MCAT[Index_val] <- ""

df1$L_M_H <- ""

for (i in 1:nrow(df1)) {
  ifelse((df1$TOTAL_SOLD[i]>=1 && df1$TOTAL_SOLD[i]<=3),df1$L_M_H[i] <- "Low_Sold",
         ifelse((df1$TOTAL_SOLD[i]>=4 && df1$TOTAL_SOLD[i]<=7),df1$L_M_H[i] <- "Medium_Sold",
                ifelse(df1$TOTAL_SOLD[i]>7,df1$L_M_H[i] <- "High_Sold",df1$L_M_H[i] <- "Not_Sold")))
}

table(df1$L_M_H)
table(df1$S_NS)

df1$Concat <- paste(df1$ETO_OFR_TITLE,df1$OPTION_DESC_B,df1$GLUSR_USR_CITY,df1$PRIME_MCAT_NAME,df1$SECONDARY_MCAT,df1$FREQUENCY,df1$LOOKING_FOR_SUPPLIERS,df1$PORPOSE,df1$Retail_NR,df1$Email_NE)

df1$Concat <- tolower(df1$Concat)
df1$Concat <- rm_white(df1$Concat)

sum(grepl("[[:punct:]]",df1$Concat))

#df1$Concat <- gsub("[[:punct:]]","",df1$Concat)

df1$Label <- paste0("__label__",df1$S_NS," ",df1$Concat)

df1$Label_LMH <- paste0("__label__",df1$L_M_H," ",df1$Concat)

length(unique(df1$ETO_OFR_DISPLAY_ID))


df1$Label[seq(2,50,3)]
#df1$Label_LMH[seq(2,26,2)]

df2 <- df1[order(df1$ETO_OFR_DISPLAY_ID),]

df_SOLD <- df1[df1$S_NS=="Sold",]
df_Not_SOLD <- df1[df1$S_NS=="Not_Sold",]

#randomizing data so as to select random data-set for training and testing

df3 <- df2[sample(nrow(df2)),]

#write.csv(df3,"processed_2lakhs.csv",row.names = F,quote = F)

Train_Index <- seq(1:floor(nrow(df3)*0.80))

Train_data <- df3[Train_Index,]
Test_data <- df3[-Train_Index,]

table(Train_data$S_NS)
table(Test_data$S_NS)



#write.table(Train_data$Label,paste0(BL_PATH,"/Train/train.txt"),sep = "\t",row.names = F,col.names = F,quote = F)
#write.table(Test_data$Concat,paste0(BL_PATH,"/Test/test.txt"),sep = "\t",row.names = F,col.names = F,quote = F)

#write.csv(df3,paste0(BL_PATH,"/data_final.csv"),row.names = F,quote = F)
#write.csv(Test_data,paste0(BL_PATH,"/test_data_vlookup2.csv"),row.names = F,quote = F)

#Train_concat <- Train_data[Train_data$S_NS=='Sold',]
#write.table(Train_concat$Label,paste0(BL_PATH,"/Train/train_concat.txt"),sep = "\t",row.names = F,col.names = F,quote = F)


#CHeck <- read.table("E:/BL_Saleability/Train/train.txt",sep = "\t")
#CHeck$S_NS <- word(CHeck$V1,1,sep = " ")

#table(CHeck$S_NS)


df2a <- df2[,c("FK_GLCAT_MCAT_ID","S_NS")]   #taking only 2 coloumns
df2a$c2 <- 1   #adding 1 more col for applying pivot

df2b <- dcast(df2a, FK_GLCAT_MCAT_ID ~ S_NS, value.var = "c2",fun.aggregate = sum)   #creating pivot with sum

g=1 #addinf sum, difference and mod col
for (g in 1:length(df2b$FK_GLCAT_MCAT_ID)) {
  df2b$differ[g] <- df2b$Sold[g]-df2b$Not_Sold[g]
  df2b$sum[g] <- df2b$Sold[g]+df2b$Not_Sold[g]
  df2b$Mod[g] <- abs(df2b$differ[g])
}



df2b$col_no <- if_else(df2b$differ<0,paste(df2b$FK_GLCAT_MCAT_ID,"Sold", sep = "_"),
                       if_else(df2b$differ>0,paste(df2b$FK_GLCAT_MCAT_ID,"Not_Sold",sep = "_"),"No Change"))

df2$colno <- paste(df2$FK_GLCAT_MCAT_ID,df2$S_NS,sep="_")   #adding col in df2


###Creating labels for mod<>0, sold<>0, not sold<>0

a <- df2b[!df2b$Sold==0,]     #taking all non zero labels
a <- a[!a$Not_Sold==0,]
a <- a[!a$Mod==0,]
a <- a$FK_GLCAT_MCAT_ID          #convert them into a vector


#empdf <- df2[0,] #creating empty data frame from df2

#h=1                                #extracting rows that are to be added from df2
#for (h in 1:length(a))
#{
#  index <- df2[df2$FK_GLCAT_MCAT_ID==a[h],]
#  empdf <- rbind(empdf,index)
#}

empdf <- df2[df2$FK_GLCAT_MCAT_ID  %in% a,]  #extracting rows that are to be added from df2

empdf1 <- merge(empdf,df2b)  #getting number of rows to be added


empdf1$TR_FA <-if_else(empdf1$colno==empdf1$col_no,"True","False") 
empdf1 <- empdf1[empdf1$TR_FA=="True",]

empdf1 <- empdf1[!duplicated(empdf1$FK_GLCAT_MCAT_ID),]  #removing duplicates rows


label_df <- empdf1[0,]   #creating empty df from dataframe empdf1


f=1
for (f in 1:length(empdf1$FK_GLCAT_MCAT_ID))
{
  adro<- empdf1[rep(f,empdf1$Mod[f]),]
  label_df <- rbind(label_df,adro)
}


label_df <- label_df[,c("ETO_OFR_DISPLAY_ID","FK_GLCAT_MCAT_ID","ETO_OFR_TITLE","GLUSR_USR_CITY","GLUSR_USR_STATE","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC","UNIQUE_SOLD","TOTAL_SOLD","RETAIL_FLAG","EMAIL_FLAG","SECONDARY_MCAT","PRIME_MCAT_NAME","FREQUENCY","LOOKING_FOR_SUPPLIERS","PORPOSE","OPTION_DESC_A","OPTION_DESC_B","S_NS","Retail_NR","Email_NE","L_M_H","Concat","Label","Label_LMH","colno")]




###Extracting rows from df2 where mod =0

# b <- df2b[!df2b$Sold==0,]     #taking all mod=0 labels
# b <- b[!b$Not_Sold==0,]
b <- df2b[df2b$Mod==0,]
b <- b$FK_GLCAT_MCAT_ID 
mod_df <- df2[df2$FK_GLCAT_MCAT_ID %in% b,]




###Extracting rows from df2 where sold=0, not sold =0 and sold/not sold is not 1 


c <- df2b[df2b$sum>1,]
c_id <- c[!(c$FK_GLCAT_MCAT_ID %in% a),]
c_id <- c_id[c_id$Mod!=0,]
c_id <- c_id$FK_GLCAT_MCAT_ID
s_ns_zero_df <- df2[df2$FK_GLCAT_MCAT_ID %in% c_id,] 

#length(unique(df2_Append$FK_GLCAT_MCAT_ID))
# save.image(file = "celeb_t.RData")
#colnames(final_df)[colnames(final_df)=="col_no"] <- "colno"

final_label <- rbind(mod_df,label_df)  #adding label rows with df2
final_label$c2 <- 1
label_check <- dcast(final_label, FK_GLCAT_MCAT_ID ~ S_NS, value.var = "c2",fun.aggregate = sum)   #creating pivot with sum

s=1
for (s in 1:length(label_check$FK_GLCAT_MCAT_ID)) {
  label_check$differ[s] <- label_check$Sold[s]-label_check$Not_Sold[s]
  label_check$sum[s] <- label_check$Sold[s]+label_check$Not_Sold[s]
  label_check$Mod[s] <- abs(df2b$differ[s])
}

label_check <- label_check[!label_check$Sold==0,]     #taking all non zero labels
label_check <- label_check[!label_check$Not_Sold==0,]
label_check <- label_check[!label_check$Mod==0,]


#length(unique(final_df$FK_GLCAT_MCAT_ID))



#wordcloud::wordcloud((c("Solar","Panels","Solar","Panels","Mono")))

#set.seed(500)
#Clusters <- kmeans(df2[,c(2,9)],5)
#clus <- data.frame(Clusters$cluster)

write.csv(final_label,"BL_Saleability(final label).csv",row.names = F,quote = F)


setwd("C:/Users/imart/Documents/BL-Saleability")

dir.create("Kfold")
dir.create("./kfold/train")
dir.create("./kfold/test")
#performing kfold_cross validation:
Final_Data <- final_label[sample(nrow(final_label)),]

write.csv(Final_Data,"BL_saleab_1-5_Lakhs_Old.csv",row.names = F,quote = F)
folds <- cut(seq(1,nrow(Final_Data)),breaks=10,labels=FALSE)
#dir.create("E:/Product_classification_YARN/Polyester Yarn/Training Files2")
#Perform 10 fold cross validation
i<-1
for(i in 1:10)
{
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- Final_Data[testIndexes, ]
  trainData <- Final_Data[-testIndexes, ]
  #trainData <- data.frame(trainData[,c("Label")])
  trainLocation<-paste0("C:/Users/imart/Documents/BL-Saleability/kfold/train/",i,".txt")
  testLocation2<-paste0("C:/Users/imart/Documents/BL-Saleability/kfold/test_id",i,".txt")
  testLocation<-paste0("C:/Users/imart/Documents/BL-Saleability/kfold/test/",i,".txt")
  testLocation3<-paste0("C:/Users/imart/Documents/BL-Saleability/kfold/test_label",i,".txt")}

write.table(trainData$Label,trainLocation,sep = "\t",row.names = F,col.names = F,quote = F)
#testData<-data.frame(testData[,c("Concat")])
write.table(testData$Concat,testLocation,sep = "\t" ,row.names = F,col.names = F,quote = F)
write.table(testData$ETO_OFR_DISPLAY_ID,testLocation2,sep = "\t" ,row.names = F,col.names = F,quote = F)
write.table(testData$Label,testLocation3,sep = "\t" ,row.names = F,col.names = F,quote = F)

save.image(file = "C:/Users/imart/Documents/Buy Lead Salability 15-20Lac.RData")

