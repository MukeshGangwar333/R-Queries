options("scipen"=100, "digits"=2)
options(scipen = 999)
library(dplyr)
library(readxl)
library(stringr)
library(plyr)
library(splitstackshape)
library(reshape)
library(reshape2)



#options(scipen = 999)

#############reading all the files by assigning the file name of the file itself

flist <- list.files("D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder/input files/")

for (i in 1:length(flist)){

  assign(flist[i],read.csv(paste("D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder/input files/", flist[i], sep='')))
}


#############formatting file to workable format

df <- read.csv("D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder/3RD SLOT.csv")

df <- export4thslot.csv

df$new <- df$FK_GLCAT_MCAT_ID.......FK_PC_ITEM_ID.......PC_ITEM_FOB_PRICE.......PC_ITEM_MOQ_UNIT_TYPE


library(splitstackshape)

df <- splitstackshape::cSplit(indt = df,splitCols = "new",sep = ",")
colnames(df)[colnames(df)=="new_01"] <- "FK_GLCAT_MCAT_ID"
colnames(df)[colnames(df)=="new_02"] <- "FK_PC_ITEM_ID"
colnames(df)[colnames(df)=="new_03"]<- "PC_ITEM_FOB_PRICE"

#sum(is.na(df))
df1 <- df
#str(df1)
df1$new_04 <- as.character(df1$new_04)
df1$new_05 <- as.character(df1$new_05)
df1$new_06 <- as.character(df1$new_06)
df1$new_07 <- as.character(df1$new_07)
df1$new_08 <- as.character(df1$new_08)
df1$new_09 <- as.character(df1$new_09)
df1$new_10 <- as.character(df1$new_10)

df1[is.na(df1)] <- ""

sum(is.na(df1))

df1$PC_ITEM_MOQ_UNIT_TYPE <- paste(df1$new_04,df1$new_05,df1$new_06,df1$new_07,df1$new_08,df1$new_09,df1$new_10, sep = ",")

#colnames(df1)
df2 <- df1[,c("FK_GLCAT_MCAT_ID","FK_PC_ITEM_ID","PC_ITEM_FOB_PRICE","PC_ITEM_MOQ_UNIT_TYPE","R_NUM")]

df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,,,,,,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,,,,,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,,,,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,,,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",,","",df2$PC_ITEM_MOQ_UNIT_TYPE)
df2$PC_ITEM_MOQ_UNIT_TYPE <- sub(",$","",df2$PC_ITEM_MOQ_UNIT_TYPE)

compiled_file <- rbind(`1-20l.csv`,`3RDSLOT.csv`,`5thslot.csv`,`7thslot.csv`,export2NDSLOT.csv,export4thslot.csv,export6thslot.csv,export8thslot.csv)
compiled_file <- compiled_file[order(compiled_file$FK_GLCAT_MCAT_ID),]



#############cleaning data of compiled file

compiled_file <- read.csv("file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/compiled_file.csv")
compiled_file1 <- na.omit(compiled_file)
rm(compiled_file)
compiled_file2 <- compiled_file1[!grepl("\\$",compiled_file1$PC_ITEM_FOB_PRICE),] 
rm(compiled_file1)
compiled_file3 <- compiled_file2[!grepl("-",compiled_file2$PC_ITEM_FOB_PRICE),] 
rm(compiled_file2)
compiled_file4 <- compiled_file3[!grepl("/",compiled_file3$PC_ITEM_FOB_PRICE),] 
rm(compiled_file3)
compiled_file5 <- compiled_file4[!grepl("~",compiled_file4$PC_ITEM_FOB_PRICE),] 
rm(compiled_file4)
compiled_file5$PC_ITEM_FOB_PRICE <- gsub("\\,","",compiled_file5$PC_ITEM_FOB_PRICE)
compiled_file6 <- compiled_file5[!grepl("\\`",compiled_file5$PC_ITEM_FOB_PRICE),] 
compiled_file7 <- compiled_file6[!grepl("<",compiled_file6$PC_ITEM_FOB_PRICE), ]
rm(compiled_file5,compiled_file6)

compiled_file7a <- compiled_file7[!grepl("\\D",compiled_file7$PC_ITEM_FOB_PRICE),  ]  #excluding alphabetical data
compiled_file7b <- compiled_file7[grepl("\\D",compiled_file7$PC_ITEM_FOB_PRICE),  ]  #including non numerical data


write.csv(compiled_file7a,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/compiled_file7a.csv",row.names = F)
write.csv(compiled_file7b,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/compiled_file7b.csv",row.names = F)

rm(compiled_file7a,compiled_file7b)

options("scipen"=100, "digits"=2)



compiled_file7a <- read.csv("file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/compiled_file7a.csv")
compiled_file7b <- read.csv("file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/compiled_file7b.csv")



summary(compiled_file7a)
summary(compiled_file7b)

compiled_file8 <- rbind(compiled_file7a,compiled_file7b)
rm(compiled_file7,compiled_file7a,compiled_file7b)

compiled_file9 <- compiled_file8[!compiled_file8$PC_ITEM_FOB_PRICE>200000000, ]
rm(compiled_file8)

summary(compiled_file9)
write.csv(compiled_file9,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/Compiled_with clean.csv",row.names=F)



###############################################


compiled_file <- read.csv("D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/Input Files New Format/Compiled_with clean.csv")


library(readxl)
mcateightyfive <- read.csv("D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Next50.csv")
mcateightyfive <- mcateightyfive$FK_GLCAT_MCAT_ID
mcateightyfive_data <- compiled_file[compiled_file$FK_GLCAT_MCAT_ID %in% mcateightyfive,]

#a <- grep("-",mcateightyfive_data$PC_ITEM_FOB_PRICE)
#mcateightyfive_data$PC_ITEM_FOB_PRICE[1131557] <- 60


write.csv(mcateightyfive_data,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/mcateightyfive_data1.csv",row.names = F)
mcateightyfive <- read.csv("file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/mcateightyfive_data1.csv")


mcateightyfive1 <- mcateightyfive[!is.na(mcateightyfive$PC_ITEM_MOQ_UNIT_TYPE),]

summary(mcateightyfive1)


#mcateightyfive_test <- mcateightyfive1[mcateightyfive1$FK_GLCAT_MCAT_ID==10002,]
#mcateightyfive1 <- [1:42,]    #breaking files on the basis of coloumns
#mcateightyfive2 <- [42:84,]  #breaking files on the basis of coloumns


#rm(list = ls()[-5])


##breaking file on the basis of mcat

#read the file to be processed
#df <- read.csv("C:/Users/imart/Documents/Pricing/Input files/1-20l.csv")  

#read the unified units of mcat file
library(readxl)
unified_units <- read_excel("D:/OND'18 Work/Pricing/All mcats Pricing/With R/All mcats Unified Units1.xlsx")




df<-mcateightyfive1


#df<-compiled_file


library(plyr)
df2 <- join(df,unified_units,by="PC_ITEM_MOQ_UNIT_TYPE",type="left",match="first")
colnames(df2)[colnames(df2)=="Final Unit"] <- "final_units"

#removing units that have zero
df3 <- df2[!df2$final_units==0,c("FK_GLCAT_MCAT_ID","FK_PC_ITEM_ID","PC_ITEM_FOB_PRICE","R_NUM","final_units")]


df3 <- df3[order(df3$FK_GLCAT_MCAT_ID),]

df4 <- df3
#rm(list=ls()[-3])



mcat_wise_split <- split(df4,df4$FK_GLCAT_MCAT_ID)



top3_empty <- data.frame(
  mcat_id=numeric(),
  unit=character(),
  count=numeric()
)



j=1

for (j in 1:length(mcat_wise_split)) {
  
  top <- mcat_wise_split[[j]]
  
  top$extra <- 1
  piv1 <- dcast(top, FK_GLCAT_MCAT_ID + final_units + PC_ITEM_FOB_PRICE ~ extra, value.var = "extra", fun.aggregate = sum)
  piv2 <- aggregate(piv1$`1`,by=list(piv1$final_units),FUN=sum)
  
  productsperunit <- merge(piv1,piv2,by.x = "final_units", by.y = "Group.1", all.x = T)
  colnames(productsperunit)[colnames(productsperunit)=="1"] <- "unit_value_count"
  colnames(productsperunit)[colnames(productsperunit)=="x"] <- "unit_count"
  
  
  piv2 <- piv2[order(piv2$x, decreasing = T),]
  piv2_3_index <- piv2$Group.1[1:3]
  
  top3 <- productsperunit[productsperunit$final_units %in% piv2_3_index,]
  
  top3_empty <- rbind(top3_empty,top3)
  
  #productsperunit <- top %>% group_by(top$final_units,top$FK_GLCAT_MCAT_ID) %>% summarise(final_units= count(top$FK_GLCAT_MCAT_ID))
  #productsperunit <- aggregate(top$extra,by=list(top$FK_GLCAT_MCAT_ID,top$final_units,top$PC_ITEM_FOB_PRICE),FUN=sum)
}

top3_final <- top3_empty[,c("FK_GLCAT_MCAT_ID","final_units","PC_ITEM_FOB_PRICE","unit_value_count","unit_count")]
write.csv(top3_final,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Before_hampel.csv",row.names = F)


##############################for pc item ids##############################

top3_empty$PC_ITEM_FOB_PRICE <- as.integer(top3_empty$PC_ITEM_FOB_PRICE )
top3_empty$mcat_unit_price <- paste(top3_empty$FK_GLCAT_MCAT_ID, top3_empty$final_units,top3_empty$PC_ITEM_FOB_PRICE, sep="_"   ) 

top3_empty <- top3_empty[!duplicated(top3_empty$mcat_unit_price ), ]


df4$mcat_unit_price <- paste(df4$FK_GLCAT_MCAT_ID, df4$final_units, df4$PC_ITEM_FOB_PRICE, sep = "_" )

df_pc_ids <- merge(df4, top3_empty, by.x = "mcat_unit_price", by.y = "mcat_unit_price", all.x = T )

#df_pc_ids$FK_GLCAT_MCAT_ID.y <- as.character(df_pc_ids$FK_GLCAT_MCAT_ID.y)
#df_pc_ids$PC_ITEM_FOB_PRICE.y  <- as.character(df_pc_ids$PC_ITEM_FOB_PRICE.y)

df_pc_ids <- df_pc_ids[!is.na(df_pc_ids$FK_GLCAT_MCAT_ID.y ),]

colnames(df_pc_ids)

df_pc_ids_fin <- df_pc_ids[,c("FK_GLCAT_MCAT_ID.x","final_units.x","PC_ITEM_FOB_PRICE.x","FK_PC_ITEM_ID","unit_value_count","unit_count")]

write.csv(df_pc_ids_fin,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/Before_hampel with PC_Item_IDs.csv",row.names = F)

########################################################################



#Removing mcat unit from top 3 units which are lower than 5% of the total mcat unit sum

###Removing prodcuts less than 5% MCAT_Unit wise
top3_final1 <- top3_final
top3_final1$mcat_unit <- paste(top3_final1$FK_GLCAT_MCAT_ID,top3_final1$final_units, sep = "_" ) 


t_final <- top3_final1[,c("FK_GLCAT_MCAT_ID","final_units","unit_count")]
t_final$mc_unit <- paste(t_final$FK_GLCAT_MCAT_ID,t_final$final_units, sep = "_" )
t_final <- t_final[!duplicated(t_final$mc_unit ), ]

detach("package:plyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)

library(dplyr)


t_final_mcat_prod_count <- t_final %>% group_by(FK_GLCAT_MCAT_ID  )   %>% summarise(mcat_prod_count= sum(unit_count ) )
 
t_final1 <- merge(t_final,  t_final_mcat_prod_count, by.x = "FK_GLCAT_MCAT_ID", by.y= "FK_GLCAT_MCAT_ID" , all.x = T)

t_final1$unitcount_mcatcount <- t_final1$unit_count/t_final1$mcat_prod_count

t_final2 <- t_final1[t_final1$unitcount_mcatcount > .05, ]

t_final3 <- t_final2$mc_unit


top3_final2 <- top3_final1[top3_final1$mcat_unit %in% t_final3,  ]


top3_final2 <- top3_final2[, c("FK_GLCAT_MCAT_ID","final_units","PC_ITEM_FOB_PRICE","unit_value_count","unit_count")]

write.csv(top3_final2,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Before_hampel after removing mcat unit lower than 5%1.csv",row.names = F)

#summary(top3_final2)


################################Hampel Test##########################



#hampl_data <- top3_final[,-(4:5)]
hampl_data <- top3_final2

class(hampl_data$PC_ITEM_FOB_PRICE)
Check_index1 <- which(grepl("-",hampl_data$PC_ITEM_FOB_PRICE)==T)

#hamp_char <- hampl_data[Check_index1,]
#hampl_data <- hampl_data[-Check_index1,]

#sum(is.na(top3_final))
#sum(is.na(hampl_data))
#hampl_data [which(is.na(hampl_data)),]

hampl_data$PC_ITEM_FOB_PRICE <- as.numeric(hampl_data$PC_ITEM_FOB_PRICE)



class(hampl_data$PC_ITEM_FOB_PRICE)

hampl_data$SPPL <- paste(hampl_data$FK_GLCAT_MCAT_ID,hampl_data$final_units)

detach("package:plyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)

library(dplyr)

hampl_data_median <- hampl_data %>% group_by(SPPL) %>% summarise(Med_val = median(PC_ITEM_FOB_PRICE,na.rm = T))

hampl_data_median_merge <- merge(x = hampl_data,y = hampl_data_median,by.x = "SPPL",by.y = "SPPL",all.x = T)

#colnames(Data_hamp)[colnames(Data_hamp)=="Mean_val"] <- "Med_val"

hampl_data_median_merge$deviation <-  as.numeric(hampl_data_median_merge$PC_ITEM_FOB_PRICE) - as.numeric(hampl_data_median_merge$Med_val)


hampl_data_median_merge$deviation_mod <- abs(hampl_data_median_merge$deviation)

class(hampl_data_median_merge$deviation_mod)

hampl_data_median_merge_dev_med <- hampl_data_median_merge %>% group_by(SPPL) %>% summarise(median_of_dev = median(deviation_mod))

hampl_data_final <- merge(x = hampl_data_median_merge,y = hampl_data_median_merge_dev_med,by.x = "SPPL",by.y = "SPPL",all.x = T)


hampl_data_final$outlier_flag_4.5 <- ""

hampl_data_final$outlier_flag_4.5 <- ifelse(hampl_data_final$deviation_mod>=(4.5*(hampl_data_final$median_of_dev)),"Outlier","No")

write.csv(hampl_data_final,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/hampl_data_final.csv",row.names = F)
#rm(list = ls()[-3])


################################K Medoid##########################
#hampl_data_final <- read.csv("c:/Users/imart/Documents/hampl_data_final.csv")
#memory.limit(1048576)

library(dplyr)
library(cluster) #function pam is used to calculate k medoid
#library(fpc)  #function pamk can be used to calculate medoid, here "k" is not required
#library(factoextra) #to create graphs

#sum(is.na(kmed_data))

kmed_data <- hampl_data_final[hampl_data_final$outlier_flag_4.5=="No",]   #select the non outliers
hampl_data_final <- NULL
kmed_prod_group <- kmed_data %>% group_by(SPPL) %>% summarise(prod_count = sum(unit_value_count))       #product count MCAT_unit wise
kmed_prod_row <- kmed_data %>% group_by(SPPL) %>% summarise(row_count = length(SPPL))       #row count mcat unit wise

kmed_data_prod_count1 <- merge(kmed_data,kmed_prod_group,by.x = "SPPL",by.y = "SPPL",all.x = T)      #merging the prod count
kmed_data_prod_count <- merge(kmed_data_prod_count1,kmed_prod_row,by.x = "SPPL",by.y = "SPPL", all.x = T)

kmed_data_prod_count$SPPL <- trimws(kmed_data_prod_count$SPPL) 


kmed_df_prod_count_L50 <- kmed_data_prod_count[kmed_data_prod_count$prod_count<50 | kmed_data_prod_count$row_count<11 ,]   #product count less than 50 and row count less than 10
kmed_df_prod_count_M50 <- kmed_data_prod_count[kmed_data_prod_count$prod_count>=50 &  kmed_data_prod_count$row_count >= 11,]  #product count equal or more than 50 and row count more than or equal to 10


##Applying K Medoid on greater than 50 products
df_M50 <- split(kmed_df_prod_count_M50,f=kmed_df_prod_count_M50$SPPL)

#rm(list = ls()[-17])
#length(unique(kmed_data_prod_count_M50$SPPL))


#creating empty dataframe
emp_clus_df <- data.frame(
  SPPL= character(),
  FK_GLCAT_MCAT_ID= numeric(),
  final_units= factor(),
  PC_ITEM_FOB_PRICE= numeric(),
  unit_value_count= numeric(),
  unit_count=numeric(),
  Med_val=numeric(),
  deviation=numeric(),
  deviation_mod=numeric(),
  median_of_dev=numeric(),
  outlier_flag_4.5=factor(),
  prod_count=numeric(),
  cluster=numeric(),
  kvalue=numeric()
)


#applying k medoid for k value 5-10

k=1

for (k in 1:length(df_M50)) {
  
  l=5
  for (l in 5:10)
  {
    k_medoid_cluster <- cluster::pam(df_M50[[k]]$PC_ITEM_FOB_PRICE,l,metric = "manhattan")
    
    df_M50[[k]]$kvalue <- l
    
    df_M50[[k]]$cluster <- k_medoid_cluster$clustering
    
    emp_clus_df <- rbind(df_M50[[k]],emp_clus_df)
  }
  
  
}



#colnames(kmed_df_M50_K_med)

kmed_df_M50_K_med <- emp_clus_df   
kmed_df_prod_count_L50$kvalue <- 1
kmed_df_prod_count_L50$cluster <- 1
kmed_df_L50_K_med <- kmed_df_prod_count_L50

kmed_data_final <- rbind(kmed_df_L50_K_med,kmed_df_M50_K_med)  #combing both df i.e less than and more than 50 prodcuts data frame
write.csv(kmed_data_final,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/kmed_data_final.csv",row.names = F)

##Applying further stats on the basis of MCAT_Unit


#making data mcat unit kvlaue cluster wise
kmed_data_final$mcat_unit_kval_clus <- paste(kmed_data_final$SPPL,kmed_data_final$kvalue,kmed_data_final$cluster,sep = "_")


#applying max value 
kmed_max <- kmed_data_final %>% group_by(mcat_unit_kval_clus) %>% summarise(kmed_maxm= max(PC_ITEM_FOB_PRICE))

#applying min value 
kmed_min <- kmed_data_final %>% group_by(mcat_unit_kval_clus) %>% summarise(kmed_minm= min(PC_ITEM_FOB_PRICE))


#product count
kmed_prod <- kmed_data_final %>% group_by(mcat_unit_kval_clus) %>% summarise(kmed_prod_count= sum(unit_value_count))

kmed_max_final <- merge(kmed_data_final,kmed_max,by.x = "mcat_unit_kval_clus",by.y= "mcat_unit_kval_clus", all.x = T )
kmed_max_min_final <- merge(kmed_max_final,kmed_min,by.x = "mcat_unit_kval_clus", by.y =  "mcat_unit_kval_clus", all.x = T)

#final data with max min prod count basis of mcat unit kvalue and cluster
kmed_max_min_prod_final <- merge(kmed_max_min_final,kmed_prod,by.x= "mcat_unit_kval_clus", by.y = "mcat_unit_kval_clus", all.x=T) 

kmed_max_min_prod_final$maxmin <- kmed_max_min_prod_final$kmed_maxm/kmed_max_min_prod_final$kmed_minm
kmed_max_min_prod_final$threepercut <- (kmed_max_min_prod_final$prod_count)*.03

write.csv(kmed_max_min_prod_final,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/kmed_max_min_max_three_cut.csv",row.names = F)

#More Old cuts- if freq > 3%cut- true (take true)


kmed_max_min_prod_final$threepermaxmin <- ifelse(kmed_max_min_prod_final$threepercut> kmed_max_min_prod_final$kmed_prod_count, "False", "True")

kmed_max_min_prod_final_2 <- kmed_max_min_prod_final[kmed_max_min_prod_final$threepermaxmin=="True", ]

write.csv(kmed_max_min_prod_final_2,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/kmed_max_min_after_removing some bucket cluster_after 3percent_cut.csv",row.names = F)



#New cut- removing 3% cut  make pivot & take bucket in which 90% products covering or max size of freq in a bucket

kmed_last_df <- kmed_max_min_prod_final_2[,c(1:4,13, 15:22)]

kmed_last_df1 <- unique(kmed_last_df) 

#kmed_last_df1 <- kmed_last_df1[kmed_last_df1$cluster!=1,]

kmed_last_df1$MCAT_Unit_k <- paste(kmed_last_df1$SPPL,kmed_last_df1$kvalue,sep = "_")

prod_count_on_k <- kmed_last_df1 %>% group_by(MCAT_Unit_k) %>% summarise(prod_count_k= sum(kmed_prod_count))


kmed_last_df3 <- merge(kmed_last_df1,prod_count_on_k,by.x = "MCAT_Unit_k", by.y = "MCAT_Unit_k", all.x = T)



#kmed_last_df3$prod_count90percent <- 0.90*(kmed_last_df3$prod_count)

write.csv(kmed_last_df3,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/mcat_all cluster with all k variable before bucket selection.csv",row.names = F)
#write.csv(kmed_last_df3,"D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder/85 mcats/unique_kmed_max_min_prod_final without removing 1 cluster & 90percent cut.csv",row.names = F)


kvaluecout <-  kmed_last_df3 %>% group_by(MCAT_Unit_k) %>% summarise(kvaluecount = length(kvalue))

kmed_last_df3 <- merge(kmed_last_df3,kvaluecout, by.x = "MCAT_Unit_k", by.y="MCAT_Unit_k", all.x = T)
kmed_last_df4 <- kmed_last_df3[!duplicated(kmed_last_df3$MCAT_Unit_k),]

#kmed_last_df4$TRUEFALSE <- ""
#kmed_last_df4$TRUEFALSE <- ifelse(kmed_last_df4$prod_count_k > kmed_last_df4$prod_count90percent, kmed_last_df4$TRUEFALSE <- "TRUE",kmed_last_df4$TRUEFALSE <- "FALSE" )

#write.csv(kmed_last_df4,"D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder/85 mcats/After 90percent cut.csv",row.names = F)




SSPL <- split(kmed_last_df4, f=kmed_last_df4$SPPL)

DataF_1 <- kmed_last_df4[0,]


m <- 1

for (m in 2:length(SSPL)) {
  res1 <- SSPL[[m]]
  res1 <- res1[!is.na(res1$MCAT_Unit_k),]
  res1 <- res1[order(res1$prod_count_k,res1$kvalue,decreasing = T),]
  res1 <- na.omit(res1)
  DataF_1 <- rbind(DataF_1,res1[1,])
}


write.csv(DataF_1,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Bucket selection.csv",row.names = F)


#FOB Price and all cluster against K after bucket selection

kmed_max_min_prod_final_2$mcat_unit_k <- paste(kmed_max_min_prod_final_2$SPPL, kmed_max_min_prod_final_2$kvalue, sep = "_" )

ind <- DataF_1$MCAT_Unit_k

bucket_fob_price <- kmed_max_min_prod_final_2[kmed_max_min_prod_final_2$mcat_unit_k %in% ind,]

#write.csv(bucket_fob_price,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/mcat-final with 1 bucket selected with FOB price before removing lower 3% prices.csv",row.names = F)

#bucket_fob_price_fin <- bucket_fob_price[,c("SPPL","FK_GLCAT_MCAT_ID","final_units","prod_count","kvalue","cluster","kmed_maxm","kmed_minm","kmed_prod_count","mcat_unit_k","mcat_unit_kval_clus")]
#bucket_fob_price_fin_unq <- bucket_fob_price_fin[!duplicated(bucket_fob_price_fin$mcat_unit_kval_clus),]

#write.csv(bucket_fob_price_fin_unq,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/mcat-Final With 1 Bucket selected before removing lower 3% prices.csv",row.names = F)


#New cut of removing lower 3% prices from 1st bucket of each mcat

####Three percent cut on cluster 1 MCAT_UNIT_K wise

bucket_fob_price_fin_unq_one <- bucket_fob_price[bucket_fob_price$cluster==1, ]
bucket_fob_price_fin_unq_other_one <- bucket_fob_price[bucket_fob_price$cluster!=1, ]
bucket_fob_price_fin_unq_one$maxminusmin <- bucket_fob_price_fin_unq_one$kmed_maxm - bucket_fob_price_fin_unq_one$kmed_minm
bucket_fob_price_fin_unq_one$maxminusmindivthree <- bucket_fob_price_fin_unq_one$maxminusmin * .03
bucket_fob_price_fin_unq_one_fin <- bucket_fob_price_fin_unq_one[bucket_fob_price_fin_unq_one$PC_ITEM_FOB_PRICE> bucket_fob_price_fin_unq_one$maxminusmindivthree,]
bucket_fob_price_fin_unq_one_fin <- bucket_fob_price_fin_unq_one_fin[,-c(24,25)]

bucket_last_df <- rbind(bucket_fob_price_fin_unq_other_one,bucket_fob_price_fin_unq_one_fin)



detach("package:plyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)

library(dplyr)

bucket_last_df_minn <- bucket_last_df %>%  group_by(mcat_unit_kval_clus )  %>% summarise(kmed_minn2 = min(PC_ITEM_FOB_PRICE ) )
#length(unique(bucket_last_df$mcat_unit_kval_clus ))

bucket_last_df2 <- merge(bucket_last_df,bucket_last_df_minn, by.x = "mcat_unit_kval_clus", by.y = "mcat_unit_kval_clus", all.x = T )

bucket_last_df2 <- bucket_last_df2 %>% group_by(mcat_unit_kval_clus ) %>% mutate(kmed_prod_count_final= sum(unit_value_count ) )
bucket_last_df_f <- bucket_last_df2[, c("SPPL","FK_GLCAT_MCAT_ID","final_units","PC_ITEM_FOB_PRICE","unit_value_count","prod_count","kvalue","cluster","kmed_minn2","kmed_maxm","kmed_prod_count_final")]

  
write.csv(bucket_last_df_f,"D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/mcat-final with 1 bucket selected with FOB price.csv",row.names = F)

#colnames(bucket_last_df2)

bucket_last_df3 <- bucket_last_df2[,c("SPPL","FK_GLCAT_MCAT_ID","final_units","prod_count","kvalue","cluster","kmed_minn2","kmed_maxm","kmed_prod_count_final","mcat_unit_k","mcat_unit_kval_clus")]
bucket_unq_final <- bucket_last_df3[!duplicated(bucket_last_df3$mcat_unit_kval_clus),]


#varr <- names(bucket_last_df2)

write.csv(bucket_unq_final,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Final bucketed data with price after removing lower 3% from 1st bucket.csv",row.names = F)


#save.image("Finalcutrequired.RData")
#setwd("D:/OND'18 Work/Pricing/15 Mcats_Price_calculation_final_for insertion/All mcats Pricing/With R/New folder") 

############################Applying GP###########################
options(scipen = 999)
#bucket_unq_final <- read.csv("Final bucketed data with price after removing lower three  from 1st bucket.csv" )
#bucket_last_df_f <- read.csv("mcat-final with 1 bucket selected with FOB price.csv" )

bucket_unq_final <- bucket_unq_final[order(bucket_unq_final$SPPL , bucket_unq_final$kmed_minn2),]
bucket_unq_final <- bucket_unq_final %>% group_by(SPPL ) %>% mutate(minimum_value = min(kmed_minn2 ), maximum_value = max(kmed_maxm ) )
bucket_unq_final$maxminratio <- bucket_unq_final$maximum_value/bucket_unq_final$minimum_value
bucket_unq_final$r <- bucket_unq_final$maxminratio^(1/4) 
bucket_unq_final$second <- bucket_unq_final$minimum_value*bucket_unq_final$r
bucket_unq_final$third <- bucket_unq_final$second*bucket_unq_final$r
bucket_unq_final$fourth <- bucket_unq_final$third*bucket_unq_final$r
bucket_unq_final1 <- bucket_unq_final[,c("SPPL","minimum_value","maximum_value","maxminratio","r","second","third","fourth" ) ]
bucket_unq_final1 <- unique(bucket_unq_final1 )
bucket_unq_final1$minimum_value <- as.integer(bucket_unq_final1$minimum_value)
bucket_unq_final1$maximum_value <- as.integer(bucket_unq_final1$maximum_value)
bucket_unq_final1$second <- as.integer(bucket_unq_final1$second)
bucket_unq_final1$third <- as.integer(bucket_unq_final1$third)
bucket_unq_final1$fourth <- as.integer(bucket_unq_final1$fourth)


bucket_unq_final1$bucket1 <- paste(bucket_unq_final1$minimum_value, bucket_unq_final1$second, sep = " - " )
bucket_unq_final1$bucket2 <- paste(bucket_unq_final1$second, bucket_unq_final1$third, sep = " - ")
bucket_unq_final1$bucket3 <- paste(bucket_unq_final1$third, bucket_unq_final1$fourth, sep = " - " )
bucket_unq_final1$bucket4 <- paste(bucket_unq_final1$fourth, bucket_unq_final1$maximum_value, sep = " - " )


bucket_unq_final1$bucket1_prod <- ""
bucket_unq_final1$bucket2_prod <- ""
bucket_unq_final1$bucket3_prod <- ""
bucket_unq_final1$bucket4_prod <- ""


d=1
for (d in 1:nrow(bucket_unq_final1)) {
  
  
  subdf <- bucket_last_df_f[bucket_last_df_f$SPPL==bucket_unq_final1$SPPL[d],  ]
  subdf1 <- bucket_unq_final1[bucket_unq_final1$SPPL[d], ]
  
  
  a <- subdf[subdf$PC_ITEM_FOB_PRICE>=subdf1$minimum_value & subdf$PC_ITEM_FOB_PRICE<subdf1$second,  ]
  bucket_unq_final1$bucket1_prod[d] <-  sum(a$unit_value_count)
  
  b <- subdf[subdf$PC_ITEM_FOB_PRICE>=subdf1$second  & subdf$PC_ITEM_FOB_PRICE<subdf1$third,    ]
  bucket_unq_final1$bucket2_prod[d] <- sum(b$unit_value_count)
  
  c <- subdf[subdf$PC_ITEM_FOB_PRICE>=subdf1$third  & subdf$PC_ITEM_FOB_PRICE<subdf1$fourth,    ]
  bucket_unq_final1$bucket3_prod[d] <- sum(c$unit_value_count)
  
  g <- subdf[subdf$PC_ITEM_FOB_PRICE>=subdf1$fourth  & subdf$PC_ITEM_FOB_PRICE <= subdf1$maximum_value ,    ]
  bucket_unq_final1$bucket4_prod[d] <- sum(g$unit_value_count)
  
}




summary(bucket_unq_final1)


##########################################################################################################################################

##Adjusted boxplot based on MCAT_Unit wise:
#kmed_df <- read.csv("Kmed_data", row.names=F)

# Breaking data MCAT unit wise
kmed_df <- bucket_last_df2[order(bucket_last_df2$SPPL),]
#kmed_df <- kmed_df[kmed_df$unit_value_count>0,]

#colnames(kmed_df)[colnames(kmed_df)=="SPPL"] <- "SPPL2"

MCAT_Unit_Break <- split(kmed_df,f=kmed_df$SPPL)

#MCAT_Unit_Break[[3]]$FK_GLCAT_MCAT_ID

length(unique(kmed_df$SPPL))

#write.csv(kmed_df,"Kmed_data")

colnames(kmed_df)

#Creating an Empty data frame for statistical values:

Stats1 <- data.frame(MCAT_ID_Unit= numeric(),
                     Min=numeric(),
                     Max=numeric(),
                     Mean=numeric(),
                     Median=numeric(),
                     Product_Count=numeric(),
                     Q1=numeric(),
                     Q3=numeric(),
                     IQR=numeric(),
                     MC=numeric(),
                     lower=numeric(),
                     upper=numeric(),
                     outlier_cnt=numeric(),
                     outlier_per=numeric())

i<-1
Product_Count<-0



### Function to get the dataframe without desired empty column#################
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#### Function to assign outlier flag as 1 to any observation ######
my_populate<-function(x,lower,upper)
{
  if(x<lower|| x>upper)
  {
    return(0)# Zero Corresponds to outliers
  }
  else
  {
    return(1)
  }
}

##### Function to get mode of data ########
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Loop for each MCAT and calculate all required values

library(robustbase)
for (i in 1:length(MCAT_Unit_Break)) {
  #Assign MCAT Unit
  a <- MCAT_Unit_Break[[i]]
  #assigning total product count
  Product_Count <- Product_Count + length(a$PC_ITEM_FOB_PRICE)
  
  # Check if there any Product exist for each MCATs
  length_price<-length(MCAT_Unit_Break[[i]]$unit_value_count)
  
  if(length_price)
  {
    #Store Price Value
    Price<-MCAT_Unit_Break[[i]]$PC_ITEM_FOB_PRICE
    # Convert prices to numeric if character any
    Price<-as.numeric(MCAT_Unit_Break[[i]]$PC_ITEM_FOB_PRICE)
    # Sort the price value
    Price<-sort(Price)
    # Assign 0 to price where price is NA
    Price[is.na(Price)]<-0
    #Remove zero prices
    Price<-Price[Price>0]
    # Calculate MC,Q1,Q3,IQR and lower upper cap: mid couple coefficient required for adjusted boxplot
    MC<-mc(Price,na.rm = TRUE)
    Q1 <- quantile(Price, na.rm = TRUE)[[2]]
    Q3 <- quantile(Price, na.rm = TRUE)[[4]]
    IQR <- Q3 - Q1
    if(MC<0)
    {
      
      lower <- as.numeric(Q1 - 1.5*exp(-4*MC)*IQR)
      upper <- as.numeric(Q3 + 1.5*exp(3.5*MC)*IQR)
    }
    if(MC>=0)
    {
      lower <- as.numeric(Q1 - 1.5*exp(-3.5*MC)*IQR)
      upper <- as.numeric(Q3 + 1.5*exp(4*MC)*IQR)
    }
    # Store MCAT data in data frame named a
    a<-MCAT_Unit_Break[[i]]
    a$MID_VAL<-as.numeric(a$PC_ITEM_FOB_PRICE)
    # Apply completeFun as created above for column Price
    a<-completeFun(a, "PC_ITEM_FOB_PRICE")
    a$MID_VAL<-round(a$PC_ITEM_FOB_PRICE)
    # Reindexing of rows
    rownames(a) <- 1:nrow(a)
    # Apply my_populate function to assign outlier flag to observation
    a$check<-apply(a,1,function(params)my_populate(as.numeric(params[5]),lower,upper))
    # If Lower value as calculated by Adjusted Box Plot is negative, remove lower two percentile values and assign lowest value to lower value
    if(lower<0)
    {
      a$MID_VAL<-as.numeric(a$PC_ITEM_FOB_PRICE)
      #lower_data<-quantile(a$PC_ITEM_FOB_PRICE,0.03)
      lower_data<-min(a[a$MID_VAL>=quantile(a$MID_VAL,0.01),c(5)])
      lower<-lower_data
    }
    
    max=max(a$PC_ITEM_FOB_PRICE,na.rm = T)
    
    if(upper>max)
    {
      a$MID_VAL <- as.numeric(a$PC_ITEM_FOB_PRICE)
      #upper_data <- quantile(a$PC_ITEM_FOB_PRICE,0.97)
      #upper <- upper_data
    }
    # Check outlier count and its percentage per subcat
    count_outliers<-sum(a$check==0)
    outliers_per<-(count_outliers/nrow(a))*100
    # Appending values to empty dataframe as created above
    Stats1<-rbind(Stats1,data.frame(MCAT_ID_Unit=unique(a$SPPL),min=min(a$PC_ITEM_FOB_PRICE,na.rm = T),
                                    max=max(a$PC_ITEM_FOB_PRICE,na.rm = T),mean=mean(a$PC_ITEM_FOB_PRICE,na.rm = T),median=median(a$PC_ITEM_FOB_PRICE,na.rm = T),
                                    Product_Count=sum(a$unit_value_count),Q1=Q1,Q3=Q3,IQR=IQR,MC=MC,lower=lower,upper=upper,outlier_cnt=count_outliers,outlier_per=outliers_per))
    
  }
  
}





write.csv(Stats1,"file:///D:/OND'18 Work/Pricing/All mcats Pricing/With R/New folder/85 mcats/New folder/2Next 50/Final range with Adjusted Boxplot.csv",row.names = F)









