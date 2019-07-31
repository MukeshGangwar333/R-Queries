options(java.parameters = "-Xmx15000m")

library(data.table)


pc_isq <- fread("C:/Users/imart/Documents/Deepika Gupta (Brands Data)/Formatted File.csv")
pc_isq_brand <- pc_isq[grepl("brand",pc_isq$master_description,ignore.case = T),] #having isq as brand or similar to brand



####Computing mcats of pc ids from the pc_isq_brand data


library(xlsx)
library(RJDBC)
library(dplyr)
library(sqldf)
library(readxl)

my_file1 <- pc_isq_brand
colnames(my_file1)[colnames(my_file1)=="pc_item_id"] <- "offer_id"


parts<-ceiling(length(my_file1$offer_id )/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

df <- data.frame(GLCAT_MCAT_ID=numeric(), 
                 GLCAT_MCAT_NAME=character(), 
                 FK_PC_ITEM_ID=numeric(),
                 PC_ITEM_ID=numeric(),
                 FK_GLCAT_MCAT_ID= numeric()) 







# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")
#jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
#create MESHR connection to DB
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

if(exists("jdbcCOnMESHR"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  a[[i]]$offer_id[-length(a[[i]]$offer_id)]<-paste0(a[[i]]$offer_id[-length(a[[i]]$offer_id)],',')# to assign comma at end of every value
  select<-a[[i]]$offer_id#Extracting Id
  select<-paste(a[[i]]$offer_id,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  
  
  
  
  query <- paste0("select a.GLCAT_MCAT_ID, a.GLCAT_MCAT_NAME, b.FK_PC_ITEM_ID, b.FK_GLCAT_MCAT_ID
from glcat_mcat a, PC_ITEM_TO_GLCAT_MCAT b
                  where b.FK_GLCAT_MCAT_ID = a.GLCAT_MCAT_ID
                  and b.FK_PC_ITEM_ID in (",select,")")
  
  
  
  
  queryresult <- dbGetQuery(jdbcCOnMESHR,query)
  df<-rbind(df,queryresult)
  
}


save.image("DeepikaG.RData")

pc_isq_brand$concat <- paste(pc_isq_brand$pc_item_id, pc_isq_brand$master_description, pc_isq_brand$option_description, sep = "&&" )
pc_isq_brand_unq <- pc_isq_brand[!duplicated(pc_isq_brand$concat), ]


pc_to_mcat$concat <- paste(pc_to_mcat$GLCAT_MCAT_ID, pc_to_mcat$GLCAT_MCAT_NAME, pc_to_mcat$FK_PC_ITEM_ID, sep = "&&" )

pc_to_mcat_unq <- pc_to_mcat[!duplicated(pc_to_mcat$concat ),] 

pc_to_mcat_unq$concat <- NULL
pc_isq_brand_unq$concat <- NULL



isq_table <- as.data.frame(table(pc_isq_brand_unq$pc_item_id ) )
mcat_table <- as.data.frame(table(pc_to_mcat_unq$FK_PC_ITEM_ID))


table_all <- merge(isq_table,mcat_table, by="Var1",all = T )
table_all[is.na(table_all)] <- 0
table_all$total <- ""

table_all$total <- ifelse(table_all$Freq.y==0,table_all$total <- table_all$Freq.x, table_all$total <- table_all$Freq.x*table_all$Freq.y )
colnames(table_all) <- c("pc_item_id","isq","mcat","total")

sum(table_all$total)

combined_data <- merge(pc_to_mcat_unq,pc_isq_brand_unq,by.x = "FK_PC_ITEM_ID", by.y = "pc_item_id", all = T)

library(dplyr)
combined_data$option_description <- tolower(combined_data$option_description)
combined_data$option_description <- trimws(combined_data$option_description)


df_final <- combined_data %>% group_by(GLCAT_MCAT_NAME,option_description) %>% mutate(option_count= length(trimws(tolower(option_description))  )  )
df_final <- df_final[order(df_final$GLCAT_MCAT_NAME ),]

df_final[is.na( df_final )] <-"" 


df_final$mchar <- nchar(df_final$master_description)
df_final$ochar <- nchar(df_final$option_description)
df_final <- df_final[df_final$mchar<60,]
df_final <- df_final[df_final$ochar<201,]

sum(is.na(df_final))


df_final$master_description <- gsub("\t"," ", df_final$master_description)
df_final$option_description <- gsub("\t"," ", df_final$option_description)

sum(grepl("\t",df_final$master_description))
sum(grepl("\t", df_final$option_description))


df_final$GLCAT_MCAT_NAME <- gsub(","," ",df_final$GLCAT_MCAT_NAME)
df_final$master_description <- gsub(","," ", df_final$master_description)
df_final$option_description <- gsub(","," ",df_final$option_description)

df_final$GLCAT_MCAT_NAME <- trimws(df_final$GLCAT_MCAT_NAME)
df_final$master_description <- trimws(df_final$master_description)
df_final$option_description <- trimws(df_final$option_description)

df_final$mchar <- NULL
df_final$ochar <- NULL

df_final_unq <- df_final
df_final_unq$FK_PC_ITEM_ID <- NULL

df_final_unq$conc <- paste(df_final$GLCAT_MCAT_ID,df_final$GLCAT_MCAT_NAME,df_final$master_description,df_final$option_description,df_final$option_count, sep = "&&")

df_final_unq <- df_final_unq[!duplicated(df_final_unq$conc),]
df_final_unq$conc <- NULL

df_final_unq <- df_final_unq[order(df_final_unq$GLCAT_MCAT_NAME),  ]
mcat_to_subcat <- read.csv("C:/Users/imart/Documents/Deepika Gupta (Brands Data)/Input Files/mcat to subcat.csv")

Final_Required_data <- merge( df_final_unq, mcat_to_subcat, by.x ="GLCAT_MCAT_ID" , by.y = "MCAT_ID" , all.x = T)
Final_Required_data <- Final_Required_data[order(Final_Required_data$CAT_NAME), ]

Final_Required_data1 <- Final_Required_data[1:703155,]
Final_Required_data2 <- Final_Required_data[703156: nrow(Final_Required_data),]

save.image("C:/Users/imart/Documents/Deepika Gupta (Brands Data)/Deepika_Whole_Data.RData")

#########Products having more than 1 brand
#for this we need the df_final file which have all the pc ids with mcat_name and mcat_id
df_final <- df_final[order(df_final$FK_PC_ITEM_ID), ]
df_final$option_count <- NULL
df_final$concatenate <- paste(df_final$FK_PC_ITEM_ID, df_final$option_description, sep = "&&&")
df_final_unique <- df_final[!duplicated(df_final$concatenate ), ]

pc_id_more_than_one_brand_option <- as.data.frame(table(df_final_unique$FK_PC_ITEM_ID))
pc_id_more_than_one_brand_option <- pc_id_more_than_one_brand_option[pc_id_more_than_one_brand_option$Freq>1,]

sum(pc_id_more_than_one_brand_option$Freq)
length(pc_id_more_than_one_brand_option$Freq)


#pc ids having more than 1 brand option with full data

final_pc_ids <- df_final_unique[df_final_unique$FK_PC_ITEM_ID %in%  pc_id_more_than_one_brand_option$Var1,]
final_pc_ids$concatenate <- NULL

mcat_to_subcat <- read.csv("C:/Users/imart/Documents/Deepika Gupta (Brands Data)/mcat to subcat.csv")



final_result <- merge(final_pc_ids,mcat_to_subcat, by.x = "GLCAT_MCAT_ID", by.y = "MCAT_ID",all.x = T ) 
final_result <- final_result[order(final_result$FK_PC_ITEM_ID),]

write.xlsx(final_result,"PC_ids having more than one brand as option.xlsx", row.names = F)

