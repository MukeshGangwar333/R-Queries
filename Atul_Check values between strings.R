library(readxl)

Data <- read_excel("C:/Users/IMART/Documents/My_data(Origin).csv")


class(Data$gl_history_old_value)
class(Data$gl_history_new_value)

Data$gl_history_old_value <- as.character(Data$gl_history_old_value)
Data$gl_history_new_value <- as.character(Data$gl_history_new_value)

Data$gl_history_old_value <- gsub(",,",",",Data$gl_history_old_value)
Data$gl_history_new_value <- gsub(",,",",",Data$gl_history_new_value)


Data$gl_history_old_value <- substr(Data$gl_history_old_value,2,(nchar(Data$gl_history_old_value)-1)  )
Data$gl_history_new_value <- substr(Data$gl_history_new_value,2,(nchar(Data$gl_history_new_value)-1)  )

grep( strsplit(Data$gl_history_new_value[1],",") ,strsplit(Data$gl_history_old_value[1],","),ignore.case = T)
grep( strsplit(Data$gl_history_new_value[1],",") ,strsplit(Data$gl_history_old_value[1],","),ignore.case = T)

write.csv(Data, "My_Data.csv", row.names = F, quote = T)

df$gl_history_old_value <- as.character(df$gl_history_old_value)
df$gl_history_new_value <- as.character(df$gl_history_new_value)
summary(df)

df$Match <- ""

df <- read.csv("C:/Users/IMART/Documents/My_data_67331.csv")
df[is.na(df)] <- ""



i=1
for (i in 1:nrow(df)) {
  
  a <- unlist(strsplit(df$gl_history_old_value[i],","))
  b <- unlist(strsplit(df$gl_history_new_value[i],","))
  
  result <-  tryCatch({
    df$Match[i] <-  a[! a %in% b]
  }, error = function(e) {
    
    df$Match[i] <- 0
  }
  )
}


Negative_Data_HIST <- df[! df$Match=="", ]

#df$Match[df$Match==""] <- "0"


df_match_blank <- df[!df$Match=="", ]
df_old_blank <-  df_match_blank[!df_match_blank$gl_history_old_value=="", ]
df_new_blank <- df_old_blank[ !df_old_blank$gl_history_new_value=="", ]
df_start_0 <- df_new_blank[! grepl("^0",df_new_blank$Match, ignore.case = T  ),  ]


pc_item_ids <-  as.data.frame(unique(df_start_0$gl_history_data_ref_id  ))
colnames(pc_item_ids)[colnames(pc_item_ids)=="unique(df_start_0$gl_history_data_ref_id)" ] <- "pc_ids"

#rm(list=ls()[-12])

options(java.parameters = "-Xmx8000m")

library(RJDBC)
library(dplyr)

my_file1 <- pc_item_ids
colnames(my_file1)[colnames(my_file1)=="pc_ids"] <- "offer_id"



parts<-ceiling(length(my_file1$offer_id )/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

df <- data.frame(
  PC_ITEM_ID= numeric(),
  PC_ITEM_NAME=character(),
  PC_ITEM_STATUS_APPROVAL=numeric()
) 

# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/IMART/Documents/ojdbc6.jar")
jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
##create MESHR connection to DB
#jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

if(exists("jdbcConIMBLR"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  a[[i]]$offer_id[-length(a[[i]]$offer_id)]<-paste0(a[[i]]$offer_id[-length(a[[i]]$offer_id)],',')# to assign comma at end of every value
  select<-a[[i]]$offer_id#Extracting Id
  select<-paste(a[[i]]$offer_id,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  
  
  
  
  query <- paste0("select PC_ITEM_ID, PC_ITEM_NAME,PC_ITEM_STATUS_APPROVAL from pc_item
where PC_ITEM_STATUS_APPROVAL =40
                  and pc_item_id in (",select,")")
  
  
  
  
  imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
  df<-rbind(df,imblrQuery)
  
}


PC_ITEM_HISTORY_NAMES <- df

#rm(a,df,imblrQuery,jdbcConIMBLR,jdbcDriver,my_file1,i,parts,query,select)
write.csv(df,"PC_ITEM_HISTORY_NAMES.csv")