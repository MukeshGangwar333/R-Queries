options(java.parameters = "-Xmx8000m")

install.packages("xlsx")
install.packages("RJDBC")
install.packages("dplyr")
install.packages("sqldf")
install.packages("readxl")

library(xlsx)
library(RJDBC)
library(dplyr)
library(sqldf)
library(readxl)

my_file1 <- read.csv("C:/Users/Administrator/Desktop/r_mcats.csv")

parts<-ceiling(length(my_file1$offer_id )/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

df <- data.frame(FK_GLCAT_MCAT_ID=numeric(), 
                 GLCAT_MCAT_NAME=character(), 
                 ITEM_MAPPING_ISPRIME=numeric(),
                 PC_ITEM_ID=numeric(),
                 PC_ITEM_NAME=character(), 
                 PC_ITEM_DESC_SMALL=character(), 
                 FK_IM_SPEC_MASTER_ID= numeric(),
                 FK_IM_SPEC_MASTER_DESC=character() ,
                 FK_IM_SPEC_OPTIONS_ID=numeric(),
                 FK_IM_SPEC_OPTIONS_DESC=character() 
                 ) 

# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Administrator/Documents/ojdbc6.jar")
#jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
create MESHR connection to DB
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

if(exists("jdbcCOnMESHR"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  a[[i]]$offer_id[-length(a[[i]]$offer_id)]<-paste0(a[[i]]$offer_id[-length(a[[i]]$offer_id)],',')# to assign comma at end of every value
  select<-a[[i]]$offer_id#Extracting Id
  select<-paste(a[[i]]$offer_id,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  
  
  
  
  query <- paste0("Select FK_GLCAT_MCAT_ID,GlCAT_MCAT_NAme,ITEM_MAPPING_ISPRIME,PC_ITEM_ID,PC_ITEM_NAME,PC_ITEM_DESC_SMALL,FK_IM_SPEC_MASTER_ID,FK_IM_SPEC_MASTER_DESC,FK_IM_SPEC_OPTIONS_ID,FK_IM_SPEC_OPTIONS_DESC
from PC_ITEM_TO_GLCAT_MCAT A,GLCAT_MCAT,PC_ITEM, PC_ITEM_ATTRIBUTE B
                  where A.FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID
                  and A.FK_PC_ITEM_ID = PC_ITEM_ID
                  and A.FK_PC_ITEM_ID = B.FK_PC_ITEM_ID
                  and ITEM_MAPPING_ISPRIME = -1
                  and FK_GLCAT_MCAT_ID in (",select,")")
 
 
 
 
 queryresult <- dbGetQuery(jdbcCOnMESHR,query)
 df<-rbind(df,queryresult)
 
}


write.csv(df,"C:/Users/Administrator/Desktop/Final_Result.csv")