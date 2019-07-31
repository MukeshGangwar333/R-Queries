options(java.parameters = "-Xmx8000m")

library(xlsx)
library(RODBC)
library(RJDBC)
library(dplyr)
library(sqldf)
library(readxl)

input_file <- readxl::read_excel("C:/Users/imart/Documents/Prachi/BanLog-27-12-18.xlsx",sheet = 1)
my_file<-input_file[,1]
my_file1<-unique(my_file)
colnames(my_file1)[colnames(my_file1)=="Offer Id"] <- "offer_id"
my_file1 <- my_file1[!is.na(my_file1$offer_id),]
                            

parts<-ceiling(length(my_file1$offer_id )/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

df <- data.frame(ETO_OFR_DISPLAY_ID=numeric(),
                 FK_GLCAT_MCAT_ID=numeric()
) 

# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")
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
  

  
  
  
  query <- paste0("SELECT E1.ETO_OFR_DISPLAY_ID, E1.FK_GLCAT_MCAT_ID
FROM 
(
  SELECT * FROM ETO_OFR
  UNION
  SELECT * FROM ETO_OFR_EXPIRED
)E1,
(
  SELECT * FROM ETO_OFR_MAPPING
  UNION
  SELECT * FROM ETO_OFR_MAPPING_EXPIRED
)E2
WHERE
E1.ETO_OFR_DISPLAY_ID = E2.FK_ETO_OFR_ID
and E1.ETO_OFR_DISPLAY_ID in (",select,")
GROUP BY E1.ETO_OFR_DISPLAY_ID,E1.FK_GLCAT_MCAT_ID")
  
  
  
  
  imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
  df<-rbind(df,imblrQuery)
  
}



write.csv(df1,"C:/Users/mukesh kumar/Desktop/DNS.csv")




