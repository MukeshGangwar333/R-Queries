options(java.parameters = "-Xmx8000m")
options(scipen = 999)

library(xlsx)
library(RODBC)
library(RJDBC)
library(dplyr)
library(sqldf)
library(readxl)

my_file1 <- read.csv("file:///C:/Users/imart/Downloads/input file testing.csv")
my_file1$Email <- tolower(my_file1$Email)




parts<-ceiling(length(my_file1$MobileNumber )/1000)

a <- split(my_file1, sort(rank(row.names(my_file1))%%parts))#Divide into equal parts


df <- data.frame(
  
  GLUSR_USR_ID=numeric(),
  GLUSR_USR_FIRSTNAME= character() ,
  GLUSR_USR_LASTNAME=character(),
  GLUSR_USR_PH_MOBILE=numeric() ,
  GLUSR_USR_PH_MOBILE_ALT=numeric(),
  GLUSR_USR_EMAIL=character(),
  GLUSR_USR_EMAIL_ALT=character(),
  FCP_FLAG=numeric(),
  GLUSR_USR_CUSTTYPE_NAME=character(),
  GLUSR_USR_CUSTTYPE_ID=numeric()
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
  
  
  a[[i]]$MobileNumber <- paste0("'",a[[i]]$MobileNumber,"'")
  
  a[[i]]$MobileNumber[-length(a[[i]]$MobileNumber)] <- paste0(a[[i]]$MobileNumber[-length(a[[i]]$MobileNumber)],",")

mob <- a[[i]]$MobileNumber
mob <- paste(mob, collapse = "") 



a[[i]]$Email <- paste0("'",a[[i]]$Email,"'" )
a[[i]]$Email[-length(a[[i]]$Email)] <- paste0(a[[i]]$Email[-length(a[[i]]$Email)],",")  
  
 email <- a[[i]]$Email 
 email <- paste(email,collapse = " " )
    

  
  query <- paste0("SELECT GLUSR_USR_ID, GLUSR_USR_FIRSTNAME,GLUSR_USR_LASTNAME, GLUSR_USR_PH_MOBILE, GLUSR_USR_PH_MOBILE_ALT, GLUSR_USR_EMAIL, GLUSR_USR_EMAIL_ALT,
FCP_FLAG, GLUSR_USR_CUSTTYPE_NAME,GLUSR_USR_CUSTTYPE_ID
FROM
GLUSR_USR WHERE (
GLUSR_USR_PH_MOBILE IN (",mob,")

OR 
GLUSR_USR_PH_MOBILE_ALT IN (",mob,")

OR
GLUSR_USR_EMAIL IN (",email,")

OR 
GLUSR_USR_EMAIL_ALT IN (",email,"))")
  
  
  
  
  imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
  df<-rbind(df,imblrQuery)
  
}




###verifying data

mob_and_alt <- df$GLUSR_USR_PH_MOBILE
mob_and_alt <- append(mob_and_alt, df$GLUSR_USR_PH_MOBILE_ALT )
mob_and_alt <- mob_and_alt[!is.na(mob_and_alt)]
mob_and_alt <- unique(mob_and_alt)
mob_and_alt <- as.data.frame(mob_and_alt)
mob_and_alt$Match_M <- "Yes"

email_and_alt <- df$GLUSR_USR_EMAIL
email_and_alt <- append(email_and_alt,df$GLUSR_USR_EMAIL_ALT )
email_and_alt <- email_and_alt[!is.na(email_and_alt)]
email_and_alt <- unique(email_and_alt)
email_and_alt <- as.data.frame(email_and_alt)
email_and_alt$Match_E <- "Yes"




check <- merge(my_file1, mob_and_alt, by.x = "MobileNumber", by.y = "mob_and_alt", all.x = T )


final_data <- merge(check,email_and_alt, by.x = "Email", by.y = "email_and_alt" , all.x = T)

write.csv(final_data,"Final_Data.csv", row.names = F)
#write.csv(my_file1,"Input_File.csv", row.names = F)
