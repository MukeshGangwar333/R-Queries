options(java.parameters = "-Xmx8000m")

library(readxl)
library(xlsx)
library(stringr)
library(reshape)
library(splitstackshape)
library(RJDBC)


options("scipen"=100, "digits"=2)
options(scipen = 999)




df <- read_excel("C:/Users/imart/Documents/Scrapped data 2.0 (1).xlsx", sheet = "Earthmoving New")
df$`Company Name` <- gsub(" pvt. ltd.","", df$`Company Name`, ignore.case = T)
df$`Company Name` <- gsub(" ltd.","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(". Pvt .ltd","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("PVT. LTD","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("PVT LTD","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("PVT.LTD.","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Pvt Limited","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Pvt.ltd","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(" pvt","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Private Limited","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Private Limit","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Private Limi","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Private Lim","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("Private Lim.","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(" (P) Ltd","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(" Ltd","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("(P)","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(" Limited","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(" Limite","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub(",","", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("&","%&%", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("'","''", df$`Company Name`, ignore.case = T  )
df$`Company Name` <- gsub("^,|\\.","",df$`Company Name`, ignore.case = T )
df$`Company Name` <- trimws(df$`Company Name`)

my_file1 <- as.data.frame(unique(df$`Company Name`)) 
colnames(my_file1) <- "company_name"

my_file1 <- na.omit(my_file1)

my_file1$company_name <- as.character(my_file1$company_name)

parts<-ceiling(length(my_file1$company_name )/1000)

a <- split(my_file1, sort(rank(row.names(my_file1))%%parts))#Divide into equal parts


empty_df <- data.frame(
  GLUSR_USR_ID=numeric(),
  GLUSR_USR_USRNAME=character(), 
  GLUSR_USR_COMPANYNAME=character(),
  GLUSR_USR_EMAIL=character(), 
  GLUSR_USR_CITY=character(), 
  GLUSR_USR_STATE=character(), 
  GLUSR_USR_PH_MOBILE=numeric(),
  GLUSR_USR_CUSTTYPE_NAME=character()
) 

# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")
#jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
##create MESHR connection to DB
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

if(exists("jdbcCOnMESHR"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  a[[i]]$company_name <- paste0("'", a[[i]]$company_name,"'" )
  a[[i]]$company_name[-length(a[[i]]$company_name)]<-paste0(a[[i]]$company_name[-length(a[[i]]$company_name)],",") # to assign comma at end of every value
  select<-a[[i]]$company_name#Extracting Id
  select<-paste(a[[i]]$company_name,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  
  
  
  
  query <- paste0("select GLUSR_USR_ID, GLUSR_USR_USRNAME, GLUSR_USR_COMPANYNAME, GLUSR_USR_EMAIL, GLUSR_USR_CITY, GLUSR_USR_STATE, GLUSR_USR_PH_MOBILE, GLUSR_USR_CUSTTYPE_NAME
from glusr_usr
where GLUSR_USR_COMPANYNAME in (",select,")")
  
  
  queryresult <- dbGetQuery(jdbcCOnMESHR,query)
  empty_df<-rbind(empty_df,queryresult)
  
}

empty_df1 <- empty_df[ !duplicated(empty_df$GLUSR_USR_COMPANYNAME),  ]

Final_Data <- merge(df,empty_df1, by.x = "Company Name", by.y = "GLUSR_USR_COMPANYNAME", all.x = T)

write.csv(Final_Data, "Final_Data_from_company_name.csv", row.names = F, quote = T)
