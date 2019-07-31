# Loading the RJDBC package to connect to database

library(data.table)
library(RJDBC)
library(xlsx)
library(dplyr)
library(sqldf)
library(RODBC)
options(java.parameters = "-Xmx8000m")


startDate<-Sys.Date()-30
endDate<- Sys.Date()-1
startDate <-format(startDate,format="%d-%b-%y")
endDate <-format(endDate,format="%d-%b-%y")
############################################Testing Code###############################################


jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Imart/Desktop/sqldeveloper/jdbc/lib/ojdbc6.jar")

jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
if(exists("jdbcConIMBLR"))
{
  getId<-paste("WITH MY_OFFR AS(
               SELECT ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID, ETO_OFR_TITLE,GLUSR_USR_CITY, GLUSR_USR_STATE,ETO_ENQ_TYP,GLUSR_USR_EMAIL
               FROM
               (
               SELECT ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID, ETO_OFR_TITLE ,FK_GLUSR_USR_ID,ETO_ENQ_TYP
               FROM ETO_OFR 
               WHERE TRUNC(eto_ofr_approv_date_orig) BETWEEN '",startDate,"' AND '",endDate,"'
               and eto_ofr_approv = 'A'
               UNION
               SELECT ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID, ETO_OFR_TITLE ,FK_GLUSR_USR_ID,ETO_ENQ_TYP
               FROM ETO_OFR_EXPIRED 
               WHERE TRUNC(eto_ofr_approv_date_orig) BETWEEN '",startDate,"' AND '",endDate,"'
               and eto_ofr_approv = 'A'
               ) , GLUSR_USR
               where FK_GLCAT_MCAT_ID in (49882,192627)
               and FK_GLUSR_USR_ID = GLUSR_USR_ID),
               MY_SOLD_DATA AS
               (
               select FK_ETO_OFR_ID  from  eto_lead_pur_hist
               Where  trunc(ETO_PUR_DATE) BETWEEN '",startDate,"' AND '",endDate,"'
               ) , 
               MY_FILLED_RESPS AS
               (
               select FK_ETO_OFR_DISPLAY_ID, ETO_ATTRIBUTE_MCAT_ID, FK_IM_SPEC_MASTER_DESC, FK_IM_SPEC_OPTIONS_DESC
               from ETO_ATTRIBUTE
               WHERE FK_IM_SPEC_MASTER_ID <> -1
               )
               SELECT DISTINCT ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID, ETO_OFR_TITLE,GLUSR_USR_CITY, GLUSR_USR_STATE,FK_IM_SPEC_MASTER_DESC, FK_IM_SPEC_OPTIONS_DESC,
               COUNT(DISTINCT FK_ETO_OFR_ID) UNIQUE_SOLD,
               COUNT(FK_ETO_OFR_ID) TOTAL_SOLD,
               CASE WHEN ETO_ENQ_TYP IN (1,3,5) THEN 1 ELSE NULL END AS RETAIL_FLAG,
               CASE WHEN GLUSR_USR_EMAIL IS NULL THEN NULL ELSE 1 END AS EMAIL_FLAG
               FROM
               MY_OFFR,
               MY_FILLED_RESPS,
               MY_SOLD_DATA    
               WHERE ETO_OFR_DISPLAY_ID = FK_ETO_OFR_ID(+)
               AND ETO_OFR_DISPLAY_ID = FK_ETO_OFR_DISPLAY_ID(+)
               GROUP BY ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID, ETO_OFR_TITLE,GLUSR_USR_CITY, GLUSR_USR_STATE,FK_IM_SPEC_MASTER_DESC, FK_IM_SPEC_OPTIONS_DESC,ETO_ENQ_TYP,GLUSR_USR_EMAIL
               ")
  queryResult <- dbGetQuery(jdbcConIMBLR,getId)
}



my_file<-queryResult[,1]
my_file<-data.frame(my_file)



my_file1<-my_file
my_file1<-unique(my_file1)
my_file1<-rename(my_file1,ETO_OFR_DISPLAY_ID=my_file)

parts<-ceiling(length(my_file1$ETO_OFR_DISPLAY_ID)/1000)




if(parts==0)
{
  a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%1)))
}
if(parts!=0)
{
  a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))
}


df <- data.frame(ETO_OFR_DISPLAY_ID=numeric(),
                 SECONDARY_MCAT=numeric(), 
                 PRIME_MCAT_NAME=character(), 
                 FREQUENCY=character(),
                 LOOKING_FOR_SUPPLIERS=character(),
                 PORPOSE=character())

if(exists("jdbcConIMBLR"))
{
  
  #Query for extracting data for each loop
  for(i in 1:length(a)){
    
    a[[i]]$ETO_OFR_DISPLAY_ID[-length(a[[i]]$ETO_OFR_DISPLAY_ID)]<-paste0(a[[i]]$ETO_OFR_DISPLAY_ID[-length(a[[i]]$ETO_OFR_DISPLAY_ID)],',') #to assign comma at end of every value
    select<-a[[i]]$ETO_OFR_DISPLAY_ID#Extracting Id
    select<-paste(a[[i]]$ETO_OFR_DISPLAY_ID,collapse=" ")# making different value of vector into single string so that it can be passed in query
    
    query<-paste("Select
                 eto_ofr_display_id,
                 (select glcat_mcat_name from glcat_mcat where glcat_mcat_id = ETO_OFR_MAPPING.FK_GLCAT_MCAT_ID)Secondary_MCAT,
                 
                 (select glcat_mcat_name from glcat_mcat where glcat_mcat_id = eto_ofr.FK_GLCAT_MCAT_ID)PRIME_MCAT_NAME,
                 decode(ETO_OFR_REQ_FREQ,1,'One Time',3,'Daily',5,'Monthly',8,'Yearly','null') Frequency,
                 decode(ETO_OFR_GEOGRAPHY_ID,1,'Local only',2,'Anywhere in India',3,'global',4,'Specific Cities','null') Looking_for_suppliers,
                 decode(ETO_OFR_REQ_TYPE,1,'Reselling',2,'Personal',4,'Business Use','null') Porpose
                 from eto_ofr ,ETO_OFR_MAPPING
                 where eto_ofr_display_id IN (" ,select, ")
                 and eto_ofr_display_id = FK_ETO_OFR_ID
                 union 
                 Select
                 eto_ofr_display_id,
                 (select glcat_mcat_name from glcat_mcat where glcat_mcat_id = ETO_OFR_MAPPING_EXPIRED.FK_GLCAT_MCAT_ID)Secondary_MCAT,
                 
                 (select glcat_mcat_name from glcat_mcat where glcat_mcat_id = ETO_OFR_EXPIRED.FK_GLCAT_MCAT_ID)PRIME_MCAT_NAME,
                 decode(ETO_OFR_REQ_FREQ,1,'One Time',3,'Daily',5,'Monthly',8,'Yearly','null') Frequency,
                 decode(ETO_OFR_GEOGRAPHY_ID,1,'Local only',2,'Anywhere in India',3,'global',4,'Specific Cities','null') Looking_for_suppliers,
                 decode(ETO_OFR_REQ_TYPE,1,'Reselling',2,'Personal',4,'Business Use','null') Porpose
                 from ETO_OFR_EXPIRED , ETO_OFR_MAPPING_EXPIRED
                 where eto_ofr_display_id IN (" ,select, ")
                 and eto_ofr_display_id = FK_ETO_OFR_ID")
    
    
    imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
    df<-rbind(df,imblrQuery)
    
  }
  
}
queryResult_df <- merge(x=queryResult,y=df,by.x = "ETO_OFR_DISPLAY_ID",by.y = "ETO_OFR_DISPLAY_ID",all.x = T)  #To merge both data tables

split1 <- split(queryResult_df,queryResult_df$ETO_OFR_DISPLAY_ID)

df2 <- data.frame(ETO_OFR_DISPLAY_ID=numeric(),
                  FK_GLCAT_MCAT_ID=numeric(),
                  ETO_OFR_TITLE=character(),
                  GLUSR_USR_CITY=character(),
                  GLUSR_USR_STATE=character(),
                  UNIQUE_SOLD=numeric(),
                  TOTAL_SOLD=numeric(),
                  RETAIL_FLAG=numeric(),
                  EMAIL_FLAG=numeric(),
                  PRIME_MCAT_NAME=character(),
                  FREQUENCY=character(),
                  LOOKING_FOR_SUPPLIERS=character(),
                  PORPOSE=character(),
                  master_option=character(),
                  SECONDARY_MCAT=character())

i=1
for (i in 1:length(split1)) {
  Master_df <- split1[[i]]
  Master_desc <- Master_df$FK_IM_SPEC_MASTER_DESC
  option_desc <- Master_df$FK_IM_SPEC_OPTIONS_DESC
  master_option <- paste0(Master_desc,"|",option_desc)
  master_option <- unique(master_option)
  master_option <- sort(master_option)
  master_option <- gsub(" ","_",master_option)
  master_option <- paste0(master_option,collapse=", ")
  
  SECONDARY_MCAT<-Master_df$SECONDARY_MCAT
  SECONDARY_MCAT<-unique(SECONDARY_MCAT)
  SECONDARY_MCAT <- gsub(" ","_",SECONDARY_MCAT)
  SECONDARY_MCAT <- paste0("s_mcat|",SECONDARY_MCAT,collapse=", ")
  
  
  ETO_OFR_DISPLAY_ID<-unique(Master_df$ETO_OFR_DISPLAY_ID)
  FK_GLCAT_MCAT_ID<-unique(Master_df$FK_GLCAT_MCAT_ID)
  ETO_OFR_TITLE<-unique(Master_df$ETO_OFR_TITLE)
  GLUSR_USR_CITY<-unique(Master_df$GLUSR_USR_CITY)
  GLUSR_USR_STATE<-unique(Master_df$GLUSR_USR_STATE)
  LOCATION_USR <- paste0(GLUSR_USR_CITY,"|",GLUSR_USR_STATE)
  LOCATION_USR <- gsub(" ","_",LOCATION_USR)
  UNIQUE_SOLD<-unique(Master_df$UNIQUE_SOLD)
  TOTAL_SOLD<-unique(Master_df$TOTAL_SOLD)
  RETAIL_FLAG<-unique(Master_df$RETAIL_FLAG)
  EMAIL_FLAG<-unique(Master_df$EMAIL_FLAG)
  PRIME_MCAT_NAME<-unique(Master_df$PRIME_MCAT_NAME)
  PRIME_MCAT_NAME <- paste0("p_mcat|",PRIME_MCAT_NAME)
  PRIME_MCAT_NAME <- gsub(" ","_",PRIME_MCAT_NAME)
  FREQUENCY<-unique(Master_df$FREQUENCY)
  FREQUENCY <- gsub(" ","_",FREQUENCY)
  LOOKING_FOR_SUPPLIERS<-unique(Master_df$LOOKING_FOR_SUPPLIERS)
  LOOKING_FOR_SUPPLIERS <- gsub(" ","_",LOOKING_FOR_SUPPLIERS)
  PORPOSE<-unique(Master_df$PORPOSE)
  PORPOSE <- gsub(" ","_",PORPOSE)
  df2<-rbind(df2,data.frame(ETO_OFR_DISPLAY_ID,FK_GLCAT_MCAT_ID,ETO_OFR_TITLE,
                            LOCATION_USR,UNIQUE_SOLD,
                            TOTAL_SOLD,RETAIL_FLAG,EMAIL_FLAG,
                            PRIME_MCAT_NAME,FREQUENCY,LOOKING_FOR_SUPPLIERS,
                            PORPOSE,master_option,SECONDARY_MCAT))
  
}

#write.xlsx(df2,"C:/Users/imart/Desktop/R Folder/Trouble.xlsx",row.names = FALSE)
df2[is.na(df2)] <- 0
jdbcReportDB <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora9.intermesh.net:1521/STSGEN", "report", "report")
RJDBC::dbWriteTable(conn = jdbcReportDB,'MUK1234',df2,append=TRUE)



