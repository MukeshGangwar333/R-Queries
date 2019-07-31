options(java.parameters = "-Xmx8000m")

library(RJDBC)
library(dplyr)
library(sqldf)
library(readxl)

#JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")

#Create UMBLR connection
jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")

#create MESHR connection
#jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")



input_file <- readxl::read_excel("C:/Users/imart/Documents/Sanya/PMCAT_MCAT_Seller_Mapping/Page Enrichment _ Construction Group.xlsx", sheet = 2)
colnames(input_file) <- gsub(" ","_",colnames(input_file), ignore.case = T  )

input_file <- input_file[,c("MCAT_ID","PMCAT_ID" )]
input_file <- input_file[order(input_file$PMCAT_ID ), ]
input_file <- unique(input_file)

pmcat_list <- unique(input_file$PMCAT_ID) 




df <- data.frame(GLUSR_USR_ID= numeric(),
                 GLUSR_USR_CITY= character(),
                 GLCAT_MCAT_ID=numeric(),
                 ALREADY_MAPPED_COUNT=numeric(),
                 MAPPED_MCAT_IDS=numeric(),
                 GLCAT_MCAT_NAME= character(),
                 GLUSR_USR_COMPANYNAME= character(),
                 GLUSR_USR_CUSTTYPE_NAME= character()
) 

if(exists("jdbcConIMBLR"))
  
{   
  
 i=1
  
  for (i in 1:length(pmcat_list)) {
    
    s_data <- input_file[input_file$PMCAT_ID %in% pmcat_list[i],]  

    pmcat <- unique(s_data$PMCAT_ID)
    mcat <- s_data$MCAT_ID
    
    mcat[-length(mcat) ] <- paste(mcat[-length(mcat) ],"," )
    mcat <- paste( mcat, collapse = " " )
    
    
    query <- paste0("SELECT C.*, F.GLCAT_MCAT_NAME, E.GLUSR_USR_COMPANYNAME, E.GLUSR_USR_CUSTTYPE_NAME FROM 
(
SELECT GLUSR_USR_ID ,GLUSR_USR_CITY, GLCAT_MCAT_ID, ALREADY_MAPPED_COUNT, MAPPED_MCAT_IDS FROM
(
SELECT
GLUSR_USR_ID,GLUSR_USR_CITY, COUNT(DISTINCT FK_GLCAT_MCAT_ID) ALREADY_MAPPED_COUNT, WM_CONCAT(DISTINCT FK_GLCAT_MCAT_ID) MAPPED_MCAT_IDS
FROM GLCAT_MCAT, PC_ITEM_TO_GLCAT_MCAT, GLUSR_USR , PC_ITEM
WHERE GLCAT_MCAT_ID in (",pmcat," )
AND FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID
AND PC_ITEM_GLUSR_USR_ID = GLUSR_USR_ID
AND PC_ITEM_ID = FK_PC_ITEM_ID
AND PC_ITEM_STATUS_APPROVAL >= 20
and GLUSR_USR_CUSTTYPE_NAME in ('CATALOG','TSCATALOG','STAR','LEADER')
GROUP BY GLUSR_USR_ID,GLUSR_USR_CITY
) A, ( SELECT GLCAT_MCAT_ID FROM GLCAT_MCAT WHERE GLCAT_MCAT_ID IN (",mcat,") ) B ) C 
LEFT OUTER JOIN 
( SELECT
DISTINCT GLUSR_USR_ID UNMAP_GLID, FK_GLCAT_MCAT_ID UNMAP_MCAT_ID
FROM GLCAT_MCAT, PC_ITEM_TO_GLCAT_MCAT, GLUSR_USR , PC_ITEM
WHERE GLCAT_MCAT_ID in (",mcat,")
AND FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID
AND PC_ITEM_GLUSR_USR_ID = GLUSR_USR_ID
AND PC_ITEM_ID = FK_PC_ITEM_ID
AND PC_ITEM_STATUS_APPROVAL >= 20
and GLUSR_USR_CUSTTYPE_NAME in ('CATALOG','TSCATALOG','STAR','LEADER') ) D ON ( C.GLUSR_USR_ID = D.UNMAP_GLID AND C.GLCAT_MCAT_ID = D.UNMAP_MCAT_ID )
JOIN GLUSR_USR E ON C.GLUSR_USR_ID = E.GLUSR_USR_ID
JOIN GLCAT_MCAT F ON C.GLCAT_MCAT_ID = F.GLCAT_MCAT_ID
WHERE UNMAP_GLID IS NULL" )


imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
df<-rbind(df,imblrQuery) 

 }
  
 }


write.csv(df,"My_Final_Data.csv", row.names = F, quote = T)




