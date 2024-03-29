options(java.parameters = "-Xmx14336m")
memory.limit(14336m)

library(RJDBC)
library(xlsx)
library(RODBC)



startDate<-Sys.Date()-96
endDate<- Sys.Date()-6
startDate <-format(startDate,format="%d-%b-%y")
endDate <-format(endDate,format="%d-%b-%y")
############################################Testing Code###############################################


jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")

jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
if(exists("jdbcConIMBLR"))
{
  query<-paste("with MY_FENQ AS
               (
               SELECT FK_ETO_OFR_ID FENQ_OFR_ID,QUERY_MODID FENQ_MODID FROM ETO_OFR_FROM_FENQ WHERE TRUNC(DATE_R) BETWEEN '",startDate,"' AND '",endDate,"' 
               AND FK_ETO_OFR_ID IS NOT NULL
               UNION
               SELECT FK_ETO_OFR_ID, QUERY_MODID FROM ETO_OFR_FROM_FENQ_ARCH WHERE TRUNC(DATE_R) BETWEEN '",startDate,"' AND '",endDate,"' 
               AND FK_ETO_OFR_ID IS NOT NULL
               )
               , MY_OFFERS AS  
               (   
               SELECT DIR_QUERY_MCATID, QUERY_MODID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,QUERY_REFERENCE_URL REF_URL, 2 AS STATUS, 
               DECODE(DIR_QUERY_FREE_BL_TYP,2,'FENQ','DIRECT') BL_TYPE, 0 EXPIRED_FLAG FROM DIR_QUERY_FREE  
               WHERE TRUNC(DATE_R) BETWEEN '",startDate,"' AND '",endDate,"'  
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,1,
               DECODE(FK_GL_MODULE_ID,'FENQ','FENQ','DIRECT'), 0 EXPIRED_FLAG FROM ETO_OFR 
               WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"'    
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,1,DECODE(FK_GL_MODULE_ID,'FENQ','FENQ','DIRECT'), 1 EXPIRED_FLAG
               FROM ETO_OFR_EXPIRED WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"'  
               AND ETO_OFR_APPROV = 'A'
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,1,DECODE(FK_GL_MODULE_ID,'FENQ','FENQ','DIRECT'), 0 EXPIRED_FLAG
               FROM ETO_OFR_EXPIRED WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"'
               AND ETO_OFR_APPROV <> 'A' 
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,1,DECODE(FK_GL_MODULE_ID,'FENQ','FENQ','DIRECT'), 1 EXPIRED_FLAG 
               FROM ETO_OFR_EXPIRED_ARCH WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"'  
               AND ETO_OFR_APPROV = 'A'
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,1,DECODE(FK_GL_MODULE_ID,'FENQ','FENQ','DIRECT') , 0 EXPIRED_FLAG
               FROM ETO_OFR_EXPIRED_ARCH WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"' 
               AND ETO_OFR_APPROV <> 'A'
               UNION   
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,3,'DIRECT', 0 EXPIRED_FLAG FROM ETO_OFR_TEMP_DEL WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG)   
               BETWEEN '",startDate,"' AND '",endDate,"'    
               UNION  
               SELECT FK_GLCAT_MCAT_ID, FK_GL_MODULE_ID MODID,ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,ETO_OFR_PAGE_REFERRER REF_URL,3,'DIRECT', 0 EXPIRED_FLAG FROM ETO_OFR_TEMP_DEL_ARCH 
               WHERE  TRUNC(ETO_OFR_POSTDATE_ORIG) BETWEEN '",startDate,"' AND '",endDate,"'    
               UNION
               SELECT DIR_QUERY_MCATID, QUERY_MODID MODID,DIR_QUERY_FREE_REFID ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,QUERY_REFERENCE_URL REF_URL, 3 AS STATUS,'FENQ', 0 EXPIRED_FLAG FROM ETO_OFR_FROM_FENQ
               WHERE TRUNC(DATE_R) BETWEEN '",startDate,"' AND '",endDate,"' AND FK_ETO_OFR_ID IS NULL
               UNION
               SELECT DIR_QUERY_MCATID, QUERY_MODID MODID,DIR_QUERY_FREE_REFID ETO_OFR_DISPLAY_ID,FK_GLUSR_USR_ID,QUERY_REFERENCE_URL REF_URL, 3 AS STATUS,'FENQ', 0 EXPIRED_FLAG FROM ETO_OFR_FROM_FENQ_ARCH
               
               WHERE TRUNC(DATE_R) BETWEEN '",startDate,"' AND '",endDate,"' AND FK_ETO_OFR_ID IS NULL
               
               )  
               SELECT    
               DIR_QUERY_MCATID,  GLUSR_USR_CITY,
               COUNT( ETO_OFR_DISPLAY_ID) GENERATED_BL,
               COUNT( DECODE( BL_TYPE, 'FENQ', 1 ) ) GENERATED_FENQ,
               COUNT( DECODE( BL_TYPE, 'FENQ', DECODE( BL_SOURCE, 'MOBILE', 1, NULL  ) ) ) GENERATED_FENQ_MOBILE,
               COUNT( DECODE( BL_TYPE, 'FENQ', DECODE( BL_SOURCE, 'MOBILE', NULL, 1  ) ) ) GENERATED_FENQ_WEB,
               COUNT( DECODE( BL_TYPE, 'DIRECT', 1 ) ) GENERATED_DIRECT,
               COUNT( DECODE( STATUS, 1, DECODE( BL_TYPE, 'FENQ', ETO_OFR_DISPLAY_ID) , NULL ) ) APPROVED_FENQ,  
               COUNT( DECODE( STATUS, 1, ETO_OFR_DISPLAY_ID, NULL ) ) APPROVED_BL,  
               COUNT( DECODE( BL_SOURCE, 'MOBILE', 1, NULL  ) ) GENERATED_MOBILE,
               COUNT( DECODE( BL_SOURCE, 'MOBILE', NULL, 1  ) ) GENERATED_WEB,
               COUNT( DECODE( STATUS , 1 , DECODE( BL_SOURCE, 'MOBILE', 1, NULL  ), NULL ) ) APPROVED_MOBILE,
               COUNT( DECODE( STATUS , 1 , DECODE( BL_SOURCE, 'MOBILE', NULL, 1  ), NULL ) ) APPROVED_WEB,
               COUNT(DISTINCT FK_GLUSR_USR_ID) UNIQUE_BL_SENDER,
               COUNT( DISTINCT DECODE( BL_SOURCE, 'MOBILE', FK_GLUSR_USR_ID, NULL  ) ) UNIQUE_SENDER_MOBILE,
               COUNT( DISTINCT DECODE( BL_SOURCE, 'MOBILE', NULL, FK_GLUSR_USR_ID  ) ) UNIQUE_SENDER_WEB,
               COUNT( DISTINCT DECODE(SOLD_TRANSACTION,0,NULL,ETO_OFR_DISPLAY_ID )) SOLD_TOTAL,
               COUNT( DISTINCT DECODE(SOLD_TRANSACTION,0,NULL,DECODE( BL_SOURCE, 'MOBILE', ETO_OFR_DISPLAY_ID, NULL  ) )) SOLD_MOBILE,   
               COUNT( DISTINCT DECODE(SOLD_TRANSACTION,0,NULL,DECODE( BL_SOURCE, 'MOBILE', NULL, ETO_OFR_DISPLAY_ID  ) )) SOLD_WEB,  
               SUM( SOLD_TRANSACTION ) TRANS_TOTAL,
               SUM( DECODE( BL_SOURCE, 'MOBILE', SOLD_TRANSACTION, 0  ) ) TRANS_MOBILE,  
               SUM( DECODE( BL_SOURCE, 'MOBILE', 0, SOLD_TRANSACTION ) ) TRANS_WEB
               
               FROM
               (  
               SELECT ETO_OFR_DISPLAY_ID, DIR_QUERY_MCATID,  MODID, FK_GLUSR_USR_ID, STATUS, SOLD_TRANSACTION, BL_TYPE , GLUSR_USR_CITY,
               DECODE(  MODID, 'IMOB', 'MOBILE', 'ANDROID', 'MOBILE', 'FUSIONI', 'MOBILE', 'FUSIONW', 'MOBILE', 'FUSIONA' , 'MOBILE', 'FUSIONB', 'MOBILE', 'WEB' ) BL_SOURCE,
               EXPIRED_FLAG
               FROM
               (  
               SELECT ETO_OFR_DISPLAY_ID, DIR_QUERY_MCATID, NVL(FENQ_MODID,MODID) MODID, MY_OFFERS.FK_GLUSR_USR_ID, STATUS,  
               COUNT(DISTINCT ETO_LEAD_PUR_ID) SOLD_TRANSACTION  , BL_TYPE, EXPIRED_FLAG
               FROM  MY_OFFERS , ETO_LEAD_PUR_HIST  , MY_FENQ
               WHERE ETO_OFR_DISPLAY_ID = FK_ETO_OFR_ID(+)   
               AND ETO_OFR_DISPLAY_ID = FENQ_OFR_ID(+)
               GROUP BY ETO_OFR_DISPLAY_ID, DIR_QUERY_MCATID, NVL(FENQ_MODID,MODID), MY_OFFERS.FK_GLUSR_USR_ID, STATUS , BL_TYPE, EXPIRED_FLAG
               ),GLUSR_USR  
               where FK_GLUSR_USR_ID = GLUSR_USR_ID(+)
               )
               GROUP BY DIR_QUERY_MCATID,GLUSR_USR_CITY")
  
  
  imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
  
  write.csv(imblrQuery,"C:/Users/imart/Documents/Shivendra.csv")
  
  
  
  df <- read.csv("C:/Users/imart/Documents/Shivendra.csv")
  