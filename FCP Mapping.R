options(java.parameters = "-Xmx12000m")
library(RJDBC)
library(readxl)
library(writexl)

input_file <- readxl::read_excel("C:/Users/imart/Documents/Varun/Chemicals, Dyes & Allied Products.xlsx",sheet = 1)
input_file <- input_file[,c(2,3)]
my_file1<-unique(input_file)
my_file1<-my_file1[,1]
my_file1 <- my_file1[!is.na(my_file1$MCAT_ID),]


parts<-ceiling(length(my_file1$MCAT_ID)/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

df <- data.frame(GLCAT_MCAT_ID= numeric(),
                 GLCAT_MCAT_NAME= character(),
                 GLCAT_ALTMCAT_NAME=character(),
                 GLCAT_ALTMCAT_PRIORITY=numeric(),
                 FK_GL_LANGUAGE_ID=numeric()
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
    
    a[[i]]$MCAT_ID[-length(a[[i]]$MCAT_ID)]<-paste0(a[[i]]$MCAT_ID[-length(a[[i]]$MCAT_ID)],',')# to assign comma at end of every value
    select<-a[[i]]$MCAT_ID#Extracting Id
    select<-paste(a[[i]]$MCAT_ID,collapse=" ")# making different value of vector into single string so that it can be passed in query
    
query<-paste0("SELECT GLCAT_MCAT_ID,GLCAT_MCAT_NAME,GLCAT_ALTMCAT_NAME,GLCAT_ALTMCAT_PRIORITY,FK_gl_language_ID 
FROM GLCAT_MCAT,
GLCAT_MCAT_ALLCOUNT,
GLCAT_ALTMCAT
WHERE GLCAT_MCAT_ALLCOUNT.FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID
and glcat_mcat_id in (",select,")
AND glcat_altmcat.FK_GLCAT_MCAT_ID=GLCAT_MCAT_ID")
    
    
    imblrQuery <- dbGetQuery(jdbcCOnMESHR,query)
    df<-rbind(df,imblrQuery)
    
    }
  


