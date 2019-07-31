options(java.parameters = "-Xmx12000m")
library(RJDBC)
library(readxl)
library(writexl)

input_file <- readxl::read_excel("C:/Users/imart/Documents/Varun/Chemicals, Dyes & Allied Products & Medical, Pharma, Surgical & Healthcare.xlsx",sheet = 1)
input_file <- input_file[,c(2,3)]
input_file<-unique(input_file)
my_file1 <- input_file
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

#write.csv(ALT_NAMES,"ALT_NAMES.csv", row.names = F, quote = T)
#ALT_NAMES <- read.csv("file:///C:/Users/imart/Documents/Varun/ALT_NAMES.csv")
rm(a,df,imblrQuery,input_file,jdbcCOnMESHR,jdbcDriver,i,parts,query,select)
gc()
ALT_NAMES <- df[,c(1,2,3)]  

CHEMICAL_NAMES <- read.csv("C:/Users/imart/Documents/Varun/ChemicalNames.csv")
CHEMICAL_NAMES$Synonyms2 <- gsub("-","",CHEMICAL_NAMES$Synonyms, ignore.case = T )
CHEMICAL_NAMES$Synonyms2 <- gsub(" ","",CHEMICAL_NAMES$Synonyms2, ignore.case = T )
CHEMICAL_NAMES$Synonyms2 <- trimws(CHEMICAL_NAMES$Synonyms2)

#grep("\\b;Mononitro;\\b",a, ignore.case = T)


MCATS <- input_file
MCATS$MCAT_NAME2 <- MCATS$MCAT_NAME
MCATS$MCAT_NAME <- gsub(" ","",MCATS$MCAT_NAME, ignore.case = T)
MCATS$MCAT_NAME <- trimws(MCATS$MCAT_NAME)

#Finding keywords for MCATS

CHEMICAL_NAMES$Check1 <- ""
CHEMICAL_NAMES$Check2 <- ""

#which(MCATS$MCAT_NAME2=="Ammonium Bisulfite")

i=1
for (i in 1: nrow(MCATS)) {
  CHEMICAL_NAMES$Check2 <- ifelse(grepl(paste0("\\b;",MCATS$MCAT_NAME[i],"\\;b"), CHEMICAL_NAMES$Synonyms2, ignore.case = T ),  paste(CHEMICAL_NAMES$Check1,MCATS$MCAT_NAME2[i], sep = ":" ) ,"")
  
  j=1
  for (j in 1 : nrow(CHEMICAL_NAMES) ) {
    ifelse( CHEMICAL_NAMES$Check2[j]!="", CHEMICAL_NAMES$Check1[j] <- CHEMICAL_NAMES$Check2[j],CHEMICAL_NAMES$Check1[j] <- CHEMICAL_NAMES$Check1[j] )  
    
  }
  
  CHEMICAL_NAMES$Check2 <- ""
  }

colnames(CHEMICAL_NAMES)[colnames(CHEMICAL_NAMES)=="Check1"] <- "FROM_MCATS"


##Finding keywords for ALT MCATS

ALT_NAMES$GLCAT_ALTMCAT_NAME2 <- ALT_NAMES$GLCAT_ALTMCAT_NAME
ALT_NAMES$GLCAT_ALTMCAT_NAME <- gsub(" ","",ALT_NAMES$GLCAT_ALTMCAT_NAME, ignore.case = T)
ALT_NAMES$GLCAT_ALTMCAT_NAME <- trimws(ALT_NAMES$GLCAT_ALTMCAT_NAME)

CHEMICAL_NAMES$Check1 <- ""
CHEMICAL_NAMES$Check2 <- ""

#which(ALT_NAMES$GLCAT_ALTMCAT_NAME=="Ammoniumbisulfite")


i=1
for (i in 1: nrow(ALT_NAMES)) {
  CHEMICAL_NAMES$Check2 <- ifelse(grepl(paste0("\\b",ALT_NAMES$GLCAT_ALTMCAT_NAME[i],"\\b"), CHEMICAL_NAMES$Synonyms2, ignore.case = T ),  paste(CHEMICAL_NAMES$Check1,ALT_NAMES$GLCAT_MCAT_NAME[i],"_",ALT_NAMES$GLCAT_ALTMCAT_NAME2[i] ,sep = ":" ) ,"")
  
  j=1
  for (j in 1 : nrow(CHEMICAL_NAMES) ) {
    ifelse( CHEMICAL_NAMES$Check2[j]!="", CHEMICAL_NAMES$Check1[j] <- CHEMICAL_NAMES$Check2[j],CHEMICAL_NAMES$Check1[j] <- CHEMICAL_NAMES$Check1[j] )  
    
  }
  
  CHEMICAL_NAMES$Check2 <- ""
}


colnames(CHEMICAL_NAMES)[colnames(CHEMICAL_NAMES)=="Check1"] <- "FROM_ALT_MCATS"

CHEMICAL_NAMES$Check2 <- NULL

write.csv(CHEMICAL_NAMES, "Chemical_Names(Reuired_Data).csv", row.names = F, quote = T)
writexl::write_xlsx(CHEMICAL_NAMES,"Chemical_Names(Reuired_Data).xlsx")

save.image("Varun_26Apr.Rdata")

which(MCATS$MCAT_NAME=="PotassiumSilicate")

