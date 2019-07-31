library(readxl)
library(xlsx)
library(stringr)
library(reshape)
library(splitstackshape)
library(RJDBC)
library(writexl)


options("scipen"=100, "digits"=2)
options(scipen = 999)




df <- read.xlsx2("C:/Users/IMART/Documents/Busyin-Share.xlsx", sheetIndex = 1  )
df[is.na(df)] <- ""



####Filter data on the basis of Primary Mobile Number####


#Replace all characters in primary mobile
df1 <- df[,c("S.No","Company.Name","Name","Address.1","Address.2","City","Primary.Mobile","Secondary.Mobile","Primary.Email.ID","Secondary.Email.ID", "Landline","Group.Name","Existing.Mcats","Proposed.MCAT.NAME" ,"Identification","Website")]

df1$Primary.Mobile <- gsub("[a-z]","",df1$Primary.Mobile, ignore.case = T)
df1$Primary.Mobile <- gsub("\\(","",df1$Primary.Mobile, ignore.case = T)
df1$Primary.Mobile <- gsub("\\)","",df1$Primary.Mobile, ignore.case = T)
df1$Primary.Mobile <- str_trim(df1$Primary.Mobile)
df1$Primary.Mobile <- str_squish(df1$Primary.Mobile)

#remove rows that have nothing in primary_mobile
df1 <- df1[!df$Primary.Mobile=="",  ]


#Replace characters with comma

df1$Primary.Mobile <- gsub(";",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub(":",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub(">",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("<",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("\\.",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("\\/",",", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("\\+91","0", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("\\+","", df1$Primary.Mobile)
df1$Primary.Mobile <- gsub("0-","0", df1$Primary.Mobile)


df1$Primary.Mobile <- gsub( ",,,,,",",", df1$Primary.Mobile )
df1$Primary.Mobile <- gsub( ",,,,",",", df1$Primary.Mobile )
df1$Primary.Mobile <- gsub( ",,,",",", df1$Primary.Mobile )
df1$Primary.Mobile <- gsub( ",,",",", df1$Primary.Mobile )


#removing starting comma

df1$Primary.Mobile <- ifelse(grepl("^,",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 2,stop = nchar(df1$Primary.Mobile)), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)
df1$Primary.Mobile <- ifelse(grepl("^,",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 2,stop = nchar(df1$Primary.Mobile)), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)
df1$Primary.Mobile <- ifelse(grepl("^,",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 2,stop = nchar(df1$Primary.Mobile)), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)

#removing last comma
df1$Primary.Mobile <- ifelse(grepl(",$",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 1,stop = nchar(df1$Primary.Mobile)-1), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)
df1$Primary.Mobile <- ifelse(grepl(",$",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 1,stop = nchar(df1$Primary.Mobile)-1), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)
df1$Primary.Mobile <- ifelse(grepl(",$",df1$Primary.Mobile  )==T, substr(df1$Primary.Mobile,start = 1,stop = nchar(df1$Primary.Mobile)-1), df1$Primary.Mobile )
df1$Primary.Mobile <- trimws(df1$Primary.Mobile)


#remove rows that have lesst than 10 charcters
df1$length <- nchar(df1$Primary.Mobile )
df1$numbercount <- str_count(df1$Primary.Mobile,"[0-9]"  )
df1 <- df1[df1$Primary.Mobile!="", ]
df1 <- df1[!df1$length<10, ]

#remove rows having 10 characters but less numeric data than 10
df1 <- df1[ !(df1$length==10 & df1$numbercount<10), ]
df1 <- df1[ !(df1$length==10 & df1$Primary.Mobile<6000000000), ]

#Filtering Final Data from refined data
Final_Mobile_Data <- df1[ df1$length==10, ]
Final_Mobile_Data$length <- NULL
Final_Mobile_Data$numbercount <- NULL

#Filtering non-final data from refined data
rdf1 <- df1[ df1$length!=10, ]
rdf1$length <- NULL
rdf1$numbercount <- NULL

#remving spaced before and after comma
rdf1$Primary.Mobile <- trimws(rdf1$Primary.Mobile)
rdf1$Primary.Mobile <- gsub(", ",",", rdf1$Primary.Mobile, ignore.case = T )
rdf1$Primary.Mobile <- gsub(" ,",",", rdf1$Primary.Mobile, ignore.case = T )
#gsub("^[0-9]{5} ",paste0(str_extract("99922 54666","^[0-9]{5}")),"99922 54666"   )


#splitting primary column on the basis of comma
rdf1 <- cSplit(rdf1, splitCols = "Primary.Mobile" , sep = "," )

#Finding column index, start index and last index of splitted data
column_index <- colnames(rdf1)
col_start <- which(column_index=="Primary.Mobile_1")
col_last <-  col_start + (length(colnames(rdf1)[grepl("Primary.Mobile",colnames(rdf1), ignore.case = T  ) ] )-1) 
loop <- seq(col_start,col_last,1  )
rdf1 <- as.data.frame(rdf1)

#user defined function to remove fifth space from mobile number
rem_fifth_space <- function(x){
  fif_space <- gsub("^[0-9]{5} ",paste0(str_extract(x,"^[0-9]{5}")),x)
  return(fif_space)
}



#Finding valid mobile number

i=1

for (i in 1: length(loop)) {

  col_df <- rdf1[,c(1:col_start-1,as.numeric(loop[i]))]
  colnames(col_df)[colnames(col_df)==column_index[loop[i]]] <- "Primary.Mobile"
  col_df$Primary.Mobile <- as.character(col_df$Primary.Mobile)
  #col_df$Primary.Mobile <- gsub("^[0-9]{5} ",paste0(str_extract(col_df$Primary.Mobile,"^[0-9]{5}")),col_df$Primary.Mobile   )
  col_df$Primary.Mobile <- as.character(sapply(col_df$Primary.Mobile, rem_fifth_space))  
  col_df <- cSplit(col_df, splitCols = paste0("Primary.Mobile") , sep = " " )
  column_index2 <- colnames(col_df)
  col_start2 <- which(column_index2=="Primary.Mobile_1")
  col_last2 <-  col_start + (length(colnames(col_df)[grepl("Primary.Mobile",colnames(col_df), ignore.case = T  ) ] ) -1)
  loop2 <- seq(col_start2,col_last2,1  )
  col_df <- as.data.frame(col_df)
  
  
  j=1
  for (j in 1:length(loop2)) {
    
    col_df2 <- col_df[,c(1:col_start2-1, as.numeric(loop2[j]) )  ]
    colnames(col_df2)[colnames(col_df2)==column_index2[loop2[j]] ] <- "Primary.Mobile"
    col_df2$Primary.Mobile <- gsub("-","",col_df2$Primary.Mobile, ignore.case = T)
    col_df2$Primary.Mobile <- gsub("[a-z]","",col_df2$Primary.Mobile, ignore.case = T)
    col_df2 <- col_df2[!is.na(col_df2$Primary.Mobile), ]
    col_df2$len <- nchar(col_df2$Primary.Mobile)
    col_df2 <- col_df2[col_df2$len==10,  ]
    col_df2 <- col_df2[col_df2$Primary.Mobile>6000000000, ]
    col_df2$len <- NULL
    Final_Mobile_Data <- rbind(Final_Mobile_Data,col_df2)
    
  }
  

    }

rm(col_last,col_last2,col_start,col_start2,column_index,column_index2,i,j,loop,loop2,col_df,col_df2)

#Refining finalized data
Final_Mobile_Data$S.No <- as.numeric(as.character(Final_Mobile_Data$S.No) )
class(Final_Mobile_Data$S.No)

#Sort FInal data basis of serial number
Final_Mobile_Data <- Final_Mobile_Data[order(Final_Mobile_Data$S.No),  ]
MobileNumber <- Final_Mobile_Data[ !duplicated(Final_Mobile_Data$Primary.Mobile),c("S.No","Primary.Mobile")]

Final_Mobile_Data$Primary_Mobile_verified <- ""
Final_Mobile_Data$Primary_Mobile_verified[1] <- Final_Mobile_Data$Primary.Mobile[1]

k=2
for (k in 2:nrow(Final_Mobile_Data)) {
  Final_Mobile_Data$Primary_Mobile_verified[k] <- ifelse(Final_Mobile_Data$S.No[k]==Final_Mobile_Data$S.No[k-1],paste(Final_Mobile_Data$Primary_Mobile_verified[k-1],Final_Mobile_Data$Primary.Mobile[k],sep = ","  ), Final_Mobile_Data$Primary.Mobile[k]    )
    
}


Final_Mobile_Data <- Final_Mobile_Data[!rev(duplicated(rev(Final_Mobile_Data$S.No))),]
Final_Mobile_Data1 <- Final_Mobile_Data[,c("S.No","Primary_Mobile_verified" )]



####Filter data on the basis of Primary EMAILs####
e_df <- df[,c("S.No","Company.Name","Name","Address.1","Address.2","City","Primary.Mobile","Secondary.Mobile","Primary.Email.ID","Secondary.Email.ID", "Landline","Group.Name","Existing.Mcats","Proposed.MCAT.NAME" ,"Identification","Website")]

#removing leading, trailing, excessive and white spaces
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)

#remvong rows that do not have email ids
e_df <- e_df[!e_df$Primary.Email.ID=="",  ]

#removing rows that have less than 5 characters
e_df$len <- nchar(e_df$Primary.Email.ID  )
e_df <- e_df[ e_df$len>4, ]
e_df$len <- NULL

#removing rows that do not have "@" 
e_df <- e_df[grepl("@", e_df$Primary.Email.ID, ignore.case = T  ), ]


#replacing all expected symbols with comma
e_df$Primary.Email.ID <- gsub(";",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(":",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub("\\?",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub("#",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(", ",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(", ",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(",,,,,",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(",,,,",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(",,,",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- gsub(",,",",",e_df$Primary.Email.ID, ignore.case = T)
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)



#removing starting comma

e_df$Primary.Email.ID <- ifelse(grepl("^,",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 2,stop = nchar(e_df$Primary.Email.ID)), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)
e_df$Primary.Email.ID <- ifelse(grepl("^,",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 2,stop = nchar(e_df$Primary.Email.ID)), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)
e_df$Primary.Email.ID <- ifelse(grepl("^,",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 2,stop = nchar(e_df$Primary.Email.ID)), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)

#removing last comma
e_df$Primary.Email.ID <- ifelse(grepl(",$",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 1,stop = nchar(e_df$Primary.Email.ID)-1), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)
e_df$Primary.Email.ID <- ifelse(grepl(",$",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 1,stop = nchar(e_df$Primary.Email.ID)-1), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)
e_df$Primary.Email.ID <- ifelse(grepl(",$",e_df$Primary.Email.ID  )==T, substr(e_df$Primary.Email.ID,start = 1,stop = nchar(e_df$Primary.Email.ID)-1), e_df$Primary.Email.ID )
e_df$Primary.Email.ID <- trimws(e_df$Primary.Email.ID)


#finding out rows having multiple email ids
e_df$comma_count <- str_count(e_df$Primary.Email.ID,","  )


#Filtering Final emails data having only one email per rows
Final_Email_Data <- e_df[e_df$comma_count==0,  ]
Final_Email_Data$comma_count <- NULL


#Remaining data 
r_e_df <- e_df[e_df$comma_count!=0, ]
r_e_df$comma_count <- NULL




#splitting rows on the basis of comma

r_e_df <- cSplit(r_e_df, splitCols = "Primary.Email.ID" , sep = "," )


ecol_ind <- colnames(r_e_df)
ecol_start <- which(ecol_ind=="Primary.Email.ID_1")
ecol_last <-  ecol_start + (length(colnames(r_e_df)[grepl("Primary.Email.ID",colnames(r_e_df), ignore.case = T  ) ] )-1) 
eloop <- seq(ecol_start,ecol_last,1  )
r_e_df <- as.data.frame(r_e_df)

#Finding valid email id

x=1

for (x in 1: length(eloop)) {
  
  ecol_df <- r_e_df[,c(1:ecol_start-1,as.numeric(eloop[x]))]
  colnames(ecol_df)[colnames(ecol_df)==ecol_ind[eloop[x]]] <- "Primary.Email.ID"
  ecol_df$Primary.Email.ID <- as.character(ecol_df$Primary.Email.ID)
  
  #removing rows that have less than 5 characters
  ecol_df$len <- nchar(ecol_df$Primary.Email.ID)
  ecol_df <- ecol_df[ ecol_df$len>4, ]
  ecol_df$len <- NULL
  
  #removing rows that do not have "@" 
  ecol_df <- ecol_df[grepl("@", ecol_df$Primary.Email.ID, ignore.case = T  ), ]
  
  Final_Email_Data <- rbind(Final_Email_Data,ecol_df)
  
}


#Refining finalized  email data
Final_Email_Data$S.No <- as.numeric(as.character(Final_Email_Data$S.No))
class(Final_Email_Data$S.No)

#Sort Final email data basis of serial number
Final_Email_Data <- Final_Email_Data[order(Final_Email_Data$S.No), ]
Final_Email_Data$Primary.Email.ID <- trimws(Final_Email_Data$Primary.Email.ID)
Email <- Final_Email_Data[ !duplicated(Final_Email_Data$Primary.Email.ID),c("S.No","Primary.Email.ID")]
Final_Email_Data$Primary_Email_verified <- ""
Final_Email_Data$Primary_Email_verified[1] <- Final_Email_Data$Primary.Email.ID[1]


k=2
for (k in 2:nrow(Final_Email_Data)) {
  Final_Email_Data$Primary_Email_verified[k] <- ifelse(Final_Email_Data$S.No[k]==Final_Email_Data$S.No[k-1],paste(Final_Email_Data$Primary_Email_verified[k-1],Final_Email_Data$Primary.Email.ID[k],sep = ","  ), Final_Email_Data$Primary.Email.ID[k]    )
  
}



Final_Email_Data <- Final_Email_Data[!rev(duplicated(rev(Final_Email_Data$S.No))),]
Final_Email_Data1 <- Final_Email_Data[,c("S.No","Primary_Email_verified")]

rm(e_df,ecol_df,r_e_df,rdf1,ecol_ind,ecol_last,ecol_start,eloop,k,x)

Final_Mobile_Data <- merge(Final_Mobile_Data,Final_Email_Data1, by = "S.No", all.x = T)

Final_Email_Data <- merge(Final_Email_Data, Final_Mobile_Data1, by = "S.No", all.x = T)
Final_Email_Data <- Final_Email_Data[is.na(Final_Email_Data$Primary_Mobile_verified), ]


Final_Data <- rbind(Final_Mobile_Data, Final_Email_Data )
Final_Data[is.na(Final_Data)] <- ""

write.xlsx2(Final_Data,"Final.xlsx", row.names = F )

###########Checking Mobile and Email ids with Exisiting database###############



MobileNumber$Index <- seq(1,nrow(MobileNumber),1)
Email$Index <- seq(1,nrow(Email),1 )
my_file1 <- merge(MobileNumber, Email, by = "Index", all = T )
my_file1[is.na(my_file1)] <- ""
my_file1$Index <- NULL


colnames(my_file1)[colnames(my_file1)=="Primary.Mobile"] <- "MobileNumber"
colnames(my_file1)[colnames(my_file1)=="Primary.Email.ID"] <- "Email"
colnames(my_file1)[colnames(my_file1)=="S.No.x"] <- "S.No.M"
colnames(my_file1)[colnames(my_file1)=="S.No.y"] <- "S.No.E"

parts<-ceiling(length(my_file1$MobileNumber )/1000)

a <- split(my_file1, sort(rank(row.names(my_file1))%%parts))#Divide into equal parts


emp_df <- data.frame(
  
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
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

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
  emp_df<-rbind(emp_df,imblrQuery)
  
}
emp_df <- emp_df[,c("GLUSR_USR_ID","GLUSR_USR_PH_MOBILE","GLUSR_USR_PH_MOBILE_ALT","GLUSR_USR_EMAIL","GLUSR_USR_EMAIL_ALT","GLUSR_USR_CUSTTYPE_NAME","GLUSR_USR_CUSTTYPE_ID") ]
emp_df[is.na(emp_df)] <- ""

rm(a,imblrQuery,email,mob,parts,i,query)

###verifying data

#mob_and_alt <- emp_df$GLUSR_USR_PH_MOBILE
#mob_and_alt <- append(mob_and_alt, emp_df$GLUSR_USR_PH_MOBILE_ALT )
#mob_and_alt <- mob_and_alt[!is.na(mob_and_alt)]
#mob_and_alt <- unique(mob_and_alt)
#mob_and_alt <- as.data.frame(mob_and_alt )
#mob_and_alt$Match_M <- "Yes"

#email_and_alt <- emp_df$GLUSR_USR_EMAIL
#email_and_alt <- append(email_and_alt,emp_df$GLUSR_USR_EMAIL_ALT )
#email_and_alt <- email_and_alt[!is.na(email_and_alt)]
#email_and_alt <- unique(email_and_alt)
#email_and_alt <- as.data.frame(email_and_alt)
#email_and_alt$Match_E <- "Yes"

mobile_data1 <- emp_df[emp_df$GLUSR_USR_PH_MOBILE!="",]
mobile_data1 <- mobile_data1[,c("GLUSR_USR_ID","GLUSR_USR_PH_MOBILE","GLUSR_USR_CUSTTYPE_NAME","GLUSR_USR_CUSTTYPE_ID")]

mobile_data2 <- emp_df[emp_df$GLUSR_USR_PH_MOBILE_ALT!="",]
mobile_data2 <- mobile_data2[,c("GLUSR_USR_ID","GLUSR_USR_PH_MOBILE_ALT","GLUSR_USR_CUSTTYPE_NAME","GLUSR_USR_CUSTTYPE_ID")]
colnames(mobile_data2)[colnames(mobile_data2)=="GLUSR_USR_PH_MOBILE_ALT"] <- "GLUSR_USR_PH_MOBILE"

mobile_data <- rbind(mobile_data1,mobile_data2)
colnames(mobile_data) <- paste0(colnames(mobile_data),"_mobile")

rm(mobile_data1,mobile_data2)
mobile_data <- mobile_data[!duplicated(mobile_data$GLUSR_USR_PH_MOBILE_mobile), ]
mobile_data <- mobile_data[!is.na(mobile_data$GLUSR_USR_PH_MOBILE_mobile), ]
mobile_data <- merge(mobile_data,MobileNumber, by.x = "GLUSR_USR_PH_MOBILE_mobile", by.y = "Primary.Mobile", all.x = T )
mobile_data$Index <- NULL



email_data1 <- emp_df[emp_df$GLUSR_USR_EMAIL!="",  ]
email_data1 <- email_data1[,c("GLUSR_USR_ID","GLUSR_USR_EMAIL","GLUSR_USR_CUSTTYPE_NAME","GLUSR_USR_CUSTTYPE_ID")]

email_data2 <- emp_df[emp_df$GLUSR_USR_EMAIL_ALT!="",  ]
email_data2 <- email_data2[,c("GLUSR_USR_ID","GLUSR_USR_EMAIL_ALT","GLUSR_USR_CUSTTYPE_NAME","GLUSR_USR_CUSTTYPE_ID")]
colnames(email_data2)[colnames(email_data2)=="GLUSR_USR_EMAIL_ALT"] <- "GLUSR_USR_EMAIL"

email_data <- rbind(email_data1,email_data2)
colnames(email_data) <- paste0(colnames(email_data),"_email")

rm(email_data1,email_data2)
email_data <- email_data[!duplicated(email_data$GLUSR_USR_EMAIL_email), ]
email_data <- email_data[!is.na(email_data$GLUSR_USR_EMAIL_email), ]
email_data <- merge(email_data,Email, by.x = "GLUSR_USR_EMAIL_email", by.y = "Primary.Email.ID", all.x = T )
email_data$Index <- NULL




Matched_data_Mobile <- merge(Final_Data,mobile_data, by = "S.No" , all.x = T  )

Final_Matched_Data <- merge(Matched_data_Mobile,email_data, by = "S.No", all.x = T)
Final_Matched_Data[is.na(Final_Matched_Data)] <- ""


##############Existing Product Mapping###############


glusr <- unique(Final_Matched_Data$GLUSR_USR_ID_mobile) 
glusr <- append(glusr,Final_Matched_Data$GLUSR_USR_ID_email)
glusr <- glusr[glusr!=""]
glusr <- glusr[ !is.na(glusr)]
glusr <- unique(glusr)
glusr <- as.data.frame(glusr)
glusr$glusr <- as.numeric(as.character(glusr$glusr))


parts2<-ceiling(length(glusr$glusr )/1000)

a <- split(glusr, sort(rank(row.names(glusr))%%parts2))#Divide into equal parts


mcats_result <- data.frame(
  FK_GLCAT_MCAT_ID=numeric(),
  GLCAT_MCAT_NAME=character(),
  PC_ITEM_GLUSR_USR_ID=numeric(),
  PC_ITEM_ID=numeric()
) 


if(exists("jdbcCOnMESHR"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  
  a[[i]]$glusr[-length(a[[i]]$glusr)] <- paste0(a[[i]]$glusr[-length(a[[i]]$glusr)],",")
  
  
  glusr_ids <- a[[i]]$glusr
  glusr_ids <- paste(glusr_ids, collapse = "") 
  
  
  
  
  query <- paste0("select distinct FK_GLCAT_MCAT_ID,GLCAT_MCAT_NAME, pc_item_glusr_usr_id,PC_ITEM_ID
from PC_ITEM_to_glcat_mcat, pc_item, GLCAT_MCAT
where PC_ITEM_ID = FK_PC_ITEM_ID(+)
and FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID
and pc_item_glusr_usr_id IN (",glusr_ids,")
order by pc_item_glusr_usr_id")
  
  
  
  
  meshQuery <- dbGetQuery(jdbcCOnMESHR,query)
  mcats_result<-rbind(mcats_result,meshQuery)
  
}

rm(parts2,a,i,glusr_ids,query,meshQuery)


str(mcats_result)

mcats_result <- mcats_result[order(mcats_result$PC_ITEM_GLUSR_USR_ID), ]

mcats_result$mcats_concat <- ""
mcats_result$mcatids_concat <- ""
mcats_result$pc_ids_concat <- ""

mcats_result$mcats_concat[1] <- mcats_result$GLCAT_MCAT_NAME[1]
mcats_result$mcatids_concat[1] <- mcats_result$FK_GLCAT_MCAT_ID[1]
mcats_result$pc_ids_concat[1] <- mcats_result$PC_ITEM_ID[1]



h=2
for (h in 2:nrow(mcats_result)) {
  
mcats_result$mcats_concat[h] <- ifelse( mcats_result$PC_ITEM_GLUSR_USR_ID[h]==mcats_result$PC_ITEM_GLUSR_USR_ID[h-1],paste(mcats_result$mcats_concat[h-1],mcats_result$GLCAT_MCAT_NAME[h], sep = ","), mcats_result$GLCAT_MCAT_NAME[h] )  
}


h=2
for (h in 2:nrow(mcats_result)) {
  
  mcats_result$mcatids_concat[h] <- ifelse( mcats_result$PC_ITEM_GLUSR_USR_ID[h]==mcats_result$PC_ITEM_GLUSR_USR_ID[h-1],paste(mcats_result$mcatids_concat[h-1],mcats_result$FK_GLCAT_MCAT_ID[h], sep = ","), mcats_result$FK_GLCAT_MCAT_ID[h] )  
}


h=2
for (h in 2:nrow(mcats_result)) {
  
  mcats_result$pc_ids_concat[h] <- ifelse( mcats_result$PC_ITEM_GLUSR_USR_ID[h]==mcats_result$PC_ITEM_GLUSR_USR_ID[h-1],paste(mcats_result$pc_ids_concat[h-1],mcats_result$PC_ITEM_ID[h], sep = ","), mcats_result$PC_ITEM_ID[h] )  
}

Final_mcat_results <- mcats_result[!rev(duplicated(rev(mcats_result$PC_ITEM_GLUSR_USR_ID))),]





#removing duplicate words:
rem_dup.one <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split=",",fixed=F,perl=T)))),collapse = ",")
}


Final_mcat_results$mcats_concat <- as.character(lapply(Final_mcat_results$mcats_concat, rem_dup.one  ))
Final_mcat_results$mcatids_concat <- as.character(lapply(Final_mcat_results$mcatids_concat, rem_dup.one))
Final_mcat_results$pc_ids_concat <- as.character(lapply(Final_mcat_results$pc_ids_concat, rem_dup.one))
Final_mcat_results <- Final_mcat_results[,c("PC_ITEM_GLUSR_USR_ID","mcats_concat","mcatids_concat","pc_ids_concat")]
rm(h,rem_dup.one,jdbcDriver,jdbcConIMBLR,jdbcCOnMESHR)
gc()


colnames(Final_mcat_results) <- paste0(colnames(Final_mcat_results),"_Mob")


MCats_merged_Final_Data <- merge(Final_Matched_Data, Final_mcat_results, by.x = "GLUSR_USR_ID_mobile", by.y = "PC_ITEM_GLUSR_USR_ID_Mob", all.x = T )


colnames(Final_mcat_results) <- gsub("_Mob$","_EMail", colnames(Final_mcat_results) )


Final_Result <- merge(MCats_merged_Final_Data,Final_mcat_results,by.x = "GLUSR_USR_ID_email", by.y = "PC_ITEM_GLUSR_USR_ID_EMail", all.x = T )
Final_Result[is.na(Final_Result)] <- ""
Final_Result <- Final_Result[,c(3:19,2,21:23,27:29,20,1,24:26,30:32)]

write_xlsx(Final_Result,"Final_Required_Data.xlsx")


