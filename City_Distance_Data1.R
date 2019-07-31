options(java.parameters="Xmx8000m")
library(RJDBC)
library(readxl)
library(xlsx)


jdbcdriver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",classPath = "C:/Users/imart/Documents/ojdbc6.jar")
imblrcon <- RJDBC::dbConnect(jdbcdriver,"jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")

city_ids <- read_xlsx("C:/Users/imart/Documents/City_Distance/City_ID_Name_Data from Nitin.xlsx",sheet = 1)

parts <- ceiling(length(city_ids$`City ID`)/1000)

a <- split(city_ids, factor(sort(rank(row.names(city_ids))%%parts)))#Divide into equal parts

df <- data.frame(
  FROM_CITY_ID= numeric(),
  TO_CITY_ID= numeric(),
  DISTANCE_VAL=numeric(),
  GL_CITY_NAME= character()
)



if(exists("imblrcon"))
  
  i <- 1
#Query for extracting data for each loop

for(i in 1:length(a))
{
  
  a[[i]]$`City ID`[-length(a[[i]]$`City ID`)]<-paste0(a[[i]]$`City ID`[-length(a[[i]]$`City ID`)],',')# to assign comma at end of every value
  select<-a[[i]]$`City ID`#Extracting Id
  select<-paste(a[[i]]$`City ID`,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  query<-paste0("select ETO_AST_BUY_CITY_DEST.FROM_CITY_ID,ETO_AST_BUY_CITY_DEST.TO_CITY_ID,
ETO_AST_BUY_CITY_DEST.DISTANCE_VAL, Gl_CITY.GL_CITY_NAME, Gl_City.GL_CITY_CLASSIFICATION 
                from Gl_City,ETO_AST_BUY_CITY_DEST 
                where ETO_AST_BUY_CITY_DEST.FROM_CITY_ID = Gl_City.Gl_City_ID
                and Gl_City_ID in(",select,")")
  
  
  queryresult <- dbGetQuery(imblrcon,query)
  df<-rbind(df,queryresult)
  
}

df <- queryresult

colnames(df)[colnames(df)=="GL_CITY_NAME"] <- "FROM_CITY_NAME"
colnames(df)[colnames(df)=="GL_CITY_CLASSIFICATION"] <- "CITY_TIER" 

city_name_ids <- read_excel("C:/Users/imart/Documents/City_Distance/City_ID_Name_Data from Nitin.xlsx",sheet = 1)  #Total Data

df1 <- merge(df,city_name_ids,by.x = "TO_CITY_ID",by.y = "City ID",all.x = T)

colnames(df1)[colnames(df1)=="City Name"] <- "TO_CITY_NAME" 





#km <- kmeans(df1,centers = 5)

#table(km$centers)
#table(km$cluster)






save.image("City_Distance.Rdata")

getwd()
write.csv(queryresult,"city_distance_data.csv",row.names = F)
