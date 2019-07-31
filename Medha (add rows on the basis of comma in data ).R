df <- read_excel("C:/Users/imart/Documents/Medha/29 Mar/UserGroup Data Final.xlsx", sheet = 1, progress = T)

df$concat <- paste(df$Keyword,df$usrpcatname,df$glusrid,df$imgrpid,df$`Concatenate String`,df$Status, sep = "&&")

df <- df[!duplicated(df$concat),]

df$comma_count <- str_count(df$imgrpid, "," ) 

df$concat <- NULL

data_zero <- df[df$comma_count==0,]
data_zero$imgrpid2 <- data_zero$imgrpid



final_data <- data_zero

##sum(df$comma_count)+nrow(df)

data_non_zero <- df[df$comma_count!=0,]


i=1
for (i in 1:nrow(data_non_zero)) {

  index <- data_non_zero[i,]
  
  indexxx <- index[rep(seq_len(nrow(index)), each=index$comma_count[1]+1 ), ]
  indexxx$imgrpid2 <- ""
  j=1
  for (j in 1:nrow(indexxx)) {
  
    indexxx$imgrpid2[j] <- word(indexxx$imgrpid[j],start = j, sep = "," )
    
  }
  
  final_data <- rbind(final_data,indexxx)
  
    
}

final_data$imgrpid <- NULL


final_data$conc <- paste(final_data$Keyword,final_data$usrpcatname,final_data$glusrid,final_data$`Concatenate String`,final_data$Status,final_data$Status,final_data$comma_count,final_data$imgrpid2)

final_data <- final_data[!duplicated(final_data$conc),]

final_data$conc <- NULL
final_data$comma_count <- NULL

write.csv(final_data,"Final_Data.csv", row.names = F)
