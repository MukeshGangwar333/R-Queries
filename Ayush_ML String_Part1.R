options(java.parameters="-Xmx10000m")
#memory.limit()

library(RJDBC)
library(reshape2)
library(xlsx)
library(readxl)

df <- read_excel("file:///C:/Users/imart/Documents/5 SUBCATS Product Classification/5Subcat.xlsx", sheet = 1)
colnames(df)
df <- df[,c(1,2,12,13,17,19)]
colnames(df)[colnames(df)=="PRIME_MCAT_ID...1"] <- "PRIME_MCAT_ID"
df[is.na(df)] <- ""
df$result <- paste(df$PRIME_MCAT_NAME, df$PC_ITEM_DESC_SMALL, df$Label1, sep = " " )
df1 <- df[,c(1,2,3,7) ]




df1$result <- gsub("\\s+ NA","",df1$result)
df1$result <- gsub("NA NA","",df1$result)
df1$result <- gsub("NA ","",df1$result)
df1$result <- gsub(" NA","",df1$result)
df1$result <- gsub("</p p>","",df1$result)
df1$result <- gsub("<p p>","",df1$result)
df1$result <- gsub("</table>","",df1$result)
df1$result <- gsub("<table>","",df1$result)
df1$result <- gsub("</td>","",df1$result)
df1$result <- gsub("</tr>","",df1$result)
df1$result <- gsub("<tr>","",df1$result)
df1$result <- gsub("<td>","",df1$result)
df1$result <- gsub("</br>","",df1$result)
df1$result <- gsub("<br/>","",df1$result)
df1$result <- gsub("<br />","",df1$result)
df1$result <- gsub("<br>","",df1$result)
df1$result <- gsub("<b>","",df1$result)
df1$result <- gsub("</b>","",df1$result)


df1$result <- gsub(","," ",df1$result)
df1$result <- gsub("</li li>"," ",df1$result)
df1$result <- gsub("</li>"," ",df1$result)
df1$result <- gsub("<li>"," ",df1$result)
df1$result <- gsub("<ol>"," ",df1$result)
df1$result <- gsub("</ul>"," ",df1$result)
df1$result <- gsub("<ul>"," ",df1$result)
df1$result <- gsub("<p>"," ",df1$result)
df1$result <- gsub("<b>"," ",df1$result)
df1$result <- gsub("<br />"," ",df1$result)
df1$result <- as.character(df1$result)
df1$result <- gsub("[[:punct:]]"," ",df1$result)
df1$result <- gsub("[0-9] \\w+ *", "",df1$result)
df1$result <- gsub("[0-9]\\w+ *", "",df1$result)
df1$result <- gsub("[0-9]", "",df1$result)
df1$result <- gsub("\\s+"," ",str_trim(df1$result))




df1$result <- tolower(df1$result)
df1$result <- trimws(df1$result)


mcat_index <- unique(df1$PRIME_MCAT_NAME)

df1 <- df1 %>% group_by(PRIME_MCAT_NAME) %>% mutate(mcat_count = length(PRIME_MCAT_NAME))

df1 <- df1[order(df1$mcat_count),]

whole_file <- df1

write.csv(whole_file,"Full_file.csv", row.names = F)

#fr <- as.data.frame(table(df1$mcat_count))

df1 <- df1[df1$mcat_count>50,]

df_less_than_1000 <- df1[df1$mcat_count<=1000,]
df_more_than_1000 <- df1[df1$mcat_count>1000,]


final_data <- df_less_than_1000

mcat_index <- unique(df_more_than_1000$PRIME_MCAT_NAME)


i=1

for (i in 1:length(mcat_index)) {
 
  subdf <- df_more_than_1000[df_more_than_1000$PRIME_MCAT_NAME %in% mcat_index[i],  ]
  subdf1000 <- subdf[1:1000,]
  final_data <- rbind(final_data,subdf1000)
   
}


MCAT_fre_check <- as.data.frame(table(final_data$PRIME_MCAT_NAME))


write.csv(final_data,"C:/Users/imart/Documents/Ayush_ML String/Data_Part1(All).csv", row.names = F)

save.image("Part1.Rdata")
