library(readxl)

#Reading data
city_df <- read_excel("C:/Users/imart/Documents/Aditya/29 Apr/Final.xlsx", sheet = 1)
keywords_multiplier <- read_excel("C:/Users/imart/Documents/Aditya/29 Apr/Keyword-Multiplier-Macro.xls", sheet = 1)
keywords <- read_excel("C:/Users/imart/Documents/Aditya/29 Apr/Updated Final keyword (1).xlsx", sheet = 1)



#Resizing keywords data
keywords$Source <- NULL
city_index <- as.data.frame(table(city_df$MCAT_NAME ) )
keywords_refined <- keywords[keywords$CD_MCAT %in% city_index$Var1, ]
rm(keywords)
keywords_index <- as.data.frame(table(keywords_refined$CD_MCAT ))

#Adding additional keywords to keywords data
colnames(keywords_multiplier)[colnames(keywords_multiplier)=="Add Keyword(s) 1"] <- "KEYWORD"
keywords_final <- keywords_refined[0,]

i=1
for (i in 1:nrow(keywords_index)) {
  sub_df <- keywords_refined[keywords_refined$CD_MCAT== keywords_index$Var1[i] ,]
  kw <- keywords_multiplier
  kw$CD_MCAT <- keywords_index$Var1[i]
  kw_append <- rbind(kw,sub_df )
  keywords_final <- rbind(keywords_final,kw_append)
  }

rm(sub_df,kw,kw_append)

#Checking total rows count
keywords_index_new <- as.data.frame(table(keywords_final$CD_MCAT ))
rows_index <- merge(city_index,keywords_index_new, by.x = "Var1", by.y = "Var1", all = T )
summary(rows_index)
rows_index$multi <- ifelse(!is.na(rows_index$Freq.y), rows_index$Freq.x * rows_index$Freq.y , rows_index$Freq.x)
sum(rows_index$multi)

rm(keywords_index_new, city_index, keywords_index, keywords_multiplier )
#merging city and keyword dataframes
city_df_merge <- merge(city_df,keywords_final, by.x = "MCAT_NAME", by.y = "CD_MCAT", all = T )

FINAL_DATA <- city_df_merge
rm(city_df_merge)


library(writexl)

write_xlsx(FINAL_DATA,"Final_Data.xlsx")


options(scipen = 999)


x=1
y=1000000
z=1

for (x in 1: ceiling(nrow(FINAL_DATA)/1000000)) {
  
  write_xlsx(FINAL_DATA[z:y,],paste0("Sheet",x,".xlsx"))

  z=y+1
  y=y+1000000
  
  gc()    

}




