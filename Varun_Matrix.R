library(data.table)
library(dplyr)
library(readxl)
library(writexl)


dat <- read_excel("c:/Users/imart/Documents/avg_cosine_total_47mcats.xlsx", sheet = 1)
matrix_df <- read_excel("c:/Users/imart/Documents/avg_cosine_total_47mcats.xlsx", sheet = 2)

mcats <- sort(unique(dat$MCAT))


i=1

for (i in 1:length(mcats)) {
  
  
  mini_data <- dat[ dat$MCAT %in% mcats[i], ]
  
  avg_data <- mini_data %>% group_by(Similar_sentence ) %>% summarise(Avg=  mean(Cosine_Distance))
  
  td <- dcast(melt(avg_data, id.vars = "Similar_sentence"), variable ~ Similar_sentence)
  
  colnames(td)[colnames(td)== "variable"] <- "MCAT"
  td$MCAT <- as.character(td$MCAT)
  
  td[1,1] <- mcats[i]
  
  matrix_df <- rbind(matrix_df,td , deparse.level = 1)
  
  
}

write.csv(matrix_df,"Final_Matrix.csv", row.names = F, quote = T)
write.csv(td,"excavator_matrix.csv", row.names = F, quote = T )


#colnames(matrix_df)
#colnames(td)
#matrix_df[1,-1] <- td[1,-1]



