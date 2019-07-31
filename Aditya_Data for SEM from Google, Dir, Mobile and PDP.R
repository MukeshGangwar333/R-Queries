library(data.table)
library(dplyr)


#PDP Data

pdp_df <- fread("C:/Users/imart/Documents/Aditya/PDP_Final.csv")
pdp_df <- pdp_df[,-1]
pdp_df <- pdp_df[,c(3,5,6)]
pdp_df <- pdp_df %>% group_by(CD_MCAT) %>% mutate(Mcat_Count= length(CD_MCAT) )
pdp_df <- pdp_df[ order(pdp_df$CD_MCAT, pdp_df$Pageviews , decreasing = T),]

mcat_index <- unique(pdp_df$CD_MCAT)


Final_Data <- pdp_df[0,]

i=1
for (i in 1:length(mcat_index)) {
  
sub_df <- pdp_df[pdp_df$CD_MCAT==mcat_index[i], ]  
top5 <- sub_df[1:5,]
top5 <- na.omit(top5) 
Final_Data <- rbind(Final_Data,top5)
 
}



write.csv(Final_Data, "PDP_DATA.csv", row.names = F, quote = T)

save.image("PDP.Rdata")


#####################################################################################################################

library(data.table)
library(dplyr)

dir_jan <- fread("C:/Users/imart/Documents/Aditya/Site Search Terms_Jan'19.csv")
colnames(dir_jan)[colnames(dir_jan)=="Search Term"] <- "Search_Term"
dir_jan <- dir_jan[,c(1,2)]
dir_jan <- dir_jan[ order(dir_jan$CD_MCAT, dir_jan$Search_Term ),]
dir_jan <- dir_jan[!dir_jan$CD_MCAT==-1 ,]
dir_jan <- dir_jan[!dir_jan$CD_MCAT==0 ,]
dir_jan <- dir_jan[-1,]


#dir_jan <- dir_jan %>% group_by(CD_MCAT,Search_Term) %>% mutate(search_count= length(Search_Term)  )

dir_jan2 <- dir_jan %>% group_by(CD_MCAT,Search_Term) %>% summarise(search_count= length(Search_Term)  )

dir_jan2 <- dir_jan2[order(dir_jan2$CD_MCAT, dir_jan2$search_count, decreasing = T), ]

Final_Data <- dir_jan2[0,]

mcat <- unique(dir_jan2$CD_MCAT)

i=1
for (i in 1:length(mcat)) {
  
  sub_data <- dir_jan2[dir_jan2$CD_MCAT==mcat[i], ]
  top5 <- sub_data[1:5,]
  top5 <- na.omit(top5)
  Final_Data <- rbind(Final_Data,top5)
  
}

write.csv(Final_Data,"Dir_Jan.csv", row.names = F, quote = T)

save.image("Dir_jan.Rdata")
#####################################################################################################################


library(readxl)

google_df <- read_excel("C:/Users/imart/Documents/Aditya/Googledata3.xlsx", sheet = 1)


mcat_list <- unique(google_df$MCAT_NAME)


Final_Data <- google_df[0,]

i=1

for (i in 1:length(mcat_list)) {
  
  sub_df <- google_df[google_df==mcat_list[i], ]
  top5 <- sub_df[1:5, ]  
  top5 <- na.omit(top5)
  Final_Data <- rbind(Final_Data,top5)
  
  
}

write.csv(Final_Data,"Google_Data.csv", row.names = F, quote = T)


save.image("Google_Data.Rdata")



