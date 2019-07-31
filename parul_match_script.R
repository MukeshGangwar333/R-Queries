DATA_EXCV2 <- readxl::read_excel("c:/Users/imart/Desktop/parul/DBA 10-16th Feb.xlsx",sheet = 1)

library(stringi)
library(stringr)
DATA_EXCV$Original <- DATA_EXCV2$Keywords
DATA_EXCV$Keywords <- gsub("Motor Grader","",DATA_EXCV$Keywords)
DATA_EXCV$Keywords <- gsub("Skid Steer Loader","",DATA_EXCV$Keywords)
DATA_EXCV$Keywords <- gsub("Road Roller","",DATA_EXCV$Keywords)

write.csv(DATA_EXCV,"c:/Users/imart/Desktop/parul/vlook_up_sid_names.csv",row.names = F,quote = F)

library(qdapRegex)

DATA_EXCV$Keywords <- rm_white(DATA_EXCV$Keywords)

DATA_EXCV$Keywords1 <- word(DATA_EXCV$Keywords,1,sep = " ")
DATA_EXCV$Keywords2 <- word(DATA_EXCV$Keywords,2,sep = " ")

DATA_EXCV$combined <- paste(DATA_EXCV$Keywords1,DATA_EXCV$Keywords2)

write.table(DATA_EXCV$combined,"c:/Users/imart/Desktop/parul/keywors_brand_model.txt",row.names = F,col.names = F,quote = F)

Keyword_corpus <- c(DATA_EXCV$Keywords1,DATA_EXCV$Keywords2)
write.table(Keyword_final$Keyword_corpus,"c:/Users/imart/Desktop/parul/keywors_brand_model_combined.txt",row.names = F,col.names = F,quote = F)
Keyword_final <- data.frame(Keyword_corpus)


SID_DATA1 <- readxl::read_excel("C:/Users/imart/Desktop/parul/earthmoving products.xlsx",sheet = 1)
SID_DATA2 <- readxl::read_excel("C:/Users/imart/Desktop/parul/earthmoving products2.xlsx",sheet = 1)


final_data <- rbind(SID_DATA1,SID_DATA2)

rm(SID_DATA2)

final_data1 <- final_data[!duplicated(final_data$PC_ITEM_ID),]


KW_list <- as.vector(DATA_EXCV$Keywords)

rm(Indexes)

final_data1$MATCH_IN_NAME <- ""
final_data1$MATCH_IN_DESC <- ""
Index_a <- as.vector("1")
for (i in 1:length(KW_list)) {
  Indexes <- which(grepl(tolower(KW_list[i]),tolower(final_data1$PC_ITEM_NAME))==T)
  final_data1$MATCH_IN_NAME[Indexes] <- KW_list[i]
}

for (i in 1:length(KW_list)) {
  Indexes <- which(grepl(tolower(KW_list[i]),tolower(final_data1$PC_ITEM_DESC_SMALL))==T)
  final_data1$MATCH_IN_DESC[Indexes] <- KW_list[i]
}

library(xlsx)

write.csv(Final_matched_data1,"c:/Users/imart/Desktop/parul/final_matched_data.csv",row.names = F)


Final_matched_data$PC_ITEM_DESC_SMALL <- gsub(",","'",Final_matched_data$PC_ITEM_DESC_SMALL)

Final_matched_data <- final_data1[,c(7,23,24)]

Final_matched_data1 <- Final_matched_data[Final_matched_data$MATCH_IN_NAME!="" | Final_matched_data$MATCH_IN_DESC!="",]

#which(grepl(tolower(KW_list[i]),tolower(final_data1$PC_ITEM_NAME))==T)


ISQ_DATA <- readxl::read_excel("C:/Users/imart/Downloads/ISQ data.xlsx",sheet = 3)
ISQ_DATA <- ISQ_DATA[,c("PC_ITEM_ID","brand_model_isq")]
ISQ_DATA$match <- ""

for (i in 1:length(KW_list)) {
  Indexes <- which(grepl(tolower(KW_list[i]),tolower(ISQ_DATA$brand_model_isq))==T)
  ISQ_DATA$match[Indexes] <- KW_list[i]
}

sum(ISQ_DATA$match != "")

ISQ_DATA_1 <- ISQ_DATA[ISQ_DATA$match!="",]

write.csv(ISQ_DATA_1,"c:/Users/imart/Desktop/parul/isq_data_match.csv",row.names = F,quote = F)

BL_DATA <- readxl::read_excel("c:/Users/imart/Downloads/BL Data complete Excavator (2).xlsx",sheet = 1)

BL_DATA <- BL_DATA[,c(1,4)]

BL_DATA <- BL_DATA[!duplicated(BL_DATA$ETO_OFR_DISPLAY_ID),]

BL_DATA$MATCH <- ""

for (i in 1:length(KW_list)) {
  Indexes <- which(grepl(tolower(KW_list[i]),tolower(BL_DATA$ETO_OFR_TITLE))==T)
  BL_DATA$MATCH[Indexes] <- KW_list[i]
}

BL_DATA1 <- BL_DATA[BL_DATA$MATCH!="",]
write.csv(BL_DATA1,"c:/Users/imart/Desktop/parul/bl_data.csv",row.names = F,quote = F)


