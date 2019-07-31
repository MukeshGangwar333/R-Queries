#write.csv(BL_DATA1,"c:/Users/imart/Desktop/parul/bl_data.csv",row.names = F,quote = F)
#Data_new1 <- readxl::read_excel("C:/Users/imart/Downloads/brand model bl data (1).xlsx",sheet = 5)
Data_new1 <- read_excel("C:/Users/imart/Downloads/sheet_5.xlsx", sheet=1)
Data_new1$Complete_Name <- as.character(Data_new1$Complete_Name)
class(Data_new1$Complete_Name)


KW_DATA <- read.csv("C:/Users/imart/Downloads/vlook_up_sid_names.csv")
#KW_DATA$Keywords <- gsub("[0-9] hp", "", tolower(KW_DATA$Keywords))
KW_DATA1 <- KW_DATA[!duplicated(KW_DATA$Keywords),]

KW_DATA$combined <- as.character(KW_DATA$combined)

KW_DATA1$Keywords <- trimws(KW_DATA1$Keywords)
Data_new1$Complete_Name <- trimws(Data_new1$Complete_Name)

KW_DATA1$Keywords <- toupper(KW_DATA1$Keywords)
Data_new1$Complete_Name <- toupper(Data_new1$Complete_Name)




fina_data <- merge(Data_new1,KW_DATA1,by.x = "Complete_Name",by.y = "Keywords", all.x = T)
fina_data <- fina_data[,c("ETO_OFR_TITLE","Complete_Name","SELLER_GLUSR","Original") ]

write.csv(fina_data,"Final Data.csv", row.names = F)
write.csv(KW_DATA1,"Keywords.csv", row.names = F)


######old query#######
New_kwList <- as.vector(trimws(unique(KW_DATA$combined)))
New_kwList <- New_kwList[-length(New_kwList)]
Data_new1$New_Match <- ""
i=1
library(qdapRegex)

#New_kwList[i] <- KW_DATA$combined[]
for (i in 1:length(New_kwList)) {
  Indexes <- which(grepl(tolower(New_kwList[i]),tolower(Data_new1$Complete_Name))==T)
  Data_new1$New_Match[Indexes] <- as.character(New_kwList[i])
}


