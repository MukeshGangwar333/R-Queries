options(java.parameters = "-Xmx15000m")

df <- data.table::fread("/home/imart/Desktop/Ashutosh/processed_data_8.5cr(with3col).csv")
df <- df[,-1]
df <- df[order(df$MCAT_ID), ]
df$Result <- tolower(df$Result)
colnames(df)
df <- df[order(df$MCAT_ID),]


#df$Result <- gsub("[0-9] \\w+ *", "",df$Result)
#df$Result <- gsub("[0-9]\\w+ *", "",df$Result)
#df$Result <- gsub("\\s+"," ",str_trim(df$Result))


df$Result <- gsub( "<.*?>"," ", df$Result)
df <- data.table::fread("/home/imart/Desktop/Ashutosh/processed_data_8.5cr(with3col)_clean3.csv")
df$Result <- gsub("[[:punct:]]"," ",df$Result)
df <- data.table::fread("/home/imart/Desktop/Ashutosh/processed_data_8.5cr(with3col)_clean4.csv")
df$Result <- gsub("[0-9]", "",df$Result)
df$Result <- as.character(df$Result)
df$Result <- tolower(df$Result)
df$Result <- trimws(df$Result)
write.csv(df,"processed_data_8.5cr(with3col)_clean5(last).csv", row.names=F)


###################Medha Work ##################

fake_data <- df[grepl("\\b1st copy\\b | \\bFirst copy\\b | \\bClone\\b | \\bDuplicate\\b | \\bReplica\\b | \\bBranded Copy\\b | \\bAA Copy\\b| \\bA copy\\b | \\b7A\\b | \\bFake\\b", df$Result , ignore.case = T ) ,]
write.csv(fake_data,"Fake_data_from_whole_data.csv", row.names = F)
fake_keywords <- read_excel("/home/imart/Desktop/Medha/fake.xlsx", sheet = 1)
library(dplyr)
fake_data <-  fake_data %>% group_by(MCAT_ID) %>% mutate(word_count = length(PC_ITEM_ID ))


library(stringr)
fake_data$replica <- str_count(fake_data$Result,"replica")
fake_data$F1st_copy <- str_count(fake_data$Result,"1st copy")
fake_data$first_copy <- str_count(fake_data$Result,"first copy")
fake_data$clone <- str_count(fake_data$Result,"clone")
fake_data$duplicate <- str_count(fake_data$Result,"duplicate")
fake_data$branded_copy <- str_count(fake_data$Result,"branded copy")
fake_data$aa_copy <- str_count(fake_data$Result,"aa copy")
fake_data$a_copy <- str_count(fake_data$Result,"a copy")
fake_data$S7_A <- str_count(fake_data$Result,"7a")
fake_data$fake <- str_count(fake_data$Result,"fake")

fake_data <-  fake_data %>% group_by(MCAT_ID) %>% mutate(word_count2 = sum( replica,F1st_copy,first_copy,clone,duplicate,branded_copy,aa_copy,a_copy,S7_A,fake))

fake_data$MCAT_ID <- as.character(fake_data$MCAT_ID)
write.csv(fake_data,"Finalfake_data_medha.csv", row.names = F)

save.image("Medha.Rdata")

load("C:/Users/imart/Documents/Medha/03 Apr/Fake Data/Medha.Rdata")
write.xlsx2(fake_data,"Final_Fake_Data_Medha.xlsx", row.names = F )



#######Ayush Big Data#####

cleand_data_three_col <- fread("/home/imart/Desktop/Ashutosh/processed_data_8.5cr(with3col)_clean5(last).csv")