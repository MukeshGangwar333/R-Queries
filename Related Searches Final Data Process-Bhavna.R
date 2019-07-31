library("psych")
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")
library("reshape2")

df <- read.csv("C:/Users/imart/Desktop/PUNEET Final - Related Search data/Search_Terms_with_Mcatid_draft.csv")
df <- df[-1,]

colnames(df)[1] <- "Search_Term"
colnames(df)[2] <- "MCAT_ID"
colnames(df)[3] <- "Search_Freq"

df$Search_Term <- as.character(df$Search_Term)
df$MCAT_ID <- as.numeric(df$MCAT_ID)
df$Search_Freq <- as.numeric(df$Search_Freq)
df$Search_Term <- tolower(df$Search_Term)
df$Search_Term2 <- df$Search_Term
df <- df[,c(1,4,2,3)]


spellcheck <- read_excel("C:/Users/imart/Desktop/PUNEET Final - Related Search data/Spell Suggest Data.xlsx",sheet = 1) 
spellcheck <- spellcheck[!is.na(spellcheck$origsearchstring),]
spellcheck$origsearchstring <- tolower(spellcheck$origsearchstring)
spellcheck$origsearchstring <- rm_white(spellcheck$origsearchstring)  

j=1
for (j in 1:length(spellcheck$origsearchstring)) {
  df$Search_Term2 <-gsub(spellcheck$origsearchstring[j],spellcheck$spellsuggest[j],df$Search_Term2) 
}

#Creating a function to remove biz type

Funrem_stop <- function(x)
{
  Stopwords <- c("manufacturers","manufacturer","wholesaler","wholesalers","Retailer","Retailers","exporter","exporters","retailer","retailers","dealer","dealers","indiamart")
  x <- removeWords(x,Stopwords)
  return(x)
}


#stopwords <- read_xlsx("C:/Users/imart/Desktop/PUNEET Final - Related Search data/City Names Indian.xlsx", sheet = 1)     #Your stop words file

stopwords = readLines('C:/Users/imart/Desktop/PUNEET Final - Related Search data/city_names.txt')

# Function to remove city stopwords
removeWords1 <- function(str, stopwords)
{
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}


df$Search_term_new <- lapply(df$Search_Term2,Funrem_stop)
df$Search_term_new <- as.character(df$Search_term_new)
df$Search_term_new <- rm_white(df$Search_term_new)

for(i in 1:nrow(df))
{
  df$Search_term_new[i]<-removeWords1(df$Search_term_new[i], stopwords)
  
}
df$Search_term_new2 <- as.character(df$Search_term_new)
df$Search_term_new2 <- rm_white(df$Search_term_new2)
#selecting only words available before in and from
df$Search_term_new3 <- sapply(strsplit(df$Search_term_new2," in$"),"[",1)
df$Search_term_new_final <- sapply(strsplit(df$Search_term_new3," from "),"[",1)
df$Search_term_new_final <- as.character(df$Search_term_new_final)

#removing extra spaces:
df$Search_term_new_final <- rm_white(df$Search_term_new_final)


#Creating pivot
result <- df %>% dplyr::group_by(Search_term_new_final) %>% summarise(Sum_searchFreq = sum(Search_Freq,na.rm = T))

write.csv(result,"Result.csv",row.names = F,quote = F)
