Files <- list.files(path = "C:/Users/imart/Desktop/Flooring/kfold/final/",pattern = ".csv")
df1 <- read.csv("C:/Users/imart/Desktop/Flooring/kfold/final/K1.csv")

for (i in 2:10) 
{
  Location_c <- paste0("C:/Users/imart/Desktop/Flooring/kfold/final/K",i,".csv")
  df <- read.csv(Location_c)
  df1 <- rbind(df1,df)
}


vlookupfile1 <- read.csv("file:///C:/Users/imart/Desktop/Flooring/kfold/vlookup_kfold.csv")
vlookupfile <- vlookupfile1[,c("PMCAT2","PRIME_MCAT_NAME","PC_ITEM_ID","Fasttext_test")]


new_df <- merge(df1,vlookupfile,by.x= "test_data",by.y = "Fasttext_test", all.x = T)


newdf2 <- new_df[!duplicated(new_df$PC_ITEM_ID),]
