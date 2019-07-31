#reading text file having combined data of products and buy lead
df <- read.table("C:/Users/imart/Documents/Excavator/Super_Combined/Super_PMCAT/Super_model_training.txt", sep = "\n"  )

library(stringr)

df1 <- df

#formatting the data
df1$label <- word(df1$V1, start=1, sep = " " )
df1$label <- sub("__label__[a-zA-Z_]+ ","",df1$V1)
df1$label <- trimws(df1$label)

getwd()
df1$label <- gsub("__label__","", df1$label )
df1$label <- gsub("_"," ", df1$label  )
df1$label <- gsub("Wheel Loaders", "Wheel Loader", df1$label  )

df1$V1 <- gsub("__label__Wheel_Loaders", "__label__Wheel_Loader", df1$V1  )
unique(df1$label )

df2 <- df1[df1$label!="Bucket & Rock Breaker for Excavator and Backhoe Loader" & df1$label!="Used & Second Hand Backhoe Loader and Excavator",]

unique(df2$label )

#taking the name of super pmcats of the all the pmcats
pmcats <- as.data.frame(unique(df2$label))
write.csv(pmcats,"pmcats.csv", row.names = F)
pmcats <- read.csv("C:/Users/imart/Documents/Excavator/Super_Combined/pmcats to superpmcats.csv")

#mapping the name of super pmcats to the pmcat
df3 <- merge(df2,pmcats, by.x = "label", by.y = "pmcats", all.x = T)

df3$stri <- ""
df3$stri <- sub("__label__[a-zA-Z_]+ ","",df3$V1)

#sub("__label__[a-zA-Z_]+ ", "","__label__Earthmoving_Bucket excavator bucket unit model")

df3$stri <- gsub("label", "", df3$stri )

df3$super_pmcats <- as.character(df3$super_pmcats)
str(df3)

df3$super_label <- paste0("__label__", gsub(" ","_", df3$super_pmcats)," ", df3$stri  )
df3$super_label[1]

colnames(df3)[colnames(df3)=="label" ] <- "pmcats"
colnames(df3)[colnames(df3)=="V1" ] <- "pmcats_label"
colnames(df3)[colnames(df3)=="stri" ] <- "string"

table(df3$super_pmcats)



###label loading#####

df3_labelled <- df3[df3$super_pmcats=="Earthmoving Machinery",  ]

index <- df3[df3$super_pmcats== "Earthmoving Machinery Parts", ]
data_multi <- index[rep(seq_len(nrow(index)), each=5     ),   ]
df3_labelled <- rbind(df3_labelled,data_multi )

table(df3_labelled$super_pmcats)
#df3_labelled <- readxl::read_excel("C:/Users/imart/Documents/Excavator/Super_Combined/super_labelled.xlsx")
write.csv(df3_labelled,"C:/Users/imart/Documents/Excavator/Super_Combined/super_labelled.csv",row.names = F)
write.table(df3_labelled$super_label , "C:/Users/imart/Documents/Excavator/Super_Combined/super_train_labelled.txt", row.names = F, col.names = F,quote = F )

write.csv(df3, "C:/Users/imart/Documents/Excavator/Combined Files_New_Previous/PMCAT/Combined_pmcat_data.csv", row.names = F )
write.table(df3$pmcats_label, "C:/Users/imart/Documents/Excavator/Combined Files_New_Previous/PMCAT/pmcat_training.txt", row.names = F, col.names = F,quote = F )


###creating text file for earthmoving machinery parts layer 2

df4 <- df3[df3$super_pmcats=="Earthmoving Machinery Parts",]
table(df4$pmcats)

write.table(df4$pmcats_label,"C:/Users/imart/Documents/Excavator/Previous Files (Ashutosh)/Bins/pmcat_jcbmodelV6.bin",row.names = F,col.names = F,quote=F)


data = ocr("https://5.imimg.com/data5/BD/XN/MY-2/diclofenac-pot-50mg-para-325mg-250x250.jpg")

