options(java.parameters="-Xmx6000m")
library(readxl)
library(data.table)


###Data Cleaning for Products Data###

pc_df <- read_excel("C:/Users/Mukesh-PC/Documents/Agrima/product_data.xlsx", sheet = 1)
colnames(pc_df)
pc_df <- pc_df[,c("PRIME_MCAT_ID","PRIME_MCAT_NAME","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_GLUSR_USR_ID","PC_ITEM_DESC_SMALL","OTHER_MCATS","FK_GLCAT_MCAT_ID",
                  "FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC")]

#remove rows having stopwords in product names
pc_df1 <- pc_df[!grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", pc_df$PC_ITEM_NAME, ignore.case = T),]

#remove rows having stopwords in mcat names
pc_df2 <- pc_df1[!grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", pc_df1$PRIME_MCAT_NAME, ignore.case = T),]


#remove rows having restricted options in brand

pc_df2$FK_IM_SPEC_MASTER_DESC <- tolower(trimws(pc_df2$FK_IM_SPEC_MASTER_DESC)) 
pc_df2$FK_IM_SPEC_OPTIONS_DESC <- tolower(trimws(pc_df2$FK_IM_SPEC_OPTIONS_DESC)) 

ban_brands <- pc_df2[pc_df2$FK_IM_SPEC_MASTER_DESC %like% "brand" & pc_df2$FK_IM_SPEC_OPTIONS_DESC %like% "all |\\bany\\b|\\bgood\\b|\\bbest\\b|,"  ,]
ban_brands_gl_ids <- unique(ban_brands$PC_ITEM_GLUSR_USR_ID)
write.csv(ban_brands,"Banned_Brands_Products.csv", row.names = F, quote = T)

pc_df3 <- pc_df2[ !pc_df2$PC_ITEM_GLUSR_USR_ID %in% ban_brands_gl_ids ,]

rm(pc_df,pc_df1,pc_df2,ban_brands_gl_ids,ban_brands)
gc()
#remove rows having restricted options in model

pc_df3$FK_IM_SPEC_MASTER_DESC <- tolower(trimws(pc_df3$FK_IM_SPEC_MASTER_DESC))
pc_df3$FK_IM_SPEC_OPTIONS_DESC <- tolower(trimws(pc_df3$FK_IM_SPEC_OPTIONS_DESC ))


ban_model <- pc_df3[pc_df3$FK_IM_SPEC_MASTER_DESC %like% "model"& pc_df3$FK_IM_SPEC_OPTIONS_DESC %like% "\\ball\\b|\\bany\\b", ]
ban_model_gl_ids <- unique(ban_model$PC_ITEM_GLUSR_USR_ID)
write.csv(ban_model,"Banned_Models_Products.csv", row.names = F, quote = T)
pc_df4 <- pc_df3[!pc_df3$PC_ITEM_GLUSR_USR_ID %in% ban_model_gl_ids, ]

#remove all rows having stopwords in options
stop_options <- pc_df4[grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", pc_df4$FK_IM_SPEC_OPTIONS_DESC, ignore.case = T),]
stop_options_gl_ids <- unique(stop_options$PC_ITEM_GLUSR_USR_ID)
write.csv(stop_options,"Options_Stopwords_Products.csv", row.names = F, quote = T)
pc_df5 <- pc_df4[!pc_df4$PC_ITEM_GLUSR_USR_ID %in% stop_options_gl_ids, ]


#sorting data in ascending order on the basis of pc_item_id
pc_df6 <- pc_df5[order(pc_df5$PC_ITEM_ID), ]

rm(pc_df3,pc_df4,pc_df5,ban_model_gl_ids,stop_options_gl_ids,ban_model, stop_options)
gc()


#concatenating product name, option description and master desciption and make one entry for each pc item id

pc_df6[is.na(pc_df6) ] <- ""  #remove na and assign blanks to them
pc_df6$option_concat <- ""

pc_df6$option_concat[1] <- pc_df6$FK_IM_SPEC_OPTIONS_DESC[1]



i=2

for (i in 2:nrow(pc_df6)) {

  pc_df6$option_concat[i] <- ifelse(pc_df6$PC_ITEM_ID[i]==pc_df6$PC_ITEM_ID[i-1], paste(pc_df6$option_concat[i-1],pc_df6$FK_IM_SPEC_OPTIONS_DESC[i],sep = " " ), pc_df6$FK_IM_SPEC_OPTIONS_DESC[i]  )
    
}

pc_df7 <- pc_df6[!rev(duplicated(rev(pc_df6$PC_ITEM_ID))),]

#length(unique(pc_df6$PC_ITEM_ID))

#concat product name, proudct description, and product concatenated string
pc_df7$concat_string <- paste(pc_df7$PC_ITEM_NAME,pc_df7$PC_ITEM_DESC_SMALL,pc_df7$option_concat,sep = " ")

Final_Product_Data <- pc_df7

#Final_Product_Data$concat_string <- gsub("-"," ",Final_Product_Data$concat_string)
Final_Product_Data$concat_string <- trimws(Final_Product_Data$concat_string)

rm(pc_df6,pc_df7,i)
gc()




###Extracting Brand & Model from SID Sheet
sid_df <- read_excel("C:/Users/Mukesh-PC/Documents/Agrima/pmcat name from sid name.xlsx", sheet=2)
colnames(sid_df)
sid_df <- sid_df["sid"]

library(stringr)
sid_df$last_num <- str_extract(sid_df$sid, ".+\\d")
sid_df$last_caps <- str_extract(sid_df$sid,".+[A-Z] ")


sid_df[is.na(sid_df)] <- ""

sid_df$consider <- ""

sid_df$consider <- ifelse(nchar(sid_df$last_num)>nchar(sid_df$last_caps),"Last_Number","Last_Caps")

write.csv(sid_df,"SID_DF.csv", row.names = F, quote = T)

Final_SID_DF <- choose.files()
Final_SID_DF <- read.csv(Final_SID_DF)

Final_SID_DF$Brand <- word(Final_SID_DF$last_num,start = 1,sep = " ")

k=1
for (k in 1:nrow(Final_SID_DF)) {
  Final_SID_DF$Model[k] <- gsub(Final_SID_DF$Brand[k],"", Final_SID_DF$last_num[k], ignore.case = T )
    
}


#match brand and model from product data

Matched_data_Product <- Final_Product_Data[0,]

i=1
for (i in 1:nrow(Final_SID_DF)) {
  data <- Final_Product_Data[(grepl(Final_SID_DF$Brand[i],  Final_Product_Data$concat_string, ignore.case = T) & grepl(Final_SID_DF$Model[i], Final_Product_Data$concat_string, ignore.case = T ) ) , ]  
  if (nrow(data)>0) {
    data$brand <- Final_SID_DF$Brand[i]
    data$model <- Final_SID_DF$Model[i]
    Matched_data_Product <- rbind(Matched_data_Product,data)
    
  } 
}

write.csv(Matched_data_Product,"Match_Data_Product.csv",row.names = F,quote = T)

rm(data,Final_Product_Data,Matched_data_Product,sid_df,i,k)


######Data Cleaning for BuyLead Data######

bl_df <- read_excel("C:/Users/Mukesh-PC/Documents/Agrima/bl printers sample.xlsx", sheet = 1)
statelist <- read_excel("C:/Users/Mukesh-PC/Documents/Agrima/bl printers sample.xlsx", sheet = 2)

colnames(bl_df)
bl_df <- bl_df[,c("ETO_OFR_DISPLAY_ID","FK_GLCAT_MCAT_ID","GLCAT_MCAT_NAME","ETO_OFR_TITLE","GLUSR_USR_CITY","GLUSR_USR_STATE","SELLER_GLUSR","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC")]


#remove rows having stopwords in product names
bl_df1 <- bl_df[!grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", bl_df$ETO_OFR_TITLE, ignore.case = T),]

#remove rows having stopwords in mcat names
bl_df2 <- bl_df1[!grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", bl_df1$GLCAT_MCAT_NAME , ignore.case = T),]

#remove rows having restricted options in brand


bl_df2$FK_IM_SPEC_MASTER_DESC <- tolower(trimws(bl_df2$FK_IM_SPEC_MASTER_DESC)) 
bl_df2$FK_IM_SPEC_OPTIONS_DESC <- tolower(trimws(bl_df2$FK_IM_SPEC_OPTIONS_DESC)) 

ban_brands <- bl_df2[bl_df2$FK_IM_SPEC_MASTER_DESC %like% "brand" & bl_df2$FK_IM_SPEC_OPTIONS_DESC %like% "all |\\bany\\b|\\bgood\\b|\\bbest\\b|,"  ,]
ban_brands_gl_ids <- unique(ban_brands$SELLER_GLUSR)
write.csv(ban_brands,"Banned_Brands_BuyLead.csv", row.names = F, quote = T)

bl_df3 <- bl_df2[ !bl_df2$SELLER_GLUSR %in% ban_brands_gl_ids ,]

rm(bl_df,bl_df1,bl_df2,ban_brands_gl_ids,ban_brands)
gc()


#remove rows having restricted options in model

bl_df3$FK_IM_SPEC_MASTER_DESC <- tolower(trimws(bl_df3$FK_IM_SPEC_MASTER_DESC))
bl_df3$FK_IM_SPEC_OPTIONS_DESC <- tolower(trimws(bl_df3$FK_IM_SPEC_OPTIONS_DESC ))


ban_model <- bl_df3[bl_df3$FK_IM_SPEC_MASTER_DESC %like% "model"& bl_df3$FK_IM_SPEC_OPTIONS_DESC %like% "\\ball\\b|\\bany\\b", ]
ban_model_gl_ids <- unique(ban_model$SELLER_GLUSR)
write.csv(ban_model,"Banned_Models_BuyLead.csv", row.names = F, quote = T)
bl_df4 <- bl_df3[!bl_df3$SELLER_GLUSR %in% ban_model_gl_ids, ]
rm(bl_df3,ban_model_gl_ids,ban_model)

#remove all rows having stopwords in options
stop_options <- bl_df4[grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b", bl_df4$FK_IM_SPEC_OPTIONS_DESC, ignore.case = T),]
stop_options_gl_ids <- unique(stop_options$SELLER_GLUSR)
write.csv(stop_options,"Options_Stopwords_BuyLead.csv", row.names = F, quote = T)
bl_df5 <- bl_df4[!bl_df4$SELLER_GLUSR %in% stop_options_gl_ids, ]
rm(ban_model,bl_df4,stop_options,stop_options_gl_ids)

#remove all rows having foreign states
bl_df6 <- bl_df5[bl_df5$GLUSR_USR_STATE %in% statelist$GLUSR_USR_STATE, ]


#sorting data in ascending order on the basis of pc_item_id
bl_df6 <- bl_df6[order(bl_df6$ETO_OFR_DISPLAY_ID), ]

rm(bl_df5, statelist)
gc()


#concatenating eto ofr title, option description and master desciption and make one entry for each pc item id

bl_df6[is.na(bl_df6)] <- "" #remove na and assign blank space
bl_df6$option_concat <- ""

bl_df6$option_concat[1] <- bl_df6$FK_IM_SPEC_OPTIONS_DESC[1]


i=2

for (i in 2:nrow(bl_df6)) {
  
  bl_df6$option_concat[i] <- ifelse(bl_df6$ETO_OFR_DISPLAY_ID[i]==bl_df6$ETO_OFR_DISPLAY_ID[i-1], paste(bl_df6$option_concat[i-1],bl_df6$FK_IM_SPEC_OPTIONS_DESC[i],sep = " " ), bl_df6$FK_IM_SPEC_OPTIONS_DESC[i]  )
  
}

bl_df7 <- bl_df6[!rev(duplicated(rev(bl_df6$ETO_OFR_DISPLAY_ID))),]
#length(unique(bl_df6$ETO_OFR_DISPLAY_ID))



#concat product name, proudct description, and product concatenated string
bl_df7$concat_string <- paste(bl_df7$ETO_OFR_TITLE,bl_df7$option_concat,sep = " ")

Final_BuyLead_Data <- bl_df7

#Final_BuyLead_Data$concat_string <- gsub("-"," ",Final_BuyLead_Data$concat_string)
Final_BuyLead_Data$concat_string <- trimws(Final_BuyLead_Data$concat_string)

rm(bl_df6,bl_df7,i)
gc()

#Match brand and model from Final Buylead data
Matched_data_BuyLead <- Final_BuyLead_Data[0,]

i=1
for (i in 1:nrow(Final_SID_DF)) {
  data <- Final_BuyLead_Data[(grepl(Final_SID_DF$Brand[i],  Final_BuyLead_Data$concat_string, ignore.case = T) & grepl(Final_SID_DF$Model[i], Final_BuyLead_Data$concat_string, ignore.case = T ) ) , ]  
  if (nrow(data)>0) {
    data$brand <- Final_SID_DF$Brand[i]
    data$model <- Final_SID_DF$Model[i]
    Matched_data_BuyLead <- rbind(Matched_data_BuyLead,data)
    
  } 
}

write.csv(Matched_data_BuyLead,"Matched_data_buylead.csv", row.names = F, quote = T)

rm(data)
