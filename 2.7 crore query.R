df <- read.csv("C:/Users/Imart/Documents/2.7 Crore/PC_ITEM_ISQ.csv")


####adding colnames to the data 
{
  df1 <- data.frame(pc_item_id= numeric(),
                  master_description= character(),
                  option_description= character())

df1$master_description <- as.character(df1$master_description)
df1$option_description <- as.character(df1$option_description)

df1[1,1] <-   111501615               
df1[1,2] <-  "Output Type"
df1[1,3] <- "Color"                  
                  
colnames(df)[colnames(df)=="X111501615"] <- "pc_item_id"
colnames(df)[colnames(df)=="Output.Type"] <- "master_description"
colnames(df)[colnames(df)=="Color"] <- "option_description"


df <- rbind(df,df1 )
rm(df1)

}


#write.csv(df, "PC_ITEM_ISQ2.csv", row.names = F  )


df <- df[order(df$pc_item_id), ]   #sorting data on the basis of pc ids
df2 <- df[!grepl("\\D",df$pc_item_id), ] #excluding data having non numeric data
df2$pc_item_id <- trimws(df2$pc_item_id)
df2 <- df2[grepl("\\d", df2$pc_item_id), ]
df2$pc_item_id <- as.numeric(df2$pc_item_id)
df2 <- df2[order(df2$pc_item_id), ] 
df2 <- df2[df2$pc_item_id>8936416, ]
df2 <- df2[df2$pc_item_id<4008321094100,  ]
df2 <- df2[order(df2$pc_item_id), ]

df2$option_description <- trimws(df2$option_description)
df2$pc_item_id <- trimws(df2$pc_item_id)
df2$master_description <- trimws(df2$master_description)
rm(df)

#write.csv(df2,"formatfile.csv",row.names = F  )
#df2 <- read.csv("C:/Users/imart/Documents/2.7 Crore/formatfile.csv")

options("scipen"=100, "digits"=4)
summary(df2)



df2a <- NULL
df2a$label <- ""
df2a$label[1] <- df2a$option_description[1]

  
i=2
for (i in 2:nrow(df2a)) {
  df2a$label[i] <- ifelse(df2a$pc_item_id[i] == df2a$pc_item_id[i-1], df2a$label[i] <- paste(df2a$option_description[i], df2a$label[i-1] , sep = " "), df2a$label[i] <-  df2a$option_description[i] ) 
}


df <- rbind(df,df2a )


df_label_unique <- df_label[!rev(duplicated(rev(df_label$pc_item_id))),]

write.csv(df_label,"df_label.csv", row.names = F)
write.csv(df_label,"df_label_unique.csv", row.names = F)

#setwd( "C:/Users/imart/Documents/2.7 Crore/")
#save.image("df_labelandunique.RData" )
#load("df_label.RData"  )






