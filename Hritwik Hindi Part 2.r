load("HRI2.Rdata")
colnames(merged_df)


merged_df <- merged_df[,-3]
rm(data_without_na)

file_list <- list.files("C:/Users/indiamart/Desktop/PDN/automl/Product_name_automl/Files for match/")
file_list <-file_list[-1] 

#read all file with as their names as the file name
path <- "C:/Users/indiamart/Desktop/PDN/automl/Product_name_automl/Files for match/"
i=1
for (i in 1:length(file_list)) {
  assign( gsub(".xlsx","",file_list[i]) , read_excel(paste0(path,file_list[i]), sheet =  1)  )  
}



#to check the coloumn names of all teh files
i=1
for (i in 1:16) {
   b <- a[i]
print(colnames(get(a))) 
}


#combining data of all the files and latter remove them
df <-  rbind(set10,set11,set12,set13,set14,set15,set16,set17,set18,set19,set20,set21,set22,set23,set24,set25)

rm(set10,set11,set12,set13,set14,set15,set16,set17,set18,set19,set20,set21,set22,set23,set24,set25)
rm(a,b,file_list,i,path)
gc()

#check <- df[ grepl("&#39;",df$Hindi_Name), ]
#a <- gsub("&#39;","'",check$Hindi_Name[1])
#print(a)
rm(check)
gc()


#replacing all the unwanted characters with the desired characters from the file
df$Hindi_Name <- gsub("&#39;","'",df$Hindi_Name )
df$Hindi_Name <- gsub("&quot;",'"',df$Hindi_Name )
df$Hindi_Name <- gsub("&lt;","<",df$Hindi_Name )
df$Hindi_Name <- gsub("&Gt;",">",df$Hindi_Name )
df$Hindi_Name <- gsub("&amp;","&",df$Hindi_Name )


rm(a)

na_data <- merged_df[is.na(merged_df$`Hindi Name.y`),]
rm(merged_df)
gc()



#length(unique(na_data$PC_ITEM_NAME))
#length(unique(df$PC_Item_Name))
#nrow(df)-length(unique(df$PC_Item_Name))
na_data$`Hindi Name.y` <- NULL
gc()
df <- df[!duplicated(df$PC_Item_Name ),]

options(java.parameters="-Xmx15000m")

merge_data <- merge(na_data,df, by.x = "PC_ITEM_NAME", by.y = "PC_Item_Name", all.x = T)

rm(na_data,df)
gc()


colnames(merge_data)


na_data <- merge_data[ is.na(merge_data$Hindi_Name),  ]
wona_data <- merge_data[ !is.na(merge_data$Hindi_Name),  ]

wona_data$Result <- paste(wona_data$PC_ITEM_ID, wona_data$PC_ITEM_NAME, wona_data$Hindi_Name, sep = "||"  )

head(wona_data$Result)

#write data which is having no hindi pdn
write.csv(na_data,"NA_DATA.csv", row.names = F, quote = T)
gc()

rm(na_data)
gc()

#writing data with hindi names in text file
writeLines(wona_data$Result, "Final_Data2.txt", useBytes = T )
