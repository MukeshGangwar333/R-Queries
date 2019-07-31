hr_data <- fread("C:/Users/imart/Desktop/2.5crore.csv")
unique_subcat_id <- unique(hr_data$FK_GLCAT_CAT_ID)

dir.create("C:/Users/imart/Desktop/Hrithwik/")

i=1
for (i in 1:length(unique_subcat_id) ) {
  s_data <- hr_data[hr_data$FK_GLCAT_CAT_ID==unique_subcat_id[i],]
  write.csv(s_data,paste0("C:/Users/imart/Desktop/Hrithwik/",unique_subcat_id[i],".csv"), row.names = F)
  
  }


subcat_data_count <- as.data.frame(table(hr_data$FK_GLCAT_CAT_ID ) )
colnames(subcat_data_count) <- c("SUBCAT_ID","Count")


write.xlsx (subcat_data_count,"subcat_data_count.xlsx")
