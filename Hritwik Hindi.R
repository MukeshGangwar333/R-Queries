options(java.parameters= "-Xmx10000m")
#Encoding=" ISO/IEC 8859-1"
#Sys.setlocale(category = "LC_ALL", locale = "Hindi")
#Sys.getlocale()

#iconv(df$`Hindi Name`[1],from = "UTF-8", to = "x-mac-arabic")
#iconvlist()

########################################################


file_list <- list.files("C:/Users/imart/Desktop/hri")
path <- "C:/Users/imart/Desktop/hri/"
for (i in 1:length(file_list)) {
  assign( gsub(".xlsx","",file_list[i]) , read_excel(paste0(path,file_list[i]), sheet =  1)  )  
}

colnames(sheet30)[colnames(sheet30)== "Hindi Name.y"] <-  "Hindi Name"

final_df <- rbind(Sheet1,Sheet2,sheet3,sheet4,sheet5,sheet6,sheet7,sheet8,sheet9,sheet10,sheet11,sheet12,sheet13,sheet14,sheet15,sheet16,sheet17,sheet18,sheet19,sheet20,sheet21,sheet22,sheet23,sheet24,sheet25,sheet26,sheet27,sheet28,sheet29,sheet30,sheet31,sheet32,sheet33,sheet34,sheet35,sheet36,sheet37,sheet38,sheet39,sheet40,sheet41,sheet42)
rm(Sheet1,Sheet2,sheet3,sheet4,sheet5,sheet6,sheet7,sheet8,sheet9,sheet10,sheet11,sheet12,sheet13,sheet14,sheet15,sheet16,sheet17,sheet18,sheet19,sheet20,sheet21,sheet22,sheet23,sheet24,sheet25,sheet26,sheet27,sheet28,sheet29,sheet30,sheet31,sheet32,sheet33,sheet34,sheet35,sheet36,sheet37,sheet38,sheet39,sheet40,sheet41,sheet42)

#con <- grep("|", final_df$PC_ITEM_NAME, ignore.case = T)

final_df$final_col <- paste( final_df$PC_ITEM_ID, final_df$PC_ITEM_NAME, final_df$`Hindi Name`, sep = " || " )

writeLines(final_df$final_col ,"Final_File_Ayush.txt",useBytes = T)  #to get data in hindi



save.image("AyushGarg4cr.RData")
