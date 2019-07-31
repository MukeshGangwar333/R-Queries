options(java.parameters="-Xmx12000m")
#memory.limit()

library(RJDBC)
library(xlsx)
library(readxl)

#creating connection with sql
jdbcdriver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",classPath = "C:/Users/imart/Documents/ojdbc6.jar")
#imblrcon <- RJDBC::dbConnect(jdbcdriver,"jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
meshcon <- dbConnect(jdbcdriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")


  query1 <- paste0("select      
( SELECT GLCAT_MCAT_ID FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID and rownum=1) PRIME_MCAT_ID,     
( SELECT GLCAT_MCAT_IS_GENERIC FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) PRIME_MCAT_IS_GENERIC, 
( SELECT FK_MCAT_TYPE_ID FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1) FK_MCAT_TYPE_ID, 
( SELECT GLCAT_MCAT_NAME FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) PRIME_MCAT_NAME, 
( SELECT WM_CONCAT(GLCAT_MCAT_NAME) FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND NVL(ITEM_MAPPING_ISPRIME,0) <> -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) OTHER_MCATS, 
pc_item_to_glcat_mcat.FK_GLCAT_MCAT_ID,PC_ITEM_ID,PC_ITEM_NAME ,PC_ITEM_IMG_ORIGINAL, PC_ITEM_GLUSR_USR_ID, PC_ITEM_GLCAT_MCAT_ID_LIST, PC_ITEM_GLCAT_MCAT_NAME_LIST,pc_item.PC_ITEM_STATUS_APPROVAL ,pc_item.PC_ITEM_DESC_SMALL ,     
FK_IM_SPEC_MASTER_ID, FK_IM_SPEC_MASTER_DESC,FK_IM_SPEC_OPTIONS_ID, FK_IM_SPEC_OPTIONS_DESC--PC_ITEM_ATTRIBUTE_MCATID \"Response MCAT\"
,PC_ITEM_ATTRIBUTE_MOD_DATE,fk_glcat_mcat_id \"Current MCAT\" ,PC_ITEM_FOB_PRICE \"Product Price\", PC_ITEM_MOQ_UNIT_TYPE
from pc_item     
left join pc_item_to_glcat_mcat     
on (pc_item_to_glcat_mcat.fk_pc_item_id=pc_item.pc_item_id)     
left join PC_ITEM_ATTRIBUTE     
on (PC_ITEM_ATTRIBUTE.FK_PC_ITEM_ID=pc_item_to_glcat_mcat.fk_pc_item_id )     
where fk_glcat_mcat_id in (184943)     
and pc_item_id is not null     
order by PC_ITEM_ATTRIBUTE_MCATID,FK_IM_SPEC_MASTER_ID,FK_IM_SPEC_OPTIONS_ID,pc_item_id,PC_ITEM_MOQ_UNIT_TYPE")
  
  
  querydata <- dbGetQuery(meshcon,query1)

  df <- querydata
  Total_Unique_Rows <- length(unique(querydata$PC_ITEM_ID))  
  df[is.na(df) ] <- ""  #remove na and assign blanks to them

  #sorting data in ascending order on the basis of pc_item_id
  df <- df[order(df$PC_ITEM_ID), ]
  
  
  #concatenating product name, option description and master desciption and make one entry for each pc item id
  
  df$option_concat <- ""
  df$option_concat[1] <- df$FK_IM_SPEC_OPTIONS_DESC[1]
  
  i=2
  
  for (i in 2:nrow(df)) {
    
    df$option_concat[i] <- ifelse(df$PC_ITEM_ID[i]==df$PC_ITEM_ID[i-1], paste(df$option_concat[i-1],df$FK_IM_SPEC_OPTIONS_DESC[i],sep = " " ), df$FK_IM_SPEC_OPTIONS_DESC[i]  )
    
  }
  
  df <- df[!rev(duplicated(rev(df$PC_ITEM_ID))),]
  
  #concat product name, proudct description, and product concatenated string
  df$concat_string <- paste(df$PC_ITEM_NAME,df$PC_ITEM_DESC_SMALL,df$option_concat,sep = " ")
  
  df$concat_string <- trimws(tolower(df$concat_string))
  
  #data frame having stopwords
  df_stopwords <- df[grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b|\\preowned\\b|\\pre-owned\\b|\\preown\\b|\\pre-own\\b", df$concat_string, ignore.case = T),]
  
  #data after removing stopwords
  df <- df[!grepl("\\bOld\\b|\\bPart\\b|\\bParts\\b|\\bRefurbished\\b|\\bRent\\b|\\bRental\\b|\\bResale\\b|\\bservice\\b|\\bServices\\b|\\bSpare\\b|\\bSpares\\b|\\bUsed\\b|\\preowned\\b|\\pre-owned\\b|\\preown\\b|\\pre-own\\b", df$concat_string, ignore.case = T),]
  
  df$PC_ITEM_MOQ_UNIT_TYPE <- trimws(tolower(df$PC_ITEM_MOQ_UNIT_TYPE))
  df[is.na(df)] <- ""
  
  #Bucket having no price
  Bucket4_Without_Price <- df[df$`Product Price`=="",]

  
  #data with prices
  df <- df[df$`Product Price`!="", ]
  unit_table <- read_excel("c:/Users/imart/Documents/Agrima/Price Calculation/new units.xlsx")  #unit uniform table
  unit_table$PC_ITEM_MOQ_UNIT_TYPE <- trimws(tolower(unit_table$PC_ITEM_MOQ_UNIT_TYPE))
  unit_table$`Final Unit` <- trimws(tolower(unit_table$`Final Unit`))
  price_range <- read_excel("C:/Users/imart/Documents/Agrima/Price Calculation/Price_range.xlsx")   #range of prices
  price_range$Unit <- trimws(tolower(price_range$Unit))
  
  #data having units of mcats (price range table) 
  
  filtered_mcat_units <- unit_table[unit_table$`Final Unit` %in% price_range$Unit,   ]
  data_with_mcat_units <- df[df$PC_ITEM_MOQ_UNIT_TYPE %in%  filtered_mcat_units$PC_ITEM_MOQ_UNIT_TYPE, ]
  
  
  Bucket3_Data_with_other_units <- df[!df$PC_ITEM_MOQ_UNIT_TYPE %in% filtered_mcat_units$PC_ITEM_MOQ_UNIT_TYPE, ] 
  
  data_with_mcat_units <- merge(data_with_mcat_units, unit_table, by.x = "PC_ITEM_MOQ_UNIT_TYPE", by.y = "PC_ITEM_MOQ_UNIT_TYPE", all.x = T  )
  
  data_with_mcat_units$`Product Price` <- as.numeric(as.character(data_with_mcat_units$`Product Price`))
  
  Bucket2_Witn_Unit_and_Correct <- data_with_mcat_units[0,]
  Bucket1_Witn_Unit_and_Incorrect <- data_with_mcat_units[0,]
  
  
  i=2 
  for (i in 1:nrow(price_range)) {
    data_1 <- data_with_mcat_units[data_with_mcat_units$`Final Unit` %in% price_range$Unit[i], ]
    data_1a <- data_1[data_1$`Product Price` >= price_range$Min_Price[i], ]
    data_1b <- data_1[data_1$`Product Price` < price_range$Min_Price[i], ]
    Bucket2_Witn_Unit_and_Correct <- rbind(Bucket2_Witn_Unit_and_Correct,data_1a )
    Bucket1_Witn_Unit_and_Incorrect <- rbind(Bucket1_Witn_Unit_and_Incorrect,data_1b )
  }
  

  
