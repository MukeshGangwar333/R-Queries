options(java.parameters="-Xmx10000m")
#memory.limit()

library(RJDBC)
library(reshape2)
library(readxl)

#creating connection with sql
jdbcdriver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",classPath = "C:/Users/imart/Documents/ojdbc6.jar")
#imblrcon <- RJDBC::dbConnect(jdbcdriver,"jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
meshcon <- dbConnect(jdbcdriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

#reading mcat ids for which data is to be extracted
mcat_ids <- read_excel("c:/Users/imart/Desktop/Heavy, Medium, Light Commercial Vehicles & Three Wheelers/Commercial MCATs.xlsx", sheet = 1)
#colnames(mcat_ids) <- as.character(unlist(mcat_ids[4,]))   #renaming col names with the rows of the data frame
Dat1 <- paste0(mcat_ids$`MCAT ID`,",",collapse = " ")
Dat1 <- sub(",$","",Dat1)
mcat_ids <- Dat1
rm(Dat1)



if (exists("meshcon"))
{
query1 <- paste0("select      
( SELECT GLCAT_MCAT_ID FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID and rownum=1) PRIME_MCAT_ID,     
( SELECT GLCAT_MCAT_IS_GENERIC FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) PRIME_MCAT_IS_GENERIC,     
( SELECT FK_MCAT_TYPE_ID FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1) FK_MCAT_TYPE_ID,     
( SELECT GLCAT_MCAT_NAME FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND ITEM_MAPPING_ISPRIME = -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) PRIME_MCAT_NAME,     
( SELECT WM_CONCAT(GLCAT_MCAT_NAME) FROM PC_ITEM_TO_GLCAT_MCAT, GLCAT_MCAT WHERE FK_PC_ITEM_ID = PC_ITEM_ID AND NVL(ITEM_MAPPING_ISPRIME,0) <> -1 AND GLCAT_MCAT_ID = FK_GLCAT_MCAT_ID  and rownum=1 ) OTHER_MCATS,     
pc_item_to_glcat_mcat.FK_GLCAT_MCAT_ID,PC_ITEM_ID,PC_ITEM_NAME ,PC_ITEM_IMG_ORIGINAL, PC_ITEM_GLUSR_USR_ID, PC_ITEM_GLCAT_MCAT_ID_LIST, PC_ITEM_GLCAT_MCAT_NAME_LIST,pc_item.PC_ITEM_STATUS_APPROVAL ,pc_item.PC_ITEM_DESC_SMALL,     
FK_IM_SPEC_MASTER_ID, FK_IM_SPEC_MASTER_DESC,FK_IM_SPEC_OPTIONS_ID, FK_IM_SPEC_OPTIONS_DESC,PC_ITEM_ATTRIBUTE_MOD_DATE,fk_glcat_mcat_id \"Current MCAT\"     
from pc_item     
left join pc_item_to_glcat_mcat     
on (pc_item_to_glcat_mcat.fk_pc_item_id=pc_item.pc_item_id)     
left join PC_ITEM_ATTRIBUTE     
on (PC_ITEM_ATTRIBUTE.FK_PC_ITEM_ID=pc_item_to_glcat_mcat.fk_pc_item_id )     
where fk_glcat_mcat_id in (",mcat_ids,")     
and pc_item_id is not null     
order by PC_ITEM_ATTRIBUTE_MCATID,FK_IM_SPEC_MASTER_ID,FK_IM_SPEC_OPTIONS_ID,pc_item_id","")

 querydata1 <- dbGetQuery(meshcon,query1)
  
 
 query2 <- paste0("SELECT GLCAT_MCAT.GLCAT_MCAT_ID, GLCAT_MCAT.GLCAT_MCAT_NAME,  FK_PARENT_MCAT_ID, PARENT_MCAT.GLCAT_MCAT_NAME PARENT_NAME, 
GLCAT_MCAT_MAPPING_ISPRIME isprime_parent, 
                     NVL(GLCAT_MCAT.GLCAT_MCAT_IS_GENERIC,0) isgeneric, 
                     NVL(GLCAT_MCAT.GLCAT_MCAT_ISBRAND,0) isbrand, 
                     NVL(GLCAT_MCAT.IS_SEARCH_TERM,0) is_searchterm, 
                     NVL(GLCAT_MCAT.FK_MCAT_TYPE_ID,0) MCAT_FLAG,
                     GLCAT_GRP_TO_CAT.fk_glcat_cat_id subcat_id, 
                     glcat_cat_name subcat_name, isprime isprime_scat, glcat_grp_id, glcat_grp_name, 
                     ( SELECT COUNT(1) FROM GLCAT_MCAT_TO_MCAT WHERE FK_PARENT_MCAT_ID = GLCAT_MCAT.GLCAT_MCAT_ID ) CHILD_COUNT,
                     NVL(PARENT_MCAT.FK_MCAT_TYPE_ID,0) PMCAT_FLAG ,
                     
                     ( SELECT COUNT(1) FROM GLCAT_MCAT_TO_MCAT X,GLCAT_MCAT Y WHERE X.FK_PARENT_MCAT_ID = GLCAT_MCAT.GLCAT_MCAT_ID AND X.FK_CHILD_MCAT_ID = Y.GLCAT_MCAT_ID AND Y.IS_SEARCH_TERM = 1 ) DUP_CHILD_COUNT
                     
                     
                     
                     FROM GLCAT_MCAT, (SELECT * FROM GLCAT_MCAT_TO_MCAT WHERE FK_PARENT_MCAT_ID > 0 ) GLCAT_MCAT_TO_MCAT , GLCAT_MCAT PARENT_MCAT, 
                     GLCAT_CAT_TO_MCAT G , 
                     glcat_cat gc, 
                     GLCAT_GRP_TO_CAT, 
                     glcat_grp 
                     WHERE GLCAT_MCAT.GLCAT_MCAT_ID = FK_CHILD_MCAT_ID(+)  
                     AND PARENT_MCAT.GLCAT_MCAT_ID(+) = FK_PARENT_MCAT_ID 
                     AND GLCAT_MCAT.GLCAT_MCAT_ID in (",mcat_ids,")
                     AND G.FK_GLCAT_MCAT_ID = GLCAT_MCAT.GLCAT_MCAT_ID 
                     AND g.fk_glcat_cat_id =gc.glcat_cat_id(+) 
                     AND gc.glcat_cat_id =GLCAT_GRP_TO_CAT.fk_glcat_cat_id 
                     AND GLCAT_GRP_TO_CAT.fk_glcat_grp_id=glcat_grp.glcat_grp_id AND ISPRIMEGRP=-1")
 
 mcat_pmcat <- dbGetQuery(meshcon,query2)
}


#write.csv(mcat_pmcat,paste0(getwd(),"/mcat_pmcat.csv"))


df <- querydata1
df[is.na(df)] <- ""  #replacing all missing values with blank space



mcat_pmcat1 <- mcat_pmcat[!is.na(mcat_pmcat$ISPRIME_PARENT) & mcat_pmcat$ISPRIME_PARENT==-1,]
mcat_pmcat2 <- mcat_pmcat1[,c("GLCAT_MCAT_ID","FK_PARENT_MCAT_ID","PARENT_NAME","PMCAT_FLAG","SUBCAT_NAME")]
mcat_pmcat2 <- mcat_pmcat2[order(mcat_pmcat2$GLCAT_MCAT_ID),]
mcat_pmcat2$mcatid_parentname <- paste0(mcat_pmcat2$GLCAT_MCAT_ID,mcat_pmcat2$PARENT_NAME)
mcat_pmcat3 <- mcat_pmcat2[!duplicated(mcat_pmcat2$mcatid_parentname),]
#mcat_pmcat4 <- mcat_pmcat2[!duplicated(mcat_pmcat2$GLCAT_MCAT_ID, mcat_pmcat1$PARENT_NAME),]



df2 <- merge(df,mcat_pmcat3,by.x="PRIME_MCAT_ID",by.y = "GLCAT_MCAT_ID",all.x = T, na.rm = TRUE)  #applying v lookup to get pmcat of prime mcat id and their pmcat flag
df2$PMCAT2 <- ""
df3 <- df2[,c("PRIME_MCAT_ID","PRIME_MCAT_NAME","PRIME_MCAT_IS_GENERIC","FK_MCAT_TYPE_ID","FK_PARENT_MCAT_ID" ,"PARENT_NAME","PMCAT_FLAG" ,"PMCAT2","OTHER_MCATS","FK_GLCAT_MCAT_ID","PC_ITEM_NAME","PC_ITEM_IMG_ORIGINAL","PC_ITEM_DESC_SMALL","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC","PC_ITEM_ID","SUBCAT_NAME")]
df3[is.na(df3)] <- ""


df3$PMCAT2 <- ifelse(df3$FK_MCAT_TYPE_ID==2, df3$PMCAT2 <- df3$PRIME_MCAT_NAME, 
                     ifelse( df3$PMCAT2!="" & df3$PMCAT_FLAG==2, df3$PMCAT2 <-  df3$PARENT_NAME, df3$PMCAT2 <- ""))


#df4 <- df3[!is.na(df3$PARENT_NAME) & !is.na(df3$PMCAT2),]

df5 <- df3[order(df3$PC_ITEM_ID),]   #sorted data in increasing order
df5[is.na(df5)] <- ""  #replacing all missing values with blank space
df5$FK_IM_SPEC_OPTIONS_DESC <- trimws(df5$FK_IM_SPEC_OPTIONS_DESC)


df5$Label=""

df5$Label[1] <- df5$FK_IM_SPEC_OPTIONS_DESC[1]


j=2
for (j in 2:nrow(df5)) {
  df5$Label[j] <- ifelse(df5$PC_ITEM_ID[j]==df5$PC_ITEM_ID[j-1],paste(df5$Label[j-1],df5$FK_IM_SPEC_OPTIONS_DESC[j],sep = " "),df5$FK_IM_SPEC_OPTIONS_DESC[j])  
}


length(unique(df5$PC_ITEM_ID))
df6 <- df5[!rev(duplicated(rev(df5$PC_ITEM_ID))),]

sum(is.na(df5$Label1))

df6$Label <- trimws(df6$Label)
write.csv(df6,"Commercials_Data.csv", row.names = F, quote = T)

df_final <- df6[df6$PRIME_MCAT_ID!="",]  #remove pc ids for which prime mcat id is not present

#rm(list = ls()[-c(2,6)])

##old way to get the unique last concatenated row (Label) of PC Item ID
#df5$Label1 <- ""
#i=1

#for (i in 1:nrow(df5)) {
#  df5$Label1[i] <- ifelse(df5$PC_ITEM_ID[i]==df5$PC_ITEM_ID[i+1],"AAOOV",df5$Label[i])  
#}


#df_final <- df5[!df5$Label1=="AAOOV",]


#df_final1[Prime_mcat_good,"PMCAT2"] <- df_final1[Prime_mcat_good,"PRIME_MCAT_NAME"]
#df_final1[Prime_pmcat_good,"PMCAT2"] <- df_final1[Prime_pmcat_good,"PARENT_NAME"]



save.image("Dyes.Rdata")
