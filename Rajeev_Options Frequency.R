options(java.parameters = "-Xmx12000m")

library(RJDBC)


# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/ojdbc6.jar")
#jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
##create MESHR connection to DB
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")



query <-  paste0("SELECT IM_SPEC_MASTER_ID,IM_CAT_SPEC_CATEGORY_ID,C.FK_IM_SPEC_OPTIONS_DESC,C.FK_IM_SPEC_MASTER_DESC,PC_ITEM_ATTRIBUTE_MCATID,FK_PC_ITEM_ID  
FROM IM_SPECIFICATION_MASTER A,IM_CAT_SPECIFICATION B,PC_ITEM_ATTRIBUTE C
                 WHERE IM_SPEC_MASTER_BUYER_SELLER IN (0,2)
                 AND IM_CAT_SPEC_CATEGORY_TYPE = 3
                 AND PC_ITEM_ATTRIBUTE_MCATID in (41536,37335,37339)
                 and PC_ITEM_ATTRIBUTE_MCATID = IM_CAT_SPEC_CATEGORY_ID
                 AND IM_CAT_SPEC_STATUS = 1
                 AND B.FK_IM_SPEC_MASTER_ID = A.IM_SPEC_MASTER_ID
                 AND C.FK_IM_SPEC_MASTER_ID = A.IM_SPEC_MASTER_ID" )

queryresult <- dbGetQuery(jdbcCOnMESHR,query )
df <- queryresult
rm(jdbcDriver,jdbcCOnMESHR, query)

#write.csv(queryresult,"6.3Cr.csv", row.names = F, quote = T)
df$IM_SPEC_MASTER_ID <- NULL
df$IM_CAT_SPEC_CATEGORY_ID <- NULL
colnames(df)
df <- df[,c(3,4,2,1)]

df <- na.omit(df)
#sum(is.na(queryresult))
gc()


library(dplyr)


df$FK_IM_SPEC_MASTER_DESC <- trimws(df$FK_IM_SPEC_MASTER_DESC)
df$FK_IM_SPEC_OPTIONS_DESC <- trimws(df$FK_IM_SPEC_OPTIONS_DESC)
df$FK_IM_SPEC_MASTER_DESC <- tolower(df$FK_IM_SPEC_MASTER_DESC)
df$FK_IM_SPEC_OPTIONS_DESC <- tolower(df$FK_IM_SPEC_OPTIONS_DESC)


df <- df %>% group_by(PC_ITEM_ATTRIBUTE_MCATID,FK_IM_SPEC_OPTIONS_DESC ) %>% mutate( option_count = length(FK_IM_SPEC_OPTIONS_DESC) )
df <- df[ order(df$PC_ITEM_ATTRIBUTE_MCATID, df$FK_IM_SPEC_OPTIONS_DESC), ]

df_unique <- df[,c("PC_ITEM_ATTRIBUTE_MCATID","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC","option_count" )]
df_unique <- unique(df_unique)

df_unique <- df_unique %>% group_by( PC_ITEM_ATTRIBUTE_MCATID,FK_IM_SPEC_OPTIONS_DESC) %>% mutate(isq_count = length(FK_IM_SPEC_OPTIONS_DESC) )
df_unique <- df_unique[ order(df_unique$PC_ITEM_ATTRIBUTE_MCATID, df_unique$FK_IM_SPEC_OPTIONS_DESC, df_unique$FK_IM_SPEC_MASTER_DESC ), ]


write.csv(df_unique,"Final_Data.csv", row.names = F, quote = T)
