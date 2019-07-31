library(RJDBC)
library(xlsx)

options(java.parameters="Xmx8000m")

jdbcdriver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",classPath = "C:/Users/imart/Desktop/R Folder/Queries/ojdbc6.jar")
imblrcon <- RJDBC::dbConnect(jdbcdriver,"jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
meshcon <- dbConnect(jdbcdriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")


if(exists("meshcon"))
  {
  query1 <- paste("select IM_CAT_SPEC_CATEGORY_ID MCAT_ID,
  (select glcat_mcat_NAME from glcat_mcat WHERE GLCAT_MCAT_ID = IM_CAT_SPEC_CATEGORY_ID)MCAT_NAME, 
  im_spec_master_desc ,
  DECODE(IM_SPEC_MASTER_BUYER_SELLER, 0, 'Both', 1, 'Buyer', 2, 'Supplier', 'Null') IM_SPEC_MASTER_BUYER_SELLER,
  DECODE(IM_SPEC_MASTER_TYPE, 1, 'Text', 2, 'Radio', 3, 'Dropdown', 4, 'Multiple Select', 'Null') ISQ_TYPE
  from im_specification_master, im_cat_specification
  where (im_spec_master_desc like '% ' or im_spec_master_desc like ' %')
  AND im_spec_master_desc NOT IN ('Quantity', 'Quantity Unit', 'Currency', 'Approximate Order Value', 'Usage/Applications')
  and im_spec_master_buyer_seller <> 2
  and im_cat_spec_category_type = 3
  and IM_CAT_SPEC_PRIORITY NOT IN (-1, 49)
  and im_cat_spec_status = 1
  and FK_IM_SPEC_MASTER_ID = IM_SPEC_MASTER_ID")
  
  query2 <- paste("select IM_CAT_SPEC_CATEGORY_ID MCAT_ID,
--(select glcat_mcat_NAME from glcat_mcat WHERE GLCAT_MCAT_ID = IM_CAT_SPEC_CATEGORY_ID)MCAT_NAME, 
                  im_spec_master_desc ,im_spec_options_desc,
                  DECODE(IM_SPEC_MASTER_buyer_seller, 0, 'Both', 1, 'Buyer', 2, 'Supplier', 'Null') IM_SPEC_MASTER_BUYER_SELLER,
                  DECODE(IM_SPEC_opt_buyer_seller, 0, 'Both', 1, 'Buyer', 2, 'Supplier', 'Null') IM_SPEC_OPT_BUYER_SELLER
                  --DECODE(IM_SPEC_MASTER_TYPE, 1, 'Text', 2, 'Radio', 3, 'Dropdown', 4, 'Multiple Select', 'Null') ISQ_TYPE
                  from im_specification_master A, im_cat_specification B, IM_SPECIFICATION_OPTIONS C
                  where (im_spec_options_desc like '% ' or im_spec_options_desc like ' %')
                  AND im_spec_master_desc NOT IN ('Quantity', 'Quantity Unit', 'Currency', 'Approximate Order Value', 'Usage/Applications')
                  and im_spec_master_buyer_seller <> 2
                  and im_cat_spec_category_type = 3
                  and IM_CAT_SPEC_PRIORITY NOT IN (-1, 49)
                  and im_cat_spec_status = 1
                  AND IM_SPEC_OPTIONS_STATUS = 1
                  and B.FK_IM_SPEC_MASTER_ID = A.IM_SPEC_MASTER_ID
                  AND C.FK_IM_SPEC_MASTER_ID = A.IM_SPEC_MASTER_ID")
  
  query3 <- paste("select * from
  (
  select IM_CAT_SPEC_CATEGORY_ID MCAT_ID,IM_SPEC_MASTER_ID, IM_SPEC_MASTER_DESC,IM_SPEC_OPTIONS_ID,IM_SPEC_OPTIONS_DESC, count(1) over(partition by
  IM_SPEC_MASTER_ID,
  IM_SPEC_OPTIONS_DESC) cnt,
  IM_SPEC_MASTER_BUYER_SELLER
  from
  (
  select distinct IM_CAT_SPEC_CATEGORY_ID,IM_SPEC_MASTER_ID,IM_SPEC_MASTER_DESC,IM_SPEC_MASTER_BUYER_SELLER,IM_SPEC_OPTIONS_ID,IM_SPEC_OPTIONS_DESC
  from im_cat_specification A,im_specification_master B,IM_SPECIFICATION_OPTIONS C
  where A.FK_IM_SPEC_MASTER_ID=B.IM_SPEC_MASTER_ID
  and IM_CAT_SPEC_CATEGORY_TYPE=3
  and IM_CAT_SPEC_STATUS=1
  and IM_SPEC_MASTER_BUYER_SELLER in (0,1)
  and IM_SPEC_MASTER_DESC<> 'Quantity Unit'
  
  and A.FK_IM_SPEC_MASTER_ID = C.FK_IM_SPEC_MASTER_ID
  and IM_SPEC_OPTIONS_STATUS =1
  order by IM_CAT_SPEC_CATEGORY_ID desc
  )
  ) where cnt>1")
  
  
  queryresult1 <- dbGetQuery(meshcon,query1)
  write.xlsx(queryresult1,"C:/Users/imart/Desktop/R Folder/Extra Spacing in ISQ.xlsx",row.names = F)
  
  queryresult2 <- dbGetQuery(meshcon,query2)
  write.xlsx(queryresult2,"C:/Users/imart/Desktop/R Folder/Extra Spacing in Options.xlsx",row.names = F)
  
  queryresult3 <- dbGetQuery(meshcon,query3)
  write.xlsx(queryresult3,"C:/Users/imart/Desktop/R Folder/Duplicate Options on Buyer Side.xlsx",row.names = F)
  }