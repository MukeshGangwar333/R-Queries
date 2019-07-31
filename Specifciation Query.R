  library(RODBC)
  library(RJDBC)
  library(xlsx)
  library(dplyr)
  library(sqldf)
  library(stringi)
  library(stringr)
  library(readxl)
  
  #memory.limit(6000)
  #Creating greater heap space
  options(java.parameters = "-Xmx8000m")
  
  #Creating variables startDate and endDate
  #startDate<-Sys.Date()-7# Format here is 2018-04-17
  #endDate<- Sys.Date()-1
  #Converting into suitable format
  #startDate <-format(startDate,format="%d-%b-%y")# Format here is 17-Apr-18
  #endDate <-format(endDate,format="%d-%b-%y")
  
  ## Create JDBC Connection
  jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Desktop/R Folder/Queries/ojdbc6.jar")
  ##create MESHR connection to DB
  jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")
  #jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")        #Create IMBLR Connection to DB
  
  
  if(exists("jdbcCOnMESHR"))
   
  {
     
    getId <- paste("select A.*,OPTION_COUNT
                   from
                   (
                   select FK_GLCAT_CAT_ID,IM_CAT_SPEC_CATEGORY_ID as MCAT_ID,GLCAT_MCAT_NAME as MCAT_NAME,IM_SPEC_MASTER_ID, IM_SPEC_MASTER_DESC,IM_SPEC_MASTER_FULL_DESC,
                   IM_SPEC_MASTER_BUYER_SELLER,IM_CAT_SPEC_PRIORITY,
                   IM_CAT_SPEC_SUP_PRIORITY , IM_SPEC_MASTER_TYPE,IM_SPEC_OPTIONS_DESC,IM_SPEC_OPT_BUYER_SELLER,IM_SPECIFICATION_OPT_PRIORITY,IM_CAT_SPEC_TYPE,IM_SPEC_AFFIX_TYPE
                   from IM_SPECIFICATION_MASTER ,IM_CAT_SPECIFICATION,GLCAT_MCAT,IM_SPECIFICATION_OPTIONS,GLCAT_CAT_TO_MCAT
                   where IM_SPEC_MASTER_ID = IM_CAT_SPECIFICATION.FK_IM_SPEC_MASTER_ID
                   and IM_CAT_SPEC_CATEGORY_TYPE =3
                   and IM_CAT_SPEC_STATUS = 1
                   and IM_CAT_SPEC_CATEGORY_ID = glcat_mcat_id
                   AND IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID = IM_CAT_SPECIFICATION.FK_IM_SPEC_MASTER_ID
                   and GLCAT_MCAT.GLCAT_MCAT_ID = GLCAT_CAT_TO_MCAT.FK_GLCAT_MCAT_ID
                   and GLCAT_MCAT_DELETE_STATUS = 0
                   AND IM_SPEC_OPTIONS_STATUS = 1
                   ) A, 
                   ( 
                   SELECT IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID,COUNT(1) OPTION_COUNT FROM IM_SPECIFICATION_OPTIONS,IM_CAT_SPECIFICATION
                   WHERE IM_CAT_SPECIFICATION.fk_IM_SPEC_MASTER_ID = IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID
                   AND  IM_CAT_SPEC_CATEGORY_TYPE =3
                   AND  IM_CAT_SPEC_STATUS = 1
                   AND IM_SPEC_OPTIONS_STATUS = 1
                   GROUP BY IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID )B
                   WHERE A.IM_SPEC_MASTER_ID= B.FK_IM_SPEC_MASTER_ID
                   AND MCAT_ID IN ( SELECT FK_GLCAT_MCAT_ID FROM GLCAT_CAT_TO_MCAT WHERE FK_GLCAT_CAT_ID IN (150,209,53,151,249,85,358,640,370,795,71,127,840,196,564,66,92,842,198,164,577,777,785,490,94,123,36,589,385,158,126,729,625,517,500,722,87,74,147,568,360,742,56,58,762,750,61,202,155,604,725,723,82,248,121,598,691,63,163,199,145,192,614,432,578,59,88,51,341,440,257,200,229,171,740,571,649,451,433,223,821,639,572,743,14,717,25,180,241,812,569,434,197,570,187,132,11,64,701,2,157,799,203,849,714,826,516,390,210,780,381,713,122,222,102,810,72,646,62,224,185,153,162,756,177,580,32,316,583,40,663,170,781,26,502,149,225,383,582,27,584,12,7,118,642,190,758,54,501,838,533,617,15,515,143,228,136,413,175,108,240,815,16,771,21,525,697,159,148,234,667,628,3,647,611,101,779,84,5,702,629,520,738,211,33,384,319,853,69,117,427,695,176,239,745,789,800,237,519,9,90,716,627,336,612,57,741,114,749,39,459,70,591,746,672,458,107,169,327,294,372,76,220,214,541,539,184,89,465,415,245,773,772,195,30,593,803,165,22,658,247,160,752,561,830,601,161,10,827,565,833,599,764,805,733,645,278,616,99,154,848,715,410,653,218,618,527,574,796,156,79,784,369,575,8,793,138,73,524,392,491,786,78,368,408,609,654,608,95,193,191,364,801,567,173,112,839,778,811,835,205,735,688,189,556,523,256,798,832,844,354,106,130,429,120,409,206,605,678,831,80,478,802,129,24,540,719,235,768,128,13,644,783,35,650,809,656,359,782,543,776,823,837,355,684,693,238,442,818,576,592,28,97,787,806,351,67,587,20,332,636,595,790,137,29,550,487,630,454,379,731,648,483,446,769,362,622,720,131,763,42,554,219,48,207,551,1,365,530,111,301,135,457,437,431,700,194,751,624,411,236,34,664,260,847,699,416,213,386,734,168,845,641,340,37,283,324,271,761,730,613,552,549,93,100,377,334,329,322,49,563,635,436,133,531,254,125,266,728,814,836,226,38,41,710,142,412,603,77,46,594,792,825,217,328,380,464,424,822,828,232,600,68,843,243,536,528,242,559,788,686,513,606,623,23,179,55,75,361,544,178,208,481,104,152,113,615,216,98,144,816,489,597,103,797,626,105,250,115,430,141,718,662,297,492,765,834,666,261,313,739,610,643,766,302,607,456,480,694,479,230,166,119,841,748,522,311,116,560,227,573,690,497,182,267,637,110,709,590,581,512,696,204,296,532,808,775,371,394,407,65,542,246,558,186,452,747,221,215,255,511,495,231,791,52,711,767,484,632,43,86,60,448,633,18,342,188,252,455,680,453,496,31,181,96,315,391,376,378,852,660,441,535,813,727,445,134,824,292,521,503,356,418,794,846,344,201,602,774,270,139,676,566,83,498,44,596,744,435,510,682,820,349,447,737,274,482,689,345,726,586,233,661,303,259,167,679,804,183,659,325,347,50,299,286,698,343,305,290,670,819,331,687,309,330,323,268,671,310,339,346,673,262,817,829,677,736,277,382,19,357,669,338,285,675,374,657,703,172,534,251,721,439,426,724,529,759,770))")
    
    queryResult <- dbGetQuery(jdbcCOnMESHR,getId)
    
  }
  
  
  
  queryResult[is.na(queryResult)] <- 0
  queryResult$Error <- NULL
  queryResultB <- queryResult
  queryResultS <- queryResult
  
  
  
  BuyerCheck <- 
    ifelse((queryResultB$IM_SPEC_MASTER_TYPE>2)&(queryResultB$OPTION_COUNT<2),queryResultB$Error <- "Error-Multi Select / Dropdown Type Specs with less than 2 option",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Approximate Order Value")&(queryResultB$IM_SPEC_MASTER_TYPE!=3),queryResultB$Error <- "Error-AOV is not Dropdown",
    ifelse((queryResultB$IM_SPEC_MASTER_TYPE==2)&(queryResultB$OPTION_COUNT<2),queryResultB$Error <- "Error- Radio Type spec with less than 2 option",
    ifelse((queryResultB$IM_SPEC_MASTER_BUYER_SELLER==1)&(queryResultB$IM_SPEC_OPT_BUYER_SELLER==2),queryResultB$Error <- "Error-Question and option flag mismatch",
    ifelse((queryResultB$IM_SPEC_MASTER_BUYER_SELLER==2)&(queryResultB$IM_SPEC_OPT_BUYER_SELLER==1),queryResultB$Error <- "Error-Question and option flag mismatch",
    ifelse((queryResultB$IM_SPEC_MASTER_TYPE==1)&(queryResultB$OPTION_COUNT>1),queryResultB$Error <- "Error-Text question with more than one option",
    ifelse((queryResultB$IM_SPEC_MASTER_TYPE>4),queryResultB$Error <- "Error-Wrong Question type flagging",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Quantity Unit")&(queryResultB$IM_SPEC_MASTER_TYPE!=3),queryResultB$Error <- "Error-Quantity Unit is not Dropdown",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Quantity Unit")&(queryResultB$IM_CAT_SPEC_PRIORITY!=-1),queryResultB$Error <- "Error- Quantity unit not at -1 Priority", 
    ifelse((queryResultB$IM_SPEC_MASTER_DESC!="Quantity Unit")&(queryResultB$IM_SPECIFICATION_OPT_PRIORITY==0),queryResultB$Error <- "Error- Zero Option priority except Quantity Unit on Buyer Side",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Quantity")&(queryResultB$IM_SPEC_MASTER_TYPE!=1),queryResultB$Error <- "Error-Quantity is not Text Type",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Quantity")&(queryResultB$IM_CAT_SPEC_PRIORITY!=-1),queryResultB$Error <- "Error- Quantity not at -1 Priority",
    ifelse((queryResultB$IM_SPEC_MASTER_DESC=="Why do you need this")&(queryResultB$IM_CAT_SPEC_PRIORITY!=999),queryResultB$Error <- "Error- WDYNT at other then 999 priority",queryResultB$Error <- "No Error")))))))))))))
    
  table(BuyerCheck)
  
  SellerCheck <- 
    ifelse((queryResultS$IM_SPEC_MASTER_TYPE>2)&(queryResultS$OPTION_COUNT<2),queryResultS$Error <- "Error- MultiSelect / Dropdown type specs with less than 3 option",
    ifelse((queryResultS$IM_SPEC_MASTER_DESC=="Approximate Order Value")&(queryResultS$IM_SPEC_MASTER_BUYER_SELLER=2),queryResultS$Error <-"Error-AOV at seller side",
    ifelse((queryResultS$IM_SPEC_MASTER_TYPE==2)&(queryResultS$OPTION_COUNT<2),queryResultS$Error <- "Error- Radio Type Spec with less than Two option",
    ifelse((queryResultS$IM_SPEC_MASTER_BUYER_SELLER==1)&(queryResultS$IM_SPEC_OPT_BUYER_SELLER==2),queryResultS$Error <- "Error-Question and Option flag mismatch",
    ifelse((queryResultS$IM_SPEC_MASTER_BUYER_SELLER==2)&(queryResultS$IM_SPEC_OPT_BUYER_SELLER==1),queryResultS$Error <- "Error-Question and Option flag mismatch",
    ifelse((queryResultS$IM_SPEC_MASTER_TYPE==1)&(queryResultS$OPTION_COUNT>1),queryResultS$Error <- "Error-Text question with more than one option",
    ifelse((queryResultS$IM_SPEC_MASTER_TYPE>4),queryResultS$Error <- "Error-Wrong Question type flagging",       
    ifelse((queryResultS$IM_SPEC_MASTER_DESC=="Quantity Unit")&(queryResultS$IM_SPEC_MASTER_BUYER_SELLER=2),queryResultS$Error <-"Error-Quantity Unit at seller side",
    ifelse((queryResultS$IM_SPEC_MASTER_DESC!="Quantity Unit")&(queryResultS$IM_SPECIFICATION_OPT_PRIORITY==0),queryResultS$Error <- "Error- Zero Option priority except Quantity Unit on Seller Side",
    ifelse((queryResultS$IM_SPEC_MASTER_DESC=="Quantity")&(queryResultS$IM_SPEC_MASTER_BUYER_SELLER=2),queryResultS$Error <-"Error-Quantity at seller side",       
    ifelse((queryResultS$IM_SPEC_MASTER_DESC=="Why do you need this")&(queryResultS$IM_CAT_SPEC_PRIORITY!=999),queryResultS$Error <- "Error-WDYNT Priority in not 999",
    ifelse((queryResultS$IM_CAT_SPEC_TYPE=="K")&(queryResultS$IM_SPEC_MASTER_TYPE>3),queryResultS$Error <- "Error-K is not Radio, Text and Dropdown",
    ifelse((queryResultS$IM_CAT_SPEC_TYPE=="C")&(queryResultS$IM_SPEC_MASTER_TYPE>3),queryResultS$Error <- "Error-C is not Radio, Text and Dropdown",
    ifelse((queryResultS$IM_SPEC_AFFIX_TYPE=="P")&(queryResultS$IM_SPEC_MASTER_TYPE>2),queryResultS$Error <-"Error-Prefix is not Radio and Text",
    ifelse((queryResultS$IM_SPEC_AFFIX_TYPE=="S")&(queryResultS$IM_SPEC_MASTER_TYPE>2),queryResultS$Error <-"Error-Suffix is not Radio and Text",
    ifelse((queryResultS$IM_CAT_SPEC_TYPE=="K")&(queryResultS$IM_SPEC_AFFIX_TYPE==0),queryResultS$Error <-"Error-No PS mark on Key",
    ifelse((queryResultS$IM_CAT_SPEC_TYPE=="C")&(queryResultS$IM_SPEC_AFFIX_TYPE==0),queryResultS$Error <-"Error-No PS mark on Config",queryResultB$Error <- "No Error")))))))))))))))))
    
  
  table(SellerCheck)
  
  BuyerCheck <- as.data.frame(BuyerCheck)
  SellerCheck <- as.data.frame(SellerCheck)
  
  
  queryResultBuyer<-cbind(queryResult,BuyerCheck)
  queryResultSeller<-cbind(queryResult,SellerCheck)
  
  queryResultBuyer$Error <- NULL
  queryResultSeller$Error <- NULL
  
  
  names(which(sapply(queryResultBuyer,anyNA)))
  names(which(sapply(queryResultSeller,anyNA)))
  
  buyer2 <- queryResultBuyer[queryResultBuyer$BuyerCheck!="No Error",]
  buyerFinal <- buyer2[buyer2$IM_SPEC_MASTER_BUYER_SELLER!=2,]
  buyerFinalUnique <- buyerFinal[!duplicated(buyerFinal[c("MCAT_ID","IM_SPEC_MASTER_ID")]),]
  
  seller2 <- queryResultSeller[queryResultSeller$SellerCheck!="No Error",]
  sellerFinal <- seller2[seller2$IM_SPEC_MASTER_BUYER_SELLER!=1,] # Seller side error
  sellerFinalUnique <- sellerFinal[!duplicated(sellerFinal[c("MCAT_ID","IM_SPEC_MASTER_ID")]),]
  #sellerFinalUnique$IM_SPEC_OPTIONS_DESC <- (trimws(sellerFinalUnique$IM_SPEC_OPTIONS_DESC, which = "both"))
  #selleranyother <-ifelse((sellerFinalUnique$SellerCheck=="Error-Question and Option flag mismatch") & (grepl("^Any$|^Other$",sellerFinalUnique$IM_SPEC_OPTIONS_DESC)),"Error-Question and Option flag mismatch w/o Any Other", "")
  #selleranyother <- as.data.frame(selleranyother)
  sellerFinalUnique <- cbind(sellerFinalUnique,selleranyother)

  
  write.xlsx(sellerFinalUnique,"C:/Users/imart/Desktop/R Folder/Seller Side Errors.xlsx",row.names = F)
  write.xlsx(buyerFinalUnique,"C:/Users/imart/Desktop/R Folder/Buyer Side Errors.xlsx",row.names = F)
  
  
  table(sellerFinal$SellerCheck)
  table(buyerFinal$BuyerCheck)
  