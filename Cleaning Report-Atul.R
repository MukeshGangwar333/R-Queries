library(RSelenium)
library(readxl)
library(RJDBC)
library(lubridate)
library(data.table)
library(stringr)

setwd("C:/Users/imart/Documents/MCat_Cleaning")
getwd()

# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/imart/Documents/MCat_Cleaning/ojdbc6.jar")
#jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")
##create MESHR connection to DB
jdbcCOnMESHR <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//ora4-dl.intermesh.net/mesh","indiamart","ora926mesh)%")

eCaps <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 2,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = "C:/Users/imart/Documents/MCat_Cleaning/"
    )
    )
)

#eCaps <-  RSelenium::getChromeProfile("c:/Users/imart/Documents/MCat_Cleaning/" ,"Profile 1")


#driver$client$closeall()   #use to free port
#driver$server$stop()       #use to free port

#dvr <-  remoteDriver(browser=c("chrome"),  extraCapabilities = eCaps ) 
driver <- rsDriver(browser=c("chrome"), chromever = "74.0.3729.6",extraCapabilities = eCaps)   #starting selenium server for chrome browser
remDr <- driver$client
remDr$navigate("http://gladmin.intermesh.net/")



Signin <- remDr$findElement( using = "css", "[Class='btn btn-danger btn-block']")
Signin$clickElement()

Email <- remDr$findElement(using = "css", "[Class='whsOnd zHQkBf' ]" )
Email$sendKeysToElement(list("mukeshkumar@indiamart.com"))

nextt <- remDr$findElement(using = "css","[Class='RveJvd snByac']")
nextt$clickElement()

Sys.sleep(5)


pw <- remDr$findElement(using = "css","[Class='whsOnd zHQkBf']")
pw$sendKeysToElement(list("mukku333"))

nxt <- remDr$findElement(using= "css","[Class='RveJvd snByac']" )
nxt$clickElement()

Sys.sleep(15)

mng_mktplc <- remDr$findElement(using= "xpath","//*[@id='container']/div[2]" )
mng_mktplc$clickElement()



mcat_clean <- remDr$findElement( using = "link text", "MCAT Cleaning" )
mcat_clean$clickElement()

remDr$switchToFrame("frame1")

audit_mis <- remDr$findElement(using = "xpath", "//*[@id='menu3697']/a" )
audit_mis$clickElement()


remDr$switchToFrame("mainframe")


mcatwise <- remDr$findElements(using = "xpath", "//*[@id='rType']" )
mcatwise[[2]]$clickElement()

mcats <- read_excel("c:/Users/imart/Documents/MCat_Cleaning/mcats.xlsx",sheet = 1)
Report <- read_excel("c:/Users/imart/Documents/MCat_Cleaning/mcats.xlsx",sheet = 2, trim_ws = F)
Report[is.na(Report)] <- ""
mcats$MCAT <- trimws(mcats$MCAT)



j=1
for (j in 1:nrow(mcats)) {

    mcat_name_search_box <- remDr$findElement(using = "id","mcat_name")
    mcat_name_search_box$clearElement()
    mcat_name_search_box$sendKeysToElement(list(mcats$MCAT[j]))

    Sys.sleep(5)    
    
  
    ser <- remDr$findElements(using = "xpath", "//*[@id='tt']" )
    ser1 <- unlist(ser[[1]]$getElementText())
    ser2 <- unlist(strsplit(ser1,split = "\n"))
    ser3 <- grep(paste0("\\b",mcats$MCAT[j],"\\b") ,ser2,ignore.case = T )
    
    search_result <- remDr$findElements(using = "xpath", paste0("//*[@id='tt']/div[",ser3,"]"))
    search_result[[1]]$clickElement()
    
    button <- remDr$findElement( using = "xpath", "/html/body/form/table/tbody/tr[3]/td/input")
    button$clickElement()
        
    Sys.sleep(7)
       
      
    report_data <- remDr$findElement(using = "xpath", "/html/body/table/tbody")
    report_data1 <- unlist(report_data$getElementText())
    #report_data2 <- report_data1[[1]][1]
    report_data3 <- str_extract(report_data1,"SUM.+" )
    report_data4 <- unlist(stringr::str_split(report_data3, " "))
        
    #category
    Report$Data[1] <- mcats$MCAT[j]
        
    #mcat worked
    Report$Data[5] <- 1
        
    #product processes
    Report$Data[6] <- report_data4[3]
       
    #product_worked
    Report$Data[7] <- report_data4[5]
        
    report_data5 <- length(unlist(stringr::str_split(report_data1, "\n")))
        
    issue_type <- remDr$findElement(using = "xpath", paste0("/html/body/table/tbody/tr[",report_data5, "]/td[5]/a"))
    issue_type$clickElement()
        
    Sys.sleep(5)
        
  
    #get a vector of all filenames
    files <- list.files(path="c:/Users/imart/Documents/MCat_Cleaning/",pattern="\\b.xls\\b",full.names = T,recursive = TRUE)
    
    #get the directory names of these (for grouping)
    dirs <- dirname(files)
    
    #File last modification time
    
    lastfiles <- as.character(tapply(files,dirs,function(v) v[which.max(file.mtime(v))]))
    

    issue_type1 <- data.table::fread(lastfiles)
    Report$Data[10] <- nrow(issue_type1[issue_type1$CHANGED_ATTRIBUTE=="PC_ITEM_GLCAT_MCAT_NAME_LIST", ])
        
        
    Rejected <- remDr$findElement(using = "xpath", paste0("/html/body/table/tbody/tr[", report_data5,"]/td[7]/b" )  )
        
    rr <- as.numeric(unlist(Rejected$getElementText()))
    #rr1 <- as.numeric(rr[[1]][1])  
        
        if (rr != 0) {
          
          datee <- remDr$findElement( using = "xpath", paste0("/html/body/table/tbody/tr[",report_data5-1,"]/td[4]/a"))
          datee$clickElement()
          Sys.sleep(5)
          
          datee1 <- remDr$findElement( using = "xpath", "//*[@id='empData']/table/tbody/tr[2]/td[1]")
          datee2 <- unlist(datee1$getElementText())
          #datee3 <- datee2[[1]][1]
          #dat4 <- dmy(dat3 )
          
          
          query <- paste0("Select fk_pc_item_id, pc_item_rejection_reason, emp_name, pc_item_status_approval, emp_id, trunc_audit_dt 
from pc_item_audit_report_arch 
where trunc_audit_dt = ","'", datee2,"'", "
and im_emp_id = 14667 
and pc_item_status_approval in (5, 50) 
and lower(EMP_NAME) like '%dexter%'")
          
          my_data <- dbGetQuery(jdbcCOnMESHR, query )
          
          
          Report$Data[12] <- nrow(my_data[my_data$PC_ITEM_REJECTION_REASON=="Product Name not clear", ]) 
          Report$Data[13] <- nrow(my_data[my_data$PC_ITEM_REJECTION_REASON=="Duplicate Product", ]) 
          Report$Data[14] <- nrow(my_data[my_data$PC_ITEM_REJECTION_REASON=="Product image mismatch", ]) 
          Report$Data[15] <- nrow(my_data[!my_data$PC_ITEM_REJECTION_REASON== "Product Name not clear" | my_data$PC_ITEM_REJECTION_REASON== "Duplicate Product"   | my_data$PC_ITEM_REJECTION_REASON== "Product image mismatch"  , ] )
          
          
          
        }
    
    else
      
    {
      Report$Data[12] <- 0
      Report$Data[13] <- 0
      Report$Data[14] <- 0
      Report$Data[15] <- 0
   
    }
   
    assign(gsub(" ","_",paste0("Report","_",mcats$MCAT[j]),ignore.case = T) , Report )
    
    Report$Data <- ""
    
    
    rm(mcat_name_search_box,ser,ser1,ser2,ser3,search_result,button,report_data,report_data1,report_data2,report_data3,report_data4,report_data5,issue_type1,Rejected,rr,datee,datee1,datee2)
    
        
      }
      
     

      last_files <- ls()
      last_files <- last_files[grep("Report_",last_files, ignore.case = T )]
      #last_files <- gsub(" ","_", last_files)
      
      k=1

      for (k in 1: length(last_files)) {
        
        
        write.csv(get(last_files[k]), paste0(last_files[k],".csv"  ), row.names = F, quote = T )
        
       
      }
      
      
    