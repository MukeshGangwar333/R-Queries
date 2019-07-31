library(RSelenium)
library(readxl)
library(stringr)



#dvr <-  remoteDriver(browser=c("chrome"),  extraCapabilities = eCaps ) 
driver <- rsDriver(browser=c("chrome"), chromever = "74.0.3729.6")   #starting selenium server for chrome browser
remDr <- driver$client


file_select <- choose.files()
Image_File <- read_excel(file_select)
Image_File$Dimension <- ""

i=1
for (i in 1: nrow(Image_File )) {
  a <- Image_File$`Image Url's`[i]
  remDr$navigate(a)
  
  image_title <- remDr$findElements( using = "xpath", "/html/head/title" )
  image_title_d <- unlist(image_title[[1]]$getTitle())
  image_title_d1 <- str_extract(image_title_d, " (.*?)$" )
  image_title_d1 <- trimws(image_title_d1)
  Image_File$Dimension[i] <- as.character(image_title_d1)
  }

driver$client$closeall()   #use to free port
driver$server$stop()       #use to free port



