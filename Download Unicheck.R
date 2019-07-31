library(RSelenium)


driver<- rsDriver(browser=c("chrome"), chromever = "73.0.3683.68")   #starting selenium server for chrome browser
remDr <- driver[["client"]]              #linking chrome browser
remDr$navigate("https://online2pdf.com/pdf2word")

EMail <- remDr$findElement(using = "css","[class='_1uOq0o']")   #selecting  email id block
EMail$sendKeysToElement(list("category_kp@indiamart.com"))      #inserting email id


Password <- remDr$findElement(using = "css","[type='password']")  #selecting pw block
Password$sendKeysToElement(list("dehradun@224"))                   #insert password

Sys.sleep(15)

login <- remDr$findElement(using = "css","[type='submit']")          #select submit button
login$clickElement()                                      #click on the submit button

Sys.sleep(15)


folder <- remDr$findElement(using = "css","[title='3rd Batch']")  #selecting block
folder$clickElement()                                            #click the selected block


#B1 <- remDr$findElement(using = "css","[title='360017406112']")         
#B1$clickElement()
#B2 <- remDr$findElement(using = "css","[class='cabinet-core-Button-___index__button___3Yv6Z cabinet-routes-viewer-viewerButtons-___viewerButtons__button___2EKxB cabinet-core-Button-___index__button--circle___1augV']")
#B2$clickElement()
#remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")       #scroll to end

pg <- remDr$findElement(using = "css","[class='cabinet-routes-library-File-file-actions-___file-actions__file-actions__item___GTy4A']")

#lenOfPage <-  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);var lenOfPage=document.body.scrollHeight;return lenOfPage;")

pg$clickElement()
pg$clickElement()

j=1
for (j in 1:100) {
  webElem <- remDr$findElement("css", "body") 
  webElem$sendKeysToElement(list(key="end"))
  webElem$sendKeysToElement(list(key="down_arrow"))
  webElem$sendKeysToElement(list(key="end"))
  webElem$sendKeysToElement(list(key="down_arrow"))
  webElem$sendKeysToElement(list(key="end"))}


UrlIndex <- remDr$findElements(using = "xpath","//*[@id='app']/div/div[1]/div[2]/div/div[4]/a")


Url <- remDr$getCurrentUrl()[[1]]

i=1
for (i in 2:length(UrlIndex)) {
  remDr$navigate(Url)
  
  pg <- remDr$findElement(using = "css","[class='cabinet-routes-library-File-file-actions-___file-actions__file-actions__item___GTy4A']")
  
  #lenOfPage <-  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);var lenOfPage=document.body.scrollHeight;return lenOfPage;")
  
  pg$clickElement()
  pg$clickElement()
  
  j=1
  for (j in 1:100) {
    webElem <- remDr$findElement("css", "body") 
    webElem$sendKeysToElement(list(key="end"))
    webElem$sendKeysToElement(list(key="down_arrow"))
    webElem$sendKeysToElement(list(key="end"))
    webElem$sendKeysToElement(list(key="down_arrow"))
    webElem$sendKeysToElement(list(key="end"))}
  
  A1 <- remDr$findElement(using = "xpath",paste0("//*[@id='app']/div/div[1]/div[2]/div/div[4]/a[",i,"]"))
  
  A1$clickElement()
  Sys.sleep(10)
 #ifelse(remDr$findElement(using = "css","['class=cabinet-routes-process-___index__process-tooltip_text___166Kc']"),A1$doubleclick(),A1$clickElement())
    B2 <- remDr$findElement(using = "css","[class='cabinet-core-Button-___index__button___3Yv6Z cabinet-routes-viewer-viewerButtons-___viewerButtons__button___2EKxB cabinet-core-Button-___index__button--circle___1augV']")
  B2$clickElement()
  
  Sys.sleep(10)
  
  
  tryCatch(remDr$findElement(using = "css","[class='cabinet-routes-process-___index__process-tooltip_text___166Kc']"), 
           error=function(e) ifelse(remDr$findElement(using = "css","[class='cabinet-routes-process-___index__process-tooltip_text___166Kc']")$getElementText() == "Done",B2$clickElement(),Sys.sleep(45)),
           finally = B2$clickElement() )
  

  remDr$navigate(Url)
  
  Sys.sleep(30)
  }
 


