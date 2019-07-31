library(filesstrings)

pdflist <- list.files("C:/Users/IMART/Desktop/MukehPDF",pattern = ".pdf")
pdflist <- gsub("Unicheck_Report_","",pdflist)
pdflist <- gsub(".pdf","",pdflist)
pdflist <- as.data.frame(pdflist)
pdflist$Yes <- "Yes"


doclist <- list.files("C:/Users/IMART/Desktop/mukeshDoc",pattern = ".docx")
doclist <- gsub(".docx","",doclist)
doclist <- as.data.frame(doclist)


com <- merge(doclist,pdflist,by.x = "doclist",by.y="pdflist",all.x = T)

com[is.na(com)] <- "No"

com_no <- as.character(com[com$Yes=="No","doclist"])
com_yes <- as.character(com[com$Yes=="Yes","doclist"])

dir.create("C:/Users/IMART/Desktop/Palagrism/",showWarnings = T)
dir.create("C:/Users/IMART/Desktop/Palagrism/Matched",showWarnings = T)
dir.create("C:/Users/IMART/Desktop/Palagrism/Non_Matched",showWarnings = T)



#copying macthed file from the main folder
i=1
for (i in 1:length(com_yes)) {
  file.copy(paste0("C:/Users/IMART/Desktop/mukeshDoc/",com_yes[i],".docx"),"C:/Users/IMART/Desktop/Palagrism/Matched")  
}

#copying unmatched file from the main folder

j=1
for (j in 1:length(com_no)) {
  file.copy(paste0("C:/Users/IMART/Desktop/mukeshDoc/",com_no[j],".docx"),"C:/Users/IMART/Desktop/Palagrism/Non_Matched")  
}

