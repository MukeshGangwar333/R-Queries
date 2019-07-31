options(java.parameters = "-Xmx12000m")
library(RJDBC)
library(readxl)
library(writexl)

chemical_names <- read_excel("c:/Users/imart/Documents/Drugs keywords for MCAT creation.xlsx", sheet = 1)
chemical_names$Keywords <- trimws(chemical_names$Keywords)

mcats <- read_excel("c:/Users/imart/Documents/Drugs keywords for MCAT creation.xlsx", sheet = 2)
mcats$MCAT_Name <- trimws(mcats$MCAT_Name)


##########################################

chemical_names$matched_mcats <- ""

x=1
for (x in 1:nrow(chemical_names)) {
  
  chemical_index <- paste0("\\b",chemical_names$Keywords[x],"\\b") 
  mcats_found <- mcats[grepl(chemical_index,mcats$MCAT_Name, ignore.case = T  ),  ]
  
  
  if (nrow(mcats_found)==0) {
    chemical_names$matched_mcats[x] <- ""
   }
  
  
  else if (nrow(mcats_found)==1) {
    chemical_names$matched_mcats[x] <- mcats_found$MCAT_Name
  }
  
 else {
   mcats_found$concat <-""
   mcats_found$concat[1] <- mcats_found$MCAT_Name[1]
   
   i=2
   for (i in 2:nrow(mcats_found)) {
     mcats_found$concat[i] <- paste(mcats_found$concat[i-1],mcats_found$MCAT_Name[i],sep = " | "   )
     
   }
   
   mcats_found <- mcats_found[nrow(mcats_found),  ]
   
   chemical_names$matched_mcats[x] <- mcats_found$concat
   
 }
   
  }

writexl::write_xlsx(chemical_names,"Final_Matched_Mcats.xlsx")