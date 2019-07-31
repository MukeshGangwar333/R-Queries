options(java.parameters = "-Xmx12000m")
library(RJDBC)
library(readxl)
library(writexl)

chemical_names <- read_excel("c:/Users/imart/Documents/R Name Match/1404.xlsx", sheet = 1)
chemical_names$Keywords <- trimws(chemical_names$Keywords)

mcats <- read_excel("c:/Users/imart/Documents/R Name Match/packaging Mcat with Alt names.xlsx", sheet = 1)
mcats$MCAT_Name <- trimws(mcats$MCAT_Name)

################################### MATCH MCAT NAMES #####################################

chemical_names$matched_mcats <- ""

x=1
for (x in 1:nrow(chemical_names)) {
  
  chemical_index <- paste0(chemical_names$Keywords[x]) 
  mcats_found <- mcats[grepl(chemical_index,mcats$MCAT_Name, ignore.case = T  ),  ]
  
  
  if (nrow(mcats_found)==0) {
    chemical_names$matched_mcats[x] <- ""
  }
  
  
  else if (nrow(mcats_found)==1) {
    mcats_found$MCAT_Name <- as.character(mcats_found$MCAT_Name)
	chemical_names$matched_mcats[x] <- mcats_found$MCAT_Name
  }
  
  else {
    mcats_found$concat <-""
    mcats_found$MCAT_Name <- as.character(mcats_found$MCAT_Name)
	mcats_found$concat[1] <- mcats_found$MCAT_Name[1]
    
    i=2
    for (i in 2:nrow(mcats_found)) {
      mcats_found$concat[i] <- paste(mcats_found$concat[i-1],mcats_found$MCAT_Name[i],sep = " | "   )
      
    }
    
    mcats_found <- mcats_found[nrow(mcats_found),  ]
    
    chemical_names$matched_mcats[x] <- mcats_found$concat
    
  }
  
}


write_xlsx(chemical_names,"Chemical_Names_MCAT.xlsx")

################################### MATCH ALT MCAT NAMES ##########################################################################################################

alt_names <- read_excel("c:/Users/imart/Documents/R Name Match/packaging Mcat with Alt names.xlsx", sheet = 2)

chemical_names$alt_mcat_names <- ""
chemical_names$from_which_alt_mcat <- ""

y=1
for (y in 1:nrow(chemical_names)) {
  
  chemical_index <- paste0(chemical_names$Keywords[y]) 
  alt_mcats_found <- alt_names[grepl(chemical_index,alt_names$ALT_NAME, ignore.case = T  ),  ]
  
  
  if (nrow(alt_mcats_found)==0) {
    chemical_names$alt_mcat_names[y] <- ""
    chemical_names$from_which_alt_mcat[y] <- ""
  }
  
  
  else if (nrow(alt_mcats_found)==1) {
    alt_mcats_found$MCAT_Name <- as.character(alt_mcats_found$MCAT_Name)
    alt_mcats_found$ALT_NAME <- as.character(alt_mcats_found$ALT_NAME)
    chemical_names$alt_mcat_names[y] <- alt_mcats_found$MCAT_Name
    chemical_names$from_which_alt_mcat[y] <- alt_mcats_found$ALT_NAME
  }
  
  else {
    alt_mcats_found$m_concat <-""
    alt_mcats_found$a_concat <- ""
	alt_mcats_found$MCAT_Name <- as.character(alt_mcats_found$MCAT_Name)
    alt_mcats_found$ALT_NAME <- as.character(alt_mcats_found$ALT_NAME)
	alt_mcats_found$m_concat[1] <- alt_mcats_found$MCAT_Name[1]
    alt_mcats_found$a_concat[1] <- alt_mcats_found$ALT_NAME[1]
    
    j=2
    for (i in 2:nrow(alt_mcats_found)) {
      alt_mcats_found$m_concat[j] <- paste(alt_mcats_found$m_concat[j-1],alt_mcats_found$MCAT_Name[j],sep = " | "   )
      alt_mcats_found$a_concat[j] <- paste(alt_mcats_found$a_concat[j-1],alt_mcats_found$ALT_NAME[j], sep = " | "  )
      
    }
    
    alt_mcats_found <- alt_mcats_found[nrow(alt_mcats_found),  ]
    
    chemical_names$alt_mcat_names[y] <- alt_mcats_found$m_concat
    chemical_names$from_which_alt_mcat[y] <- alt_mcats_found$a_concat
    
  }
  
}

write_xlsx(chemical_names,"Chemical_Names_MCAT_AND_ALT_MCAT_NAMES.xlsx")
