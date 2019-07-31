df <- read_excel("C:/Users/imart/Downloads/Taxonomy Related MCAT Data.xlsx", sheet = 1)

colnames(df)


df1 <- df[,c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41)]
df1[is.na(df1)] <- ""

new <- melt(df1,id.vars = "Keyword")
req_data <- new[!new$value=="", c(1,3)]


write.csv(req_data,"Required_data.csv", row.names = F)
