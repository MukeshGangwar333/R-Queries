library(stringr)
library(stringi)

options(scipen=999)
#df <- read.delim("C:/Users/mukesh kumar/Downloads/imageclassificationresults2.txt", header = F, sep = "")
df <- read.table("C:/Users/imart/Documents/Image Classification/28 Dec/pmcat_n122_e100_b30_nT1220.txt",sep = "\n")
V3 <- (data.frame(df$V1))
df2 <- cbind(df,V3)
df2$V1 <- as.character(df2$V1)
df2$df.V1 <- as.character(df2$df.V1)
vec2 <- seq(1,nrow(df2),by=7)
vec3 <- seq(2,nrow(df2),by=7)
vec4 <- seq(3,nrow(df2),by=7)
v3 <- c(vec2,vec3,vec4)
v3 <- sort(v3)
df3 <- data.frame(df[v3,])
df3$df.v3... <- as.character(df3$df.v3...)
url_1 <- seq(1,nrow(df3),by=3)
mcat_index <- seq(2,nrow(df3),by=3)
mcat_index2 <- seq(3,nrow(df3),by=3)
df3$v2 <- word(df3$df.v3...,2,sep = "\t")
df3$v3 <- word(df3$df.v3...,1,sep = "\t")
mcat1 <- df3$v2[mcat_index]
mcat2 <- df3$v2[mcat_index2]
 
prob1 <- df3$v3[mcat_index]
prob2 <- df3$v3[mcat_index2]
URL_original <- df3$df.v3...[url_1] 

final2 <- data.frame(cbind(URL_original,mcat1,prob1,mcat2,prob2))

final2$URL_original <- as.character(final2$URL_original)

final2$url1 <- sapply(strsplit(final2$URL_original,"train/"),"[",2)
final2$original_mcat <- sapply(strsplit(final2$url1,"/"),"[",1)
final2 <- final2[,c(1,7,2:6)]
write.csv(final2,"pmcat_n122_e100_b30_nT1220.csv",row.names = F,quote=F)

###END#####
 
