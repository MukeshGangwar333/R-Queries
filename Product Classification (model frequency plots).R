#reading text file having combined data of products and buy lead
file_names <- list.files("C:/Users/imart/Documents/Excavator/Super_Combined/MCAT_Parts/", pattern= "txt$" )
library(stringr)

i=1
for (i in 1:length(file_names)) {

  df <- read.table(paste0("C:/Users/imart/Documents/Excavator/Super_Combined/MCAT_Parts/", file_names[i]), sep = "\n"  )
  
  #df <- read.table("file:///C:/Users/imart/Documents/Excavator/Super_Combined/Super_PMCAT/Super_model_training.txt", sep = "\n"  )
  
  df1 <- df
  
  #formatting the data
  df1$label <- word(df1$V1, start=1, sep = " " )
  #df2 <- df1[grepl("3dx",df1$V1,ignore.case = T),]
  
  df1$label <- trimws(df1$label)
  
  df1$label <- gsub("__label__","", df1$label )
  df1$label <- gsub("_"," ", df1$label  )
  
  #Super_Training_File <- as.data.frame(table(df1$label))
  
  assign(paste0("MCAT_Parts","_",gsub("\\.txt","",file_names[i])), as.data.frame(table(df1$label)))

  rm(df,df1)
  
    
}


save.image("Plots_2.RData")
load("Plots.Rdata")

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")



p <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', mode = 'markers',
             marker = list(size = ~Gap, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))



p <- plot_ly(F_Data, x = ~Layer, y = ~Freq, text = ~Var1, type = 'scatter', mode = 'markers',
             marker = list(size = ~Dim, opacity = 1, color = 'rgb(255, 65, 54)')) %>%
  layout(title = 'Label Loading Frequency',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

p

Super_Training_File$Layer <- 1

PMCAT_Machines$Layer <- 2
PMCAT_Parts$Layer <- 2
Super_Training_File$Dim <- 50

PMCAT_Machines$Dim <- 25
PMCAT_Parts$Dim <- 25

Third_layer <- rbind(MCAT_Machinery_Backhoe_Loader,MCAT_Machinery_Bulldozer,MCAT_Machinery_End_Loader,MCAT_Machinery_Excavator,MCAT_Machinery_Motor_Grader,MCAT_Machinery_Road_Roller,MCAT_Machinery_Skid_Steer_Loaders,MCAT_Machinery_Trencher,MCAT_Machinery_Wheel_Loaders,MCAT_Parts_Earthmoving_Bucket,MCAT_Parts_Earthmoving_Machinery_Parts,MCAT_Parts_Excavator_Parts)
Third_layer$Layer <- 3
Third_layer$Dim <- 8
F_Data <- rbind(Super_Training_File,PMCAT_Machines,PMCAT_Parts,Third_layer)
Layer1 <- F_Data[F_Data$Layer==1, ]
Layer2 <- F_Data[F_Data$Layer==2, ]
Layer3 <- F_Data[F_Data$Layer==3, ]




map1 <- ggplot(Layer1) + geom_point(aes(x=Var1, y=Freq, color=Layer)) + geom_smooth(aes(x=Var1, y=Freq, color=Layer))
plotly::ggplotly(map1)

map2 <- ggplot(Layer2) + geom_point(aes(x=Var1, y=Freq, color=Layer)) + geom_smooth(aes(x=Var1, y=Freq, color=Layer))
plotly::ggplotly(map2)

map3 <- ggplot(Layer3) + geom_point(aes(x=Var1, y=Freq, color=Layer)) + geom_smooth(aes(x=Var1, y=Freq, color=Layer))
plotly::ggplotly(map3)

#api_create(map1, filename = "Layer-1")




df1$label <- gsub("Wheel Loaders", "Wheel Loader", df1$label  )

df1$V1 <- gsub("__label__Wheel_Loaders", "__label__Wheel_Loader", df1$V1  )
unique(df1$label )

df2 <- df1[df1$label!="Bucket & Rock Breaker for Excavator and Backhoe Loader" & df1$label!="Used & Second Hand Backhoe Loader and Excavator",]

unique(df2$label )

#taking the name of super pmcats of the all the pmcats
pmcats <- as.data.frame(unique(df2$label))
write.csv(pmcats,"pmcats.csv", row.names = F)
pmcats <- read.csv("C:/Users/imart/Documents/Excavator/Super_Combined/pmcats to superpmcats.csv")

#mapping the name of super pmcats to the pmcat
df3 <- merge(df2,pmcats, by.x = "label", by.y = "pmcats", all.x = T)

df3$stri <- ""
df3$stri <- sub("__label__[a-zA-Z_]+ ","",df3$V1)

#sub("__label__[a-zA-Z_]+ ", "","__label__Earthmoving_Bucket excavator bucket unit model")

df3$stri <- gsub("label", "", df3$stri )

df3$super_pmcats <- as.character(df3$super_pmcats)
str(df3)

df3$super_label <- paste0("__label__", gsub(" ","_", df3$super_pmcats)," ", df3$stri  )
df3$super_label[1]

colnames(df3)[colnames(df3)=="label" ] <- "pmcats"
colnames(df3)[colnames(df3)=="V1" ] <- "pmcats_label"
colnames(df3)[colnames(df3)=="stri" ] <- "string"

table(df3$super_pmcats)



###label loading#####

df3_labelled <- df3[df3$super_pmcats=="Earthmoving Machinery",  ]

index <- df3[df3$super_pmcats== "Earthmoving Machinery Parts", ]
data_multi <- index[rep(seq_len(nrow(index)), each=5     ),   ]
df3_labelled <- rbind(df3_labelled,data_multi )

table(df3_labelled$super_pmcats)
#df3_labelled <- readxl::read_excel("C:/Users/imart/Documents/Excavator/Super_Combined/super_labelled.xlsx")
write.csv(df3_labelled,"C:/Users/imart/Documents/Excavator/Super_Combined/super_labelled.csv",row.names = F)
write.table(df3_labelled$super_label , "C:/Users/imart/Documents/Excavator/Super_Combined/super_train_labelled.txt", row.names = F, col.names = F,quote = F )

write.csv(df3, "C:/Users/imart/Documents/Excavator/Combined Files_New_Previous/PMCAT/Combined_pmcat_data.csv", row.names = F )
write.table(df3$pmcats_label, "C:/Users/imart/Documents/Excavator/Combined Files_New_Previous/PMCAT/pmcat_training.txt", row.names = F, col.names = F,quote = F )


###creating text file for earthmoving machinery parts layer 2

df4 <- df3[df3$super_pmcats=="Earthmoving Machinery Parts",]
table(df4$pmcats)

write.table(df4$pmcats_label,"C:/Users/imart/Documents/Excavator/Previous Files (Ashutosh)/Bins/pmcat_jcbmodelV6.bin",row.names = F,col.names = F,quote=F)


data = ocr("https://5.imimg.com/data5/BD/XN/MY-2/diclofenac-pot-50mg-para-325mg-250x250.jpg")

