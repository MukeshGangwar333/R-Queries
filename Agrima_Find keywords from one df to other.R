main_df <-  readxl::read_excel("C:/Users/imart/Downloads/for match.xlsx", sheet = 1)
model_brand <- readxl::read_excel("C:/Users/imart/Downloads/for match.xlsx", sheet = 2)

my_data <- data.frame( PRIME_MCAT_ID=numeric(),
                       FK_GLCAT_MCAT_ID=numeric(),
                       PC_ITEM_ID=numeric(),
                       Prodname_desc_option=character(),
                       brand=character(),
                       model=character())

i=1
for (i in 1:nrow(model_brand)) {
data <- main_df[(grepl(model_brand$Brand[i], main_df$OFRTITLE_MASTERDES_OPTIONDESC, ignore.case = T) & grepl(model_brand$Model_new[i], main_df$OFRTITLE_MASTERDES_OPTIONDESC, ignore.case = T ) ) , ]  
if (nrow(data)>0) {
  data$brand <- model_brand$Brand[i]
  data$model <- model_brand$Model_new[i]
  my_data <- rbind(my_data,data)
  
} 
}


write.csv(my_data,"Final_Data(Bl Printers Match).csv", row.names = F)


getwd()
