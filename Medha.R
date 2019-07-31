df<-read_excel("C:/Users/imart/Downloads/Mukesh_super pmcat data.xlsx")
df$X__1<-NULL
df$X__2<-NULL
df$X__3<-NULL
df<-df[,c(3,1,2,4)]
i<-3
fix(df)
master_df<-data.frame(unique_word=character(),
                      combination=character())
unique_list<-unique(df$X1)
unique_list<-unique_list[!is.na(unique_list)]
i<-100
for( i in 1:length(unique_list))
{
  check<-data.frame(unique_word=character(),
                        combination=character())
  first_element<-unique_list[i]
  first_element<-paste0("^",first_element,"$")
  find_superpmcatindex<-grep(pattern =first_element ,x =df$`Super PMCAT`)
  if(length(find_superpmcatindex)!=0)
  {
    
  find_otherindex<-grep(pattern =first_element ,x =df$X1)
  j<-1
  for(j in 1:length(find_superpmcatindex))
  {
    k<-1
    for(k in 1:length(find_otherindex))
    {
      index<-find_superpmcatindex[j]
      index_other<-find_otherindex[k]
      unique_word=first_element
      if(is.na(df[index,c(4)]))
      {
        combination=paste0(df[index_other,c(3)])
        check<-rbind(check,data.frame(unique_word,combination))
      }
      if(!is.na(df[index,c(4)]))
      {
        combination=paste0(df[index_other,c(3)]," ",df[index,c(4)])
        check<-rbind(check,data.frame(unique_word,combination))
      }
      
    }
    
  }
}
  master_df<-rbind(master_df,check)
}
master_df1<-master_df
master_df$unique_word<-substring(master_df$unique_word, 2)

master_df$unique_word = substr(master_df$unique_word,1,nchar(master_df$unique_word)-1)
