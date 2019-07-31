folder_path <- paste0("C:/Users/imart/Downloads/Washing Machine Images/")
folder_index <- list.files("C:/Users/imart/Downloads/Washing Machine Images")
folder_index <- order(folder_index)


i=1

for (i in 1:length(folder_index)) {
  file_path <- paste0(folder_path,folder_index[i],"/")
  #file_index <- list.files(paste0(folder_path,folder_index[i])) 
  file_index <- list.files(file_path)
  
  j=1
  for (j in 1:length(file_index)) {
    file.rename(paste0(file_path,file_index[j]),paste0(file_path,folder_index[i],"_",file_index[j])) 
  
    file.copy(paste0(file_path,folder_index[i],"_",file_index[j]), "C:/Users/imart/Downloads/New" )
    
    }
  

}





