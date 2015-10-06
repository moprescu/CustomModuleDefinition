#library(clipr)

merge2Datasets <- function(dataset1, dataset2, swap = FALSE){
  library(clipr)
  if(swap){
    return(rbind(dataset2, dataset1))
  }
  else{
    return(rbind(dataset1, dataset2))
  }
}

merge3Datasets <- function(dataset1, dataset2, dataset3){
  return(rbind(rbind(dataset1, dataset2), dataset3))
}