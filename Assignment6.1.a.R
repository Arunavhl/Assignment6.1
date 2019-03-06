library(stringr)
library(dplyr)
library(plyr)

mainFunc<- function(){
  titanicDf<- read.csv("C:/Users/arunabhl/Documents/MyRFiles/titanic3.csv")
  tDF<- data.frame(cbind(sapply(titanicDf$name, function(x) getTitle(x),
                                simplify =T )))
  colnames(tDF)<- "titleName"
  ttlCnt<- count(tDF, "titleName")
  mCnt<- max(ttlCnt[,2])+1
  plot(ttlCnt,type="p",main="Family Title and Count Representation", ylab="No. of Family members", 
       xlab="Family Title", ylim=c(0,mCnt))
}


getTitle<- function(x){
  if(str_detect(x, ",")== T){
    cPtr<- str_locate(x,",")
    titleName<- substr(x,1,cPtr-1)

    return(titleName)
  }
}
mainFunc()

