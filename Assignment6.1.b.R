library(stringr)
library(dplyr)
library(plyr)
library(data.table)

mainFunc<-function(){
  titanicDf<- read.csv("C:/Users/arunabhl/Documents/MyRFiles/titanic3.csv")
  tDf<-data.frame(cbind(sapply(titanicDf$name,function(x)  getTitle(x),simplify = T)))
  colnames(tDf)<-"TitlNm"
  ttlCnt<-count(tDf, "TitlNm")
  
  tDf$survived<-data.frame(cbind(titanicDf$survived))
  survivedCnt<- count(filter(tDf,survived==1), "TitlNm")
  
  tNsChrt<-merge(ttlCnt, survivedCnt, by.x="TitlNm", by.y="TitlNm", all.x=T)
  nChrt<-data.frame()
  
  for (i in 1:length(tNsChrt[,1])){
    if ( is.na(tNsChrt[i,3]) == T ) {
      tNsChrt[i,3] = 0
      nChrt[1,i]=tNsChrt[i,2]
      nChrt[2,i]=tNsChrt[i,3]
    }else{
      tNsChrt[i,2] = tNsChrt[i,2] - tNsChrt[i,3]
      nChrt[1,i]=tNsChrt[i,2]
      nChrt[2,i]=tNsChrt[i,3]
    }
  }
  
  bpChrt<-data.matrix(nChrt[1:50])  # considering 1st 50 record to get clear graph
  rownames(bpChrt)<-c("Dead","Survived")
  colnames(bpChrt)<-tNsChrt$TitlNm[1:50]
  View(bpChrt)
  
  barplot(bpChrt, col=c("Red","Green"), legend=rownames(bpChrt),main="Family Count, Dead and Survival Representation",ylab="No. of Family members",           xlab="Family Title")

}

getTitle <- function (x){
  if (str_detect(x,",") == T ) {
    cPtr <- str_locate(x,",")
    titleNm<-substr(x,1, cPtr-1)
    return (titleNm)
  }

}

mainFunc()
