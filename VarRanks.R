# create ranks variable

ranks <- function(df,var,nranks=10){
  nrows <- nrow(df)
  rankObs <-as.integer(nrows/nranks)
  rankVar <-paste("rank",var,sep="_")
  
  # Keep original sequence
  df1 <-data.frame("sno"=seq(1,nrow(df),by=1),"xvar"=df[,var])
  ordIndex <- order(df1[,2])
  # Sorted 
  df2 <- df1[ordIndex,]
  print(df1[1:15,])
  for(i in 1:nrow(df)){
    if (i==1){
      x=1
      y=df2[,"xvar"][i]
      rnk=1
    }
    if(x< nobs){
      df2[i,rankVar]<- rnk
      y<-df2[,"xvar"][i]
      x<-x+1
    }else{
      rnk<- rnk+1
      x<- 1
      df2[i,rankVar]<- rnk
      y<-df2[,"xvar"][i]
    }
  }
  # output but with original index
  
  df <- merge(df1,df2[,c("sno",rankVar)], by = "sno")
}

