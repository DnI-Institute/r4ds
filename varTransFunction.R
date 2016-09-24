## Create Transformation Variable

TransVars <-function(df,exclude){
  # Variables
  VarNames <- names(df)
  # Check if a variable in numeric
  VarNames <- VarNames[sapply(df,function(x) is.numeric(x))]
  # Exclude variables
  if(length(exclude)>0){
    VarNames <- VarNames[!VarNames %in% exclude]
  }
  # create transformation variable for each input variable
  for (v in VarNames){
    # Log Transformation
    df[,paste("log",v,sep="_")] <- log(df[,v]+(1+abs(min(df[,v]))))
    df[,paste("sqrt",v,sep="_")] <- sqrt(df[,v]+(1+abs(min(df[,v]))))
    df[,paste("exp",v,sep="_")] <- exp(df[,v]/max(df[,v]))
    df[,paste("sqr",v,sep="_")] <- (df[,v])*(df[,v])
  }
  df
}

#df<-TransVars(df,c("A","T"))



