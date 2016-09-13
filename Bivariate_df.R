#' Bivariate Summary Table
#'
#' Gives a bivariate count for event and non event for a given binary variable and a continuous variable.
#' It takes a data frame, target variable(with two levels), continuous variable and number of bins to be created
#' for the continuous variable before calculating WOE and other statistics
#' @param Needs to give a data frame as input.
#' @keywords WOE for a Binary Response Variable and a continuous variable
#' @export
#' @examples
#' BiContVar()
#' df1 <- data.frame(Target=as.integer(as.logical(rbinom(100,5,0.2))),Age=as.integer(rnorm(100,45,15)))
#' Bivar <-Bivariate_df(df1,"Target")
#' View(Bivar)
#'
Bivariate_df<- function(df,target,idvar=NULL){
  # All variables from a data frame
  allvars <- names(df)
  # Exclude ID and Target Variable
  try(if(!target %in% allvars ) stop("Input Target Variable not available in the input data frame"))
  # Only Binary targer
  try(if((length(unique(df[,target]))>2 | length(unique(df[,target]))<2 )) stop("Target Variable is not a binary"))
  # Exclude ID and Target Variables
  if(length(idvar)>0){
    allvars1 <- allvars[!allvars %in% c(idvar,target)]
  }else{
    allvars1 <- allvars[!allvars %in% c(target)]
  }
  # Try to get summary Statistics
  ii <- 0
  tempdf <- data.frame()
  for(v in allvars1){
    # Check class
    cl<-class(df[,v])
    # Count distinct values
    uni <- length(unique(df[,v]))
    if(uni>1){
      if(uni>10 & cl %in% c("numeric","integer")){
        # Get Cut off Points
        points<-unique(quantile(df[,v], seq(0,1,0.1)))
        # Create Labels
        p1 <- points[1:length(points)-1]
        p2 <- points[2:length(points)]
        label1 <- paste(p1,p2,sep="-")

        # Create Categorical Variables
        v.cat <- cut(df[,v],unique(quantile(df[,v], seq(0,1,0.1))),label1)
        # Summarize
        t <- table(v.cat,df[,target])
        v.sm <- data.frame(VarName=rep(v,length(row.names(t))),
                           VarGroups=row.names(t),
                           Good=t[,1],
                           Bad=t[,2],
                           Pct_R_Good=paste(round(t[,1]*100/(t[,1]+t[,2]),2),"%",sep=""),
                           Pct_R_Bad=paste(round(t[,2]*100/ (t[,1]+t[,2]),2),"%",sep=""))
        names(v.sm) <- c("Variable","VarGroup","Count:Good","Count:Bad","%:Good","%:Bad")
      }else{
        # Summarize
        t <- table(df[,v],df[,target])
        v.sm <- data.frame(VarName=rep(v,length(row.names(t))),
                           VarGroups=row.names(t),
                           Good=t[,1],
                           Bad=t[,2],
                           Pct_R_Good=paste(round(t[,1]*100/(t[,1]+t[,2]),2),"%",sep=""),
                           Pct_R_Bad=paste(round(t[,2]*100/(t[,1]+t[,2]),2),"%",sep=""))
        names(v.sm) <- c("Variable","VarGroup","Count:Good","Count:Bad","%:Good","%:Bad")
        row.names(v.sm) <- NULL
      }

      ii<- ii+1
      if(ii==1){
        tempdf <-v.sm
      }else{
        tempdf <- rbind(v.sm,tempdf)
      }

    }#Numeric Condition

  }# Multiple Level requirement


  tempdf
}

