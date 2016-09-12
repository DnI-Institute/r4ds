#' A Quantiles Summary
#'
#' A quantiles summary for each of the numeric column of an input data frame
#' @param Needs to give a data frame as input.
#' @keywords Summary Quantiles gives 0%, 1%,5%,10%, 25%, 50%,75%, 90%,95%,99% and 100% quantiles
#' @export
#' @examples
#' quantile_df()
#' df1 <- data.frame(a=rbinom(100,5,0.2),b=as.integer(rnorm(100,10,2)))
#' qn.df <-quantile_df(df1)
#' View(qn.df)
#'
quantile_df <-function(df){
  # Check input is data frame and have at least one observation
  VarNames <- names(df)
  # Check if a variable in numeric
  VarNames <- VarNames[sapply(df,function(x) is.numeric(x))]
  # Calculate Quantiles for each variable
  for(v in 1:length(VarNames) ){
    var <- VarNames[v]
    # Check if a variable in numeric
    if(is.numeric(df[,var])){
      df.q <- data.frame(quantile(df[,var],
                                  probs = c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),
                                  na.rm = T))


      if(v==1){
        # Add row ID and quantile labels
        df.q <- data.frame(ID=1:nrow(df.q),
                           Quantile=row.names(df.q),
                           df.q[,1])
        names(df.q) <- c("ID","Quantile",paste(var,"Statistics",sep="_"))
        out.df <- df.q
      }else{
        # Add row ID and quantile labels
        df.q <- data.frame(ID=1:nrow(df.q),
                           df.q[,1])
        names(df.q) <- c("ID",paste(var,"Statistics",sep=""))
        out.df <- merge(out.df,
                        df.q,
                        by="ID")
      }
    }# end if numeric condition
  }# end of for loop on each variable
  out.df
}
