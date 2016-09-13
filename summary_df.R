#' A Summary Statistics Function
#'
#' A Summary Statistics Function to get into a data frame. Gives missing count and %
#' @param Needs to give a data frame as input.
#' @keywords cats
#' @export
#' @examples
#' summary_df()
#' df1 <- data.frame(a=(1:5),b=rnorm(5,10,2))
#' sm.df <-summary_df(df1)
#' View(sm.df)
summary_df <-function(in.df){
  varNames <- names(in.df)
  varClass <- sapply(in.df,class)
  MissingCount <- sapply(in.df, function(x) sum(is.na(x)))
  MissingPct <- sapply(in.df, function(x) sum(is.na(x))/length(x))
  VarMean <- sapply(in.df, function(x) ifelse(is.numeric(x),mean(x,na.rm =T),NA))
  VarMedian <- sapply(in.df, function(x) ifelse(is.numeric(x),median(x,na.rm =T),NA))
  VarStdDev <- sapply(in.df, function(x) ifelse(is.numeric(x),sd(x,na.rm =T),NA))
  outDf <- data.frame(varNames,
                      varClass,
                      VarMean,
                      VarMedian,
                      VarStdDev,
                      MissingCount,
                      MissingPct)
  row.names(outDf) <- NULL
  outDf
}




