#' Information Value for all variables of a data frame
#'
#' For a given data frame and a binary dependent variable, it calculate IV values and gives a output data frame
#' Information Value can be used for variable reduction in Logistic Regression Based Predictive Model Development
#' A continuous/numeric variable is splitted into 20 buckets before calculating IV
#' Character Variables are directly used for IV calculations
#' @param Needs to give a data frame as input, target variable and independent variable
#' @keywords Information Value for an Independent Variable
#' @export GIves an output data frame with a list of variables and respective IVs
#' @examples
#' BiContVar()
#' df1 <- data.frame(Target=as.integer(as.logical(rbinom(100,5,0.2))),
#'                   Age=as.integer(rnorm(100,45,15)),
#'                   Gender=rep(c("F","M"),50))
#' ivs <- df_iv(df1,"Target")
#' View(ivs)
#'

df_iv <- function(df,Target_Var, exclude_vars=NULL){
  # All variables from a data frame
  allvars <- names(df)
  # Exclude ID and Target Variable
  try(if(!Target_Var %in% allvars ) stop("Input Target Variable not available in the input data frame"))
  # Only Binary targer
  try(if((length(unique(df[,Target_Var]))>2 | length(unique(df[,Target_Var]))<2 )) stop("Target Variable is not a binary"))
  # Exclude ID and Target Variables
  if(length(exclude_vars)>0){
    allvars1 <- allvars[!allvars %in% c(exclude_vars,Target_Var)]
  }else{
    allvars1 <- allvars[!allvars %in% c(Target_Var)]
  }
  cnt <-0
  outDF <- data.frame()
  for (v in allvars1){
    cnt<-cnt+1
    temp.df <- Var_IV(df,Target_Var,v)
    if(cnt==1){
      outDF <- temp.df
    }else{
      outDF <- rbind(outDF,temp.df)
    }
  }
  outDF
}
