#' Information Value for an Independent Variable
#'
#' Calculate Information value for an input variable, given that the dependent variable is a Binary variable
#' Information Value can be used for variable reduction in Logistic Regression Based Predictive Model Development
#' A continuous/numeric variable is splitted into 20 buckets before calculating IV
#' A Character Variables are directly used for IV calculations
#' @param Needs to give a data frame as input, target variable and independent variable
#' @keywords Information Value for an Independent Variable
#' @export Output a data frame with the input variable name and its IV
#' @examples
#' Var_IV()
#' df1 <- data.frame(Target=as.integer(as.logical(rbinom(100,5,0.2))),
#'                   Age=as.integer(rnorm(100,45,15)),
#'                   Gender=rep(c("F","M"),50))
#' iv_age <-Var_IV(df1,"Target","Age")
#' iv_Gender <-Var_IV(df1,"Target","Gender")
#' View(iv_age)
#' View(iv_Gender)

Var_IV <- function (df,Target_Var, Ind_var){
  # All variables from a data frame
  allvars <- names(df)
  # Check If Target Variable Available
  try(if(!Target_Var %in% allvars ) stop("Input Target Variable is not available in the input data frame"))
  # Only Binary Targer
  try(if((length(unique(df[,Target_Var]))>2 | length(unique(df[,Target_Var]))<2 )) stop("Target Variable is not a binary"))
  # Check if Independent Variable is available
  try(if(!Ind_var %in% allvars ) stop("Input Independent Variable is not available in the input data frame"))
  Ivar <- df[,Ind_var]
  Tvar <- df[,Target_Var]
  # Check if Independent Variable is Categorical or Numeric
  if(is.numeric(Ivar)|is.integer(Ivar)){
    cutoffs <- unique(quantile(Ivar,na.rm = T,prob=c(seq(0,1,0.2))))
    Factor_Ivar <- factor(cut(Ivar,
                       breaks=cutoffs
                       ))
  }else if(is.character(Ivar) | is.factor(Ivar)){
    Factor_Ivar <- Ivar
    rm(Ivar)
  }else{
   stop("Independent Variable is not a numeric or character")
  }
  # Cross Tab between Target and Exploratory Variables
  t <-table(Factor_Ivar,Tvar)
  t1 <-prop.table(t,2)
  t2 <- as.matrix(t1) # convert to matrix
  l <-as.integer(dim(t2)[1]) # Find number of rows
  for (i in 1:l){
    if (i==1){
      iv=0
    }
    iv <- iv+ (t2[i,1]-t2[i,2])*log((0.00001+t2[i,1])/(0.00001+t2[i,2]))
  }
  iv <- data.frame(VarName=Ind_var, iv_val =iv)
}
