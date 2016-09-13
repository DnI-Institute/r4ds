#' Bivariate Weight of Evidence
#'
#' Gives a bivariate summary with Weight of Evidence (WOE) for Binary Target and a continuous variable.
#' It takes a data frame, target variable(with two levels), continuous variable and number of bins to be created
#' for the continuous variable before calculating WOE and other statistics
#' @param Needs to give a data frame as input.
#' @keywords WOE for a Binary Response Variable and a continuous variable
#' @export
#' @examples
#' BiContVar()
#' df1 <- data.frame(Target=as.integer(as.logical(rbinom(100,5,0.2))),Age=as.integer(rnorm(100,45,15)))
#' bi.woe.df <-BiContVar(df1,"Target","Age",20)
#' View(bi.woe.df)
#'
BiContVar <- function(df,targetVar,contVar,groups  ){
  # Create copy of input data frame
  df.temp <- df
  #Get quantiles between 0 and 1
  varVect <- round(df.temp[,contVar],2)
  cutpoints<-unique(quantile(varVect,(0:groups)/groups))
  rm(varVect)
  # Groups are relevant for at least 3 cut offs
  if (length(cutpoints)>2){
    # Add a new variables to the data frame

    df.temp[,"Group"]<-cut(df.temp[,contVar], cutpoints, include.lowest=TRUE)

    # get cross tabl
    cross.tab <-table(df.temp[,"Group"],
                      df.temp[,targetVar])
    # Convert to matrix for getting columns
    cross.tab.mat <- as.matrix(cross.tab)
    # delete any row with sum zero
    cross.tab.mat<-cross.tab.mat[!rowSums(cross.tab.mat) == 0, ]

    cross.tab.col <-as.matrix(prop.table(cross.tab,2))
    cross.tab.col<-cross.tab.col[!rowSums(cross.tab.col) == 0.0, ]
    # Min and Max for each group
    min <-as.vector(aggregate(df.temp[,contVar], by=list(df.temp[,"Group"]), FUN=min)[2])
    max <-as.vector(aggregate(df.temp[,contVar], by=list(df.temp[,"Group"]), FUN=max)[2])
    # Add variable name
    varNameV <- rep(contVar, length(max))
    # Combination of count and column percent
    cross.tab.df   <-data.frame(varNameV,
                                row.names(cross.tab.col),
                                min,
                                max,
                                cross.tab.mat[,1],
                                cross.tab.mat[,2],
                                cross.tab.col[,1],
                                cross.tab.col[,2]
    )

    names(cross.tab.df) <- c("VarName",
                             "Group",
                             "Min Value",
                             "Max Value",
                             "Count Target=No",
                             "Count Target=Yes",
                             "% Target=No",
                             "% Target=Yes"
    )
    row.names(cross.tab.df) <- NULL
    # Calculate WOE
    WOE <- 100*log((cross.tab.df[,8]+0.0001)/(cross.tab.df[,7]+0.0001))
    IV <- ((cross.tab.df[,8]+0.0001)-(cross.tab.df[,7]+0.0001))*log((cross.tab.df[,8]+0.0001)/(cross.tab.df[,7]+0.0001))
    All_count <- cross.tab.mat[,1]+cross.tab.mat[,2]
    All_Percent <- All_count/sum(All_count)
    # Calculate Information Value
    library('scales')
    cross.tab.df[,7] <- percent(cross.tab.df[,7])
    cross.tab.df[,8] <- percent(cross.tab.df[,8])
    cross.tab.df[,"WOE"] <- round(WOE,2)
    cross.tab.df[,"IV"] <- round(IV,4)
    cross.tab.df[,"All_Count"] <-All_count
    cross.tab.df[,"All_Percent"] <- percent(All_Percent)
    # delete temp data frame
    rm(df.temp)
    # write to a csv file
    fileName <- paste("WOE",contVar,sep="_")
    filName1 <- paste(fileName,"csv",sep=".")
    write.csv(file=filName1,cross.tab.df)
    cross.tab.df
  }

}
