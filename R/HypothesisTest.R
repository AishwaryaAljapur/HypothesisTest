#' @title Hypothesis tests for variables in data
#'
#' @description This package includes function which accepts data frame and performs Chisquare, anova and t test on the variable
#'
#' @param symbol
#'
#' @return dataframe
#'
#' @examples  HypothesisTest(data,target)
#'
#' @export HypothesisTest

# The function is for performing hypothesis testing on numerical, categorical variables against binary target variables
# It does T test for 2 level numerical variables and ANOVA for multilevel numerical varaibles
# It does chi square test for categorical varaibles
# Function accepts two parameters
# @params DataFrame having numerical and categorical variables, excluding the target varibale
# @params Target Variable
# @result will be a dataFrame having the variable name, t/f/chisquare statistics and p - value

HypothesisTest <- function(data,variable)
{
  testStatistics = c() # vectors for storing the t/f/chisquare statistics
  pValues = c() # vectors for storing the p values
  variableNames = c() # vectors for storing the variable names
  if(!is.data.frame(data)) #checking if the data given is in the form data frame
    stop("The given object is not a data frame")
  index=1
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i])) # checking if the column values are of type numeric, to perform T test or ANOVA
    {
      if(length(unique(data[,i]))==2) # if the level of data is 2, then t test is done
      {
        tstat = t.test(variable ~ data[,i])$statistic[["t"]] # extracts t statistics from the test result
        pVal = t.test(variable ~ data[,i])$p.value  # extracts p value from the test result
        testStatistics[index] = tstat
        pValues[index] = pVal
        variableNames[index] = names(data)[i]
      }
      else # if the level of data is more than 2, then chi square is done
      {
        aovRes = aov(variable ~ data[,i])
        fVal = summary(aovRes)[[1]][[1,"F value"]] # extracts F statistics from the test result
        pVal =summary(aovRes)[[1]][[1,"Pr(>F)"]] # extracts p value from the test result
        testStatistics[index] = fVal
        pValues[index] = pVal
        variableNames[index] = names(data)[i]
      }
    }
    else if (is.factor(data[,i])) # else if the column values are categorical, performs chi square test
    {
      chstat = chisq.test(table(data[,i], variable))$statistic[["X-squared"]] # extracts chi square statistics from the test result
      pVal = chisq.test(table(data[,i], variable))$p.value # extracts p value from the test result
      testStatistics[index]=chstat
      pValues[index]=pVal
      variableNames[index] = names(data)[i]
    }
    index=index+1

  }
  resultDataFrame = data.frame(variableNames,testStatistics,pValues) # creating a data frame with the values and returning it
  return(resultDataFrame)
}
