#' @title Generates a Frequency and Crosstabulation of Multiresponse data.
#'
#' @description This package contains two functions: (1) Generates Frequencies and Percentage of multiresponse data, (2) Generates crosstabulation of multiresponse data with other categorical variables.
#'
#' @param fName, varString, countVal
#'
#' @return freqPerc.lst
#'
#' @examples freq.lst <- multRespFreq(fName, varString, countVal)
#'
#' @export

multRespFreq <- function(fName, varString, countVal){
  nVars <- length(varString)
  fData <- eval(parse(text=fName))
  percFact <- nrow(fData) / 100
  varFreq <- rep(0, nVars)
  varPerc <- rep(0, nVars)
  for(i in 1:nVars){
    varID <- paste(fName,"$",varString[i], sep = "")
    varVal <- eval(parse(text=varID))
    varFreq[i] <- length(which(varVal==countVal))
    varPerc[i] <- varFreq[i] / percFact
  }
  freqPerc.lst <- list("Variables"=varString,
                       "Frequency"=varFreq,
                       "Percentage"=varPerc)
  return(freqPerc.lst)
}
