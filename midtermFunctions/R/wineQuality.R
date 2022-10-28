#' The Wine Quality Dataset Generator Function
#'
#' This function generates a mock wine quality survey. The function generates data for a total of 9 different countries and returns the total number of wines surveyed and the number of wines that were labelled as good for each country.
#' @param nObservations The total number of observations requested. Defaults to 100
#' @param scoreCutOff The cutoff score set to determine whether a wine is good. Defaults to 25
#' @keywords mockdata wineQuality FDVMidterm
#' @export
#' @examples
#' wineQuality(scoreCutOff = 50)

wineQuality <- function(nObservations = 100, scoreCutOff = 25){
  country <- c("Italy", "Austria", "Germany", "Chile", "Spain", "France", "Canada", "Argentina", "USA")
  probabilityOfCountry <- runif(length(country))
  countryList <- sample(country, size = nObservations, replace =  TRUE, prob = probabilityOfCountry/sum(probabilityOfCountry))
  continent <- c(1,1,1,4,1,1,3,4,3)
  meanScore <- c(60,60,60,50, 60,60, 40,50,40)
  score <- NULL
  for (i in 1:length(country)){
    IDX <- countryList == country[i]
    score[IDX] <- rnorm(1:sum(IDX), mean = meanScore[i], sd = 20)
  }
  wineQualityData <- data.frame(Country = countryList, Score = score)
  wineQualityData$GoodWines <- wineQualityData$Score > scoreCutOff

  proportions <- NULL
  nObservations <- NULL
  for (i in 1:length(country)){
    proportions[i] <- sum(wineQualityData[wineQualityData$Country == country[i], "GoodWines"])
    nObservations[i] <- length(wineQualityData[wineQualityData$Country == country[i], "GoodWines"])
  }

  prop <- data.frame(country = country, nWines = nObservations, nGoodWines = proportions)
  return(prop)
}
