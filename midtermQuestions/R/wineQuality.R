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
  score <- rnorm(nObservations, mean = 50, sd = 10)
  country <- c("Italy", "Austria", "Germany", "Chile", "Spain", "France", "Canada", "Argentina", "USA")
  probabilityOfCountry <- runif(length(country))
  countryList <- sample(country, size = nObservations, replace =  TRUE, prob = probabilityOfCountry/sum(probabilityOfCountry))

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
