#' The Wine Quality Model Results
#'
#' This function builds a logistic regression from mock wine quality data. We aim, with the data to  build a model that can accurately predict wine quality according to its variety, fixed acidity, sulfates, and alcohol level. This function builds a data set for you, trains a classifier (logistic regression) and returns the predictions of the model in addition to the test data set. The relevant columns to use here are GoodWine (the outcome) and prediction (the model prediction).
#' @param nObservations The number of requested observations to generate. Defaults to 1000
#' @param seed Sets the seed of the random number generator (you will be instructed on what value to use here in your midterm). Defaults to 1
#' @keywords midterm logisticregression modelperformance
#' @export
#' @examples
#' wineQualityModel(nObservation = 1000, seed = 1)

wineQualityModel <- function(nObservations = 1000, seed = 1){
  set.seed(seed)
  score <- rnorm(nObservations, mean = 50, sd = 10)
  country <- c("Italy", "Austria", "Germany", "Chile", "Spain", "France", "Canada", "Argentina", "USA")
  probabilityOfCountry <- runif(length(country))
  countryList <- sample(country, size = nObservations, replace =  TRUE, prob = probabilityOfCountry/sum(probabilityOfCountry))

  variety <- c("White Blend","Portuguese Red","Pinot Gris","Riesling", "Pinot Noir", "Tempranillo-Merlot", "Frappato", "Gewürztraminer", "Cabernet Sauvignon","Nerello Mascalese")
  probOfVariety <- runif(length(variety))
  varietyList <- sample(variety, size = nObservations, replace =  TRUE, prob = probOfVariety/sum(probOfVariety))

  wineQualityData <- data.frame(Country = countryList)
  wineQualityData$GoodWine <- score > 50
  wineQualityData$Variety <- varietyList

  scoreJumbled <- score + runif(length(score), min = -10, max = 10)
  wineQualityData$FixedAcidity <- round(((3+-2*scoreJumbled) - min(3+-2*scoreJumbled))/(max(3+-2*scoreJumbled)-min(3+-2*scoreJumbled)) * (15.9-4.6)+4.6,2)
  wineQualityData$Sulfates <-round(((3+0.01*scoreJumbled) - min(3+-0.01*scoreJumbled))/(max(3+-0.01*scoreJumbled)-min(3+-0.01*scoreJumbled)) * (2-0.33)+0.33,2)
  wineQualityData$Alcohol <-round(((3+1.76*scoreJumbled) - min(3+1.76*scoreJumbled))/(max(3+1.76*scoreJumbled)-min(3+1.76*scoreJumbled)) * (14.9-8.4)+8.4,2)


  IDX <- sample(c(TRUE, FALSE), size = nObservations, replace = TRUE, prob = c(.75,.25))
  wineQualityData.Train <- wineQualityData[IDX, ]
  wineQualityData.Test <- wineQualityData[!IDX, ]

  model <- glm(GoodWine ~ Variety+FixedAcidity+Sulfates+Alcohol, data = wineQualityData.Train, family = "binomial")
  Ypred <- predict(model, newdata = wineQualityData.Test, type = "response")
  wineQualityData.Test$prediction <- Ypred > .5

  return(wineQualityData.Test)
}
