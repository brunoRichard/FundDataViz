#' add.alpha function
#' 
#' This function adds a transparency value to colours in R. 
#' @param col The colour value(s)
#' @param alpha The transparency value. A number between 0 and 1 where 0 is fully transparent and 1 fully opaque. Defaults to 0.5
#' @keywords alpha
#' @return The new colour value(s) with  transparency
#' @export
#' @examples 
#' add.alpha("gray75", .25)

add.alpha <- function(col, alpha = .5){
  apply(col2rgb(col)/255,2, function(x) {rgb(x[1],x[2],x[3], alpha)})
}
