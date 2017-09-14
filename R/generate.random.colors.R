#' Generate Random Colors
#'
#' An function to generate a given number of random colors.
#'
#' @param number_of_colors_to_get The number of colors to generate.
#'
#' @return A vector of R color names.
#' @export generate.random.colors
#'
#' @examples
#' generate.random.colors(5)
generate.random.colors <- function(number_of_colors_to_get){
	return(colors(distinct = TRUE)[runif(number_of_colors_to_get, min = 1, max = length(colors(distinct = TRUE)))])
}
