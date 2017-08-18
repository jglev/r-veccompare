#' Generate Random Colors
#'
#' An internal function to generate a given number of random colors.
#'
#' @param number_of_colors_to_get The number of colors to generate.
#'
#' @return A vector of R color names.
# #' @export # I have commented this out so that it is not shown to users. This is an internal function.
#'
#' @examples
#' generate_random_colors(5)
generate.random.colors <- function(number_of_colors_to_get){
	return(colors(distinct = TRUE)[runif(number_of_colors_to_get, min = 1, max = length(colors(distinct = TRUE)))])
}
