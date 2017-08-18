#' Title
#'
#' @param number_of_colors_to_get
#'
#' @return
# #' @export # I have commented this out so that it is not shown to users. This is an internal function.
#'
#' @examples
generate_random_colors <- function(number_of_colors_to_get){
	return(colors(distinct = TRUE)[runif(number_of_colors_to_get, min = 1, max = length(colors(distinct = TRUE)))])
}
