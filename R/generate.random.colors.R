# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

generate_random_colors <- function(number_of_colors_to_get){
	return(colors(distinct = TRUE)[runif(number_of_colors_to_get, min = 1, max = length(colors(distinct = TRUE)))])
}
