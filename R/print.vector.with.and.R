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

print.vector.with.and <- function(
	vector_to_print,
	string_to_return_if_vector_is_empty = ""
){
	if(length(vector_to_print) == 0){
		return(string_to_return_if_vector_is_empty)
	} else if(length(vector_to_print) == 1){
		return(vector_to_print)
	} else if(length(vector_to_print) == 2){
		return(paste(vector_to_print, sep = " and ", collapse = " and "))
	} else {
		return(
			paste(
				paste(
					head(
						vector_to_print,
						-1 # Remove the last element, so that we can add "and" before it : )
					),
					collapse = ", "
				),
				", and ",
				tail(vector_to_print, n = 1),
				sep = ""
			)
		)
	}
}

# Test the function:
print.vector.with.and(c("One", "Two", "Three", "Four"))
print.vector.with.and(c("One", "Two"))
print.vector.with.and(c("One"))
print.vector.with.and(c(""))
print.vector.with.and(c())
