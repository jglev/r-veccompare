#' Print a vector with commas and a final "and".
#'
#' @param vector_to_print A vector of strings (or elements able to be coerced into strings) to print.
#' @param string_to_return_if_vector_is_empty If \code{vector_to_print} is empty, the string that should be returned (for example, "", "(None)", etc.)
#' @param use_oxford_comma A logical (TRUE/FALSE) value indicating whether to use an Oxford comma ("One, two, and three" vs. "One, two and three").
#'
#' @return A single string that concatenates the input, separating with commas and adding "and" before the final item.
#' @export
#'
#' @examples
#' vector.print.with.and(c("One", "Two", "Three", "Four"))
#' vector.print.with.and(c("One", "Two", "Three", "Four"), use_oxford_comma = FALSE)
#' vector.print.with.and(c("One", "Two"))
#' vector.print.with.and(c("One"))
#' vector.print.with.and(c(), string_to_return_if_vector_is_empty = "(None)") # Outputs "(None)"
#' vector.print.with.and(c(""), string_to_return_if_vector_is_empty = "(None)") # Outputs ""
vector.print.with.and <- function(
	vector_to_print,
	string_to_return_if_vector_is_empty = "",
	use_oxford_comma = TRUE
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
				if(use_oxford_comma == TRUE){","},
				" and ",
				tail(vector_to_print, n = 1),
				sep = ""
			)
		)
	}
}
