#' Extract elements from the output of \code{\link{compare.vectors}}
#'
#' Straightforwardly extract particular elements from the output of \code{\link{compare.vectors}}.
#'
#' @param output_from_compare.vectors The list output of \code{\link{compare.vectors}}.
#' @param vector_names An optional vector of names to extract from the named list (\code{named_list_of_vectors_to_compare}) used with \code{\link{compare.vectors}}.
#' @param only_match_vector_names A logical (TRUE/FALSE) indicator whether to match \strong{only} \code{vector_names}. If \code{vector_names} is \code{c("a", "b")}, for example, and \code{only_match_vector_names} is \code{TRUE}, this function will output only the comparison between \code{a} and \code{b}. If \code{only_match_vector_names} is \code{FALSE}, however, this function will output the comparison between \code{a} and \code{b}, as well as between \code{a}, \code{b}, and \code{c}, etc.
#' @param degrees_of_comparison An optional number of vector of numbers indicating which degrees of comparison to return (for example, 2 will return only two-way comparisons from \code{output_from_compare.vectors}.
#' @param elements_of_output An optional vector of element names from \code{output_from_compare.vectors} to return (for example, "elements_involved"). See the \strong{Value} section of \code{\link{compare.vectors}} for a list of the elements to choose from.
#'
#' @return
#' A winnowed version of \code{output_from_compare.vectors}. Depending on arguments, either a list, a vector, or a string.
#'
#' @export
#'
#' @examples
#' example <- veccompare::compare.vectors(veccompare::example.vectors.list)
#'
#' # To extract similar elements across list items:
#' veccompare::extract.compared.vectors(
#'   example,
#'   elements_of_output = "elements_involved"
#' )
#'
#' # To extract all comparisons that involve "vector_a":
#' veccompare::extract.compared.vectors(
#'   example,
#'   vector_names = "vector_a"
#' )
#'
#' # To find all comparisons that were about "vector_a" and "vector_c":
#' veccompare::extract.compared.vectors(
#'   example,
#'   vector_names = c("vector_a", "vector_c"),
#'   only_match_vector_names = TRUE
#' )
#'
#' # To get all elements that did a two-way comparison:
#' veccompare::extract.compared.vectors(
#'   example,
#'   degrees_of_comparison = 2
#' )
#'
#' # A more complex / specific example:
#' extract.compared.vectors(
#'   example,
#'   vector_names = c("vector_a", "vector_c"),
#'   only_match_vector_names = FALSE,
#'   degrees_of_comparison = c(2, 3),
#'   elements_of_output = "elements_involved"
#' )
extract.compared.vectors <- function(
	output_from_compare.vectors,
	vector_names = NULL,
	only_match_vector_names = FALSE,
	degrees_of_comparison = NULL,
	elements_of_output = NULL
){

	output <- output_from_compare.vectors # We'll start with output_from_compare.vectors, and winnow it as we go.

	if(!is.null(degrees_of_comparison)){
		# Get all elements that did a two-way comparison:
		output <- output[
		which(
			sapply(
				purrr::map(output, "elements_involved"),
				function(x){length(x) %in% degrees_of_comparison}
				)
			)
		]
	}

	if(!is.null(vector_names)){
		if(only_match_vector_names == TRUE){
			# To find all comparisons that were about the particular vector_names:
			output <- output[
				sapply(
					purrr::map(output, "elements_involved"),
					function(x){setequal(x, vector_names)}
				)
			]
		} else {
			# To extract all comparisons that involve "vector_a":
			output <- output[
				sapply(
					purrr::map(output, "elements_involved"),
				   function(x){all(vector_names %in% x)}
			   )
			]
		}
	}

	# Remove NULL elements we possibly created above:
	# First, though, check whether there's anything left to remove: The user may have entered options that resulted in just a blank list():
	if(length(output) > 0){
		output <- output[
			sapply(
				output,
				function(x){!is.null(x)}
			)
		]
	}

	if(!is.null(elements_of_output)){
		# output <- purrr::map(output, elements_of_output)

		# Extract just the elements_of_output elements from across the list items of output:
		output <- lapply(output, function(x){
			return(x[elements_of_output])
		})
	}

	if(length(output) == 1){
		output <- output[[1]]
	}

	return(output)
}
