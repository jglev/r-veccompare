# example <- veccompare::compare.vectors(veccompare::example.vectors.list)

extract.compared.vectors(
	example
	, vector_names = c("vector_a", "vector_c")
	, only_match_vector_names = FALSE
	, degrees_of_comparison = c(2, 3)
	, elements_of_output = "elements_involved"
)


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
		output <- purrr::map(output, elements_of_output) # Extract just the elements_of_output elements from across the list items of output.
	}

	return(output)
}
