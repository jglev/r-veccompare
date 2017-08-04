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

compare.vectors.and.return.text.analysis.of.overlap <- function(
	named_list_of_vectors_to_compare,
	degrees_of_comparison_to_include = NULL # By default, all degrees of comparison will be included (e.g., for three vectors, all 1-, 2-, and 3-way comparisons). If you only want to include 2- and 3-way comparisons, for example, you can use 'c(2, 3)' here.
){
	output_markdown <- NULL # We'll fill this in below.

	vector_names <- names(named_list_of_vectors_to_compare)

	combination_set_operations <- veccompare::compare.vectors(named_list_of_vectors_to_compare)

	if(is.null(degrees_of_comparison_to_include)){ # If we *have not* been told which comparisons (e.g., 2-way, 3-way, etc.) to include, we'll use all of them by default:
		degrees_of_comparisons <- 1:length(vector_names)
	} else { # If we *have* been told which comparisons to include, we'll set that here:
		if(!is.numeric(degrees_of_comparison_to_include)){
			stop("The argument 'degrees_of_comparison_to_include' is expected to be numeric.")
		} else { # If we are dealing with a numeric argument, as expected.
			degrees_of_comparisons <- degrees_of_comparison_to_include
		}
	}

	for(n_way_comparison in degrees_of_comparisons){
		output_markdown <- paste(output_markdown, "\n",  "\n# ", n_way_comparison, "-Way Comparisons", sep = "", collapse = "")

		comparisons_at_this_level_of_combination <- combination_set_operations[
			sapply(
				purrr::map(combination_set_operations, "elements_involved"),
				function(x){length(x) == n_way_comparison}
			)
			]

		for(list_element in comparisons_at_this_level_of_combination){
			if(length(list_element) == 1){
				list_element <- list_element[[1]] # Take off an annoying feature of R, whereby we need to select the first (and only) sub-element.
			}

			#
			# Print the results of the set operations comparing the elements:
			#
			output_markdown <- paste(output_markdown, "\n",  "- **Elements: *", print.vector.with.and(list_element[["elements_involved"]]), "***", sep = "", collapse = "")

			output_markdown <- paste(output_markdown, "\n",  "\t- Total number of values (not counting duplicates): ", length(list_element[["union_of_elements"]]), sep = "", collapse = "")

			if(length(list_element[["elements_involved"]]) > 1){ # If it's not just the element compared with itself:
				output_markdown <- paste(output_markdown, "\n",
					"\t- Total number of elements that **overlap among ",
					print.vector.with.and(list_element[["elements_involved"]]), ":** ",
					length(list_element[["overlap_of_elements"]]),
					" (",
					round(
						length(list_element$overlap_of_elements)/length(list_element$union_of_elements)*100,
						digits = 2
					), "% of the total number of values)",
					sep = "", collapse = ""
				)

				output_markdown <- paste(output_markdown, "\n",
					"\t\t- Items that overlap among ",
					print.vector.with.and(list_element[["elements_involved"]]), ": *",
					print.vector.with.and(list_element[["overlap_of_elements"]]),
					"*"
				)

				for(involved_vector_for_getting_unique_elements in list_element[["elements_involved"]]){
					output_markdown <- paste(output_markdown, "\n",
						"\t- Total number of elements that are **unique to ",
						involved_vector_for_getting_unique_elements, ":** ",
						length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]]),

						# Get the percentage equivalent:
						" (", round(length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]])/length(named_list_of_vectors_to_compare[[involved_vector_for_getting_unique_elements]])*100, 2), "% of ", involved_vector_for_getting_unique_elements, ")"
					)
					output_markdown <- paste(output_markdown, "\n",
						"\t\t- Items that are unique to ",
						involved_vector_for_getting_unique_elements, ": *",
						print.vector.with.and(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]]),
						"*",
						sep = "", collapse = ""
					)
				} # End of if statement re: unique elements
			} # End of if statement re: length of elements involved.
		} # End of for loop over comparisons_at_this_level_of_combination
	} # End of for loop over degree of combinations

	return(output_markdown)
} # End of function definition

# Test the function:
# compare.vectors.and.return.text.analysis.of.overlap(example.vectors.list)
