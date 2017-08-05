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
	degrees_of_comparison_to_include = NULL, # By default, all degrees of comparison will be included (e.g., for three vectors, all 1-, 2-, and 3-way comparisons). If you only want to include 2- and 3-way comparisons, for example, you can use 'c(2, 3)' here.
	cat_immediately = FALSE, # Whether to immediately print to the console using cat(). This needs to be true if venn diagrams are to be drawn.
	draw_venn_diagrams = FALSE # Whether we shold draw venn digrams for 2- to 5-way comparisons (the VennDiagram package can only draw up to five-way comparisons).
){
	if(draw_venn_diagrams == TRUE){ # Sanitize the user input
		draw_venn_diagrams_value <- TRUE
	} else {
		draw_venn_diagrams_value <- FALSE
	}

	if(draw_venn_diagrams_value == TRUE & cat_immediately != TRUE){
			warning("'draw_venn_diagrams' is TRUE, but 'cat_immediately' is FALSE. 'cat_immediately' needs to be set to TRUE in order for Venn diagrams to be drawn in the output. Therefore, skipping drawing diagrams...")
	}

	output_markdown <- NULL # We'll fill this in below.

	vector_names <- names(named_list_of_vectors_to_compare)

	combination_set_operations <- veccompare::compare.vectors(named_list_of_vectors_to_compare, draw_venn_diagrams = draw_venn_diagrams_value)

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

		addition_to_output_markdown <- paste("\n",  "\n# ", n_way_comparison, "-Way Comparisons", sep = "", collapse = "")

		if(cat_immediately == TRUE){
			cat(addition_to_output_markdown)
		} else {
			output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
		}

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
			addition_to_output_markdown <- paste("\n\n",  "## **", print.vector.with.and(list_element[["elements_involved"]]), "**", sep = "", collapse = "")

			if(cat_immediately == TRUE){
				cat(addition_to_output_markdown)
			} else {
				output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
			}

			# If we have a venn diagram to draw, go ahead and draw it:
			if(draw_venn_diagrams == TRUE & cat_immediately == TRUE){ # Note that we give the user a warning above if draw_venn_diagrams is TRUE but cat_immediately is FALSE
				if(!is.null(list_element[["venn_diagram"]])){
					cat("\n\n")
					veccompare::render.venn.diagram(list_element[["venn_diagram"]])
					cat("\n\n")
				}
			}

			addition_to_output_markdown <- paste("\n",  "- Total number of values (not counting duplicates): ", length(list_element[["union_of_elements"]]), sep = "", collapse = "")

			if(cat_immediately == TRUE){
				cat(addition_to_output_markdown)
			} else {
				output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
			}

			if(length(list_element[["elements_involved"]]) > 1){ # If it's not just the element compared with itself:
				addition_to_output_markdown <- paste("\n",
					"- Total number of elements that **overlap among ",
					print.vector.with.and(list_element[["elements_involved"]]), ":** ",
					length(list_element[["overlap_of_elements"]]),
					" (",
					round(
						length(list_element$overlap_of_elements)/length(list_element$union_of_elements)*100,
						digits = 2
					), "% of the total number of values)",
					sep = "", collapse = ""
				)

				if(cat_immediately == TRUE){
					cat(addition_to_output_markdown)
				} else {
					output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
				}

				addition_to_output_markdown <- paste("\n",
					"\t- Items that **overlap among ",
					print.vector.with.and(list_element[["elements_involved"]]), ":** *",
					print.vector.with.and(list_element[["overlap_of_elements"]]),
					"*",
					sep = "", collapse = ""
				)

				if(cat_immediately == TRUE){
					cat(addition_to_output_markdown)
				} else {
					output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
				}

				for(involved_vector_for_getting_unique_elements in list_element[["elements_involved"]]){

					percent_unique_to_involved_vector <- round(length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]])/length(named_list_of_vectors_to_compare[[involved_vector_for_getting_unique_elements]])*100, 2)

					addition_to_output_markdown <- paste(
						"\n\n### Elements Unique to ", involved_vector_for_getting_unique_elements,
						"\n\nTotal number of elements that are **unique to ",
						involved_vector_for_getting_unique_elements, ":** ",
						length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]]),

						# Get the percentage equivalent:
						" (", percent_unique_to_involved_vector, "% of ", involved_vector_for_getting_unique_elements, "; put differently, ", 100-percent_unique_to_involved_vector , "% of ", involved_vector_for_getting_unique_elements, " is overlapping)",
						sep = "", collapse = ""
					)

					if(cat_immediately == TRUE){
						cat(addition_to_output_markdown)
					} else {
						output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
					}

					addition_to_output_markdown <- paste("\n",
						"\n\nItems that are **unique to ",
						involved_vector_for_getting_unique_elements, ":**",
						"\n\n>*",
						print.vector.with.and(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]]),
						"*",
						sep = "", collapse = ""
					)

					if(cat_immediately == TRUE){
						cat(addition_to_output_markdown)
					} else {
						output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
					}
				} # End of if statement re: unique elements
			} # End of if statement re: length of elements involved.
		} # End of for loop over comparisons_at_this_level_of_combination
	} # End of for loop over degree of combinations

	return(output_markdown)
} # End of function definition

# Test the function:
# compare.vectors.and.return.text.analysis.of.overlap(example.vectors.list)
