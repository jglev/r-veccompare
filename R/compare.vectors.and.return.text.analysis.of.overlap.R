#' Create a Markdown report from the output of \code{\link{compare.vectors}}
#'
#' This function is a wrapper for \code{\link{compare.vectors}}. It creates a Markdown report of all degrees of set comparisons between a named list of vectors.
#'
#' Use of this function is illustrated with the \code{Veccompare Overlap Report} RMarkdown template for RStudio that is installed as part of the \pkg{veccompare} package.
#' @inheritParams compare.vectors
#' @inheritParams render.venn.diagram
#' @param cat_immediately A logical (TRUE/FALSE) indicator whether to immediately print the output, as in an RMarkdown document.
#' @param base_heading_level_to_use An integer indicating the highest-level heading to print. Defaults to \code{1} (i.e., start by using first-level headings); \code{1} is also the minimum value used.
#'
#' @return A string of Markdown (and Venn diagrams, if \code{draw_venn_diagrams} is \code{TRUE}).
#'
#' If \code{cat_immediately} is \code{TRUE}, nothing is returned by the function; rather, the output Markdown is printed immediately (for example, as part of a Knitted RMarkdown document, or to the console).
#'
#' If \code{cat_immediately} is \code{FALSE}, the output can be saved to an object (as in the example below). This object can then be printed using \code{cat()}.
#'
#' NOTE WELL: If \code{cat_immediately} is \code{FALSE}, the output \emph{should} be saved to an object. If it is not, R will give an error message when printing to the console, because of unescaped special characters (which work correctly when \code{cat()} is used).
#'
#' @export
#'
#' @examples
#' example <- compare.vectors.and.return.text.analysis.of.overlap(
#'     veccompare::example.vectors.list,
#'     cat_immediately = FALSE,
#'     draw_venn_diagrams = FALSE
#' )
#' cat(example)
compare.vectors.and.return.text.analysis.of.overlap <- function(
	named_list_of_vectors_to_compare,
	degrees_of_comparison_to_include = NULL, # By default, all degrees of comparison will be included (e.g., for three vectors, all 1-, 2-, and 3-way comparisons). If you only want to include 2- and 3-way comparisons, for example, you can use 'c(2, 3)' here.
	cat_immediately = FALSE, # Whether to immediately print to the console using cat(). This needs to be true if venn diagrams are to be drawn.
	draw_venn_diagrams = FALSE, # Whether we shold draw venn digrams for 2- to 5-way comparisons (the VennDiagram package can only draw up to five-way comparisons).
	viewport_npc_width_height_for_images = 1.0,
	vector_colors_for_venn_diagrams = NULL,
	save_venn_diagram_files = FALSE,
	location_for_venn_diagram_files = "",
	prefix_for_venn_diagram_files = "",
	base_heading_level_to_use = 1
){

	if(!is.numeric(base_heading_level_to_use)){
		stop("'base_heading_level_to_use' is expected to be an integer (for example, 1, 2, 3, etc.).")
	} else {
		# If base_heading_level_to_use *is* an integer, round it to the nearest whole number, with a minimum of 1
		base_heading_level_to_use <- max(round(base_heading_level_to_use, digits = 0), 1)

		# We'll then create a heading prefix for use when creating Markdown below:
		markdown_base_heading <- paste(rep("#", base_heading_level_to_use), sep = "", collapse = "")
	}

	# First, we'll escape special Markdown characters:
	message("Escaping special Markdown characters (_, *, /)...")

	named_list_of_vectors_to_compare <- lapply(named_list_of_vectors_to_compare, function(x){sub("_", "\\\\_", x)})
	named_list_of_vectors_to_compare <- lapply(named_list_of_vectors_to_compare, function(x){sub("*", "\\\\*", x, fixed = TRUE)})
	named_list_of_vectors_to_compare <- lapply(named_list_of_vectors_to_compare, function(x){sub("/", "\\\\/", x)})

	if(draw_venn_diagrams == TRUE){ # Sanitize the user input
		draw_venn_diagrams_value <- TRUE

		if(save_venn_diagram_files == TRUE){ # Sanitize the user input
			save_venn_diagram_files_value <- TRUE
		} else {
			save_venn_diagram_files_value <- FALSE
		}

		if(location_for_venn_diagram_files != ""){ # Sanitize the user input
			location_for_venn_diagram_files_value <- as.character(location_for_venn_diagram_files)
		} else {
			location_for_venn_diagram_files_value <- ""
		}

		if(prefix_for_venn_diagram_files != ""){ # Sanitize the user input
			prefix_for_venn_diagram_files_value <- as.character(prefix_for_venn_diagram_files)
		} else {
			prefix_for_venn_diagram_files_value <- ""
		}

		if(!is.null(vector_colors_for_venn_diagrams)){ # Sanitize the user input
			message("Using the following Venn diagram colors: ", veccompare::vector.print.with.and(vector_colors_for_venn_diagrams))

			vector_colors_for_venn_diagrams_value <- vector_colors_for_venn_diagrams
		} else {
			vector_colors_for_venn_diagrams_value <- NULL
		}

		if(!is.numeric(viewport_npc_width_height_for_images)){
			stop("'viewport_npc_width_height_for_images' is expected to be numeric (e.g., 1.0, 0.5, etc.).")
		}
	} else {
		draw_venn_diagrams_value <- FALSE
	}

	if(draw_venn_diagrams_value == TRUE & cat_immediately != TRUE){
			warning("'draw_venn_diagrams' is TRUE, but 'cat_immediately' is FALSE. 'cat_immediately' needs to be set to TRUE in order for Venn diagrams to be drawn in the output. Therefore, skipping drawing diagrams...")
	}

	output_markdown <- NULL # We'll fill this in below.

	vector_names <- names(named_list_of_vectors_to_compare)

	degrees_of_comparison_to_include_value <- degrees_of_comparison_to_include # This allows us below to avoid passing the argument 'degrees_of_comparison_to_include_value = degrees_of_comparison_to_include_value'

	combination_set_operations <- veccompare::compare.vectors(
		named_list_of_vectors_to_compare,
		degrees_of_comparison_to_include = degrees_of_comparison_to_include_value,
		draw_venn_diagrams = draw_venn_diagrams_value,
		vector_colors_for_venn_diagrams = vector_colors_for_venn_diagrams_value,
		save_venn_diagram_files = save_venn_diagram_files_value,
		location_for_venn_diagram_files = location_for_venn_diagram_files_value,
		prefix_for_venn_diagram_files = prefix_for_venn_diagram_files_value
	)

	if(is.null(degrees_of_comparison_to_include)){ # If we *have not* been told which comparisons (e.g., 2-way, 3-way, etc.) to include, we'll use all of them by default:
		degrees_of_comparisons <- 1:length(vector_names)
	} else { # If we *have* been told which comparisons to include, we'll set that here:
		if(!is.numeric(degrees_of_comparison_to_include)){
			stop("The argument 'degrees_of_comparison_to_include' is expected to be numeric.")
		} else { # If we are dealing with a numeric argument, as expected.
			degrees_of_comparisons <- degrees_of_comparison_to_include
		}
	}

	message("Creating output Markdown text...")

	for(n_way_comparison in degrees_of_comparisons){

		if(n_way_comparison == 1){
			addition_to_output_markdown <- paste("\n\n", markdown_base_heading, " Number of Items in Each Element", sep = "", collapse = "")
		} else {
			addition_to_output_markdown <- paste("\n\n", markdown_base_heading, " ", n_way_comparison, "-Way Comparisons", sep = "", collapse = "")
		}

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
			addition_to_output_markdown <- paste(
				"\n\n",
				markdown_base_heading,
				"# **",
				vector.print.with.and(
					list_element[["elements_involved"]],
					string_to_return_if_vector_is_empty = "(None)"),
				"**",
				sep = "",
				collapse = ""
			)

			if(cat_immediately == TRUE){
				cat(addition_to_output_markdown)
			} else {
				output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
			}

			# If we have a venn diagram to draw, go ahead and draw it:
			if(draw_venn_diagrams == TRUE & cat_immediately == TRUE){ # Note that we give the user a warning above if draw_venn_diagrams is TRUE but cat_immediately is FALSE
				if(!is.null(list_element[["venn_diagram"]])){
					cat("\n\n")
					veccompare::render.venn.diagram(
						list_element[["venn_diagram"]],
						viewport_npc_width_height_to_force = viewport_npc_width_height_for_images
					)
					cat("\n\n")
				}
			}

			addition_to_output_markdown <- paste("\n",  "- Total number of values (not counting duplicates): ", length(unique(list_element[["union_of_elements"]])), sep = "", collapse = "") # unique() is needed here just for when the total number of elements is 1 (i.e., we're just reporting on the number of elements) -- in that case, this is necessary to count only non-duplicate values (without this, venn diagrams in higher-level comparisons won't add up with the values in the level-1 comparisons (i.e., with the number of elements printed for each vector individually).

			if(cat_immediately == TRUE){
				cat(addition_to_output_markdown)
			} else {
				output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
			}

			if(length(list_element[["elements_involved"]]) > 1){ # If it's not just the element compared with itself:
				addition_to_output_markdown <- paste("\n",
					"- Total number of elements that **overlap among ",
					vector.print.with.and(list_element[["elements_involved"]]), ":** ",
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
					vector.print.with.and(list_element[["elements_involved"]]), ":** *",
					vector.print.with.and(
						list_element[["overlap_of_elements"]],
						string_to_return_if_vector_is_empty = "(None)"
					),
					"*",
					sep = "", collapse = ""
				)

				if(cat_immediately == TRUE){
					cat(addition_to_output_markdown)
				} else {
					output_markdown <- paste(output_markdown, addition_to_output_markdown, sep = "", collapse = "")
				}

				for(involved_vector_for_getting_unique_elements in list_element[["elements_involved"]]){

					percent_unique_to_involved_vector <- round(length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]])/length(unique(named_list_of_vectors_to_compare[[involved_vector_for_getting_unique_elements]]))*100, 2)

					addition_to_output_markdown <- paste(
						"\n\n",
						markdown_base_heading,
						"## Elements Unique to ", involved_vector_for_getting_unique_elements,
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
						"\n\n> *",
						vector.print.with.and(
							list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]],
							string_to_return_if_vector_is_empty = "(None)"
						),
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

	if(cat_immediately != TRUE){
		return(output_markdown)
	}
} # End of function definition
