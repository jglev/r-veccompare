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

compare.vectors <- function(
	named_list_of_vectors_to_compare,
	draw_venn_diagrams = FALSE, # Whether we shold draw venn digrams for 2- to 5-way comparisons (the VennDiagram package can only draw up to five-way comparisons).
	vector_colors_for_venn_diagrams = NULL,
	save_venn_diagram_files = FALSE
){
	vector_names <- names(named_list_of_vectors_to_compare)

	# If we're generating Venn diagrams, we'll create a consistent color to use for each vector:
	if(draw_venn_diagrams == TRUE){
		if(!is.null(vector_colors_for_venn_diagrams)){
			if(length(vector_colors_for_venn_diagrams) != length(vector_names)){
				stop("The number of colors for Venn diagrams (", length(vector_colors_for_venn_diagrams), ") does not match the number of vectors we are comparing (", length(vector_names), ").")
			} else {
				vector_colors <- as.list(vector_colors_for_venn_diagrams)
				names(vector_colors) <- vector_names
			}
		} else { # If we've not been given colors to use, we'll generate random ones:
			vector_colors <- as.list(generate_random_colors(length(vector_names)))
			names(vector_colors) <- vector_names
		}
		} # End of if draw_venn_diagrams == TRUE
	combinations_of_vector_names <- as.data.frame(
		gtools::combinations(
			length(vector_names), # size of the source vector
			length(vector_names), # size of the target vectors
			vector_names,
			set = TRUE, # Remove duplicates from the output
			repeats.allowed = TRUE # Do all permutations of starting orders (vs. just combinations, where order doesn't matter). Setting this to TRUE will, when the items are chunked down, give us all 1:length(vector_names) sized combinations of items.
		)
	)

	# Remove duplicate combinations, assessed after chunking down unique items in each row (so that, e.g., "a a a b" and "a a b b" are seen as the same thing, and only one of them is retained, since they both contain just elements "a" and "b"):
	# The 'as.data.frame()' below is so that we know we're always dealing with a data.frame here -- if there's more than one combination of vector names, this would be a data.frame anyway, but if there's just one vector (and thus no combinations), this would otherwise be a factor. So setting this to be consistently a data.frame simplifies the steps below by allowing more consistency.
	combinations_of_vector_names_chunked_for_unique_items <- as.data.frame(
		combinations_of_vector_names[
			!duplicated(
				apply(
					combinations_of_vector_names,
					1, # iterate over rows
					unique
				)
			)
			, # Use all columns
			]
	)
	# As a check of our work, this should usually be TRUE, unless length(vector_names) <= 2:
	# nrow(combinations_of_vector_names_chunked_for_unique_items) < nrow(combinations_of_vector_names)
	if(nrow(combinations_of_vector_names_chunked_for_unique_items) > 1){
		rownames(combinations_of_vector_names_chunked_for_unique_items) <- NULL # This is just for aesthetic purposes; it makes debugging easier.
	}

	combination_set_operations <- apply(
		combinations_of_vector_names_chunked_for_unique_items,
		1, # Iterate over rows
		function(row){
			# length(unique(x))
			# print(row[1])

			unique_names_in_row <- as.character(unique(row))

			if(nrow(combinations_of_vector_names_chunked_for_unique_items) == 1){
				vector_items_for_unique_names_in_row <- lapply(named_list_of_vectors_to_compare[unique_names_in_row], unique)
			} else {
				vector_items_for_unique_names_in_row <- named_list_of_vectors_to_compare[unique_names_in_row]
			}

			# Run setdiff() on with each element being first in turn (since setdiff gives the unique items for whichever item is printed first)
			list_of_elements_unique_to_vectors = list() # We'll fill this in below.

			for(vector_name in unique_names_in_row){
				list_of_elements_unique_to_vectors[[vector_name]] = Reduce(
					setdiff,
					c(
						vector_items_for_unique_names_in_row[vector_name], # Put vector_name first (since setdiff gives the unique items for whichever item is printed first)
						vector_items_for_unique_names_in_row[-which( names(vector_items_for_unique_names_in_row) %in% vector_name)]
					)
				)
			}

			list_to_return <- list(
				"elements_involved" = as.character(unique_names_in_row),
				# The Reduce() approach below comes from https://www.r-bloggers.com/intersect-for-multiple-vectors-in-r/:
				"union_of_elements" = Reduce(union, vector_items_for_unique_names_in_row),
				"overlap_of_elements" = Reduce(intersect, vector_items_for_unique_names_in_row),
				"elements_unique_to_first_element" = list_of_elements_unique_to_vectors
			)

			return(list_to_return)
		}
	)

	# str(combination_set_operations)

	# -------------------------------------------------
	# Draw Venn diagrams
	# -------------------------------------------------
	if(draw_venn_diagrams == TRUE){
		# We will now create Venn diagrams for each level of comparison (e.g., 2-way, 3-way, etc.), from 2 to the maximum level of comparison (up to 5-way, since that's the most that the VennDiagram package I'm using can draw):

		# Define a sub-function to make it easier to query overlaps between elements
		get_overlap_of_elements_from_combination_set_operations <- function(
			... # This should be a list of element names. "..." is R's way of accepting an arbitrary number of arguments.
		){
			element_names <- unlist(list(...)) # Parse the "..." arbitrary number of arguments into a vector.

			overlap_value <- length(
				combination_set_operations[[ # sapply() below should only bring back one matching element, since above we made sure we weren't calculating repeats, so this (just assuming that there will only be one matching element) seems safe to do.
					which(
						sapply(
							purrr::map(combination_set_operations, "elements_involved"),
							function(x){setequal(x, c(element_names))}
						)
					)
					]]$overlap_of_elements
			)
			return(overlap_value)
		} # End of sub-function definition

		maximum_degree_of_comparison_calculated <- length(named_list_of_vectors_to_compare)

		if(maximum_degree_of_comparison_calculated >= 2){
			if(maximum_degree_of_comparison_calculated >= 6){
				message("Note: We can only draw up to 5-way diagrams. Thus, combinations of greater than 5 degrees (i.e., 6+ - way comparisons) will not be drawn...")
			}
			for(degree_of_comparison in 2:(min(maximum_degree_of_comparison_calculated, 5))){ # The Venn Diagram package can only draw up to 5-way comparisons, so we won't go above 5 when drawing Venn-Diagrams.

				message("Calculating Venn diagram for all ", degree_of_comparison, "-way comparisons...", sep = "")

				combination_set_elements_relevant_for_current_degree_of_comparison <- which(
					sapply(
						purrr::map(combination_set_operations, "elements_involved"),
						function(x) {length(x) == degree_of_comparison}
					)
				)

				for(combination_set_element_number in combination_set_elements_relevant_for_current_degree_of_comparison){

					names_of_elements_in_this_comparison_set <- combination_set_operations[[combination_set_element_number]]$elements_involved

					message("Drawing comparison between ", veccompare::print.vector.with.and(names_of_elements_in_this_comparison_set), "...", sep = "")

					if(length(names_of_elements_in_this_comparison_set) == 2){
						venn_diagram <- VennDiagram::draw.pairwise.venn(
							area1 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[1]]])),
							area2 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[2]]])),
							cross.area = length(combination_set_operations[[combination_set_element_number]]$overlap_of_elements),
							category = names_of_elements_in_this_comparison_set,
							# lty = rep("blank", 2), # Line dash pattern of the circles
							fill = unlist(vector_colors[names_of_elements_in_this_comparison_set]),
							alpha = rep(0.5, 2),
							cat.pos = rep(0, 2), # Category position around the circles (in degrees)
							cat.dist = rep(0.025, 2), # Category names' distances from the edges of the circles (can be negative)
							scaled = TRUE,
							margin = 0,
							cex = rep(1.5, 3),
							cat.cex = rep(1.5, 2),
							ind = FALSE # Do not automatically draw the diagram
						)
					} else if(length(names_of_elements_in_this_comparison_set) == 3){
						venn_diagram <- VennDiagram::draw.triple.venn(
							area1 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[1]]])),
							area2 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[2]]])),
							area3 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[3]]])),
							n12 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2]),
							n23 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							n13 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3]),
							n123 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							category = names_of_elements_in_this_comparison_set,
							#lty = rep("blank", 3), # Line dash pattern of the circles
							fill = unlist(vector_colors[names_of_elements_in_this_comparison_set]),
							alpha = rep(0.5, 3),
							cat.pos = c(0, 0, 180), # Category position around the circles (in degrees)
							cat.dist = rep(0.025, 3), # Category names' distances from the edges of the circles (can be negative)
							scaled = TRUE,
							margin = 0,
							cex = rep(1.5, 7),
							cat.cex = rep(1.5, 3),
							ind = FALSE # Do not automatically draw the diagram
						)
					} else if(length(names_of_elements_in_this_comparison_set) == 4){
						venn_diagram <- VennDiagram::draw.quad.venn(
							area1 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[1]]])),
							area2 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[2]]])),
							area3 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[3]]])),
							area4 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[4]]])),
							n12 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2]),
							n13 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3]),
							n14 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[4]),
							n23 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							n24 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4]),
							n34 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n123 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							n124 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4]),
							n134 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n234 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n1234 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							category = names_of_elements_in_this_comparison_set,
							# lty = rep("blank", 4), # Line dash pattern of the circles
							fill = unlist(vector_colors[names_of_elements_in_this_comparison_set]),
							alpha = rep(0.5, 4),
							#cat.pos = c(0, 0, 180), # Category position around the circles (in degrees)
							#cat.dist = rep(0.025, 3), # Category names' distances from the edges of the circles (can be negative)
							scaled = TRUE,
							margin = 0,
							cex = rep(1.5, 15),
							cat.cex = rep(1.5, 4),
							ind = FALSE # Do not automatically draw the diagram
						)
					} else if(length(names_of_elements_in_this_comparison_set) == 5){
						venn_diagram <- VennDiagram::draw.quintuple.venn(
							area1 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[1]]])),
							area2 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[2]]])),
							area3 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[3]]])),
							area4 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[4]]])),
							area5 = length(unique(named_list_of_vectors_to_compare[[names_of_elements_in_this_comparison_set[5]]])),
							n12 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2]),
							n13 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3]),
							n14 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[4]),
							n15 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[5]),
							n23 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							n24 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4]),
							n25 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[5]),
							n34 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n35 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[5]),
							n45 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n123 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3]),
							n124 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4]),
							n125 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[5]),
							n134 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n135 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[5]),
							n145 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n234 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n235 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[5]),
							n245 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n345 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n1234 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4]),
							n1235 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[5]),
							n1245 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n1345 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n2345 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							n12345 = get_overlap_of_elements_from_combination_set_operations(names_of_elements_in_this_comparison_set[1], names_of_elements_in_this_comparison_set[2], names_of_elements_in_this_comparison_set[3], names_of_elements_in_this_comparison_set[4], names_of_elements_in_this_comparison_set[5]),
							category = names_of_elements_in_this_comparison_set,
							# lty = rep("blank", 5), # Line dash pattern of the circles
							fill = unlist(vector_colors[names_of_elements_in_this_comparison_set]),
							alpha = rep(0.5, 5),
							#cat.pos = c(0, 0, 180), # Category position around the circles (in degrees)
							#cat.dist = rep(0.025, 3), # Category names' distances from the edges of the circles (can be negative)
							scaled = TRUE,
							margin = 0,
							cex = rep(1.5, 31),
							cat.cex = rep(1.5, 5),
							ind = FALSE # Do not automatically draw the diagram
						)
					} # End of if statement over length of elements

					combination_set_operations[[combination_set_element_number]]$venn_diagram <- venn_diagram

					if(save_venn_diagram_files == TRUE){
						filename_to_use <- make.names(
							paste(
								paste(combination_set_operations[[combination_set_element_number]]$elements_involved, sep = "_", collapse = "_"),
								"_venn_diagram.png",
								sep = "",
								collapse = ""
							)
						)

						message("Saving Venn diagram to '", file.path(getwd(), filename_to_use), "'...")

						ggplot2::ggsave(file=filename_to_use, venn_diagram)
					}
				}
			} # End of for loop over degree_of_comparison
		} # End of if statement re: draw_venn_diagrams
	} # End of if statement re: whether length(maximum_degree_of_comparison_calculated) > 1

	# -------------------------------------------------
	# End of Draw Venn diagrams
	# -------------------------------------------------

	# To test the last-drawn diagram:
	# grid::grid.newpage()
	# grid::grid.draw(venn_diagram)

	return(combination_set_operations)
} # End of function definition

# Test the function:
# test <- veccompare::compare.vectors(veccompare::example.vectors.list)

# for(venndiagram in purrr::map(test, "venn_diagram")){
#
# }



# If we want to get similar elements across list items, we can do so with the purrr package, as an example:
# library('purrr')
# test_example <- compare_vectors(vectors_to_use[1:2])
# purrr::map(test_example, "elements_involved")

# Hence, e.g., to find all comparisons that involve "vector_a":
#test_example[
# 	sapply(
# 		purrr::map(combination_set_operations, "elements_involved"),
# 		function(x){"vector_a" %in% x}
# 	)
# ]

# To find all comparisons that were about "vector_a" and "vector_c":
# test_example[
# 	sapply(
# 		purrr::map(combination_set_operations, "elements_involved"),
# 		function(x){setequal(x, c("vector_a", "vector_c"))}
# 	)
# ]

# Or, to get all elements that did a two-way comparison:
# test_example[[
# 	which(
# 		sapply(
# 			purrr::map(test_example, "elements_involved"),
# 			function(x){length(x) == 5}
# 		)
# 	)
# ]]$elements_involved
