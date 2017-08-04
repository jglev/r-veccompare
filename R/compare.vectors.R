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

compare.vectors <- function(named_list_of_vectors_to_compare){
	vector_names <- names(named_list_of_vectors_to_compare)

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
			vector_items_for_unique_names_in_row <- named_list_of_vectors_to_compare[unique_names_in_row]

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

			# #
			# # Create a Venn Diagram if length(elements_involved) <= 5 (since that's the most the VennDiagram package can draw)
			# #
			# if(length(list_to_return$elements_involved) == 2){
			# 	list_to_return$venn_diagram <- VennDiagram::draw.pairwise.venn(
			# 		area1 = length(unique(vector_items_for_unique_names_in_row[[1]])),
			# 		area2 = length(unique(vector_items_for_unique_names_in_row[[2]])),
			# 		cross.area = length(list_to_return$overlap_of_elements),
			# 		category = c(list_to_return$elements_involved[1], list_to_return$elements_involved[2]),
			# 		lty = rep("blank", 2), # Line dash pattern of the circles
			# 		fill = c("light blue", "light green"),
			# 		alpha = rep(0.5, 2),
			# 		cat.pos = rep(0, 2), # Category position around the circles (in degrees)
			# 		cat.dist = rep(0.025, 2), # Category names' distances from the edges of the circles (can be negative)
			# 		scaled = TRUE,
			# 		margin = 0,
			# 		cex = rep(1.5, 3),
			# 		cat.cex = rep(1.5, 2),
			# 		ind = FALSE # Do not automatically draw the diagram
			# 	)
			# } else if(length(list_to_return$elements_involved) == 3){
			# 	list_to_return$venn_diagram <- VennDiagram::draw.triple.venn(
			# 		area1 = length(unique(vector_items_for_unique_names_in_row[[1]])),
			# 		area2 = length(unique(vector_items_for_unique_names_in_row[[2]])),
			# 		area3 = length(unique(vector_items_for_unique_names_in_row[[3]])),
			# 		n12 = length(output$unique_overlapping_elements_between_vectors_one_and_two),
			# 		n23 = length(output$unique_overlapping_elements_between_vectors_two_and_three),
			# 		n13 = length(output$unique_overlapping_elements_between_vectors_one_and_three),
			# 		n123 = length(output$unique_overlapping_elements_between_vectors_one_two_and_three),
			# 		category = c(label_one, label_two, label_three),
			# 		lty = rep("blank", 3), # Line dash pattern of the circles
			# 		fill = c("light blue", "light green", "pink"),
			# 		alpha = rep(0.5, 3),
			# 		cat.pos = c(0, 0, 180), # Category position around the circles (in degrees)
			# 		cat.dist = rep(0.025, 3), # Category names' distances from the edges of the circles (can be negative)
			# 		scaled = TRUE,
			# 		margin = 0,
			# 		cex = rep(1.5, 7),
			# 		cat.cex = rep(1.5, 3),
			# 		ind = FALSE # Do not automatically draw the diagram
			# 	)
			# }

			return(list_to_return)
		}
	)

	# str(combination_set_operations)

	return(combination_set_operations)
} # End of function definition

# Test the function:
# compare_vectors(vectors_to_use)

# If we want to get similar elements across list items, we can do so with the purrr package, as an example:
# library('purrr')
# test_example <- compare_vectors(vectors_to_use[1:2])
# purrr::map(test_example, "elements_involved")

# Get a Venn diagram
# test_example <- compare_vectors(vectors_to_use[1:2])
# # Plot the diagram:
# grid::grid.newpage()
# grid::grid.draw(test_example[[2]]$venn_diagram)

# Hence, e.g., to find all comparisons that involve "vector_a":
# test_example[
# 	sapply(
# 		purrr::map(test_example, "elements_involved"),
# 		function(x){"vector_a" %in% x}
# 	)
# ]

# Or, to get all elements that did a two-way comparison:
# test_example[
# 	sapply(
# 		purrr::map(test_example, "elements_involved"),
# 		function(x){length(x) == 2}
# 	)
# ]
