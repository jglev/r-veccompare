#' Summarize Percentage Overlap for Two-Way Comparisons between Vectors
#'
#' @param named_list_of_vectors (As in \code{\link{compare.vectors}}.)
#' @param output_type Either \code{"table"}, \code{"matrix_plot"}, or \code{"network_graph"}. \code{"table"} will return a matrix showing percentage overlap between each pair of vectors. \code{"matrix_plot"} will plot this table, coloring it by the amount of overlap. \code{"network_graph"} will return a network graph image illustrating the overlap percentages between each pair of vectors.
#' @param melt_table A logical (TRUE/FALSE) indicator, when \code{output_type} is \code{"table"}, whether to print the output in \code{\link[reshape2]{melt}ed} form (using the \pkg{reshape2} package).
#' @param network_graph_minimum \code{minimum} argument from \code{\link[qgraph]{qgraph}}, for when \code{output_type} is \code{"network_graph"}.
#'
#' @return Either a matrix (if \code{output} is \code{"table"}), or an image (if \code{output} is \code{"matrix_plot"} or \code{"network_graph"}). If an image is printed, nothing is returned by the function; rather, the output is printed immediately.
#'
#' If \code{output} is \code{"table"} and \code{melt_table} is \code{FALSE}, the output will be a matrix with \code{nrow} and \code{ncol} both equal to the number of vectors in \code{named_list_of_vectors}. This table shows the decimal percentage overlap (e.g., "0.20" = 20\%) between each combination of vectors. \emph{This table is intended to be read with column names first, in this form:} "[column title] overlaps with [row title] [cell value] percent."
#'
#' If \code{output} is \code{"table"} and \code{melt_table} is \code{TRUE}, the output will be a \code{\link[reshape2]{melt}ed} data.frame with three columns: \code{Vector_Name}, \code{Overlaps_With}, and \code{Decimal_Percentage}.
#'
#' @export
#'
#' @examples
#' summarize.two.way.comparisons.percentage.overlap(veccompare::example.vectors.list)
#' summarize.two.way.comparisons.percentage.overlap(
#' 	veccompare::example.vectors.list,
#' 	output_type = "table",
#' 	melt_table = TRUE
#' )
#'
#' summarize.two.way.comparisons.percentage.overlap(
#' 	veccompare::example.vectors.list,
#' 	output_type = "matrix_plot"
#' )
#'
#' summarize.two.way.comparisons.percentage.overlap(
#' 	veccompare::example.vectors.list,
#' 	output_type = "network_graph"
#' )
summarize.two.way.comparisons.percentage.overlap <- function(
	named_list_of_vectors,
	output_type = "table", # c("table", "matrix_plot", "network_graph")
	melt_table = FALSE, # Overridden by output_type
	network_graph_minimum = 0
){

	if(! output_type %in% c("table", "matrix_plot", "network_graph")){
		stop("'output_type' must be one of the following: 'table', 'matrix_plot', 'network_graph'.")
	} else {
		if(length(output_type) != 1){
			stop("'output_type' must have only 1 value.")
		}
	}

	# Comput all two-way comparisons:
	two_way_comparison_output <- veccompare::compare.vectors(
		named_list_of_vectors,
		degrees_of_comparison_to_include = 2
	)

	output_table <- matrix( # We'll fill this in below
		nrow = length(named_list_of_vectors),
		ncol = length(named_list_of_vectors)
	)

	# Set the diagonal to 1, since every element overlaps 100% with itself
	diag(output_table) <- 1.00

	rownames(output_table) <- names(named_list_of_vectors)
	colnames(output_table) <- names(named_list_of_vectors)

	for(list_element in two_way_comparison_output){

		for(involved_vector_for_getting_unique_elements in list_element[["elements_involved"]]){

			percent_unique_to_involved_vector <- 1.00 - round(length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]])/length(unique(named_list_of_vectors[[involved_vector_for_getting_unique_elements]])), 2)

			# We specify row and column in this way because it works correctly with reshape2::melt below. The way to view the output is downward: "[column title] overlaps with [row title] [cell value] percent."
			output_table[
				list_element[["elements_involved"]][list_element[["elements_involved"]] != involved_vector_for_getting_unique_elements],
				involved_vector_for_getting_unique_elements
				] <- percent_unique_to_involved_vector

		} # End of for loop over involved_vector_for_getting_unique_elements
	} # End of for loop over list_element

	if(
		(output_type == "table" & melt_table == TRUE) |
		output_type == "network_graph"
	){
			melted_matrix <- reshape2::melt(as.matrix(output_table))
			colnames(melted_matrix) <- c("Vector_Name", "Overlaps_With", "Decimal_Percentage")
	}

	if(output_type == "matrix_plot"){
		# See https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html for corrplot examples:

		message("Drawing plot of table...")

		plot_of_table <- corrplot::corrplot(
			output_table,
			method="number",
			is.corr = FALSE,
			tl.col = "black", # Set title color to black
			diag = FALSE,

			# Widen the legend:
			cl.ratio=0.2,
			cl.align="r"
		)

		# return(plot_of_table)
	} else if(output_type == "table"){
		if(melt_table == TRUE){
			return(melted_matrix)
		} else {
			return(output_table)
		}
	}	else if(output_type == "network_graph") {
		# Remove self-directed edges, as they aren't useful for this type of network graph:
		melted_matrix <- melted_matrix[
			melted_matrix$Vector_Name != melted_matrix$Overlaps_With
			, # Use all columns
			]

		list_item_sizes <- sapply(named_list_of_vectors[order(names(named_list_of_vectors))], length)
		list_item_relative_sizes <- list_item_sizes / max(list_item_sizes)*10

		message("Drawing network graph...")

		qgraph_output <- qgraph::qgraph(
			# as.matrix(melted_matrix[order(melted_matrix$Vector_Name),1:2]), # Put this in alphabetical order by the 'Vector_Name' column. This way, we can line these names up with the vsize names below.
			output_table,
			esize = 5,
			directed = TRUE,
			theme = "gray",
			edge.labels = TRUE,
			shape = "circle",
			# labels = Labels
			vsize = list_item_relative_sizes, # Get the size of each list object in the order it appears in the melted table (this is from using 'vsize = c(1,2,3,4,5,6)' and noting that the size increases were in line with the output of 'unique(melted_matrix$Vector_Name)')
			minimum = network_graph_minimum,
			threshold = -1, # Set this lower than 0, to effectively turn it off.
			DoNotPlot = FALSE,

			legend = TRUE,
			# labels = FALSE,
			labels = c(1:length(named_list_of_vectors)), # names(named_list_of_vectors)[order(names(named_list_of_vectors))],
			label.scale = TRUE,
			label.scale.equal = FALSE,
			label.cex = 2,
				#labels = c("a"), # as.character(seq(1:length(named_list_of_vectors))), # There currently seems to be a bug with using this option (whether with "a" or something more substantive, including a vector like 'names(named_list_of_vectors)[order(names(named_list_of_vectors))]').
			#groups = names(named_list_of_vectors)[order(names(named_list_of_vectors))], # Cause the nodes to be colored based on their names.

			nodeNames = names(named_list_of_vectors)[order(names(named_list_of_vectors))]
		)

		# return(qgraph_output)
	} else {
		stop("No 'output_type' selected.") # This shouldn't happen, but just in case it does, we'll throw an error message here.
	}

} # End of function definition
