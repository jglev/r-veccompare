#' Summarize Percentage Overlap for Two-Way Comparisons between Vectors
#'
#' @inheritParams compare.vectors
#' @param output_type Either \code{"table"}, \code{"matrix_plot"}, or \code{"network_graph"}. \code{"table"} will return a matrix showing percentage overlap between each pair of vectors. \code{"matrix_plot"} will plot this table, coloring it by the amount of overlap. \code{"network_graph"} will return a network graph image illustrating the overlap percentages between each pair of vectors.
#' @param melt_table A logical (TRUE/FALSE) indicator, when \code{output_type} is \code{"table"}, whether to print the output in \code{\link[reshape2]{melt}ed} form (using the \pkg{reshape2} package).
#' @param network_graph_minimum \code{minimum} argument from \code{\link[qgraph]{qgraph}}, for when \code{output_type} is \code{"network_graph"}.
#' @param margins_for_plot The margins for image output (if \code{output_type} is \code{matrix_plot} or \code{network_graph}). Specified as a vector of numbers, in the form \code{c(bottom, left, top, right)}. If \code{output_type} is \code{matrix_plot}, defaults to \code{c(2, 0, 1, 0)}; if \code{output_type} is \code{network_graph}, defaults to \code{c(3, 3, 3, 0.5)}.
#'
#' @return Either a matrix (if \code{output} is \code{"table"}), or an image (if \code{output} is \code{"matrix_plot"} or \code{"network_graph"}). If an image is printed, nothing is returned by the function; rather, the output is printed immediately.
#'
#' If \code{output} is \code{"table"} and \code{melt_table} is \code{FALSE}, the output will be a matrix with \code{nrow} and \code{ncol} both equal to the number of vectors in \code{named_list_of_vectors_to_compare}. This table shows the decimal percentage overlap (e.g., "0.20" = 20\%) between each combination of vectors. \emph{This table is intended to be read with row names first, in this form:} "[row title] overlaps with [column title] [cell value] percent."
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
	named_list_of_vectors_to_compare,
	output_type = "table", # c("table", "matrix_plot", "network_graph")
	melt_table = FALSE, # Overridden by output_type
	network_graph_minimum = 0,
	margins_for_plot = NULL
){

	if(! output_type %in% c("table", "matrix_plot", "network_graph")){
		stop("'output_type' must be one of the following: 'table', 'matrix_plot', 'network_graph'.")
	} else {
		if(length(output_type) != 1){
			stop("'output_type' must have only 1 value.")
		}
	}

		if(is.null(margins_for_plot)){
			if(output_type == "matrix_plot"){
				margins_for_plot <- c(2, 0, 1, 0) # Increase the margins, so that nothing gets cut off at the top and bottom of the plot.
			} else if(output_type == "network_graph"){
				margins_for_plot <- c(3, 3, 3, 0.5) # Margins (for the plot, not the legend): c(bottom, left, top, right)
			} else { # We shouldn't ever get to this step; I'm just providing a fallback to make future code expansion easier.
				margins_for_plot <- c(1, 1, 1, 1)
			}
		}

	# Compute all two-way comparisons:
	two_way_comparison_output <- veccompare::compare.vectors(
		named_list_of_vectors_to_compare,
		degrees_of_comparison_to_include = 2,
		suppress_messages = TRUE
	)

	output_table <- matrix( # We'll fill this in below
		nrow = length(named_list_of_vectors_to_compare),
		ncol = length(named_list_of_vectors_to_compare)
	)

	# Set the diagonal to 1, since every element overlaps 100% with itself
	diag(output_table) <- 1.00

	rownames(output_table) <- names(named_list_of_vectors_to_compare)
	colnames(output_table) <- names(named_list_of_vectors_to_compare)

	for(list_element in two_way_comparison_output){

		for(involved_vector_for_getting_unique_elements in list_element[["elements_involved"]]){

			percent_unique_to_involved_vector <- 1.00 - round(length(list_element[["elements_unique_to_first_element"]][[involved_vector_for_getting_unique_elements]])/length(unique(named_list_of_vectors_to_compare[[involved_vector_for_getting_unique_elements]])), 2)

			# We specify row and column in this way because it works correctly with reshape2::melt below. The way to view the output is across: "[row title] overlaps with [column title] [cell value] percent."
			output_table[
				involved_vector_for_getting_unique_elements,
				list_element[["elements_involved"]][list_element[["elements_involved"]] != involved_vector_for_getting_unique_elements]
			] <- percent_unique_to_involved_vector

		} # End of for loop over involved_vector_for_getting_unique_elements
	} # End of for loop over list_element

	if(output_type == "matrix_plot"){
		# See https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html for corrplot examples:

		message("Drawing plot of table...")

		par(xpd=TRUE)

		corrplot::corrplot(
			output_table,
			method="number",
			is.corr = FALSE,
			tl.col = "black", # Set title color to black
			diag = FALSE,
			order = "alphabet",

			# Widen the legend:
			cl.ratio = 0.2,
			cl.align = "r",
			cl.lim = c(0, 1.0),
			tl.cex = 0.8, # Title font size ratio.
			mar = margins_for_plot
		)

		# return(plot_of_table)
	} else if(output_type == "table"){
		if(melt_table == TRUE){
			melted_matrix <- reshape2::melt(as.matrix(output_table))
			colnames(melted_matrix) <- c("Vector_Name", "Overlaps_With", "Decimal_Percentage")

			# Remove self-directed edges, as they aren't really meaningful here:
			melted_matrix <- melted_matrix[
				melted_matrix$Vector_Name != melted_matrix$Overlaps_With
				, # Use all columns
			]

			rownames(melted_matrix) <- NULL # For aesthetics, remove row numbers.

			return(melted_matrix)
		} else {
			return(output_table)
		}
	}	else if(output_type == "network_graph") {

		message("Drawing network graph...")

		list_item_sizes <- sapply(named_list_of_vectors_to_compare[order(names(named_list_of_vectors_to_compare))], length)
		list_item_relative_sizes <- list_item_sizes / max(list_item_sizes)*10

		qgraph::qgraph(
			output_table[order(rownames(output_table)), order(colnames(output_table))], # We're alphabetizing row and column name orders here to more easily match up with the values for the 'nodeNames' argument below.
			esize = 5,
			directed = TRUE,
			theme = "gray",
			edge.labels = TRUE,
			shape = "circle",
			vsize = list_item_relative_sizes, # Get the size of each list object in the order it appears in the melted table (this is from using 'vsize = c(1,2,3,4,5,6)' and noting that the size increases were in line with the output of 'unique(melted_matrix$Vector_Name)')
			minimum = network_graph_minimum,
			threshold = -1, # Set this lower than 0, to effectively turn it off.
			DoNotPlot = FALSE,

			layout = "circle",

			legend = TRUE,
			labels = c(1:length(named_list_of_vectors_to_compare)), # names(named_list_of_vectors_to_compare)[order(names(named_list_of_vectors_to_compare))], # Note: There seems to be a bug with this package when using an edge list and this option, which is why I'm using output_table here above.
			label.scale = TRUE,
			label.scale.equal = FALSE,
			label.cex = 2,
			#groups = names(named_list_of_vectors_to_compare)[order(names(named_list_of_vectors_to_compare))], # Cause the nodes to be colored based on their names.

			nodeNames = names(named_list_of_vectors_to_compare)[order(names(named_list_of_vectors_to_compare))],
			legend.cex = 0.3,
			mar = margins_for_plot
		)

		# return(qgraph_output)
	} else {
		stop("No 'output_type' selected.") # This shouldn't happen, but just in case it does, we'll throw an error message here.
	}

} # End of function definition
