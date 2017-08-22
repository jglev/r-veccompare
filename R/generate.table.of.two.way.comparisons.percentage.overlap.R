summarize.two.way.comparisons.percentage.overlap <- function(
	named_list_of_vectors,
	output_type = "table", # c("table", "matrix_plot", "network_graph")
	melt_table = FALSE # Overridden by output_type
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
		melted_table <- melted_table[
			melted_table$Vector_Name != melted_table$Overlaps_With
			, # Use all columns
			]
		
		list_item_sizes <- sapply(named_list_of_vectors[order(names(named_list_of_vectors))], length)
		list_item_relative_sizes <- list_item_sizes / max(list_item_sizes)*10
		
		message("Drawing network graph...")
		
		qgraph_output <- qgraph::qgraph(
			melted_table[order(melted_table$Vector_Name),], # Put this in alphabetical order by the 'Vector_Name' column. This way, we can line these names up with the vsize names below.
			esize = 5,
			directed = TRUE,
			theme = "gray",
			edge.labels = TRUE,
			shape = "circle",
			# labels = Labels
			vsize = list_item_relative_sizes, # Get the size of each list object in the order it appears in the melted table (this is from using 'vsize = c(1,2,3,4,5,6)' and noting that the size increases were in line with the output of 'unique(melted_table$Vector_Name)')
			minimum = 0,
			threshold = -1, # Set this lower than 0, to effectively turn it off.
			DoNotPlot = FALSE
		)
		
		# return(qgraph_output)
	} else {
		stop("No 'output_type' selected.") # This shouldn't happen, but just in case it does, we'll throw an error message here.
	}
	
} # End of function definition

summarize.two.way.comparisons.percentage.overlap(veccompare::example.vectors.list)

summarize.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	output_type = "table",
	melt_table = TRUE
)

summarize.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	output_type = "matrix_plot"
)

summarize.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	output_type = "network_graph"
)

