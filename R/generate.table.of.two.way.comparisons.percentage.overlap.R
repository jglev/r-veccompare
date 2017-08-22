generate.table.of.two.way.comparisons.percentage.overlap <- function(
	named_list_of_vectors,
	output_table_as_plot = FALSE,
	melt_table = FALSE # Overridden by output_table_as_plot
){
	
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
	
	if(output_table_as_plot == TRUE){
		# See https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html for corrplot examples:
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
		
		return(plot_of_table)
	} else if(output_table_as_plot == FALSE & melt_table == TRUE){
		melted_matrix <- reshape2::melt(as.matrix(output_table))
		
		colnames(melted_matrix) <- c("Vector_Name", "Overlaps_With", "Decimal_Percentage")
		
		return(melted_matrix)
	}	else{
		return(output_table)
	}
	
} # End of function definition

generate.table.of.two.way.comparisons.percentage.overlap(veccompare::example.vectors.list)

generate.table.of.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	melt_table = TRUE
)

generate.table.of.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	output_table_as_plot = TRUE
)






melted_table <- generate.table.of.two.way.comparisons.percentage.overlap(
	veccompare::example.vectors.list,
	melt_table = TRUE
)

# Remove self-directed edges, as they aren't useful for this type of network graph:
melted_table <- melted_table[
	melted_table$Vector_Name != melted_table$Overlaps_With
	, # Use all columns
]

igraph_object <- graph.data.frame(melted_table, directed = TRUE)
# We can see that the Decimal percentages are present; we'll use them as weights below:
# E(igraph_object)$Decimal_Percentage


plot.igraph(
	igraph_object,
	vertex.label = V(igraph_object)$name,
	vertex.shape = "square",
	layout = 
		# layout.fruchterman.reingold,
		layout.circle,
	edge.color = "gray",
	
	edge.arrow.size = 0.5,
	
	edge.curved = rep(0.2, ecount(igraph_object)),
	edge.width = E(igraph_object)$Decimal_Percentage*10,
	vertex.frame.color = "white",
	vertex.color = "white",
	vertex.size = 30
	
	#, vertex.label.dist = 1
	#, vertex.label.degree = 180
)



library(GGally)

igraph_object %v% "vertex_size" <- sapply(named_list_of_vectors, length)


ggnet2(
	igraph_object,
	node.color = "black",
	edge.size = 1,
	edge.color = "grey",
	mode = "circle",
	size = "phono"
)









