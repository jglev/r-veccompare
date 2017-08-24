#' Render (Print) a Previously-Computed Venn Diagram
#'
#' A wrapper function for printing a \code{grid}-based image using \code{grid::grid.draw()}.
#'
#' @param venn_diagram_created_with_VennDiagram_package A grid-based diagram object. For example, a Venn diagram previously generated using \code{veccompare::compare.vectors()}.
#' @param viewport_npc_width_height_for_images The scale at which to print an image. If the image is cut off at its edges, for example, this can be set lower than 1.0.
#'
#' @return The function will not return a value; rather, it will print the image.
#' @export
#'
#' @examples
#' # Create the comparisons across vectors:
#' example <- veccompare::compare.vectors(
#' veccompare::example.vectors.list,
#' draw_venn_diagrams = TRUE
#' )
#'
#' # Get the first comparison that includes a diagram:
#' diagram <- example[[
#' which(
#' 	sapply(
#' 		purrr::map(example, "elements_involved"),
#' 		function(x){length(x) == 3}
#' 	)
#' )[1]
#' ]]$venn_diagram
#'
#' # Print the diagram:
#' veccompare::render.venn.diagram(diagram)
render.venn.diagram <- function(
	venn_diagram_created_with_VennDiagram_package,
	viewport_npc_width_height_for_images = 1.0
){
	grid::grid.newpage()
	grid::pushViewport(grid::viewport(
		width = grid::unit(viewport_npc_width_height_for_images, "npc"),
		height = grid::unit(viewport_npc_width_height_for_images, "npc")
	)); # Following https://stackoverflow.com/questions/21234439/how-to-force-the-labels-to-fit-in-venndiagram#comment75690400_22826211, force the output rendering mechanism to be smaller than normal, in order not to cut off diagram names.
	grid::grid.draw(venn_diagram_created_with_VennDiagram_package)
}
