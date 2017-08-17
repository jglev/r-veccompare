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

render.venn.diagram <- function(
	venn_diagram_created_with_VennDiagram_package,
	viewport_npc_width_height_to_force = 1.0
){
	grid::grid.newpage()
	grid::pushViewport(grid::viewport(
		width = grid::unit(viewport_npc_width_height_to_force, "npc"),
		height = grid::unit(viewport_npc_width_height_to_force, "npc")
	)); # Following https://stackoverflow.com/questions/21234439/how-to-force-the-labels-to-fit-in-venndiagram#comment75690400_22826211, force the output rendering mechanism to be smaller than normal, in order not to cut off diagram names.
	grid::grid.draw(venn_diagram_created_with_VennDiagram_package)
}
