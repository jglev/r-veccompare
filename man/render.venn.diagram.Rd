% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render.venn.diagram.R
\name{render.venn.diagram}
\alias{render.venn.diagram}
\title{Render (Print) a Previously-Computed Venn Diagram}
\usage{
render.venn.diagram(venn_diagram_created_with_VennDiagram_package,
  viewport_npc_width_height_to_force = 1)
}
\arguments{
\item{venn_diagram_created_with_VennDiagram_package}{A grid-based diagram object. For example, a Venn diagram previously generated using \code{veccompare::compare.vectors()}.}

\item{viewport_npc_width_height_to_force}{The scale at which to print the image. If the image is cut off at its edges, for example, this can be set lower than 1.0.}
}
\value{
The function will not return a value; rather, it will print the image.
}
\description{
A wrapper function for printing a \code{grid}-based image using \code{grid::grid.draw()}.
}
\examples{
# Create the comparisons across vectors:
example <- veccompare::compare.vectors(
veccompare::example.vectors.list,
draw_venn_diagrams = TRUE
)

# Get the first comparison that includes a diagram:
diagram <- example[[
which(
	sapply(
		purrr::map(example, "elements_involved"),
		function(x){length(x) == 3}
	)
)[1]
]]$venn_diagram

# Print the diagram:
veccompare::render.venn.diagram(diagram)
}