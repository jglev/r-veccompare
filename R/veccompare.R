#' veccompare: Automatically Generate All n-Wise Set Comparisons on Vectors
#'
#' The \pkg{veccompare} package contains functions for automating set operations. Given a named list of 5 vectors, for example, \pkg{veccompare} can calculate all 2-, 3-, 4-, and 5-way comparisons between those vectors, recording information for each comparison about the set "union" (combined elements), "intersection" (overlap / shared elements), and compliments (which elements are unique to each vector involved in the comparison).
#'
#' The veccompare package contains functions for automating set operations (i.e., comparisons of overlap) between multiple vectors.
#'
#' The package also contains a function for automating reporting in RMarkdown, by generating markdown output for easy analysis, as well as an RMarkdown template for use with RStudio.
#'
#' The primary function from \pkg{veccompare} is \code{\link{compare.vectors}}. Complementarily, \code{\link{compare.vectors.and.return.text.analysis.of.overlap}} will call \code{\link{compare.vectors}} and generate Markdown-style output from it (for example, for use within an RMarkdown file).
#'
#' An RMarkdown template illustrating several of \pkg{veccompare}'s features can be used from within RStudio by clicking \code{File -> New File -> R Markdown... -> From Template -> Veccompare Overlap Report}.
#'
#' \pkg{veccompare} also provides a function, \code{\link{summarize.two.way.comparisons.percentage.overlap}}, that can create correlation-plot-style images and network graphs for all two-way comparisons between vectors. This function is also demonstrated in the \code{Veccompare Overlap Report} described above.
#'
# The below were recommended after using Ctrl + Shift + E in RStudio to do CRAN-like checks on the package.
#' @importFrom "grDevices" "colors"
#' @importFrom "graphics" "par"
#' @importFrom "stats" "runif"
#' @importFrom "utils" "head"
#' @importFrom "utils" "tail"
"_PACKAGE"
