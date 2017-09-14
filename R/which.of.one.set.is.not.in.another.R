#' Which of One Set is not in Another
#'
#' This function is a wrapper for \code{\link{setdiff}}. It makes it easier to remember which vector is being subtracted from the other, by displaying an explicit message.
#'
#' @param set_1 A vector to be subtracted from.
#' @param set_2 A vector to subtract from \code{set_1}.
#' @param suppress_messages A logical (TRUE/FALSE) indicator whether to suppress messages.
#'
#' @return A vector of the values of \code{set_1} that are not present in \code{set_2}. Put differently, a vector resulting from subtracting \code{set_2} from \code{set_1}.
#' @export which.of.one.set.is.not.in.another
#'
#' @examples
#' veccompare::which.of.one.set.is.not.in.another(
#'     veccompare::example.vectors.list$vector_a,
#'     veccompare::example.vectors.list$vector_b
#' )
#'
#' veccompare::which.of.one.set.is.not.in.another(
#'     veccompare::example.vectors.list$vector_b,
#'     veccompare::example.vectors.list$vector_a
#' )
#'
which.of.one.set.is.not.in.another <- function(
	set_1,
	set_2,
	suppress_messages = FALSE
){
	if(suppress_messages != TRUE){
		message("Displaying the elements of the first set that are not present in the second set (i.e., subtracting the second set from the first set)...")
	}

	return(setdiff(set_1, set_2))
}
