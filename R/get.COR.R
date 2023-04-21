#' Gets a correlation value for two vectors.
#'
#' Calculates a correlation between two vectors.
#'
#' @param sim A vector with the predicted values
#' @param obs A vector with the observed values
#' @param ... Variables passed to method implementations.
#'
#' @return A logical vector with \code{TRUE} if the corresponding
#'         value in \code{x} is divisible by the corresponding value
#'         in \code{y} considering the specified tolerance level;
#'         \code{FALSE} otherwise
#'
#' @export
#'
get.COR <-function(sim, obs, ...) UseMethod("get.COR")

#' @describeIn get.COR Default implementation
#'
#' @details
#' This is the default implementation that works with
#' two vectors of classes "integer", "numeric" or "ts".
#'
#' @param use, method Pass to an internal call to the
#'        \code{stats::cor()} function
#' @param ... Ignored
#'
#' @importFrom stats cor
#'
#' @export
#'
get.COR.default <- function(sim, obs,
                           use = "everything",
                           method = c("pearson", "kendall", "spearman"),
                           ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)

  COR <- cor(sim, obs, use = use, method = method)

  return(COR)
}
