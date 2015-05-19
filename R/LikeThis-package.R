#' Adds infix regular expression operators to R. 
#' 
#' R package \bold{LikeThis} adds infix regular expression operators to R that will be
#' familiar to perl users. The package provides two matching operators: \code{\%~\%}, 
#' which returns a MatchResultR (which has methods for accessing capture groups), and 
#' \code{\%?~\%}, which returns a logical and makes capture groups accessible in the local 
#' environment via the \code{.match} list variable. The later can be used iteratively
#' in a loop to perform progressive matching. The package also provides a substitution 
#' operator, \code{\%!~\%}.
#'
#' @name LikeThis-package
#' @aliases LikeThis
#' @seealso \code{\link{RegularExpression}}, \code{\link{RegExpR-class}}
NULL
