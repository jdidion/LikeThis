% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LikeThis.R
\name{\%?~\%}
\alias{\%?~\%}
\title{Infix regular expression match operator with
side effects.}
\usage{
lhs \%?~\% rhs
}
\arguments{
\item{lhs}{the string to be matched}

\item{rhs}{a regular expression string or RegularExpression
object (obtained from `get.regexp` or `RE`).}
}
\value{
logical
}
\description{
If there is a match, returns TRUE and also
assigns the match result to a variable in the
current environment (`.match` by default).
}
\details{
Note: if lhs is not a string, this method tries to coerce
it to one.
}
\note{
due to the way scoping works in R, the
value of the match variable will be overwritten
upon each use of this operator. To perform nested
use of this operator, you need to place each within
a `local()` block.
}
\examples{
# prints 'a'; .match is set in the calling environment
if ('a' \%?~\% '([ab])') print(.match$group(1))

# prints 'a'; .match is *not* set in the calling environment
local(if ('a' \%?~\% '([ab])') print(.match$group(1))) # prints 'a';
}
\seealso{
local
}

