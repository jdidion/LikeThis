#' RegExp class that uses native R regular expresssion functions.
#' Any alternative implementations should implement the same API.
#' 
#' @name RegExpR-class
#' @aliases RegExpR
#' @rdname RegExpR-class
#' @exportClass RegExpR
#' @export RegExpR
RegExpR <- setRefClass("RegExpR",
fields=c("pattern"),
methods=list(
    #' @param pattern an re2-compatible regular expression
    #' (POSIX or PERL, although some PERL symbols are not
    #' supported).
    initialize=function(pattern) {
        .self$pattern <- pattern
        .self
    },
    #' Match a string (or vector of strings) against
    #' a regular expression.
    #'
    #' @param x vector of strings to match against
    #' 
    #' @return a MatchResult object
    match=function(x) {
        result <- regexpr(.self$pattern, x, perl=T)
        .create.MatchResult(x, result)
    }
))

.create.MatchResult <- function(text, result) {
    if (result[1] == -1) {
        NULL
    }
    else {
        MatchResultR$new(text=text, result=result)
    }
}

#' MatchResult object encapsulating regexec match results.
#' 
#' @name MatchResultR-class
#' @aliases MatchResultR
#' @rdname MatchResultR-class
#' @exportClass MatchResultR
#' @export MatchResultR
MatchResultR <- setRefClass("MatchResultR",
fields=c("text", "result", "groups", "num.groups"),
methods=list(
    initialize=function(text, result) {
        .self$text <- text
        .self$result <- result
        starts <- attr(result, "capture.start", exact=TRUE)
        N <- ifelse(is.null(starts), 0, length(starts))
        .self$groups <- rep(NA, N+1)
        .self$num.groups <- N
        .self
    },
    group=function(idx) {
        if (idx > .self$num.groups) {
            stop(paste("Invalid group:", idx))
        }
        if (is.na(.self$groups[idx+1])) {
            if (idx == 0) {
                start <- .self$result[1]
                end <- start + attr(.self$result, "match.length", exact=TRUE) - 1
            }
            else {
                start <- attr(.self$result, "capture.start", exact=TRUE)[idx]
                end <- start + attr(.self$result, "capture.length", exact=TRUE)[idx] - 1
            }
            group <- substr(.self$text, start, end)
            .self$groups[idx+1] <- group
        }
        else {
            group <- .self$groups[idx+1]
        }
        group
    }
))