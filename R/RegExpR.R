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
    initialize=function(pattern, flags=NULL) {
        if (!is.null(flags) && length(flags) > 0) {
            flag.str <- paste0(
                .flag.set(flags, 'ignore.case', 'i', ''),
                .flag.set(flags, 'multi.line', 'm', ''),
                .flag.set(flags, 'single.line', 's', ''),
                .flag.set(flags, 'xtended', 'x', '')
            )
            if (nchar(flag.str) > 0) {
                pattern <- paste0("(?", flag.str, ")", pattern)    
            }
        }
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
        if (result[1] == -1) {
            NULL
        }
        else {
            MatchResultR$new(text=x, result=result)
        }
    }
))

.flag.set <- function(flags, name, when.true=TRUE, when.false=FALSE) {
    ifelse(name %in% names(flags) && flags[[name]], when.true, when.false)
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
    group=function(i) {
        if (is.vector(i)) {
            sapply(i, function(j) .self$group(j))
        }
        else {
            if (i > .self$num.groups) {
                stop(paste("Invalid group:", i))
            }
            if (is.na(.self$groups[i+1])) {
                if (i == 0) {
                    start <- .self$result[1]
                    end <- start + attr(.self$result, "match.length", exact=TRUE) - 1
                }
                else {
                    start <- attr(.self$result, "capture.start", exact=TRUE)[i]
                    end <- start + attr(.self$result, "capture.length", exact=TRUE)[i] - 1
                }
                group <- substr(.self$text, start, end)
                .self$groups[i+1] <- group
            }
            else {
                group <- .self$groups[i+1]
            }
            group
        }
    }
))
setMethod("[", c("MatchResultR", "numeric", "missing", "ANY"), function(x, i, j, ..., drop=TRUE) {
    x$group(i)
})
setMethod("[[", c("MatchResultR", "numeric", "missing"), function(x, i, j, ...) {
    x$group(i)
})