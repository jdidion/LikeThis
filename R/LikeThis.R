#' Options
#' 
#' @name options
#' @export
delayedAssign("options", Options$new(
    libraries=list(
        default=RegExpR, 
        re2=function() {
            stopifnot(require(Rre2, quietly=TRUE))
            RegExpRE2
        }
    ),
    defaults=c(list(
        lib=RegExpR,
        match.var=".match",
        enable.flags=TRUE
    ), .supported.flags)
))

.supported.flags <- list(
    ignore.case=FALSE,
    multi.line=FALSE,
    single.line=FALSE,
    ungreedy=FALSE,
    xtended=FALSE
)

Options <- setRefClass("Options",
fields=list(libraries="list", options="list", defaults="list"),
methods=list(
    register=function(name, fn) {
        .self$libraries[[name]] <- fn
    },
    set=function(name, value) {
        stopifnot(name %in% names(.self$defaults))
        current <- .self$options[[name]]
        switch(name,
            lib={
                stopifnot(name %in% names(.self$libraries))
                lib <- .self$libraries[[value]]
                if (is.function(lib)) {
                    lib <- lib()
                    .self$libraries[[name]] <- lib
                }
                .self$options$lib <- lib
            },
            { #default
                .self$options[[name]] <- value
            }
        )
    },
    get=function(name) {
        stopifnot(name %in% names(.self$defaults))
        if (name %in% names(.self$options)) {
            .self$options[[name]]
        }
        else {
            value <- .self$defaults[[name]]
        }
    },
    get.flags=function(flags) {
        for (n in names(.supported.flags)) {
            if (!(n %in% names(flags))) {
                flags[[n]] <- .self$get(n)
            }
        }
        flags
    }
))

#' Class that encapsulates a RegExp and provides matching. This class shouldn't
#' be instantiated directly; instead use `get.regexp` or one of the infix
#' operators.
#'
#' @name RegularExpression-class
#' @aliases RegularExpression
#' @rdname RegularExpression-class
#' @exportClass RegularExpression
#' @export RegularExpression
RegularExpression <- setRefClass("RegularExpression",
fields=c("pattern", "regexp"),
methods=list(
    #' Create a new RegularExpression, which wraps an
    #' re2 regexp.
    #'
    #' @param pattern an re2-compatible regular expression
    #' (POSIX or PERL, although some PERL symbols are not
    #' supported).
    initialize=function(..., pattern=character(), flags=list()) {
        .self$pattern <- pattern
        .self$regexp <- options$get("lib")$new(pattern, flags)
        .self
    },
    #' Match a string (or vector of strings) against
    #' a regular expression.
    #'
    #' @param x vector of strings to match against
    #' 
    #' @return a MatchResult object
    match=function(x) {
        .self$regexp$match(x);
    }
))

#' Fetch a cached regexp for `pattern` if it was previously requested, 
#' or compile the pattern and cache the regexp.
#'
#' @param pattern Perl-compatible regular expression.
#' @param opts regular expression flags
#' @param recompile ignore any cached regexp for `pattern`
#'
#' @return `RegularExpression` object (regexp)
#' @export
get.regexp <- function(pattern, flags=list(), recompile=FALSE) {
    if (options$get("enable.flags")) {
        flags <- options$get.flags(flags)
        key <- paste(c(pattern, flags), collapse="/")
    }
    else {
        flags <- NULL
        key <- pattern
    }
    if (!recompile && pattern %in% names(attr(get.regexp, "cache"))) {
        regexp <- cache[[key]]
    }
    else {
        regexp <- RegularExpression$new(pattern=pattern, flags=flags)
        attr(get.regexp, "cache")[[key]] <- regexp
    }
    regexp
}
attr(get.regexp, "cache") <- list()

#' Convenience function for passing a pattern and flags to the right-hand side
#' of an infix regular expression operator.
#' 
#' @param pattern
#' @param ... flag names (can be single-character)
#' 
#' @return RegularExpression
#' 
#' @examples
#' # matches because the 'i' flag makes it case-insensitive
#' # and the 's' flag matches the dot to newlines
#' 'a\nb' %~% RE('A.B', 'i', 's') 
#' 
#' @export
RE <- function(pattern, ...) {
    flags <- c(...)
    if (length(flags) > 0) {
        flag.names <- match.arg(flags, names(.supported.flags), several.ok=TRUE)
        flags <- rep(TRUE, length(flags))
        names(flags) <- flag.names
    }
    get.regexp(pattern, flags)
}

#' Infix regular expression match operator.
#'
#' Note: if lhs is not a string, this method tries to coerce
#' it to one.
#'
#' @param lhs the string to be matched
#' @param rhs a regular expression string or RegularExpression
#' object (obtained from `get.regexp` or `RE`).
#' 
#' @return a `MatchResult` if there was a match, otherwise NULL.
#'
#' @rdname match
#' @export
`%~%` <- function(lhs, rhs) {
    if (!is.character(lhs)) {
        lhs <- as.character(lhs)
    }
    if (is.character(rhs)) {
        rhs <- get.regexp(rhs)
    }
    stopifnot(is(rhs, RegularExpression))
    rhs$match(lhs)
}

#' Infix regular expression match operator with
#' side effects.
#' 
#' If there is a match, returns TRUE and also
#' assigns the match result to a variable in the
#' current environment (`.match` by default).
#'
#' Note: if lhs is not a string, this method tries to coerce
#' it to one.
#' 
#' @param lhs the string to be matched
#' @param rhs a regular expression string or RegularExpression
#' object (obtained from `get.regexp` or `RE`).
#' 
#' @return logical
#' 
#' @note due to the way scoping works in R, the
#' value of the match variable will be overwritten
#' upon each use of this operator. To perform nested
#' use of this operator, you need to place each within
#' a `local()` block.
#' 
#' @examples
#' # prints 'a'; .match is set in the calling environment
#' if ('a' %?~% '([ab])') print(.match$group(1)) 
#' 
#' # prints 'a'; .match is *not* set in the calling environment
#' local(if ('a' %?~% '([ab])') print(.match$group(1))) # prints 'a'; 
#' 
#' @seealso local
#' @export
`%?~%` <- function(lhs, rhs) {
    if (!is.character(lhs)) {
        lhs <- as.character(lhs)
    }
    if (is.character(rhs)) {
        rhs <- get.regexp(rhs)
    }
    stopifnot(is(rhs, RegularExpression))
    match <- regexp$match(lhs)
    if (is.null(match)) {
        assign(options$get("match.var"), NULL)
        FALSE
    }
    else {
        assign(options$get("match.var"), match, envir=parent.frame())
        TRUE
    }
}
