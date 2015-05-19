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
            .self$defaults[[name]]
        }
    }
))

#' Options
#' 
#' @export
options <- Options$new(
    libraries=list(
        default=RegExpR, 
        re2=function() {
            require(Rre2, quietly=TRUE)
            RegExpRE2
        }
    ),
    defaults=list(
        lib=RegExpR
    )
)

#' Class that encapsulates an re2 regexp and provides matching.
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
    initialize=function(..., pattern=character()) {
        .self$pattern <- pattern
        .self$regexp <- options$get("lib")$new(pattern)
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

#' Fetch a cached regexp for `pattern` if it was
#' previously requested, or compile the pattern and cache the
#' regexp.
#'
#' @param pattern character vector of POSIX-compatible regular
#' expressions.
#' @param recompile ignore any cached regexp for `pattern`
#'
#' @return `RegularExpression` object (regexp)
#' @export
get.regexp <- function(pattern, recompile=FALSE) {
    if (!recompile && pattern %in% names(attr(get.regexp, "cache"))) {
        regexp <- cache[[pattern]]
    }
    else {
        regexp <- RegularExpression$new(pattern=pattern)
        attr(get.regexp, "cache")[[pattern]] <- regexp
    }
    regexp
}
attr(get.regexp, "cache") <- list()

LikeThis <- function() {
    function(lhs, rhs) {
        regexp <- get.regexp(rhs)
        regexp$match(lhs)
    }
}

#' LikeThis match operator.
#'
#' @rdname pipe
#' @export
`%~%`  <- LikeThis()