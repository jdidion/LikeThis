# LikeThis

LikeThis is an R package that introduces an infix regular expression operator (%~%).

## Installation

`r devtools::install_github("jdidion/LikeThis")`

In addition, you may want to make use of the fast regular expression matching provided by the re2 library. If so, install the [Rre2 package](https://github.com/jdidion/Rre2).

## Usage

LikeThis is currently in early alpha stage. You can match a string to a regular expression and extract captured groups from the result. For example:

```r
library(LikeThis)

result <- 'a' %~% '([ab])'
result$group(1) # prints 'a'
```

## Roadmap

1. Add unit tests
2. Vectorization
3. Provide substitution operator
