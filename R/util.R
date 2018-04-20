#
# Utility functions
#

# whack a zero on the front, drop the last element
#' @importFrom utils head
prepend0 <- function(x) { head(c(0, x), -1)}

# is the value a single "NA"? (is.na() is vectorised)
is.atomic.na <- function(x) { (length(x) == 1) && is.na(x) }
