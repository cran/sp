get_ll_warn <- function() {
    get("ll_warn", envir = .spOptions)
}

get_ll_TOL <- function() {
    get("ll_TOL", envir = .spOptions)
}


get_ReplCRS_warn <- function() {
    get("ReplCRS_warn", envir = .spOptions)
}


set_ll_warn <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        assign("ll_warn", value, envir = .spOptions)
        get_ll_warn()
}

set_ll_TOL <- function(value) {
        stopifnot(is.numeric(value))
        stopifnot(length(value) == 1)
        stopifnot(value > 0)
        assign("ll_TOL", value, envir = .spOptions)
        get_ll_TOL()
}

set_ReplCRS_warn <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        assign("ReplCRS_warn", value, envir = .spOptions)
        get_ReplCRS_warn()
}

