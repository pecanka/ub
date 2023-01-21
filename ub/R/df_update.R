#' Recursive update
#'
#' `rows_modify_recurse()` iteratively alters the elements of a list of data 
#' frames by applying `fun()` starting with the first element in the list and 
#' going through the rest. The intended values for `fun` are `dplyr::rows_upsert`,
#' `dplyr::rows_update`, `dplyr::rows_insert`, `dplyr::rows_patch` and 
#' `dplyr::rows_delete` but can be any function that takes two dataframes.
#'
#' `rows_upsert_recurse()`, `rows_update_recurse()`, `rows_insert_recurse(),
#' `rows_patch_recurse()` and `rows_delete_recurse()` are wrappers around
#' `rows_modify_recurse()` with the dplyr functions in the place of `fun`.
#'
#' @export
rows_modify_recurse = function(d, ..., fun) {

  if(missing(fun)) 

  if(length(d) <= 1) {
    return(d)
  } 
  
  d12 = fun(d[[1]], d[[2]], ...)

  if(length(d)==2) {
    d12
  } else {
    Recall(append(list(d12), d[-(1:2)]), ...)
  }
  
}

#' @rdname rows_modify_recurse
#' @export
rows_patch_recurse = function(d, ...) {
  rows_modify_recurse(d, ..., fun = dplyr::rows_patch)
}

#' @rdname rows_modify_recurse
#' @export
rows_upsert_recurse = function(d, ...) {
  rows_modify_recurse(d, ..., fun = dplyr::rows_upsert)
}

#' @rdname rows_modify_recurse
#' @export
rows_insert_recurse = function(d, ...) {
  rows_modify_recurse(d, ..., fun = dplyr::rows_insert)
}

#' @rdname rows_modify_recurse
#' @export
rows_delete_recurse = function(d, ...) {
  rows_modify_recurse(d, ..., fun = dplyr::rows_delete)
}

#' @rdname rows_modify_recurse
#' @export
rows_update_recurse = function(d, ...) {
  rows_modify_recurse(d, ..., fun = dplyr::rows_update)
}


