#' @title
#' Flattens a list
#'
#' @description
#'
#' An alias for `as.list(unlist(...))`.
#'
#' @family list utilities provided by utilbox
#' @export
list_empty = function(x) {
  
  if(is.list(x) && !has_all_names(x))
    error("Supply either a vector of names in 'x' or a \"fully\" named list.")
  
  if(is.list(x) || has_all_names(x)) {
    x = names(x)
  }
  
  nlapply(`names<-`(rep_list(list(NA),length(x)), x), function(x) NULL)
  
}

#' @title
#' Check of class character
#'
#' @description
#'
#' `is_all_character()` checks whether all elements in an object are of class
#' `character`. For atomic 'x', `base::as.character()` is called directly. For 
#' non-atomic `x` (e.g. `list`), `base::as.character()` is called via `base::sapply()`.
#' 
#' @examples
#' is.character(letters)                      # returns TRUE
#' is.character(list(letters))                # returns FALSE
#' is_all_character(list(letters))            # returns TRUE
#' is_all_character(list(letters, 1:10))      # returns FALSE
#'
#' @family list utilities provided by utilbox
#' @export
is_all_character = function(x) {
  if(is.atomic(x)) {
    is.character(x)
  } else {
    all(sapply(x, is.character))
  }
}

#' @title
#' Flattens a list
#'
#' @description
#'
#' An alias for `as.list(unlist(...))`.
#'
#' @family list utilities provided by utilbox
#' @export
list_flatten = function(x) {
  as.list(unlist(x))
}

#' @title
#' Mirror list structure
#'
#' @description
#'
#' Mirrors the structure of the list `x`. In other words, it returns 
#' a list with the same numbers of elements at each list-depth. For now 
#' only mirroring up to depth 1 is implemented.
#'
#' @family list utilities provided by utilbox
#' @export
list_mirror = function(x, value, depth=1) {
  stopifnot(depth>=1, is.list(x))
  if(depth==1) {
    lapply(x, function(x1) rep(value, length(x1)))
  } else {
    error("Mirroring with 'depth=",depth,"' not yet implemented.")
  }
}

#' @title
#' Recursive merge
#'
#' @description
#'
#' This does a recursive merge of lists. The code is taken from the 
#' package reshape and modified to fix the bug in it. The bug was the 
#' missing ellipsis in the call to `Recall()`.
#'
#' @family list utilities provided by utilbox
#' @export
merge_recurse = function(dfs, ...) {

  if(length(dfs) == 1) {
    dfs
  } else if(length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all = TRUE, sort = FALSE, ...)
  } else {
    merge(dfs[[1]], Recall(dfs[-1], ...), all = TRUE, sort = FALSE, ...)
  }

}

#' @title
#' Recursive append
#'
#' @description
#'
#' Appends multiple objects together by recursively calling 
#' [`base::append`].
#'
#' @examples
#' append_recurse(as.list(1:3), as.list(4:7), as.list(10:15))
#'
#' @export
append_recurse = function(lists, ...) {
  
  if(length(lists) == 1) {
    lists
  } else if(length(lists) == 2) {
    append(x=lists[[1]], lists[[2]], ...)
  } else {
    append(x=lists[[1]], Recall(lists[-1], ...), ...)
  }
  
}
  

#' @title
#' Removes zero-length elements from a list
#'
#' @description
#'
#' Takes a list and removes all of its zero-length elements. 
#' Optionally, the filtering function can be changed via `clean_by`.
#'
#' @examples
#' list_clean(list(1:10, NULL, 'a'))
#'
#' @family list utilities provided by utilbox
#' @export
list_clean = function(x, clean_by=length) {
  Filter(clean_by, x)
}

#' @title
#' Repeat elements in a list
#'
#' @description
#'
#' An extension of [`base::rep()`] which works with lists which are 
#' not unlisted. For non-list types it works the same as [`base::rep()`].
#'
#' @examples
#' rep_list(list(a=1,b=2), 3)
#' rep_list(list(a=1, mean), 2)
#'
#' @export
rep_list = function(...) {
  UseMethod("rep_list")
}

#' @rdname rep_list
#' @export
rep_list.list = function(x, n, each, length.out, bind=append_recurse, flatten=FALSE, rep_as_list=FALSE) {

  if(is_empty(x)) return(x)
  
  if(rep_as_list && !is.list(x)) 
    x = list(x)
  
  ii = if(!missing(length.out)) {
    rep(1:length(x), length.out=length.out)
  } else if(!missing(each)) {
    rep(1:length(x), each=each)
  } else {
    rep(1:length(x), n)
  }

  y = x[ii]
  
  if(flatten) list_flatten(y) else y
  
}

#' @rdname rep_list
#' @export
rep_list.default = rep_list.list

#' @rdname rep_list
#' @export
rep_list.matrix = function(x, ...) { 
  rep_list.list(x=list(x), ...)
}

#' @title
#' Modify list
#'
#' @description
#'
#' A version of [`utils::modifyList()`] which drops zero-length 
#' elements in `val` before updating `x`. Optionally, it can behave the 
#' exact same way as modifyList (when `drop_null_val=FALSE`).
#'
#' @examples
#' list_update(list(a=1, b=2), list(a=100, c=200))
#'
#' @family list utilities provided by utilbox
#' @export
list_update = function(x, val, ..., drop_null_val=TRUE) {
  utils::modifyList(x, if(drop_null_val) Filter(length, val) else val)
}

#' @rdname list_update
#' @export
modifyList2 = list_update

#' @title
#' Named list
#'
#' @description
#'
#' `nlist()` creates a named list. The code `list(a = a, b = b)` 
#' becomes `nlist(a,b)` and `list(a = a, b = 2)` becomes `nlist(a, b = 
#' 2)`, etc.
#'
#' `nlist2()` is a shorter original code which relies on existing 
#' code (`dots_to_nlist`). It should result in the exact same behavior 
#' as `nlist()`.
#'
#' Credit: The code for `nlist()` was lifted from the package `loo` 
#' to avoid having to install that package just for this one function. 
#' The code has been slightly modified.
#'
#' @examples
#' # uses the variable names to assign names
#' a = 1:10
#' nlist(a, b=2)
#' nlist2(a, b=2)
#'
#' # uses the call to assign names
#' nlist(1:10, sin(30))
#' nlist2(1:10, sin(30))
#'
#' @family list utilities provided by utilbox
#' @export
nlist = function (...) {
  m = match.call()
  dots = match.call(expand.dots=FALSE)$`...`
  out = list(...)
  no_names = is.null(names(out))
  has_name = if(no_names) FALSE else nzchar(names(out))
  if(all(has_name)) return(out)
  nms = as.character(m)[-1L]
  if (no_names) {
    names(out) = nms
  } else {
    names(out)[!has_name] = nms[!has_name]
  }
  return(out)
}

#' @rdname nlist
#' @export
nlist2 = function (...) {
  dots_to_nlist()
}

#' Named list apply
#'
#' `nlapply()` is an alias for `base::sapply() which returns a named list
#' by default (by setting `USE.NAMES=TRUE` and `simplify=FALSE`).
#'
#' @export
nlapply = function(...) {
  sapply(..., USE.NAMES=TRUE, simplify=FALSE)
}

#' @title
#' Set list attributes (element-wise)
#'
#' @description
#'
#' `list_set_attr` takes a list (`x`), attribute name (`attrib_name`) 
#' and a list of attributes (`attrib`) and sets the attribute named 
#' `attrib_name` of each element of `x` to the corresponding value in 
#' code{attrib}.
#'
#' @examples
#' set_list_attr(list('a','b'), 'size', c(10,20))
#'
#' @family list utilities provided by utilbox
#' @export
list_set_attr = function(x, attrib_name, attrib) {
  
  if(missing(attrib_name)) 
    error("Supply attribute name via 'attrib_name'.")
  if(missing(attrib)) 
    error("Supply attributes via 'attrib'.")
  
  stopifnot(any(length(attrib)==c(1,length(x))))
  stopifnot(is.list(x))
  
  if(length(attrib)==1) attrib = rep(attrib, length(x))
  
  y = x
  for(i in seq_along(y))
    attr(y[i], attrib_name) = attrib[i]
  y
}

