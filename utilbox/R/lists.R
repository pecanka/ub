#' Flattens a list
#'
#' An alias for `as.list(unlist(x))`.
#'
#' @family list utilities provided by utilbox
#' @export
list_flatten = function(x) {
  as.list(unlist(x))
}

#' Recursive merge
#'
#' This does a recursive merge of lists. The code is taken from the package 
#' reshape and modified to fix the bug in it. The bug was the missing ellipsis 
#' in the call to \code{Recall()}.
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

#' Recursive append
#'
#' Appends multiple objects together by recursively calling [base::append].
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
  

#' Removes zero-length elements from a list
#' @examples
#' list_clean(list(1:10, NULL, 'a'))
#'
#' @family list utilities provided by utilbox
#' @export
list_clean = function(L, null.rm=TRUE) {
  Filter(length, L)
}

#' Repeat elements in a list
#'
#' An extension of [base::rep] which works with lists which are
#' not unlisted. For non-list types it works the same as [base::rep]].
#'
#' @examples
#' rep_list(list(a=1,b=2), 3)
#' rep_list(list(a=1, mean), 2)
#'
#' @export
rep_list = function(x, ...) {
  UseMethod("rep_list")
}

#' @rdname rep_list
#' @export
rep_list.list = function(x, n, length.out, bind=append_recurse, flatten=FALSE) {

  if(is_empty(x)) return(x)
  
  ii = if(!missing(length.out)) {
    rep(1:length(x), length.out=length.out)
  } else {
    rep(1:length(x), n)
  }
  
  y = x[ii]
  
  #y = bind(h1(lapply(seq(1,n,1), function(i) x), length.out))
  
  if(flatten) list_flatten(y) else y
  
}

#' @rdname rep_list
#' @export
rep_list.numeric = rep_list.list

#' @rdname rep_list
#' @export
rep_list.character = rep_list.list

#' @rdname rep_list
#' @export
rep_list.logical = rep_list.list

#' @rdname rep_list
#' @export
rep_list.complex = rep_list.list

#' @rdname rep_list
#' @export
rep_list.factor = rep_list.list

#' @rdname rep_list
#' @export
rep_list.data.frame = rep_list.list

#' @rdname rep_list
#' @export
rep_list.matrix = append_body(rep_list.list, expression(x = list(x)), at_top=TRUE)

#' Modify list
#'
#' A version of 'modifyList' from utils which drops zero-length elements in 'val'
#' before updating 'x' (optionally can behave the same as modifyList)
#'
#' @family list utilities provided by utilbox
#' @export
modifyList2 = function(x, val, ..., drop_null_val=TRUE) {
  utils::modifyList(x, if(drop_null_val) Filter(length, val) else val)
}

#' Named list
#'
#' `nlist` creates a named list. The code `list(a = a, b = b)` 
#' becomes `nlist(a,b)` and `list(a = a, b = 2)` becomes 
#' `nlist(a, b = 2)`, etc. 
#'
#' `nlist2` is a shorter original code which relies on existing
#' code (`dots_to_nlist`). It should result in the exact
#' same behavior as `nlist`.
#'
#' Credit: The code for `nlist` was lifted from the package 
#' `loo` to avoid having to install that package just for this 
#' one function. The code has been slightly modified.
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

#' Set list attributes (element-wise)
#'
#' Take a list (\code{x}), attribute name (\code{attrib_name}) and a list of 
#' attributes (\code{attrib}) and sets the attribute named code{attrib_name}
#' of each element of \code{x} to the corresponding value in code{attrib}.
#' 
#' @examples
#' set_list_attr(list('a','b'), 'size', c(10,20))
#'
#' @family list utilities provided by utilbox
#' @export
list_set_attr = function(x, attrib_name, attrib) {
  stopifnot(any(length(attrib)==c(1,length(x))))
  stopifnot(is.list(x))
  if(missing(attrib_name)) error(this_fun_name(),': supply attribute name.')
  
  if(length(attrib)==1) attrib = rep(attrib, length(x))
  
  y = x
  for(i in seq_along(y))
    attr(y[i], attrib_name) = attrib[i]
  y
}

