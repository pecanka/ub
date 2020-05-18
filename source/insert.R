#' @title
#' Insert element into sequence
#'
#' @description
#'
#' Insert the elements in `what` inside `x` at the positions given in 
#' `at`. The positions in `at` are always taken relative to the original 
#' values in `x`. Each element is inserted `count` times, where `count` 
#' is expected to be a vector of the same length as `what` and is 
#' recycled when shorter. The length of `at` must match the length of 
#' `what`.
#'
#' Since the insertions positions are always taken relative to the 
#' original positions in `x`, the order in which the insertions are 
#' specified (via `what` and `at`) is irrelevant unless the values would 
#' lead to overwriting of each other (only allowed when 
#' `allow_overwriting=TRUE`).
#'
#' Primarily, `x` is expected to be of class `list`, otherwise it is 
#' converted to one. `what` can be either a vector, or a list. If it is 
#' a list, each element in it can have multiple elements, which are then 
#' inserted at the corresponding index in `at` sequentially and each 
#' once or multiple times (depending on the value in the corresponding 
#' element in `count`).
#'
#' `replace_old` determines whether the elements in `x` at positions 
#' `at` are retained (i.e. shifted down in the output, when 
#' `replace_old=FALSE`), or dropped (when `replace_old=TRUE`).
#'
#' When `new_old=TRUE` the indexes of the original (\"old\") and the 
#' added (\"new\") elements in the returned object are attached as 
#' attributes \"old\" and \"new\".
#'
#'
#' @examples
#'
#' # insert additively (once)
#' insert(as.list(1:10), list(99,999,9999), c(2,5,7))
#' insert(as.list(1:10), list(list(99),list(999),list(9999)), c(2,5,7))   # no difference
#'
#' # non-list inputs: preserves class
#' insert(1:10, c(99,999,9999), c(2,5,7))
#' insert(1:10, c(99,999,9999), c(2,5,7), count=2)
#' insert(1:10, list(99,999,9999), c(2,5,7), count=2)
#' insert(1:10, list(list(99),list(999),9999), c(2,5,7), count=2)
#'
#' # but that is not always possible
#' \dontrun{
#' insert(1:10, list('a',999,9999), c(2,5,7), count=2)
#' }
#'
#' # a non-sorted at is not a problem
#' insert(1:5, c(777,999), at=c(2,1))
#'
#' # nor are multiple insertions at the same spot
#' insert(1:5, c(777,999), at=c(2,2))
#'
#' # insert additively (once or multiple times)
#' insert(as.list(1:10), list(99,999,9999), c(2,5,7))
#' insert(as.list(1:10), list(99,999,9999), c(2,5,7), count=1:3)
#' insert(as.list(1:10), list(98:99,999,9999), c(2,5,7), count=3)
#' insert(as.list(1:10), list(as.list(98:99),999,9999), c(2,5,7), count=3)  # notice the difference from the previous one
#'
#' # insert replacively
#' insert(as.list(1:10), list(99,999,9999), c(2,5,7), replace_old=TRUE)
#'
#' # replacive addition makes more sence with multiple additions
#' insert(as.list(1:10), list(98:99,999,9999), c(2,5,7), count=3)
#' insert(as.list(1:10), list(as.list(98:99),999,9999), c(2,5,7), count=3)
#'
#' # emulating the beharior of 'each' in rep
#' insert(as.list(1:10), list(as.list(rep(c(98,99),e=3)),999,9999), c(2,5,7), count=c(1,3,3))
#' # but that's probably easier done directly
#' insert(as.list(1:10), list(98,99,999,9999), c(2,2,5,7), count=c(3,3,3,3))
#'
#' # now with non-numeric input
#' what = list(list(sin,cos),999,9999)
#' insert(as.list(1:10), what, c(2,5,7), count=c(1,2,2))
#'
#' # with repetition
#' what = list(rep_list(list(sin,cos), e=3),999,9999)
#' insert(as.list(1:10), what, c(2,5,7), count=c(1,3,3))
#'
#' # but the insertion can be done with lists
#' insert(as.list(1:3), list(sin), at=1)
#' insert(as.list(1:10), list(list(sin,cos)), at=1)
#' insert(as.list(1:10), list(list(list(sin,cos))), at=1)
#' 
#' 
#' x = as.list(do.call(paste0, rep_list(list(LETTERS[1:15]),10)))
#' what = c('a','b','c'); at = c(2, 3, 7)
#' insert(x, what, at=at, count=10)
#'
#' @name insert
#' @family sequence-related functions provided by utilbox
#' @export
insert = function(...) {
  UseMethod('insert')
}

#' @rdname insert
#' @export
insert.list = function(x, what, at, count=1, replace_old=FALSE, new_old=FALSE) {

  if(is_empty(x)) return(x)
  
  if(missing(at))
    error("Supply 'at'.")
  if(any(at<1 | at>length(x)))
    error("Value in 'at' are out of range of 'x'.")
  if(length(what)!=length(at))
    error("The lengths of 'what' and 'at' must match.")
  if(any(count<0))
    error("Negative counts are not allowed")
  
  # alter placements and counts 
  count = rep_len(count, length.out=length(what))

  if(length(what)>1) {
    ord = order(at)
    what = what[ord]
    count = count[ord]
    at = at[ord]
    at = at + cumsum(c(0, sapply(h1(what,-1), length) * h1(count,-1)))
  }
  
  if(replace_old) {
    at = at - 1:length(what) + 1
  }
  
  if(new_old) {
    x = lapply(x, `attr<-`, 'utilbox::insert::is_old', TRUE)
  }
  
  # insert the values
  mod = insert_recurse.list(x, what, at, count, replace_old)
  
  # indicate which elements are new and which are the original ones
  if(new_old) {
    old = sapply(mod, function(m) isTRUE(attr(m, 'utilbox::insert::is_old')))
    mod = lapply(mod, `attr<-`, 'utilbox::insert::is_old', NULL)
    mod = structure(mod, new=which(!old), old=which(old))
  }
  
  mod
  
}

#' @rdname insert
#' @export
insert.default = function(x, what, ...) {

  y = insert.list(as.list(x), as.list(what), ...)
  
  if(is.vector(x)) {
    
    yy = tryCatch(do.call('as.'%p%class(x)[1], list(y)), warning=hide, error=hide)
    
    if(!is_error(yy)) {
      attributes(yy) = attributes(y)
      y = yy
    }
    
  }

  y
  
}
  
#' @rdname insert
#' @export
insert_recurse.list = function(x, what, at, count=1, replace=FALSE) {
  
  if(length(what)==1) {
    insert1(x, what, at, count, replace)
  } else {
    x1 = Recall(x, h1(what,-1), h1(at,-1), h1(count,-1), replace)
    insert1(x1, t1(what), t1(at), t1(count), replace)
  }

}

#' @rdname insert
#' @export
insert1 = function(...) {
  UseMethod("insert1")
}

#' @rdname insert
#' @export
insert1.list = function(x, what, at, count=1, replace=FALSE) {
  
  if(missing(at)) at = length(x)
  
  stopifnot(length(what)==1, length(at)==1, at>=1, at<=length(x))
  
  if(is.list(what))                           # compare this: insert(as.list(1:10), list(as.list(98:99), 999, 9999), c(2,5,7), count=c(1,3,3))
    what = unlist(what, recursive=FALSE)      # with this: insert(list(1:10), list(list(sin,cos)), at=1)
  
  start = head(x, at - 1)
  middle = rep_list(what, count, rep_as_list=TRUE)
  end = if(at==1 && !replace) x else tail(x, -(at - 1 + replace))

  start %append% middle %append% end
  
}

