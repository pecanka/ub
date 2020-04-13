#' Add class to the object's classes
#'
#' `append_class` adds the class specified in `what` as the 
#' first among the classes of the object `x`. 
#'
#' `drop_class` removes the specified class from the classes of 
#' the given object.
#'
#' @examples
#' append_class(list(1:20), 'huge')
#'
#' @name classes
#' @family class-related functions provided by utilbox
#' @export
append_class = function(x, what) {
  `class<-`(x, c(what, class(x)))
}

#' @rdname classes
#' @export
drop_class = function(x, what) {
  if(missing(what)) what = class(x)[1]
  `class<-`(x, class(x)[h1(which(class(x)!=what))])
}

#' Convert objects to class `huge`
#'
#' Add `huge` as the first class of an object. Class `huge`
#' is intended to be used for objects with many elements which, 
#' if all printed, would clutter the screen. With class `huge` 
#' set, only a limited number of elements are actually printed.
#' (extracted by [base::head]).
#'
#' @examples
#' as_huge(list(1:100))
#'
#' @name class_huge
#' @family class-related functions provided by utilbox
#' @export
as_huge = function(x) {
  UseMethod("as_huge")
}

#' @rdname class_huge
#' @export
add_class_huge = function(x) {
  append_class(x, 'huge')
}

#' @rdname class_huge
#' @export
print.huge = function(x, n=10, show_note=TRUE) {
  print(drop_class(head(x, n), 'huge'))
  if(length(x)>n) {
    catn("# ... with ",length(x)-n," more elements")
    print_huge_note(show_note)
  }
  invisible(x)
}

#' @rdname class_huge
#' @export
as_huge.list = function(x) {
  add_class_huge(lapply(x, as_huge))
}

#' @rdname class_huge
#' @export
as_huge.default = function(x) {
  add_class_huge(x)
}



print_huge_note = function(x, n, show=TRUE) {
  msg = "Note: for objects of class 'huge' only the first " %.% n %.% 
        " elements were printed. Use print(n=...) to change this limit."
  catn(msg)
  invisible(msg)
}

