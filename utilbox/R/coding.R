#' Evaluate an expression given as a string
#'
#' Takes an expression in a character vector, pastes it 
#' into a single string and evaluates it in the `envir` 
#' environment (the parent frame by default).
#'
#' @examples
#' x = NULL
#' print(x)
#' cc('x','=1')
#' print(x)
#'
#' @export
cc = function(..., envir=parent.frame()) {
  eval(base::str2lang(paste0(...)), envir=envir)
}

###
# Example of the usage of the trace function
#
# Let's insert a call to the browser function at line 4 of the function force_as_real"
#
# trace(force_as_real, browser, at=4)
# force_as_real('a1.4')
# untrace(force_as_real, browser)
#
# Ways to change the arguments of a function
#
# formals(cat)$sep <- ""
# trace(base::cat, tracer=quote(if(missing(sep)) sep=''), at=1)
# cat = purrr::partial(cat, sep="")
#
# assign("cat", cat0, envir=.GlobalEnv)
# assign("cat", cat0, envir=.utilbox)

# cat = utils::getFromNamespace("cat", ns="base")
# trace(cat, tracer=quote(if(missing(sep)) sep <- ""), at=1, print=FALSE)
# #utils::assignInNamespace("cat", cat0, ns="base")

# Useful functions:
#
# utils::getAnywhere() retrieves an R Object, including from a Namespace, whether visible 
#   on the search path, registered as an S3 method or in a namespace but not exported.
# utils::argsAnywhere returns the arguments of objects that are functions.
#
# envnames::environment_name() returns the name of the variable that stores the
#   environment (input example: “< environment: 0x00000000147499b0>”)
# envnames::obj_find() looks up objects in all packages and environments including
#   the call's function cascade of frames
#
# base::typeof() returns the type or storage mode of any object (e.g. 'numeric', 'character')
#
# sloop::ftype() returns the type of a function (e.g. "primitive" or c("S3","generic"))
# sloop::s3_dispatch() helps with inspecting how an S3 method is dispatched.
# sloop::s3_get_method() shows the code of the method that gets called regardless
#   whether it is exported by a package or not. Basically, emulated the `:::` call
#   but simpler, since the method name does not have to be looked up first
# sloop::s3_methods_generic() let you find all methods defined for a generic
# sloop::s3_methods_class() does the same for a class
#
# base::NextMethod() delegated work in a method dispatch to the next method in line
#   for an S3 class (see https://adv-r.hadley.nz/s3.html)
#
# The term "S3 method" basically just refers to a scheme of method dispatching,
#   not a specific object type as the name might suggest.
#
# S4 is a stricter version (compared to S3) of the object oriented programming 
# structure in R with many of underlying ideas the same. Two main differences:
#   1. formal class definitions: unlike S3, S4 formally defines the representation 
#      and inheritance for each class
#   2. multiple dispatch: the generic function can be dispatched to a method 
#      based on the class of any number of argument, not just one
# (see http://adv-r.had.co.nz/S4.html)
#
# Pointers in R can be implemented using environments, since environments do not
#   get coppied when used as arguments of a function
#
#
# # How to overwrite a locked function in a package
#
# #library(osmdata)
# environmentIsLocked(asNamespace("curl"))
# unlockBinding(sym = "has_internet", asNamespace("curl"))
# assign(x = "has_internet", value = {function() T}, envir = asNamespace("curl"))
# curl:::has_internet()
#
###

