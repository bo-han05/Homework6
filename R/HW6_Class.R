## HW6 Class/Methods

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#'
#'
setValidity("sparse_numeric", function(object){
  if (length(object@value) != length(object@pos))
    return("The number of non-zero elements should match the number of index positions.")
  if (any(object@pos < 1L | object@pos > object@length))
    return("Index positions must be valid.")
  TRUE
})

#' @name sparse_add
#' @param x generic sparse_add
#' @param y generic sparse_add
#' @export
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' @name sparse_sub
#' @param x generic sparse_sub
#' @param y generic sparse_sub
#' @export
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' @name sparse_mult
#' @param x generic sparse_mult
#' @param y generic sparse_mult
#' @export
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' @name sparse_crossprod
#' @param x generic sparse_crossprod
#' @param y generic sparse_crossprod
#' @export
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' @name sparse_norm
#' @param x generic sparse_norm
#' @export
setGeneric("norm", function(x) standardGeneric("norm"))

#' @name sparse_standardize
#' @param x generic sparse_standardize
#' @export
setGeneric("standardize", function(x) standardGeneric("standardize"))

#' @name sparse_add
#' @param x method sparse_add
#' @param y method sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y){
  if (x@length != y@length) stop("Vector lengths are not equal.")
  xy_pos=sort(unique(c(x@pos, y@pos)))
  x_values=numeric(length(xy_pos))
  y_values=numeric(length(xy_pos))
  x_values[match(x@pos, xy_pos)]=x@value
  y_values[match(y@pos, xy_pos)]=y@value
  result=x_values + y_values
  non_zeros=which(result != 0)
  new("sparse_numeric",
      value=result[non_zeros],
      pos=as.integer(xy_pos[non_zeros]),
      length=x@length)
})

#' @name sparse_sub
#' @param x method sparse_sub
#' @param y method sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y){
  if (x@length != y@length) stop("Vector lengths are not equal.")
  xy_pos=sort(unique(c(x@pos, y@pos)))
  x_values=numeric(length(xy_pos))
  y_values=numeric(length(xy_pos))
  x_values[match(x@pos, xy_pos)]=x@value
  y_values[match(y@pos, xy_pos)]=y@value
  result=x_values - y_values
  non_zeros=which(result != 0)
  new("sparse_numeric",
      value=result[non_zeros],
      pos=as.integer(xy_pos[non_zeros]),
      length=x@length)
})

#' @name sparse_mult
#' @param x method sparse_mult
#' @param y method sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y){
  if (x@length != y@length) stop("Vector lengths are not equal.")
  xy_pos=intersect(x@pos, y@pos)
  result=x@value[match(xy_pos, x@pos)] * y@value[match(xy_pos, y@pos)]
  non_zeros=which(result != 0)
  new("sparse_numeric",
      value=result[non_zeros],
      pos=as.integer(xy_pos[non_zeros]),
      length=x@length)
})

#' @name sparse_crossprod
#' @param x method sparse_crossprod
#' @param y method sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y){
  if (x@length != y@length) stop("Vector lengths are not equal.")
  xy_pos=intersect(x@pos, y@pos)
  sum(x@value[match(xy_pos, x@pos)] * y@value[match(xy_pos, y@pos)])
})

#' @name sparse_arithmetic_add
#' @param e1 arithmetic "+"
#' @param e2 arithmetic "+"
#' @export
setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2){
            sparse_add(e1, e2)
          })

#' @name sparse_arithmetic_sub
#' @param e1 arithmetic "-"
#' @param e2 arithmetic "-"
#' @export
setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2){
            sparse_sub(e1, e2)
          })

#' @name sparse_arithmetic_mult
#' @param e1 arithmetic "*"
#' @param e2 arithmetic "*"
#' @export
setMethod("*", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2){
            sparse_mult(e1, e2)
          })

setAs("numeric", "sparse_numeric", function(from){
  non_zeros=which(from != 0)
  new("sparse_numeric",
      value=from[non_zeros],
      pos=as.integer(non_zeros),
      length=as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from){
  x=numeric(from@length)
  x[from@pos]=from@value
  x
})

#' @name sparse_show
#' @param object show method
#' @export
setMethod("show", "sparse_numeric", function(object){
  cat("Length =", object@length, "\n")
  if (length(object@value) == 0)
    cat("No non-zero elements.\n")
  else
    print(data.frame(value=object@value, pos=object@pos))
})

#' @name sparse_plot
#' @param x plot method
#' @param y plot method
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y){
  plot(x@pos, x@value, col="steelblue", pch=16,
       xlab="Position", ylab="Value",
       xlim=c(1, x@length), ylim=range(c(x@value, y@value, 0)))
  points(y@pos, y@value, col="darkred", pch=16)
  legend("topright", legend=c("x", "y"), col=c("steelblue", "darkred"), pch=16)
})

#' @name sparse_length
#' @param x length method
#' @export
setMethod("length", "sparse_numeric", function(x){
  x@length
})

#' @name sparse_mean
#' @param x mean method
#' @export
setMethod("mean", "sparse_numeric", function(x){
  sum(x@value)/x@length
})

#' @name sparse_norm
#' @param x norm method
#' @export
setMethod("norm", "sparse_numeric", function(x){
  sqrt(sum(x@value^2))
})

#' @name sparse_standardize
#' @param x standardize method
#' @export
setMethod("standardize", "sparse_numeric", function(x){
  average = mean(x)
  n = x@length
  var = sum((x@value - average)^2)
  n_zero = n - length(x@value)
  var_zero = n_zero*(average^2)
  std = sqrt((var + var_zero)/(n-1))
  if (std == 0){
    return(new("sparse_numeric",
               value = numeric(0),
               pos = integer(0),
               length = x@length))
  }
  x_values <- numeric(n)
  x_values[x@pos] <- x@value
  result <- (x_values - average)/std
  non_zeros <- which(result != 0)
  new("sparse_numeric",
      value=result[non_zeros],
      pos=as.integer(non_zeros),
      length=n)
})
