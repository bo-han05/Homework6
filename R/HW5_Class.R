## HW5 Class/Methods

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object){
  if (length(object@value) != length(object@pos))
    return("The number of non-zero elements should match the number of index positions.")
  if (any(object@pos < 1L | object@pos > object@length))
    return("Index positions must be valid.")
  TRUE
})

setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))
setGeneric("norm", function(x, y) standardGeneric("norm"))
setGeneric("standardize", function(x) standardGeneric("standardize"))

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

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y){
  if (x@length != y@length) stop("Vector lengths are not equal.")
  xy_pos=intersect(x@pos, y@pos)
  sum(x@value[match(xy_pos, x@pos)] * y@value[match(xy_pos, y@pos)])
})

setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2){
            sparse_add(e1, e2)
          })

setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2){
            sparse_sub(e1, e2)
          })

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

setMethod("show", "sparse_numeric", function(object){
  cat("Length =", object@length, "\n")
  if (length(object@value) == 0)
    cat("No non-zero elements.\n")
  else
    print(data.frame(value=object@value, pos=object@pos))
})

setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y){
  plot(x@pos, x@value, col="steelblue", pch=16,
       xlab="Position", ylab="Value",
       xlim=c(1, x@length), ylim=range(c(x@value, y@value, 0)))
  points(y@pos, y@value, col="darkred", pch=16)
  legend("topright", legend=c("x", "y"), col=c("steelblue", "darkred"), pch=16)
})

setMethod("length", "sparse_numeric", function(x){
  x@length
})

setMethod("mean", "sparse_numeric", function(x, y){
  total_sum = sum(x@value)
  n = x@length
  total_sum/n
})

setMethod("norm", "sparse_numeric", function(x, y){
  sqrt(sum(x@value^2))
})

setMethod("standardize", "sparse_numeric", function(x){
  average = mean(x)
  n = x@length
  var_nonzero = sum((x@value - average)^2)
  n_zero = n - length(x@value)
  var_zero = n_zero * (average^2)
  sd_x = sqrt((var_nonzero + var_zero)/n)
  if (sd_x == 0) {
    warning("Standard deviation is zero — returning zero vector.")
    return(new("sparse_numeric",
               value = numeric(0),
               pos   = integer(0),
               length = x@length))
  }
  new_values = (x@value - average)/sd_x
  keep = which(new_values != 0)
  new("sparse_numeric",
      value = new_values[keep],
      pos = x@pos[keep],
      length = x@length)
})
