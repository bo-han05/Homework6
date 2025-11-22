## HW6_testscript.R

library(testthat)
library(Homework6)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

test_that("check for mean method", {
  x <- as(c(0, 1, 0, 9), "sparse_numeric")
  expect_equal(mean(x), (0 + 1 + 0 + 9) / 4)
})

test_that("check for mean method when all are zero", {
  x <- as(rep(0, 5), "sparse_numeric")
  expect_equal(mean(x), 0)
})

test_that("check for norm method", {
  x <- as(c(0, 1, 0, 9), "sparse_numeric")
  expect_equal(norm(x), sqrt(1^2 + 9^2))
})

test_that("check for norm method when all are zero", {
  x <- as(rep(0, 4), "sparse_numeric")
  expect_equal(norm(x), 0)
})

test_that("check for standardize method", {
  x <- as(c(0, 1, 0, 9), "sparse_numeric")
  z <- standardize(x)
  x_values <- as(x, "numeric")
  expected <- (x_values - mean(x_values)) / sd(x_values)
  expect_equal(as(z, "numeric"), expected)
})

test_that("check for standardize method when sd = 0", {
  x <- as(rep(0, 4), "sparse_numeric")
  z <- standardize(x)
  expect_equal(as(z, "numeric"), rep(0, 4))
})

test_that("check for sparse_crossprod method", {
  x <- as(c(1, 0, 9), "sparse_numeric")
  y <- as(c(2, 1, 5), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 1*2 + 9*5)
})

test_that("check for arithmetic operators", {
  x <- as(c(1, 0, 3), "sparse_numeric")
  y <- as(c(2, 1, 2), "sparse_numeric")
  expect_equal(as(x + y, "numeric"), c(3, 1, 5))
  expect_equal(as(x - y, "numeric"), c(-1, -1, 1))
  expect_equal(as(x * y, "numeric"), c(2, 0, 6))
})

test_that("check for show method", {
  x <- as(c(0, 1, 0, 9), "sparse_numeric")
  expect_output(show(x), "Length = 4")
  expect_output(show(x), "value")
  expect_output(show(x), "pos")
  y <- as(c(0, 0, 0), "sparse_numeric")
  expect_output(show(y), "No non-zero elements")
})

test_that("check for plot method", {
  x <- as(c(0, 1, 0, 9), "sparse_numeric")
  y <- as(c(1, 0, 9, 0), "sparse_numeric")
  expect_silent(plot(x, y))
})
