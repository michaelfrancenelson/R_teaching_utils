# Check for equality of two data.frames with
# options to print informative messages.

data_frame_equals = function(
  d1, d2,
  silent = FALSE,
  enforce_col_order = FALSE,
  enforce_row_order = TRUE,
  
  err_1 = "\nOne or more input objects is not a data.frame.",
  
  err_2 = "\nData frames are not the same size",
  err_2a = paste0("\n   Row counts: ", nrow(d1), ", ", nrow(d2)),
  err_2b = paste0("\nColumn counts: ", ncol(d1), ", ", ncol(d2)),
  
  err_3  = paste0("\nData frame column names do not match."),
  err_3a = paste0("\n First data frame has names: \"", paste0(names(d1), collapse = "\", \""), "\""),
  err_3b = paste0("\nSecond data frame has names: \"", paste0(names(d2), collapse = "\", \""), "\""),
  err_3c = paste0("\nData frame columns not in the same order."),
  
  err_4  = "\nData frames are not identical",
  
  warn_3 = paste0("\nWarning: data frame columns not in the same order."),
  
  pass_1 = "\nData frames are identical.",
  pass_2 = "\nData frame entries are identical")
{
  
  source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  nl = "\n"
  
  # identical ----
  if (identical(d1, d2))
  {
    if (!silent) cat(pass_1, nl)
    return (TRUE)
  }
  
  # Both are data frames ----
  if (class(d1) != "data.frame" | class(d2) != "data.frame")
  {
    if (!silent) cat(err_1, nl)
    return (FALSE)
  }
  
  # Same dimensions ----
  if (!identical(dim(d1), dim(d2)))
  {
    if (!silent) cat(err_2, err_2a, err_2b, nl, sep = "")
    return (FALSE)
  }
  
  # Same column names ----
  if (!identical(sort(names(d1)), sort(names(d2))))
  {
    if (!silent) cat(err_3, err_3a, err_3b, nl)
    return (FALSE)
  }
  
  # If enforcing column order:
  if (enforce_col_order & !identical(names(d1), names(d2)))
  {
    if (!silent) cat(err_3c, nl)
    return (FALSE)
  }
  
  # Otherwise dataframes have the same column names,
  # not necessarily in the same order.
  names_order = match(names(d1), names(d2))
  
  if (!identical(names(d1), names(d2)))
    if (!silent) cat(warn_3, nl)
  
  # Row order ---- 
  order_1 = do.call(order, lapply(d1, c))
  order_2 = do.call(order, lapply(d2[, names_order], c))

  if (enforce_row_order & !identical(order_1, order_2))
  {
    if (!silent) cat(err_4, nl)
    return (FALSE)
  }
    
  # if (enforce_row_order) 
  # {
  #   order_1 = 1:nrow(d1)
  #   order_2 = order_1
  # } else 
  #     order_2 = do.call(order, lapply(d2[, names_order], c))
  
  d1 = d1[order_1, ]
  d2 = d2[order_2, names_order]
  
  col_err = c()
  for (i in 1:ncol(d1))
  {
    if (!vec_equals(d1[, i], d2[, i], silent = TRUE)) col_err = c(col_err, i)
  }
  
  if (length(col_err) > 0)
  {
    if (!silent) cat(err_4, nl)
    cat("columns ", col_err, " do not match")
    return (FALSE)
  }
  
  if (!silent) cat(pass_2, nl)
  return (TRUE)  
}

data_frame_equals(iris, iris)
data_frame_equals(iris, iris[, sample(ncol(iris))])
data_frame_equals(iris[1:4, ], iris)

iris2 = iris[sample(nrow(iris)), sample(ncol(iris))]
data_frame_equals(iris, iris2, enforce_row_order = TRUE)
data_frame_equals(iris, iris2, enforce_row_order = FALSE)


# source("data_frame_equals.R")
# source("https://github.com/michaelfrancenelson/R_teaching_utils/blob/master/data_frame_equals.R")
# data(iris)
# d1 = iris
# set.seed(12345)
# d2 = d1[sample(nrow(d1)), sample(ncol(d1))]
# d1_names_order = match(names(d1), names(d2))
# names(d2)[d1_names_order] == names(d1)
# 
# 
# identical(d1, d2)
# 
# 
# str(lapply(d1, c))
# order(lapply(d1, c))
# 
# 
# order_1 = do.call(order, lapply(d1, c))
# order_2 = do.call(order, lapply(d2[, d1_names_order], c))
# identical(d1[order_1, ], d2[order_2, d1_names_order])
# 
# d1a = d1[order_1, ]
# d2a = d2[order_2, d1_names_order]
# 
# tail(d1a)
# tail(d2a)
# 
# # element-wise check
# sum(d1a != d2a)

readLines("https://github.com/michaelfrancenelson/R_teaching_utils/blob/master/data_frame_equals.R")
readLines("http://github.com/michaelfrancenelson/R_teaching_utils/blob/master/data_frame_equals.R")
source.url("https://github.com/michaelfrancenelson/R_teaching_utils/blob/master/data_frame_equals.R")


readLines(url("https://github.com/michaelfrancenelson/R_teaching_utils/blob/master/data_frame_equals.R"))

read.table("http://lib.stat.cmu.edu/jcgs/tu",skip=4,header=T)