# Check for equality of two data.frames with
# options to print informative messages.

data_frame_equals = function(
  d1, d2,
  silent = FALSE,
  enforce_col_order = FALSE,
  
  err_1 = "\nOne or more input objects is not a data.frame.",
  
  err_2 = "\nData frames are not the same size",
  err_2a = paste0("\n   Row counts do not match: ", nrow(d1), " != ", nrow(d2)),
  err_2b = paste0("\nColumn counts do not match: ", ncol(d1), " != ", ncol(d2)),
  
  err_3  = paste0("\nData frame column names do not match."),
  err_3a = paste0("\n First data frame has names: \"", paste0(names(d1), collapse = "\", \""), "\""),
  err_3b = paste0("\nSecond data frame has names: \"", paste0(names(d2), collapse = "\", \""), "\""),
  
  warn_3 = paste0("\nWarning: data frame columns not in the same order."),
  
  pass_1 = "\nData frames are identical.",
  pass_2 = "\nData frame entries are identical")
{
  
  source("https://github.com/michaelfrancenelson/R_teaching_utils/blob/master/equality_tests/vec_equals.R")
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
    if (!silent) cat(err_2, nl, err_2a, nl, err_2b, sep = "")
    return (FALSE)
  }
  
  # Same column names ----
  
  
  names_order = match(names(d1), names(d2))
  names(d2)[names_order] == names(d1)


  str(lapply(d1, c))
  order(lapply(d1, c))


  order_1 = do.call(order, lapply(d1, c))
  order_2 = do.call(order, lapply(d2[, d1_names_order], c))
  
  d1 = d1[order_1, ]
  d2 = d2[order_2, names_order]
  
  col_err = c()
  for (i in 1:ncol(d1))
  {
    if (!vec_equals(d1[, i], d2[, ], silent = TRUE))
  }
  
  identical(d1[order_1, ], d2[order_2, d1_names_order])

  d1a = d1[order_1, ]
  d2a = d2[order_2, d1_names_order]

  tail(d1a)
  tail(d2a)

  # element-wise check
  sum(d1a != d2a)
  
  
}
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