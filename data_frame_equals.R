# Check for equality of two data.frames with
# options to print informative messages.


data_frame_equals = function(d_1, d_2,
                             err_1 = "\nOne or more input objects is not a data.frame.",
                             
                             err_2 = "\nData frames are not the same size",
                             err_2a = paste0("\n   Row counts do not match: ", nrow(d_1), " != ", nrow(d_2)),
                             err_2b = paste0("\nColumn counts do not match: ", ncol(d_1), " != ", ncol(d_2)),
                             
                             err_3  = paste0("\nData frame column names do not match."),
                             err_3a = paste0("\nFirst data frame has names: \"", paste0(names(d_1), collapse = "\", \""), "\""),
                             err_3b = paste0("\nFirst data frame has names: \"", paste0(names(d_2), collapse = "\", \""), "\""),
                             
                             pass_1 = "\nData frames are identical.",
                             pass_2 = "\nData frame entries are identical")
{
  # If the input data frames are identical, there is no need to continue:
  if (identical(d_1, d_2))
  {
    print(pass_1)
    return (TRUE)
  }
  
  
}

data(iris)
d_1 = iris
set.seed(12345)
d_2 = d_1[sample(nrow(d_1)), sample(ncol(d_1))]
d1_names_order = match(names(d_1), names(d_2))
names(d_2)[d1_names_order] == names(d_1)


identical(d_1, d_2)


str(lapply(d_1, c))
order(lapply(d_1, c))


order_1 = do.call(order, lapply(d_1, c))
order_2 = do.call(order, lapply(d_2[, d1_names_order], c))
identical(d_1[order_1, ], d_2[order_2, d1_names_order])

d_1a = d_1[order_1, ]
d_2a = d_2[order_2, d1_names_order]

tail(d_1a)
tail(d_2a)

# element-wise check
sum(d_1a != d_2a)

