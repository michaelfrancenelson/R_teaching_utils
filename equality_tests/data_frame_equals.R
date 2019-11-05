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
  err_3a = paste0("\nFirst data frame has names: \"", paste0(names(d1), collapse = "\", \""), "\""),
  err_3b = paste0("\nSecond data frame has names: \"", paste0(names(d2), collapse = "\", \""), "\""),
  err_3c = paste0("\nData frame columns not in the same order."),
  
  err_4  = "\nData frames are not identical",
  
  warn_3a = paste0("\nWarning: data frame columns not in the same order."),
  warn_3b = paste0("\nWarning: data frame rows not in the same order."),
  
  pass_1 = "\nData frames are identical.",
  pass_2 = "\nData frame entries are identical")
{
    source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  nl = "\n"
  nl = ""
  
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
  
  # Columns ----
  if (!identical(sort(names(d1)), sort(names(d2))))
  {
    if (!silent) cat(err_3, err_3a, err_3b, nl)
    return (FALSE)
  }
  
  
  # If enforcing column order:
  cols_in_order = identical(names(d1), names(d2))
  
  if (enforce_col_order & (!cols_in_order))
      # identical(names(d1), names(d2)))
  {
    if (!silent)
      cat(err_3c, nl)
    return (FALSE)
  } 
  
  if(!cols_in_order)
  {
    if (!silent)
      cat(warn_3a)
  }
  
  
  # Otherwise dataframes have the same column names,
  # not necessarily in the same order.
  names_order = match(names(d1), names(d2))
  
  # Row order ---- 
  order_1 = do.call(order, lapply(d1, c))
  order_2 = do.call(order, lapply(d2[, names_order], c))
  
  if (enforce_row_order & !identical(order_1, order_2))
  {
    if (!silent) cat(err_4, nl)
    return (FALSE)
  }
  
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
  
  if (!enforce_row_order & !silent) cat(warn_3b)
  if (!silent) cat(pass_2, nl)
  return (TRUE)  
}
