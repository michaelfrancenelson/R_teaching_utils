# Test for equality of two vectors
# Print helpful messages.
# This will probably be slow for very long vectors.
vec_equals = function(
  v1, v2, 
  silent = FALSE,
  err_1  = "\nAt least one object is not a vector.",
  err_2  = "\nVectors do not have the same length: ",
  err_3  = "\nVectors are not of the same class.",
  err_3a = paste0("\nVector 1 is of class ", class(v1)),
  err_3b = paste0("\nVector 2 is of class ", class(v2)),
  err_4a  = "\nElements at indices (",
  err_4b  = ") do not match.",
  pass_1 = "\nVectors are identical",
  tol_digits = 4)
{
 
  nl = "\n" 
  # If they are identical, return true
  if(identical(v1, v2))
  {
    if (!silent) cat(pass_1)
    return (TRUE)
  }
  
  # If either is not a vector, return false
  if(!(is.vector(v1) & is.vector(v2)))  
  {
    if (!silent) cat(err_1, nl)
    return (FALSE)
  }
  
  # If they are not the same length, return false
  if (length(v1) != length(v2))
  {
    if (!silent) cat(err_2, nl)
    return (FALSE)
  }
  
  c1 = class(v1); c2 = class(v2)
  
  # If they are of different classes, return false
  if (c1 != c2)
  {
    if (!silent) cat(err_3, err_3a, err_3b, nl, sep = "")
    return (FALSE)
  }
  
  err_indices = c() 
  
  # If both are numeric (i.e. floating-point), allow matching within a tolerance
  if (is.numeric(v1))
  {
    for (i in 1:length(v1))
    {
      if (!(is.nan(v1[i]) & is.nan(v2[i])))
      if(signif(v1[i], tol_digits) != signif(v2[i], tol_digits))
      {
        err_indices = c(err_indices, i)
      }
    }
  } else
  {
    err_indices = which(v1 != v2)
  }
  
  if (length(err_indices) > 0)
  {
    if (!silent)
    {
      cat(err_4a, paste0(err_indices, collapse = ", "), err_4b, nl, sep = "")
    }
    return (FALSE)
  }
  if (!silent) cat(pass_1)
  return (TRUE)
}