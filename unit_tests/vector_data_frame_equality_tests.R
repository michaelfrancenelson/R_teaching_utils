# Vector equality ----
# 
# Need to test that function returns true for:
#
#    1: self-equivalence
#    2: identical vector equivalence
#    3: identical vectors, but different element names
#    4: numeric vectors, differences below tolerance
test_equivalent_vectors = function()
{
  source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  # source("equality_tests/vec_equals.R")
  
  cat("\nPerforming equivalent vectors tests...")
  
  set.seed(12345)
  n = 100
  lambda = 50
  max_string_l = 120
  max_name_l = 33
  
  vecs_1 = vecs_2 = all_type_vecs(n = n,lambda = lambda, max_word_size = max_string_l)
  
  # self-equivalence
  test_vector_lists(vecs_1, vecs_1, "self-equivalence")
  # sapply(vecs_1, FUN = function(x) vec_equals(x, x, silent = TRUE))
  
  # equivalence of identical vectors  
  test_vector_lists(vecs_1, vecs_2, "identical vectors")
  # sapply(vecs_1, FUN = function(x) vec_equals(x, x, silent = TRUE))
  
  # element names are not checked
  vecs_3 = random_names_vec(vecs_1[names(vecs_1) != "null_1"], max_name_length = max_name_l)
  vecs_4 = random_names_vec(vecs_2[names(vecs_2) != "null_1"], max_name_length = max_name_l)
  test_vector_lists(vecs_3, vecs_4, "non-matching names")
  # sapply(vecs_1, FUN = function(x) vec_equals(x, x, silent = FALSE))
  
  # numeric vectors match within tolerance
  test_vector_lists(list(vecs_1$num_1), list(vecs_2$num_1 + 0.00000001), "numeric differences outside tolerance")
  cat("\n")
}

# Vector inequality ----
#
# Need to test unequal
#   lengths
#   classes
#   values
test_non_equivalent_vectors = function()
{
  cat("\nPerforming non-equivalent vectors tests...")
  source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  set.seed(12345)
  n1 = 100
  n2 = 100
  n3 = 130
  lambda = 50
  max_string_l = 150
  
  vecs_1 = all_type_vecs(n = n1,lambda = lambda, max_word_size = max_string_l)
  vecs_2 = all_type_vecs(n = n1,lambda = lambda, max_word_size = max_string_l, seed = 9876543)
  vecs_3 = all_type_vecs(n = n3,lambda = lambda, max_word_size = max_string_l, seed = 340598)
  
  # non-vectors
  # vector and NA
  test_vector_lists(vecs_1, NA, "vector and NA", record_passes = FALSE)
  
  # vector and NULL
  test_vector_lists(vecs_1[names(vecs_1) != "null_1"], NULL, "vector and NULL", record_passes = FALSE)
  
  # vector and NaN
  test_vector_lists(vecs_1, NaN, "vector and NaN", record_passes = FALSE)
  
  # vector and data.frame
  data("iris")
  test_vector_lists(vecs_1, iris, "vector and data frame", record_passes = FALSE)
  
  # vector and function
  test_vector_lists(vecs_1, base::polyroot, "vector and function", record_passes = FALSE)
  
  # numeric vector differences within tolerance
  test_vector_lists(list(vecs_1$num_1), list(vecs_2$num_1 + 0.001), "numeric differences within tolerance", record_passes = FALSE)
  
  # vectors hold different values of identical vectors  
  test_vector_lists(vecs_1, vecs_2, "non-identical values vectors", record_passes = FALSE, exclude_NA = TRUE, exclude_NaN = TRUE)
  
  # vectors are different sizes
  test_vector_lists(vecs_1, vecs_3, "different vector length", record_passes = FALSE, exclude_NA = TRUE, exclude_NaN = TRUE)
  
  # test_vector_lists(vecs_1, vecs_2, "identical vectors")
  
  
  
  
  cat("\n")
}

# Helper functions ----
{
  # returns a list with randomly generated vectors of types:
  # string, char, numeric, integer, boolean, NA, NULL, NaN
  all_type_vecs = function(n, lambda = 50, max_word_size = 10, seed = 12345)
  {
    set.seed(seed)
    return(list(
      chr_1 = random_string(n, max_word_size = 1),
      str_1 = random_string(n, max_word_size = 10),
      num_1 = runif(n, min = -100, max = 100),
      int_1 = sample(as.integer(c(rpois(n / 2, lambda), -rpois(n / 2, lambda)))),
      boo_1 = as.logical(rbinom(n, 1, 0.5)),
      na_1 = rep(NA, n),
      null_1 = rep(NULL, n),
      nan_1 = rep(NaN, n)
    ))
  }
  
  # give random names to all vector elements in a list of vectors
  random_names_vec = function(vecs, max_name_length)
  {
    for (i in 1:length(vecs))
      names(vecs[[i]]) = random_string(length(vecs[[i]]), max_word_size = max_name_length)
    return(vecs)
  }
  
  # generate a random string of random length
  random_string = function(n, max_word_size = 10, chars = c(letters, LETTERS))
  {
    return(
      sapply(
        sample(max_word_size, n, replace = TRUE), 
        FUN = function(x) 
          paste0(chars[sample(1:length(chars), x, replace = TRUE)], collapse = ""))
    )
  }
  
  test_vector_lists = function(vecs_1, vecs_2, test_name, record_passes = TRUE, exclude_NA = FALSE, exclude_NULL = FALSE, exclude_NaN = FALSE)
  {
    passed_sum = 0
    failed_sum = 0
    n_tests = 0
    for(i in 1:length(vecs_1))
    {
      test_1 = vecs_1[[i]]
      
      if(class(vecs_1) != class(vecs_2)) test_2 = vecs_2 else test_2 = vecs_2[[i]]
      
      if (sum(
        (sum(is.na(test_1)) == length(test_1) && exclude_NA),
        (sum(is.null(test_1)) == length(test_1) && exclude_NULL),
        (sum(is.nan(test_1)) == length(test_1) && exclude_NaN)) == 0)
      {
        n_tests = n_tests + 1
        if(vec_equals(test_1, test_2, silent = TRUE))
        {
          passed_sum = passed_sum + 1
          if (!record_passes) 
            cat("\nWarning: test vector", i, names(vecs_1)[i], "passed test when it should have failed")
        } else {
          failed_sum = failed_sum + 1
          if (record_passes) cat("\nWarning: test vector", i, names(test_1), "failed test when it should have passed")
        }
      } # else cat("\nSkipping test", i, names(vecs_1)[i])
    }
    
    if (record_passes) test_sum = passed_sum else test_sum = failed_sum
    
    return(ifelse(
      test_sum == n_tests,
      {
        cat("\n", test_name, " tests passed", sep = "")
        TRUE
      },
      {
        cat("\n", test_name, " test failed", sep = "")
        FALSE
      }
    ))
  }
}



# Data frame equality tests ----
test_equivalent_data_frames = function()
{
  
  source("http://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/data_frame_equals.R")
  # source("equality_tests/data_frame_equals.R")
  
  
  df_pass = function(passed, test_name)
  {
    val_print = "passed"
    if (!passed) val_print = "failed"
    
    cat("\nTests for", test_name, val_print, sep = " ")
    return(passed)
  }
  
  silent_i = FALSE
  silent_i = TRUE

  iris2 = iris[, sample(ncol(iris))]
  iris3 = iris[sample(nrow(iris)), ]
  iris4 = iris[sample(nrow(iris)), sample(ncol(iris))]
  
  n_fails = sum(!c(
    
    # identical data frames test
    df_pass(data_frame_equals(iris, iris, silent = silent_i), "self-equivalence"),
    
    # identical entries, rows shuffled
    df_pass(
      data_frame_equals(iris, iris[, sample(ncol(iris))], silent = silent_i),
      "shuffled rows (row order not enforced)"),
    
    # non-matching shapes
    df_pass(!data_frame_equals(iris[1:4, ], iris, silent = silent_i), "mismatched size 1"),
    df_pass(!data_frame_equals(iris[, 1:3], iris, silent = silent_i), "mismatched size 2"),
    
    # shuffled column order, 
    df_pass(
      data_frame_equals(iris, iris2, enforce_col_order = FALSE, silent = silent_i), 
      "shuffled columns (order not enforced) 1"),
    
    # column order enforced
    df_pass(
      !data_frame_equals(iris, iris2, enforce_col_order = TRUE, silent = silent_i), 
      "shuffled columns (order enforced) 1"),
      
      # shuffled row order
      df_pass(
        data_frame_equals(iris, iris3, enforce_row_order = FALSE, silent = silent_i), 
        "shuffled rows (order not enforced) 1"),
    
    # row order enforced
    df_pass(
      !data_frame_equals(iris, iris3, enforce_row_order = TRUE, silent = silent_i), 
      "shuffled rows (order enforced) 1"),
    
    df_pass(
      data_frame_equals(iris, iris2, enforce_col_order = FALSE, enforce_row_order = FALSE, silent = silent_i), 
      "shuffled rows and columns (order not enforced) 1"),
    df_pass(
      data_frame_equals(iris, iris3, enforce_col_order = FALSE, enforce_row_order = FALSE, silent = silent_i), 
      "shuffled rows and columns (order not enforced) 2"),
    df_pass(
      data_frame_equals(iris, iris4, enforce_col_order = FALSE, enforce_row_order = FALSE, silent = silent_i), 
      "shuffled rows and columns (order not enforced)")
  ))
  
  if (n_fails > 0 ) cat("One or more of the data frame equivalence unit tests failed.") else
    cat("\n\nAll data frame equivalence unit tests passed")
}

# Perform tests ----
{
  test_equivalent_vectors()
  test_non_equivalent_vectors()  
  test_equivalent_data_frames()
}

