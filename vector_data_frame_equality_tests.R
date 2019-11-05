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
}




# Vector inequality ----
#
# Need to test unequal
#   lengths
#   classes
#   values
test_non_equivalent_vectors = function()
{
  source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  set.seed(12345)
  n1 = 100
  n2 = 103
  lambda = 50
  max_string_l = 150
  
  vecs_1 = all_type_vecs(n = n1,lambda = lambda, max_word_size = max_string_l)
  vecs_2 = all_type_vecs(n = n2,lambda = lambda, max_word_size = max_string_l, seed = 9876543)
  
  # non-vectors
  # vector and NA
  test_vector_lists(vecs_1, NA, "vector and NA", record_passes = FALSE)
  
  # vector and NULL
  test_vector_lists(vecs_1[names(vecs_1) != "null_1"], NULL, "vector and Null", record_passes = FALSE)
  
  
  
  sum(sapply(vecs_1[names(vecs_1) != "null_1"], FUN = function(x) vec_equals(x, NULL, silent = TRUE)))
  
  # vector and data.frame
  data("iris")
  test_vector_lists(vecs_1, iris, "vector and data frame", record_passes = FALSE)
  
  sum(sapply(vecs_1, FUN = function(x) vec_equals(x, iris)))
  
  # vector and function
  sum(sapply(vecs_1, FUN = function(x) vec_equals(x, plot)))
  
  # null, NaN, and NA vectors
  sum(vec_equals(NA, NA), vec_equals(NULL, NULL), vec_equals(NaN, NaN))
  
  
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
  
  test_vector_lists = function(vecs_1, vecs_2, test_name, record_passes = TRUE)
  {
    # passed_sum = sum(sapply(1:length(vecs_1), FUN = function(x) vec_equals(vecs_1[[x]], vecs_2[[x]], silent = TRUE))) == 0
    
    
    # is.null(vecs_1[[i]]) && is.null(vecs_2[[i]])
    # sum(is.nan(vecs_1[[i]])) && (is.nan(vecs_2[[i]]))
    # sum(is.null(vecs_1[[i]])) == (is.null(vecs_2[[i]]))
    
    
    # vec_equals(rep(NaN, 10), rep(NaN, 10))
    # vec_equals(rep(NULL, 10), rep(NULL, 10))
    # vec_equals(rep(NA, 10), rep(NA, 10))
    
    
    test_1 = vecs_1$na_1
    test_1
    passed_sum = 0
    failed_sum = 0
    for(i in 1:length(vecs_1))
    {
      test_1 = vecs_1[[i]]
      if(!is.list(vecs_2)) test_2 = vecs_2 else test_2 = vecs_2[[i]]
      
      
      # print(names(vecs_1)[i])
      # if (!(is.null(vecs_1[[i]]) && is.null(vecs_2[[i]]))
      if(!vec_equals(test_1, test_2, silent = TRUE))
      {
        passed_sum = passed_sum + 1
      } else {
        failed_sum = failed_sum + 1
      }
      
      
    }
    
    if (record_passes) test_sum = passed_sum else test_sum = failed_sum
    
    return(ifelse(
      test_sum == 0,
      {
        cat("\n", test_name, " test passed", sep = "")
        TRUE
      },
      {
        cat("\n", test_name, " test failed", sep = "")
        FALSE
      }
    ))
    
    # test_vector_lists(list(vecs_1$num_1), list(vecs_2$num_1 + 0.00001), "numeric differences outside tolerance")
    # l1 = list(vecs_1$num_1)
    # l2 = list(vecs_2$num_1 + 0.00001)
    
    # testvec  
    
  }
}


# Perform tests ----
{
  test_equivalent_vectors()
  
}

