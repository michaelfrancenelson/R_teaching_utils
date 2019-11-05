# Simple tests to verify the correctness of the students' custom functions

lab_05_function_tests = function()
{
  
  source("https://raw.githubusercontent.com/michaelfrancenelson/R_teaching_utils/master/equality_tests/vec_equals.R")
  
  
  
  
  # Tests
  if (fun_exists("calc_errors"))
  {
    # must test 3 cases: expected is a vector, expected is a number, expected is null
    vec_equals(calc_errors_r(x1))
    
    
  }
  
  
}




