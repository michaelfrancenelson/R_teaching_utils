# A set of convenience functions for auto-grading of student lab work.

# Print a VERY simple message describing whether the function test
# passed or failed:
print_test_message_01 = function(passed, fn_name)
{
  if (passed)
  {
    cat(paste0("\ntest for function ", fn_name, " passed\n")) 
  } else
  {
    cat(paste0("\ntest for function ", fn_name, " failed\n"))
  }
}

# Simple test of whether a function exists:
fun_exists = function(fn_name, ret_boolean = FALSE)
{
  if(exists(fn_name))
  {
    if(ret_boolean) return (TRUE)
  } else
  {
    cat("\nThe function ", fn_name, " was not found in memory.\n", sep = "")
    if(ret_boolean) return(FALSE)
  }
}



