answer.key = function(question_n, ..., p_class = "<p class=\"key\">", h_class = "<h3 class=\"key\">")
{
  ans = list(...)
  pwrap = function(x)
  {
    paste0(
      p_class,
      x,
      "</p>"  
    )
  }  
  cat(
    h_class, " Q", question_n, " answers</h3>",
    sapply(ans, pwrap),
    sep = "")
}