is_numeric_values <-
function(vec) {
  #Function is checking whether provided list contains numeric values
  #Args: vec: list (of any size)
  #Returns: boolean (TRUE if all values are bumeric, FALSE otherwise)
  all(sapply(vec, is.numeric))
}
