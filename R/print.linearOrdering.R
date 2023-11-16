print.linearOrdering <-
function(x) {
  #Function prints to console: call method of function, method used for Linear Ordering, Ranking with ordered Items 
  #Args: rank_object: list (result of linearOrdering function - custom list containing call method to function, data frame with ordered items (Name, Result), method used for linear ordering)
  #Returns: nothing
  cat("call: ")
  print(x$call)
  cat("Method used for Linear Ordeing: ")
  if(x$method == "standsum") {
    print("Standardized Sums Method")
  }
  else if(x$method == "hellwig") {
    print("Hellwig's Method")
  }
  else {
    print("Topsis Method")
  }
  cat("Rank (Name, Score):\n")
  for (i in 1:nrow(x$rank)) {
    if (i == 1 || i == 2 || i == 3) {
      cat("\033[1;33m")  # Gold
    } 
    cat(i)
    cat(": ")
    cat(paste(x$rank[i, ], collapse = "\t"))  # Print row values separated by tabs
    cat("\n")
    cat("\033[0m")  # Reset color
  }
}
