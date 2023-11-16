plot.linearOrdering <-
function(x, type="all") {
  #Function plots table with all of the ordered items or top three results
  #Args: 
    #rank_object: list (result of linearOrdering function - custom list containing call method to function, data frame with ordered items (Name, Result), method used for linear ordering)
    #type: character, (function is ploting table with all of the ordered items for value "all" or top three items for value "top three"), default value for type is "all"
  #Returns: nothing
  if (!require(plotly)) install.packages("plotly")
  if (!require(ggplot2)) install.packages("ggplot2")
  if(type=="all") {
    library(plotly)
    table_plot <- plot_ly(
      type = "table",
      header = list(values = colnames(x$rank), align = "center", fill = list(color = "#daf0ff")),
      cells = list(values = list(x$rank$Name, x$rank$Result), align = "center"),
      columnwidth = c(80, 80)
    )
    table_plot
  }
  else if(type == "top three") {
    library(ggplot2)
    if(nrow(x$rank) >= 3) {
      x$rank$Rank <- rank(x$rank$Result, ties.method = "min")
      ggplot(x$rank[1:3,], aes(x = reorder(Name, -Result), y = Result, fill = factor(Result))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#a77044","#d7d7d7","#d6af36")) +
        labs(title = "Ranking - Top Three Results", x = "Name", y = "Score") +
        theme_minimal()
    }
  }
}
