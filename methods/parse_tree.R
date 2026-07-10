#Adapted from https://gist.github.com/tomasgreif/6038822
#Thanks to Jesse Shaw for creating and sharing back
#'
#' Rpart rules are changed to sql CASE statement.
#'
#' @param df data frame used for rpart model
#' @param model rpart model
#' @export
#' @examples
#' parse_tree(df=kyphosis,model=rpart(data=kyphosis,formula=Kyphosis~.))
#' parse_tree(df=mtcars,model=rpart(data=mtcars,formula=am~.))
#' parse_tree(df=iris,model=rpart(data=iris,formula=Species~.))
#' x <- german_data
#' x$gbbin <- NULL
#' model <- rpart(data=x,formula=gb~.)
#' parse_tree(x,model)

library(rpart)
library(scrutiny)

parse_tree_to_sql <- function (df=NULL, model=NULL) {
  log <- capture.output({
    rpart.rules <- path.rpart(model,rownames(model$frame)[model$frame$var=="<leaf>"])
  })  

  args <- c("<=",">=","<",">","=")
  rules_out <- "CASE "
  i <- 1

  for (rule in rpart.rules) {  
    rule_out <- character(0)
    for (component in rule) {
      sep <- lapply(args, function(x) length(unlist(strsplit(component,x)))) > 1
      elements <- unlist(strsplit(component,(args[sep])[1]))
      if(!(elements[1]=="root")) {
        if (is_numeric_like(elements[2])) {
          rule_out <- c(rule_out,paste(elements[1],(args[sep])[1],elements[2]))
        } else {
          rule_out <- c(rule_out,paste0(elements[1]," in (",paste0("'",unlist(strsplit(elements[2],",")),"'",collapse=","),")"))
        }
      }
    }
    rules_out <- c(rules_out, paste0("\n\n     WHEN ", paste(rule_out,collapse="\n      AND "),"\n     THEN ",
sprintf("%f /*node %s */",model$frame$yval[row.names(model$frame)==names(rpart.rules)[i] ],names(rpart.rules)[i]) ))
    if(i==length(rpart.rules)) rules_out <- c(rules_out,"\n\nEND AS outcome\n")
    i <- i +1
  }
  sql_out <- paste(rules_out, collapse=" ")
  sql_out <- gsub("CASE  \n\n     ", "CASE ", sql_out)
  return(sql_out)
}
