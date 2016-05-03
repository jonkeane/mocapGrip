#' @import ggplot2

test <- function(data){
  ggplot(data) + aes(y=mpg, x=cyl) + geom_point()
}
