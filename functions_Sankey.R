# Inserting Text into String Functions #
# for Sankey Plot July 20th 2020 #

#some functions #
split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index -1, nchar(target)))
}

#Taken from https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

insert_str <- function(target, insert, index) {
  insert <- insert[order(index)]
  index <- sort(index)
  paste(interleave(split_str_by_index(target, index), insert), collapse="")
}