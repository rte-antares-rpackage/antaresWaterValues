# The goal of the following lines is only to remove many useless warnings in
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax and from others syntax (loop,ggplot,..).

utils::globalVariables(
  c("x","y")
)
utils::globalVariables(".data")
