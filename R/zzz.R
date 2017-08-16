
# The goal of the following lines is only to remove many useless warnings in 
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax.
# (from antaresRead)
utils::globalVariables(c("hstorPMaxHigh"))
