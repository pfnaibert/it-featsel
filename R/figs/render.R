###########################################
#!/usr/bin/env Rscript

# Rscript --vanilla filename.R input

# 
args = commandArgs(trailingOnly=TRUE)


## program
rmarkdown::render(args[1])

