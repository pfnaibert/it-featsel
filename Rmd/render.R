###########################################
#!/usr/bin/env Rscript

# Rscript --vanilla filename.R input
# Rscript  filename.R input

# 
args = commandArgs(trailingOnly=TRUE)


## program
rmarkdown::render(args[1], output_dir="../html")

