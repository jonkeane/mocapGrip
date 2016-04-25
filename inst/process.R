cmdArgs <- commandArgs(trailingOnly = TRUE)

library(mocapGrip)

makeElanFiles(cmdArgs) -> output
