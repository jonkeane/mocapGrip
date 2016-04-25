# to install ElementTree:
# pip install --user ElementTree
# or #
# download elementtree and then:
# python setup.py install --user

cmdArgs <- commandArgs(trailingOnly = TRUE)

# library(devtools)
# load_all("./mocapProcessor")

# install.packages("./mocapProcessor", repos = NULL, type="source")
install_github("jonkeane/mocapGrip")
library(mocapGrip)

mainFunc(cmdArgs) -> output
