renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Install typically used libraries
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("assertthat")
install.packages("styler")
install.packages("janitor")
install.packages("haven")

install.packages("caret")
install.packages("ROCR")
install.packages("pROC")
install.packages("doParallel")
install.packages("ranger")
install.packages("randomForestExplainer")
install.packages("xtable")
install.packages("rattle")
install.packages("viridisLite")

renv::snapshot()