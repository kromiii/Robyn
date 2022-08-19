#Sys.setenv(DOWNLOAD_STATIC_LIBV8=1)

install.packages("remotes")

install.packages("reticulate")

remotes::install_github("facebookexperimental/Robyn/R")


library(reticulate)

#Sys.setenv(RETICULATE_PYTHON = "/opt/conda/bin") #/opt/conda/bin "/usr/bin/python3.8"

Sys.setenv(RETICULATE_PYTHON = "/Users/sinandjevdet/opt/miniconda3/envs/env_rpy37_n/bin/python3.7")
message(paste("set RETICULATE_PYTHON  ", collapse = ", "))


print(" ---------- PYTHON PATH IN RSESSION:")

print(Sys.which("python"))

print(reticulate::py_config())


reticulate::use_condaenv("env_rpy37_n")
message(paste("use_condaenv env_rpy37_n ", collapse = ", "))
#eticulate::conda_create("r-reticulate", "Python 3.7") # Only works with <= Python 3.9 sofar
#message(paste("created r-reticulate ", collapse = ", "))

#reticulate::use_condaenv("r-reticulate")

#reticulate::conda_install("r-reticulate", "nevergrad", pip=TRUE)



#virtualenv_create("r-reticulate")

#virtualenv_exists("r-reticulate")

#sessionInfo()
