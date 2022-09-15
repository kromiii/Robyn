# Library and environment for Reticulate/Nevergrad.
library("reticulate")
#virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
# py_install("nevergrad", pip = TRUE)
ng <- reticulate::import("nevergrad", delay_load = TRUE)
# Only if you use parallelism.
library(doParallel)
library(doRNG)

# Choose your optimization method below.
optimizer_name <- "TwoPointsDE"

# Now we can play with Nevergrad as usual.
# We assume here that we have 17 continuous hyperparameters with values in [0, 1].
# We can do other instrumentations, as discussed below.
my_tuple <- tuple(as.integer(19))
instrumentation <- ng$p$Array(shape = my_tuple, lower = 0, upper = 1)

num_workers <- 3  # We want to be able to evaluate 3 hyperparametrizations simultaneously.
num_iterations <- 100 * num_workers  # Let us say we have a budget of 100xnum_workers hyperparameters to evaluate.


score_b <- list(1,1,1)
instrumentation <- ng$p$Array(shape = my_tuple, lower = 0, upper = 1)
optimizer <- ng$optimizers$registry[optimizer_name](instrumentation, budget = num_iterations, num_workers = 3)
optimizer$tell(ng$p$MultiobjectiveReference(), score_b)
optimizer