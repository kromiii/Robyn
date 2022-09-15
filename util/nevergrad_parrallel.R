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
my_tuple <- tuple(as.integer(17))
instrumentation <- ng$p$Array(shape = my_tuple, lower = 0, upper = 1)

num_workers <- 3  # We want to be able to evaluate 3 hyperparametrizations simultaneously.
num_iterations <- 100 * num_workers  # Let us say we have a budget of 100xnum_workers hyperparameters to evaluate.

# Let us create a Nevergrad optimization method.
optimizer <-  ng$optimizers$registry[optimizer_name](instrumentation, budget=num_iterations, num_workers=num_workers)

# Dummy initializations.
nevergrad_hp <- list()
nevergrad_hp_val <- list()
score <- list()

for (i in 1:num_iterations) {
    for (j in 1:num_workers) {
       nevergrad_hp[[j]] <- optimizer$ask()
       nevergrad_hp_val[[j]] <- nevergrad_hp[[j]]$value
    }
    registerDoParallel(cores = num_workers)
    getDoParWorkers()
    #first <- foreach(i = 1:num_workers) %dopar% sd(nevergrad_hp_val[[j]])
    #second <- foreach(i = 1:num_workers) %dopar% mean(nevergrad_hp_val[[j]])
    score <- foreach(i=1:num_workers) %dorng% mean(nevergrad_hp_val[[j]])
    #browser()
    message(paste("Nevergrad first and second"))
    for (j in 1:num_workers) {
      message(paste("Nevergrad pre-optimiser num_workers"))
      #optimizer$tell(nevergrad_hp[[j]], tuple(first[[j]], second[[j]]))
      optimizer$tell(nevergrad_hp[[j]], list(score[[j]],0.045))
      #message(paste("Nevergrad pre-optimiser num_workers",optimizer$tell(nevergrad_hp[[j]], tuple(first[[j]], second[[j]]))))
    }
   message(paste("Nevergrad optimiser pre-value"))
   hh <- optimizer$recommend()$value
   message(paste("Nevergrad optimiser value", hh))
}

message(paste("SUUUUCCCESS"))
hh
#message(paste("Nevergrad optimiser value", hh))