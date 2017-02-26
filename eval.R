library(stats)
library(gaoptim)
library(dplyr)
library(futile.logger)

# source("config.R")
source("data.R")
source("stochastic-orders.R")




# ga1=gaOpt(ff1, ff2)

cutPoint = 175
perm = sample(1:length(mean_dec_owa_min_cen_0.025.4), cutPoint)
fc1 = mean_dec_owa_min_cen_0.025.4[perm]
fc2 = mean_ep_cen_0.0_3.4[perm]

system.time(print(combinedOpt(fc1, fc2, method="BFGS", maxit = 500000)))
print(comb)

# ga1=gaOpt(fc2, fc1, popSize=100000)

# system.time(print(bruteOpt(fc1, fc2, iters=1e6)))

# system.time(ga1$evolve(3))
# plot(ga1)
# print(max(ga1$bestFit()))

# ga1=gaOpt(ff1, ff3)
# ga1$evolve(20)
# plot(ga1)
# print(max(ga1$bestFit()))
