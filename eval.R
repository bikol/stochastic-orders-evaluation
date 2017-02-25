library(stats)
library(gaoptim)
library(dplyr)

source("data.R")
source("stochastic-orders.R")


# ga1=gaOpt(ff1, ff2)
ga1=gaOpt(mean_dec_owa_min_cen_0.025.4, mean_ep_cen_0.0_3.4, popSize=350000)

ga1$evolve(3)
# plot(ga1)
# print(max(ga1$bestFit()))

# ga1=gaOpt(ff1, ff3)
# ga1$evolve(20)
# plot(ga1)
# print(max(ga1$bestFit()))
