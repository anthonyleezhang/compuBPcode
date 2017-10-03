library(data.table)
source("compute_optimal_pvec.R")
source("make_plots.R")

# len is the grid fineness
len = 400

#######
# Version 1 - two utility levels
#######

name = "1_two_utils"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector
# Easy case - sender wants belief > 0.75
data[, Uvec := 0]
data[x >= 0.85, Uvec := 1]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec, solve_dual = TRUE, verbose = 1)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)


#######
# Version 2 - three utility levels
#######

name = "2_three_utils"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are three outcomes
data[, Uvec := 0]
data[x >= 1/3, Uvec := 1]
data[x >= 2/3, Uvec := 3]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)

# Checking the Z condition
temp = out$allvar$opt
temp2 = cumsum(temp[1:len])
data[, c_opt := cumsum(temp2 * c(0, diff(x)))]
data[, c_0 := cumsum(cumsum(p0) * c(0, diff(x)))]

data[x>1/3]
1/24

#######
# Version 3 - five utility levels, outwards - to - inwards
#######

name = "3_five_utils"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are five outcomes, more extreme beliefs are better
data[, Uvec := 3]
data[x >= 1/10, Uvec := 1]
data[x >= 3/10, Uvec := 0]
data[x >= 7/10, Uvec := 1]
data[x >= 9/10, Uvec := 3]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)

#######
# Version 4 - concave - sanity check
#######

name = "4_concave"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are five outcomes, more extreme beliefs are better
data[, Uvec := - (x - 0.5)^2]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)

#######
# Version 5 - convex - sanity check
#######

name = "5_convex"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are five outcomes, more extreme beliefs are better
data[, Uvec := (x - 0.5)^2]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)

#######
# Version 6 - sine
#######

name = "6_sine"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are five outcomes, more extreme beliefs are better
data[, Uvec := sin(2 * pi * x)]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)

#######
# Version 7 - faster sine
#######

name = "7_fastsine"
x = seq(0, 1, length = len)
data = data.table(x = x)

## Original distribution p0
# In this case, uniform
data[, temp := 1]
data[, p0 := temp / sum(temp)]
data[, temp := NULL]

## Utility vector

# In this case, there are five outcomes, more extreme beliefs are better
data[, Uvec := sin(4 * pi * x)]

out = compute_optimal_pvec(data$x, data$p0, data$Uvec)
save(out, file = paste("output/", name, ".RData", sep = ""))
make_plots(data = data, lpsolve_out = out, name = name)



