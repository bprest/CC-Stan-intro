
setwd('/Users/bcp17/Documents/GitHub/CC-Stan-intro')
# Adding stringsAsFactors = F means that numeric variables won't be
# read in as factors/categorical variables
seaice <- read.csv("seaice.csv", stringsAsFactors = F)

head(seaice)

plot(extent_north ~ year, pch = 20, data = seaice)

lm1 <- lm(extent_north ~ year, data = seaice)
summary(lm1)

abline(lm1, col = 2, lty = 2, lw = 3)

x <- I(seaice$year - 1978)
y <- seaice$extent_north
N <- length(seaice$year)

lm1 <- lm(y ~ x)
summary(lm1)

lm_alpha <- summary(lm1)$coeff[1]  # the intercept
lm_beta <- summary(lm1)$coeff[2]  # the slope
lm_sigma <- sigma(lm1)  # the residual error

# stan takes a list of data, whose names must match
stan_data <- list(N = N, x = x, y = y)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(gdata)
library(bayesplot)

# Write .stan file. Note that in stan, // is the comment character
# We did not specify any prior (except sigma>=0) so by default
# stan uses uninformative priors (uniform(-infty, +infty))
write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size. lower=1 sets a lower bound for N of 1.
 vector[N] x; // Predictor. Length of N
 vector[N] y; // Outcome. Length of N
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {
} // The posterior predictive distribution",

"stan_model1.stan")

stanc("stan_model1.stan")

stan_model1 <- "stan_model1.stan"

fit <- stan(file = stan_model1, data = stan_data, 
            warmup = 500, iter = 1000, chains = 4, 
            cores = 2, thin = 1)
fit
# the model has converged when Rhat is close to 1.

posterior <- extract(fit)
str(posterior)

plot(density(posterior$beta), type='l')
sd(posterior$beta)

# Compare to OLS
plot(y ~ x, pch = 20)
abline(lm1, col = 2, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

# Compare to range
plot(y ~ x, pch = 20)
for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}
abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)


### Try informative priors ----

write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 alpha ~ normal(10, 0.1);
 beta ~ normal(1, 0.1);
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {}",

"stan_model2.stan")

stan_model2 <- "stan_model2.stan"

st = Sys.time()
fit2 <- stan(stan_model2, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)
ed = Sys.time()
difftime(ed, st) # 39 seconds

fit2

posterior2 <- extract(fit2)

plot(y ~ x, pch = 20)
abline(alpha, beta, col = 4, lty = 2, lw = 2)
abline(mean(posterior2$alpha), mean(posterior2$beta), col = 3, lw = 2)
abline(mean(posterior$alpha), mean(posterior$beta), col = 36, lw = 3)

# 'Anything over an `n_eff` of 100 is usually "fine"' - Bob Carpenter

plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")
plot(posterior$sigma, type = "l")

fit_bad <- stan(stan_model1, data = stan_data, warmup = 25, iter = 50, chains = 4, cores = 2, thin = 1)
posterior_bad <- extract(fit_bad)

plot(posterior_bad$alpha, type = "l")
plot(posterior_bad$beta, type = "l")
plot(posterior_bad$sigma, type = "l")

par(mfrow = c(1,3))

plot(density(posterior$alpha), main = "Alpha")
abline(v = lm_alpha, col = 4, lty = 2)

plot(density(posterior$beta), main = "Beta")
abline(v = lm_beta, col = 4, lty = 2)

plot(density(posterior$sigma), main = "Sigma")
abline(v = lm_sigma, col = 4, lty = 2)

sum(posterior$beta>0)/length(posterior$beta)
sum(posterior$beta>0.2)/length(posterior$beta)

traceplot(fit)

stan_dens(fit)
stan_hist(fit)

plot(fit, show_density = FALSE, ci_level = 0.5, outer_level = 0.95, fill_color = "salmon")

# Posterior predictive checks.
# Generate
write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(x * beta + alpha, sigma);
}

generated quantities {
 real y_rep[N];

 for (n in 1:N) { // for each obs., generate a predicted value. accounts for both uncertainty in alpha, beta, and epsilon.
 y_rep[n] = normal_rng(x[n] * beta + alpha, sigma);
 }

}",
# a couple of notes: 
# 1) generated quantities aren't vectorized so you have to loop. but it loops in C++ so it's quite fast
# 2) the sampling function is normal_rng, just the sampling model with _rng at the end
"stan_model2_GQ.stan")

stan_model2_GQ <- "stan_model2_GQ.stan"

fit3 <- stan(stan_model2_GQ, data = stan_data, iter = 1000, chains = 4, cores = 2, thin = 1)

fit3

# SxN draws from the posterior predictive distribution
y_rep <- as.matrix(fit3, pars = "y_rep")
dim(y_rep)
y_rep[1,]

ppc_dens_overlay(y, y_rep[1:200, ])
plot(density(y))
# https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html

# posterior predictive checking (ppc) plots
ppc_stat(y = y, yrep = y_rep, stat = "mean")
ppc_scatter_avg(y = y, yrep = y_rep)

available_ppc()

ppc_ribbon(y=y, yrep=y_rep)
# ppc_error_binned(y=y, yrep=y_rep)
ppc_hist(y=y, yrep=y_rep)

color_scheme_view(c("blue", "gray", "green", "pink", "purple",
                    "red","teal","yellow"))
color_scheme_view("mix-blue-red")
color_scheme_set("blue")
color_scheme_set("mix-blue-red")
ppc_ribbon(y=y, yrep=y_rep)
