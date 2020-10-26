// Stan model for simple linear regression

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
} // The posterior predictive distribution
