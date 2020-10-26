// Stan model for simple linear regression

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

}
