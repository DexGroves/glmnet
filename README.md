# glmnet
Elastic nets in R, with some improvements.

Originally developed by Jerome Friedman, Trevor Hastie, Noah Simon, Rob Tibshirani & others.

CRAN glmnet changed cross validation so that the lambda vector is recomputed for each fold. I am not convinced that this is a good idea, and can lead to errors on small samples.

CRAN glmnet can't score on a single row:

```R
library("glmnet")

x    <- matrix(rnorm(100 * 20), 100, 20)
y    <- rnorm(100)
fit1 <- glmnet(x, y)
predict(fit1, newx = x[1, ], s = 0.005)

# Actually, this version can't do this either
predict(fit1, newx = as.matrix(x[1, ]), s =  0.005)

# You have to do this, which is weird
predict(fit1, newx = t(as.matrix(x[1, ])), s =  0.005)
```

Barebones, experimental and completely undocumented formula & data.frame interface à la glm, with optional enforcement of one-column-per-factor-level without having to hack `contrasts.arg`. Also subsettable coefficient profiles.
```R
# install.packages("devtools")
devtools::install_github("DexGroves/glmnet")

library("glmnet")

generate_data <- function(N) {
  # Make some fake, noisy data with a few factors
  df <- data.frame(factor_a = as.factor(sample(letters[1:4], N, TRUE)),
                   factor_b = as.factor(sample(LETTERS[1:4], N, TRUE)),
                   num_a    = runif(N),
                   num_b    = runif(N),
                   noise_a  = rnorm(N),
                   noise_b  = rnorm(N),
                   noise_factor = as.factor(sample(letters[20:26], N, TRUE)))

  df$response <- as.numeric(df$factor_a) + 
                   2 * as.numeric(df$factor_b) +
                   0.25 * df$num_a + 
                   0.5  * df$num_b + 
                   rnorm(N) * 7
  data.frame(df)
}

# Some data.frames
train <- generate_data(2000)
hold  <- generate_data(2000)

m_formula <- response ~ factor_a + factor_b + num_a + num_b + 
                        noise_a + noise_b + noise_factor

# glmnet ex data.frame, where design matrix has one column per factor level
g <- df.glmnet(m_formula, train, cv = TRUE, nfolds = 10, auto_contrast = TRUE)

# predict on a data.frame.
pred <- predict(g, hold, s = "lambda.min")

# Coefficient profile for specific coefficients only (labels are WIP)
coef.plot(g, coefs = c("factor_aa","factor_ab", "factor_ac", "factor_ad"))
```
