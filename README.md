# glmnet
Elastic nets in R, where you can score on one row. Also planning to add some new features.

Originally developed by Jerome Friedman, Trevor Hastie, Noah Simon, Rob Tibshirani & others.

CRAN glmnet can't do this:

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

Some other things which might be worth doing:
* A glm-style formula and dataframe interface
* Coefficient profile plots with a legend and coefficient selection
