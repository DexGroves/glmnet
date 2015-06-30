# glmnet
Elastic nets in R

It seems like glmnet can't score on one row.

```R
library("glmnet")

x    <- matrix(rnorm(100 * 20), 100, 20)
y    <- rnorm(100)
fit1 <- glmnet(x, y)
predict(fit1, newx = x[1, ], s = 0.005)
predict(fit1, newx = as.matrix(x[1, ]), s =  0.005)
```

Going to try to patch in this repo.
