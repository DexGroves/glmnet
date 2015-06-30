# glmnet
Elastic nets in R, where you can score on one row.

CRAN glmnet can't do this:

```R
library("glmnet")

x    <- matrix(rnorm(100 * 20), 100, 20)
y    <- rnorm(100)
fit1 <- glmnet(x, y)
predict(fit1, newx = x[1, ], s = 0.005)

# Actually, this version can't do this either
predict(fit1, newx = as.matrix(x[1, ]), s =  0.005)
```
