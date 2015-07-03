plot.glmnet=function(x,
                     xvar  = c("norm","lambda","dev"),
                     label = FALSE,
                     coefs = NA,
                     ...) {
  xvar=match.arg(xvar)

  # Accept coefs as a numeric index or a list of strings naming the columns
  if (is.na(coefs) && class(coefs) != "numeric") {
    # Convert strings to indices. Is there a better way to get beta names?
    coef_names <- names(x$beta[, 1])
    coefs <- which(coef_names %in% coefs)

    if (any(coefs %in% coef_names)) {
      missing_coefs <- coefs[!coefs %in% coef_names]
      warning(paste("Could not find: ", paste(missing_coefs, collapse = ", ")))
    }
  }

  plotCoef(x$beta,
           lambda = x$lambda,
           df     = x$df,
           dev    = x$dev.ratio,
           label  = label,
           xvar   = xvar,
           coefs  = coefs, 
           ...)
}
