# TODO: coef methods


contrast_matrix <- function(object, data, ...) {
  # Make a model matrix with one column per factor level
  object_vars <- all.vars(object)
  
  # Any character vars need to be factorized
  char_vars <- sapply(data, is.character) & colnames(data) %in% object_vars
  data[, char_vars] <- lapply(data[, char_vars], as.factor) 

  factor_vars <- sapply(data, is.factor) & colnames(data) %in% object_vars
  model.matrix(object = object, 
               ..., 
               data = data,
               contrasts.arg = lapply(data[, factor_vars],
                                      contrasts,
                                      contrasts = FALSE))
}

df.glmnet <- function(formula,
                      data,
                      contrasts     = NULL,
                      auto_contrast = FALSE,
                      cv            = FALSE,
                      ...) {
  ##############################################################################
  # Formula/dataframe interface for glmnet and cv.glmnet.                      #
  #                                                                            #
  # Args:                                                                      #
  #   formula:       formula object                                            #
  #   data:          input data.frame                                          #
  #   contrasts:     contrast argument for model.matrix                        #
  #   auto_contrast: assign one column per factor level in design matrix       #
  #   cv:            whether to use cv.glmnet                                  #
  #   ...:           additional arguments passed to glmnet or cv.glmnet        #
  # Returns:                                                                   #
  #   df.glmnet or cv.df.glmnet object                                         #
  ##############################################################################

  if (!is.element("data.frame", class(data))) {
    stop("data is not a data.frame!")
  }
  
  if (auto_contrast & !is.null(contrasts)) {
    message("contrasts option ignored as auto_contrast is TRUE.")
  }

  mframe <- model.frame(formula, data)
  mterms <- attr(mframe, "terms")

  y <- model.response(mframe, "any")

  if (auto_contrast) {
    x <- contrast_matrix(formula, data)
    contrasts <- attr(x, "contrasts")
  } else {
    x <- model.matrix(formula, data, contrasts)
  }
  
  if (cv) {
    glmnet_model <- cv.glmnet(x, y, ...)
  } else {
    glmnet_model <- glmnet(x, y, ...)
  }

  out <- list(glmnet_model = glmnet_model,
              formula      = formula,
              contrasts    = contrasts)
  if (cv) {
    class(out) <- "cv.df.glmnet"
  } else {
    class(out) <- "df.glmnet"
  }
  out 
}

predict.df.glmnet <- function(object, 
                                  newdata,
                                  ...) {
  if(missing(newdata) || is.null(newdata)) {
    stop("newdata not supplied.")
  }
  if (!is.element("data.frame", class(newdata))) {
    stop("newdata is not a data.frame!")
  }

  newx <- model.matrix(object$formula, newdata,
                       contrasts.arg = object$contrasts)
  
  predict.glmnet(object$glmnet_model, 
                 newx,
                 ...)
}

predict.cv.df.glmnet <- function(object, 
                                     newdata, 
                                     ...) {
  if(missing(newdata) || is.null(newdata)) {
    stop("newdata not supplied.")
  }
  if (!is.element("data.frame", class(newdata))) {
    stop("newdata is not a data.frame!")
  }

  newx <- model.matrix(object$formula, newdata,
                       contrasts.arg = object$contrasts)
  
  predict.cv.glmnet(object$glmnet_model, 
                    newx,
                    ...)
}

plot.df.glmnet <- function(object) {
  plot(object$glmnet_model)
}

plot.cv.df.glmnet <- function(object) {
  plot(object$glmnet_model)
}

coef.df.glmnet <- function(object, ...) {
  coef(object$glmnet_model)
}

coef.cv.df.glmnet <- function(object, ...) {
  coef(object$glmnet_model)
}

coef.plot <- function(object, ...) {
  # Make a coefficient path plot out of any flavour of glmnet object
  if (any(class(object) == "cv.df.glmnet")) {
    plot_obj <- object$glmnet_model$glmnet.fit
  }
  else if (any(class(object) == "cv.glmnet")) {
    plot_obj <- object$glmnet.fit
  }
  else if (any(class(object) %in% c("df.glmnet", "glmnet"))) {
    plot_obj <- object
  }
  plot(plot_obj, ...)
}