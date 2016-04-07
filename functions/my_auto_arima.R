my_auto_arima = 
function(x, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, 
          max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, 
          start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, 
          seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE, 
          trace = FALSE, approximation = (length(x) > 100 | frequency(x) > 12), 
          xreg = NULL, test = c("kpss", "adf", "pp"), seasonal.test = c("ocsb", "ch"), 
          allowdrift = TRUE, allowmean = TRUE, lambda = NULL, 
          parallel = FALSE, num.cores = 2) 
{
  if (stepwise == TRUE & parallel == TRUE) {
    warning("Parallel computer is only implemented when stepwise=FALSE, the model will be fit in serial.")
    parallel <- FALSE
  }
  series <- deparse(substitute(x))
  x <- as.ts(x)
  if (is.constant(x)) {
    fit <- Arima(x, order = c(0, 0, 0), fixed = mean(x, na.rm = TRUE, optim.control = list(maxit = 1000)))
    fit$x <- x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    fit$constant <- TRUE
    return(fit)
  }
  ic <- match.arg(ic)
  test <- match.arg(test)
  seasonal.test <- match.arg(seasonal.test)
  serieslength <- length(x)
  if (serieslength <= 3L) 
    ic <- "aic"
  if (seasonal) 
    m <- frequency(x)
  else m <- 1
  if (m < 1) {
    m <- 1
  }
  else m <- round(m)
  max.p <- ifelse(max.p <= floor(serieslength/3), max.p, floor(serieslength/3))
  max.q <- ifelse(max.q <= floor(serieslength/3), max.q, floor(serieslength/3))
  max.P <- ifelse(max.P <= floor((serieslength/3)/m), max.P, 
                  floor((serieslength/3)/m))
  max.Q <- ifelse(max.Q <= floor((serieslength/3)/m), max.Q, 
                  floor((serieslength/3)/m))
  orig.x <- x
  if (!is.null(lambda)) 
    x <- BoxCox(x, lambda)
  if (!is.null(xreg)) {
    nmxreg <- deparse(substitute(xreg))
    xreg <- as.matrix(xreg)
    if (ncol(xreg) == 1 & length(nmxreg) > 1) 
      nmxreg <- "xreg"
    if (is.null(colnames(xreg))) 
      colnames(xreg) <- if (ncol(xreg) == 1) 
        nmxreg
    else paste(nmxreg, 1:ncol(xreg), sep = "")
    j <- !is.na(x) & !is.na(rowSums(xreg))
    xx <- x
    xx[j] <- residuals(lm(x ~ xreg))
  }
  else xx <- x
  if (stationary) 
    d <- D <- 0
  if (m == 1) 
    D <- max.P <- max.Q <- 0
  else if (is.na(D)) {
    D <- nsdiffs(xx, m = m, test = seasonal.test, max.D = max.D)
    if (D > 0 & !is.null(xreg)) {
      diffxreg <- diff(xreg, differences = D, lag = m)
      if (any(apply(diffxreg, 2, is.constant))) 
        D <- D - 1
    }
  }
  if (D > 0) 
    dx <- diff(xx, differences = D, lag = m)
  else dx <- xx
  if (!is.null(xreg)) {
    if (D > 0) 
      diffxreg <- diff(xreg, differences = D, lag = m)
    else diffxreg <- xreg
  }
  if (is.na(d)) {
    d <- ndiffs(dx, test = test, max.d = max.d)
    if (d > 0 & !is.null(xreg)) {
      diffxreg <- diff(diffxreg, differences = d, lag = 1)
      if (any(apply(diffxreg, 2, is.constant))) 
        d <- d - 1
    }
  }
  if (d > 0) 
    dx <- diff(dx, differences = d, lag = 1)
  if (is.constant(dx)) {
    if (is.null(xreg)) {
      if (D > 0) 
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), 
                                                            period = m), fixed = mean(dx, na.rm = TRUE), 
                     include.constant = TRUE, optim.control = list(maxit = 1000))
      else if (d < 2) 
        fit <- Arima(x, order = c(0, d, 0), fixed = mean(dx, na.rm = TRUE), 
                     include.constant = TRUE, optim.control = list(maxit = 1000))
      else stop("Data follow a simple polynomial and are not suitable for ARIMA modelling.")
    }
    else {
      if (D > 0) 
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), 
                     xreg = xreg, optim.control = list(maxit = 1000))
      else fit <- Arima(x, order = c(0, d, 0), xreg = xreg, optim.control = list(maxit = 1000))
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    return(fit)
  }
  if (m > 1) {
    if (max.P > 0) 
      max.p <- min(max.p, m - 1)
    if (max.Q > 0) 
      max.q <- min(max.q, m - 1)
  }
  if (approximation) {
    if (D == 0) 
      fit <- try(arima(x, order = c(1, d, 0), xreg = xreg, optim.control = list(maxit = 1000)))
    else fit <- try(arima(x, order = c(1, d, 0), 
                          seasonal = list(order = c(0, D, 0), period = m, xreg = xreg, 
                                          optim.control = list(maxit = 1000))))
    if (!is.element("try-error", class(fit))) 
      offset <- -2 * fit$loglik - serieslength * log(fit$sigma2)
    else {
      warning("Unable to calculate AIC offset")
      offset <- 0
    }
  }
  else offset <- 0
  allowdrift <- allowdrift & (d + D) == 1
  allowmean <- allowmean & (d + D) == 0
  constant <- allowdrift | allowmean
  if (!stepwise) {
    bestfit <- search.arima(x, d, D, max.p, max.q, max.P, 
                            max.Q, max.order, stationary, ic, trace, approximation, 
                            xreg = xreg, offset = offset, allowdrift = allowdrift, 
                            allowmean = allowmean, parallel = parallel, num.cores = num.cores)
    bestfit$call <- match.call()
    bestfit$call$x <- data.frame(x = x)
    bestfit$lamba <- lambda
    bestfit$x <- orig.x
    bestfit$series <- series
    bestfit$fitted <- fitted(bestfit)
    if (!is.null(lambda)) {
      bestfit$fitted <- InvBoxCox(bestfit$fitted, lambda)
      bestfit$lambda <- lambda
    }
    return(bestfit)
  }
  p <- start.p <- min(start.p, max.p)
  q <- start.q <- min(start.q, max.q)
  P <- start.P <- min(start.P, max.P)
  Q <- start.Q <- min(start.Q, max.Q)
  results <- matrix(NA, nrow = 100, ncol = 8)
  bestfit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
  results[1, ] <- c(p, d, q, P, D, Q, constant, bestfit$ic)
  fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = constant, ic, trace, approximation, 
                 offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
  results[2, ] <- c(0, d, 0, 0, D, 0, constant, fit$ic)
  if (fit$ic < bestfit$ic) {
    bestfit <- fit
    p <- q <- P <- Q <- 0
  }
  if (max.p > 0 | max.P > 0) {
    fit <- myarima(x, order = c(max.p > 0, d, 0), seasonal = c((m > 1) & (max.P > 0), D, 0), 
                   constant = constant, ic, 
                   trace, approximation, offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
    results[3, ] <- c(1, d, 0, m > 1, D, 0, constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- (max.p > 0)
      P <- (m > 1) & (max.P > 0)
      q <- Q <- 0
    }
  }
  if (max.q > 0 | max.Q > 0) {
    fit <- myarima(x, order = c(0, d, max.q > 0), seasonal = c(0, D, (m > 1) & (max.Q > 0)), constant = constant, 
                   ic, trace, approximation, offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
    results[4, ] <- c(0, d, 1, 0, D, m > 1, constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- P <- 0
      Q <- (m > 1) & (max.Q > 0)
      q <- (max.q > 0)
    }
  }
  k <- 4
  if (constant) {
    fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = FALSE, ic, trace, approximation, 
                   offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
    results[5, ] <- c(0, d, 0, 0, D, 0, 0, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- q <- P <- Q <- 0
    }
    k <- 5
  }
  startk <- 0
  while (startk < k & k < 94) {
    startk <- k
    if (P > 0 & newmodel(p, d, q, P - 1, D, Q, constant, 
                         results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P - 1, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P - 1)
      }
    }
    if (P < max.P & newmodel(p, d, q, P + 1, D, Q, constant, 
                             results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P + 1, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P + 1)
      }
    }
    if (Q > 0 & newmodel(p, d, q, P, D, Q - 1, constant, 
                         results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q - 1), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P, D, Q - 1, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
      }
    }
    if (Q < max.Q & newmodel(p, d, q, P, D, Q + 1, constant, 
                             results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P,  D, Q + 1), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P, D, Q + 1, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
      }
    }
    if (Q > 0 & P > 0 & newmodel(p, d, q, P - 1, D, Q - 
                                 1, constant, results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q - 1), constant = constant, ic, trace, 
                     approximation, offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P - 1, D, Q - 1, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        P <- (P - 1)
      }
    }
    if (Q < max.Q & P < max.P & newmodel(p, d, q, P + 1, 
                                         D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 
                                                           1, D, Q + 1), constant = constant, ic, trace, 
                     approximation, offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q, P + 1, D, Q + 1, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        P <- (P + 1)
      }
    }
    if (p > 0 & newmodel(p - 1, d, q, P, D, Q, constant, 
                         results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p - 1, d, q), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p - 1, d, q, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p - 1)
      }
    }
    if (p < max.p & newmodel(p + 1, d, q, P, D, Q, constant, 
                             results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p + 1, d, q), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p + 1, d, q, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p + 1)
      }
    }
    if (q > 0 & newmodel(p, d, q - 1, P, D, Q, constant, 
                         results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q - 1), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q - 1, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
      }
    }
    if (q < max.q & newmodel(p, d, q + 1, P, D, Q, constant, 
                             results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p, d, q + 1), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p, d, q + 1, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
      }
    }
    if (q > 0 & p > 0 & newmodel(p - 1, d, q - 1, P, D, 
                                 Q, constant, results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p - 1, d, q - 1), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p - 1, d, q - 1, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        p <- (p - 1)
      }
    }
    if (q < max.q & p < max.p & newmodel(p + 1, d, q + 1, 
                                         P, D, Q, constant, results[1:k, ])) {
      k <- k + 1
      fit <- myarima(x, order = c(p + 1, d, q + 1), seasonal = c(P, D, Q), constant = constant, 
                     ic, trace, approximation, 
                     offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
      results[k, ] <- c(p + 1, d, q + 1, P, D, Q, constant, 
                        fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        p <- (p + 1)
      }
    }
    if (allowdrift | allowmean) {
      if (newmodel(p, d, q, P, D, Q, !constant, results[1:k, 
                                                        ])) {
        k <- k + 1
        fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = !constant, 
                       ic, trace, approximation, 
                       offset = offset, xreg = xreg, optim.control = list(maxit = 1000))
        results[k, ] <- c(p, d, q, P, D, Q, !constant, 
                          fit$ic)
        if (fit$ic < bestfit$ic) {
          bestfit <- fit
          constant <- !constant
        }
      }
    }
  }
  if (approximation & !is.null(bestfit$arma)) {
    newbestfit <- myarima(x, order = bestfit$arma[c(1, 6, 2)], seasonal = bestfit$arma[c(3, 7, 4)], 
                          constant = constant, 
                          ic, trace = FALSE, approximation = FALSE, xreg = xreg, optim.control = list(maxit = 1000))
    if (newbestfit$ic == Inf) {
      warning("Unable to fit final model using maximum likelihood. AIC value approximated")
    }
    else bestfit <- newbestfit
  }
  if (bestfit$ic == Inf) {
    cat("\n")
    stop("No suitable ARIMA model found")
  }
  bestfit$x <- orig.x
  bestfit$series <- series
  bestfit$ic <- NULL
  bestfit$call <- match.call()
  bestfit$call$x <- data.frame(x = x)
  bestfit$lambda <- lambda
  if (trace) 
    cat("\n\n Best model:", arima.string(bestfit), "\n\n")
  return(bestfit)
}
