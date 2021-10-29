#' Linear regression using QR decomposition
#'
#' @param data A data frame containg all the data
#' @param formula The regression formula
#' @return The estimated coefficients and their variance
#' @examples
#' linreg_qr_decomp(iris, as.formula("Petal.Length ~ Species"))
#' linreg_qr_decomp(iris, as.formula("Petal.Length ~ Sepal.Width + Sepal.Length"))
#' @references \url{https://en.wikipedia.org/wiki/QR_decomposition}
#' @export
linreg_qr_decomp <- function(data, formula) {
  X <- model.matrix(formula, data = data)
  y <- data[,all.vars(formula)[1]]

  A <- X
  H <- list()

  for(i in 1:ncol(X)) {
    if(i > 1) {
      A <- as.matrix((H[[i-1]] %*% A)[-1,-1])
    }
    a <- as.matrix(A[,1])
    a_norm <- norm(a, type = "2")
    v <- a + sign(A[1,1]) * a_norm * diag(((nrow(X)+1)-i))[,1]
    H[[i]] <- diag(((nrow(X)+1)-i)) - as.numeric(2 * (1/(t(v) %*% v))) * (v %*% t(v))
  }

  lapply(1:(ncol(X)), function(x) {
    H_old <- H[[x]]
    H[[x]] <<- diag(1, nrow = nrow(X), ncol = nrow(X))
    H[[x]][x:nrow(X),x:nrow(X)] <<- H_old
  })

  R <- Reduce("%*%", H[length(H):1]) %*% X
  Q <- Reduce("%*%", H)

  # Q*R = X
  round(Q %*% R)

  # R^-1 Q^t y
  beta_hat <- backsolve(R, (t(Q) %*% y))

  # Fitted values
  fitted_values <- X %*% beta_hat
  # Residuals
  res <- y - fitted_values
  # Residual variance
  res_var <- sum(res^2) / (nrow(X) - ncol(X))

  # Coefficient variance
  beta_var <- diag(solve(t(R) %*% R) * res_var)

  summary_df <- data.frame(beta_hat, beta_var)
  colnames(summary_df) <- c("Estimate", "Variance")

  return(summary_df)
}
