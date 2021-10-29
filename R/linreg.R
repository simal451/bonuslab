#' A reference class representing av linear regression model
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @field beta_hat A matrix containing the estimated coefficients
#' @field y_hat A matrix containing the fitted values
#' @field res A matrix containing the residuals
#' @field df A numeric vector containg the degrees of freedom
#' @field res_var A numeric vector containing the residual variance
#' @field beta_hat_var A matrix containing the variance of the estimated coefficients
#' @field t_values A matrix containg the t-values of the estimated coefficients
#' @field p_values A matrix containg the p-values of the estimated coefficients
#' @field resstd A matrix containg the standardized residuals
#' @return An object of class linreg
#' @examples
#' linreg(iris, as.formula("Petal.Length ~ Species"))
#' linreg(iris, as.formula("Petal.Length ~ Sepal.Width + Sepal.Length"))
#' @references \url{https://en.wikipedia.org/wiki/Least_squares}
#' @export linreg
linreg <- setRefClass("linreg",
                      fields = list(
                        beta_hat = "matrix",
                        y_hat = "matrix",
                        res = "matrix",
                        df = "numeric",
                        res_var = "numeric",
                        beta_hat_var = "matrix",
                        t_values = "matrix",
                        p_values = "matrix",
                        resstd = "matrix",
                        reg_formula = "formula",
                        data_name = "character"
                      ),
                      methods = list(
                        initialize = function(data, formula) {
                          X <- model.matrix(formula, data = data)
                          y <- data[,all.vars(formula)[1]]
                          beta_hat <<- solve(t(X) %*% X) %*% t(X) %*% y
                          y_hat <<- X %*% beta_hat
                          res <<- y - y_hat
                          df <<- nrow(X) - ncol(X)
                          res_var <<- as.numeric((t(res) %*% res) / df)
                          beta_hat_var <<- res_var * (solve(t(X) %*% X))
                          t_values <<- beta_hat / sqrt(diag(beta_hat_var))
                          p_values <<- 2*pt(-abs(t_values), df = df)
                          resstd<<- sqrt(abs(res / sqrt(res_var)))
                          resstd <<- sqrt(abs(res / sqrt(res_var)))
                          reg_formula <<- formula
                          data_name <<- deparse(substitute(data))
                        },
                        print = function() {
                          "Prints out the coefficients and the coefficient names"
                          cat("linreg(formula = ", deparse(reg_formula), ", data = ", data_name,  ")\n\n", sep = "")
                          print_df <- as.data.frame(t(beta_hat))
                          print.data.frame(print_df, row.names = FALSE)
                        },
                        plot = function() {
                          "Plots the residuals versus the fitted values and the square root of the standardized residuals versus the fitted values"
                          plot_df <- data.frame(y_hat, res)

                          p1 <- ggplot2::ggplot(data = plot_df, aes(x = y_hat, y = res)) +
                            geom_point(shape = 21, colour = "black", fill = "white") +
                            geom_path(data = as.data.frame(with(plot_df, lowess(x = y_hat, y = res))),
                                      aes(x = x, y = y), col = "red") +
                            geom_text(data = plot_df[order(abs(plot_df$res), decreasing = TRUE)[1:3],],
                                      aes(label = order(abs(plot_df$res), decreasing = TRUE)[1:3]),
                                      position = position_nudge(x = -0.1)) +
                            ggtitle("Residuals vs Fitted") +
                            labs(x = "Fitted values", y = "Residuals") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = 0.5))

                          p2 <- ggplot2::ggplot(data = data.frame(y_hat, resstd), aes(x = y_hat, y = resstd)) +
                            geom_point(shape = 21, colour = "black", fill = "white") +
                            geom_path(data = as.data.frame(with(data.frame(y_hat,resstd ), lowess(x = y_hat, y = resstd))),
                                      aes(x = x, y = y), col = "red") +
                            ggtitle("Scale-Location") +
                            labs(x = "Fitted values", y = "Standardized residuals") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = 0.5))

                          grid.arrange(p1,p2)
                        },
                        resid = function() {
                          "Returns a vector with the residuals"
                          return(c(res))
                        },
                        pred = function() {
                          "Returns a vector with the fitted values"
                          return(c(y_hat))
                        },
                        coef = function() {
                          "Returns the estimated coefficients"
                          coef_vec <- c(beta_hat)
                          names(coef_vec) <- row.names(beta_hat)
                          cat(colnames(beta_hat))
                          return(coef_vec)
                        },
                        summary = function() {
                          "Returns a summary of the linear regression model"
                          summary_df <- data.frame(beta_hat,
                                                   sqrt(diag(beta_hat_var)),
                                                   t_values,
                                                   p_values)
                          colnames(summary_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

                          row.names(summary_df) <- row.names(beta_hat)
                          printCoefmat(summary_df)
                          cat("\nResidual standard error:", sqrt(res_var), "on", df, "degrees of freedom")
                        }
                      ))
