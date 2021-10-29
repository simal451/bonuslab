context("ridgereg_unittest")
library(MASS)
data("mtcars")

test_that("Compare coefficients", {
    regmodel <- ridgereg(as.formula("mpg ~ disp + hp + drat + wt"),data = mtcars, 0.5)
    lmridge_mode <- lm.ridge(mpg ~ 1 + disp + hp + drat + wt,data = mtcars, lambda = 0.5)
    expect_equal(round(as.numeric(regmodel$coef()),1), round(as.numeric(coef(lmridge_mode)),1))
})

