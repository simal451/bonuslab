context("ridgereg_unittest")
library(MASS)
data("mtcars")

test_that("Compare coefficients", {
    regmodel <- ridgereg(mpg ~ disp + hp + drat + wt + qsec,data = mtcars)
    lmridge_mode <- lm.ridge(mpg ~ disp + hp + drat + wt + qsec,data = mtcars)
    expect_equivalent(round(regmodel$coef()[-1],1),round(lmridge_mode$coef,1))
})

