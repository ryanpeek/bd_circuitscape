# brms soap smoothers:

# resources: https://discourse.mc-stan.org/t/spatial-modeling-using-mgcv-soap-film-smoothing/10463/14
# https://www.fromthebottomoftheheap.net/2016/03/27/soap-film-smoothers/

library('tibble')
library('dplyr')
library('mgcv')
library('brms')
library('gamm4')

x <- c(0,  0,  1, 1,0)
y <- c(0, 1, 1, 0, 0)
x2 <- runif(100)
y2 <- runif(100)
z <- rnorm(100)

knots_test <- expand.grid(seq(0.1,0.9, length=5), 
                          seq(0.1,0.9, length=5)) %>%
  rename(x = Var1, y = Var2)
(testdata <- tibble(z = z, x = x2, y = y2))

# trad gam
g1 <- gam(z ~ s(x, y, bs = "so", xt = list(bnd = list(list(x=x, y=y)))),
          data = testdata, method = "REML", knots = knots_test)

# add boundary
bndry <- list(list(x = x, y = y))
g1_bound <- gamm(z ~ s(x, y, bs = "sf", xt = list(bnd = bndry)) +
                   s(x, y, bs = "sw", xt = list(bnd = bndry)),
                 data = testdata, method = "REML", knots = knots_test)
# works
gmm1 <- gamm4(z ~ s(x, y, bs = "sf", xt = list(bnd = bndry)) +
                s(x, y, bs = "sw", xt = list(bnd = bndry)),
              data = testdata, REML=TRUE, knots = knots_test)

# works
brm1 <- brm(z ~ s(x, y, bs = "sf", xt = list(bnd = bndry)) +
      s(x, y, bs = "sw", xt = list(bnd = bndry)),
    data = testdata, knots = knots_test)


names(brm1[['fit']])
summary(brm1)
brms:::rename_pars(brm1)
gsub("[ \t\r\n]", ".", names)
brm1

