# Moisture transformation funtions from Wild et al. 2019
# https://doi.org/10.1016/j.agrformet.2018.12.018
# Found in the supplementary files
# See the Suppl. Table 1
# fun1 is for sand
# fun8 is for peat
# ...
# funNA is for unknown soils from Kopecky et al. 2020

cal_fun1 <- function(x) {((-3.00e-9) * (x^2) + (1.61e-4) * x + (-1.10e-1))*100 }
cal_fun2 <- function(x) {((-1.90e-8) * (x^2) + (2.66e-4) * x + (-1.54e-1))*100 }
cal_fun3 <- function(x) {((-2.30e-8) * (x^2) + (2.82e-4) * x + (-1.67e-1))*100 }
cal_fun4 <- function(x) {((-3.80e-8) * (x^2) + (3.39e-4) * x + (-2.15e-1))*100 }
cal_fun5 <- function(x) {((-9.00e-10) * (x^2) + (2.62e-4) * x + (-1.59e-1))*100 }
cal_fun6 <- function(x) {((-5.10e-8) * (x^2) + (3.98e-4) * x + (-2.91e-1))*100 }
cal_fun7 <- function(x) {((1.70e-8) * (x^2) + (1.18e-4) * x + (-1.01e-1))*100 }
cal_fun8 <- function(x) {((1.23e-7) * (x^2) + (-1.45e-4) * x + (2.03e-1))*100 }
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

