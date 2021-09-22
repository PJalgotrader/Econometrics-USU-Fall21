# Prof. Pedram Jahangiry 
# Simple Regression Model (SRM) manual derivation



library(wooldridge)
library(dplyr)
library(stargazer)


# Example 2-3 : CEO salary and ROE

#------------------------------------------------------
# Manual calculations of OLS coefficient (optional)

# ingredients to the OLS formulas 
salary <- ceosal1$salary
roe    <- ceosal1$roe

( n           <- length(salary))
( b1hat       <- cov(roe,salary)/var(roe) )
( b0hat       <- mean(salary) - b1hat*mean(roe) )

salary # the following equations work for SRM and MRM. 
( salaryhat   <- b0hat + b1hat*roe  )
( uhat        <- salary - salaryhat)


( SST         <- (n-1) * var(salary))
( SSE         <- (n-1) * var(salaryhat))
( SSR         <- (n-1) * var(uhat))



# Four different ways of calculating R-square
( R2_mehtod1    <- SSE / SST) 
( R2_mehtod2    <- 1- (SSR / SST)  ) 
( R2_mehtod3    <- var(salaryhat) / var(salary)) 
( R2_mehtod4    <- cor(salary,salaryhat)^2)

# this last method only works in SRM (simple regression model)
( R2_method5    <- cor(salary, roe)^2)


#------------------------------------------------------

# Calculating the OLS coefficients using lm() and summary() 

reg <- lm( salary ~ roe, ceosal1)
stargazer(reg, type="text")



## finding SST, SSE and SSR using regression

SST = sum((salary - mean(salary))^2)
SST

SSE = sum((salaryhat -mean(salary))^2)
SSE

SSR = sum((salary - salaryhat)^2)
SSR


#-----------------------------------------------------

