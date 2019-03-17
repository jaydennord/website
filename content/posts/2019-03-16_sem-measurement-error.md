---
title: "Measurement error in SEM"
date: 2019-03-16
tags: ["R", "sem"]
---



Suppose we wanted to fit the following model below using health data from
Hoffman and Fidell (1979). The observed variables are

* age - age bracket of the participant
* scstress - stress measure rescaled to help SEM estimation
* esteem - self-esteem measurement
* attmar - satisfaction with marital status
* control - locus of control measurement

![](/fig/2019-03-16_01.png)<!-- -->

The diagram can be expressed formulaically:

<div>
$$
\begin{align*}
\text{age} & = \lambda_1 \text{(age_f)} + e_1 \\
\text{scstress} & = \lambda_2 \text{(stresss_f)} + e_2 \\
\text{esteem} & = \lambda_3 \text{(self)} + e_3 \\
\text{attmar} & = \lambda_4 \text{(self)} + e_4 \\
\text{control} & = \lambda_5 \text{(self)} + e_5
\end{align*}
$$
</div>

The observed measures are linear combinations of latent factors and residuals, which are sometimes interpreted as measurement error. If we assume `age` is a perfect measure of `age_f` (the true age), then the reliability of the measure is 100% and the variance of $e_1$ to be 0. For identification, we can fix $\lambda_1$ to 1, in which case the latent factor will inherit the variance of `age`. Otherwise, we can fix the variance of `age_f` to 1, in which case $\lambda_1$ will be the estimated standard deviation of `age`. 

These consequences can be determined from the calculation of the variance of a linear combination of variables. For example, suppose $Y = aX_1 + bX_2$. Thus, $\text{Var}(Y) = a^2\text{Var}\left(X_1\right) + b^2\text{Var}\left(X_2\right) + ab\text{Cov}\left(X_1, X_2\right)$.

Perhaps we have determined that the reliability of our stress measure is 90%. Thus, 90% of the variability of `scstress` is attributed to `stress_f` and 10% of the variability is attributed to $e_2$. So,

<div>
$$
\begin{align*}
\text{Var(scstress)} & = \lambda_2^2\text{Var(stress_f)} + \text{Var}\left(e_2\right) \\
1 & =  \lambda_2^2 {\text{Var(stress_f)} \over \text{Var(scstress)}} + {{\text{Var}\left(e_2\right)}\over{\text{Var(stress_f)}}}
\end{align*}
$$
</div>

and

<div>
$$
\begin{align*}
.90 & = \lambda_2^2 {\text{Var(stress_f)} \over \text{Var(scstress)}} \\
.10 & = {{\text{Var}\left(e_2\right)}\over{\text{Var(scstress)}}}
\end{align*}
$$
</div>

There are two modelling approaches for determining $\lambda^2_2$ (assuming $\text{Var(scstress)}$ is set to 1), which I will call the _two-step_ and _one-step_ approaches. I believe that the two-step approach is the most common. In the two-step approach, the sample variance for `scstress` is used for $\text{Var(scstress)}$. Below is the syntax used to estimate the model of interest with the two-step approach.




```r
# required packages
library(tidyverse)
library(lavaan)
library(semPlot)

# health data
d <- read_csv(
  "https://raw.githubusercontent.com/jaydennord/data/master/health.csv"
)

# estimated variance of scstress
var(d$scstress)
## [1] 1.681464

# lambda_2
sqrt(.90 * var(d$scstress))
## [1] 1.23017
.10 * var(d$scstress)
## [1] 0.1681464

mod <- '
# model
  age_f =~ age
  
  stress_f =~ 1.23017 * scstress
  scstress ~~ .1681464 * scstress
  
  self =~ esteem + attmar + control
'

fit <- sem(mod, data = d, mimic = "mplus", std.lv = TRUE)

summary(fit, rsquare = TRUE)
## lavaan 0.6-3 ended normally after 29 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         15
## 
##   Number of observations                           459
##   Number of missing patterns                         1
## 
##   Estimator                                         ML
##   Model Fit Test Statistic                      21.793
##   Degrees of freedom                                 5
##   P-value (Chi-square)                           0.001
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard Errors                             Standard
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   age_f =~                                            
##     age               2.226    0.073   30.401    0.000
##   stress_f =~                                         
##     scstress          1.230                           
##   self =~                                             
##     esteem            2.711    0.399    6.789    0.000
##     attmar            3.696    0.593    6.228    0.000
##     control           0.622    0.097    6.392    0.000
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   age_f ~~                                            
##     stress_f         -0.306    0.044   -6.929    0.000
##     self             -0.089    0.071   -1.263    0.207
##   stress_f ~~                                         
##     self             -0.018    0.075   -0.242    0.809
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .age               4.364    0.104   41.997    0.000
##    .scstress          2.009    0.061   33.188    0.000
##    .esteem           15.830    0.184   85.925    0.000
##    .attmar           22.730    0.413   54.989    0.000
##    .control           6.743    0.059  114.265    0.000
##     age_f             0.000                           
##     stress_f          0.000                           
##     self              0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .scstress          0.168                           
##    .age               0.000                           
##    .esteem            8.227    2.055    4.004    0.000
##    .attmar           64.763    5.385   12.026    0.000
##    .control           1.212    0.128    9.487    0.000
##     age_f             1.000                           
##     stress_f          1.000                           
##     self              1.000                           
## 
## R-Square:
##                    Estimate
##     scstress          0.900
##     age               1.000
##     esteem            0.472
##     attmar            0.174
##     control           0.242

semPaths(
  fit, 
  what = "est",
  style = "lisrel", 
  weighted = FALSE,
  rotation = 4, 
  intercepts = FALSE, 
  nCharNodes = 0,  
  edge.label.cex = 1, 
  sizeMan = 10, 
  sizeLat = 10
)
```

![](/fig/2019-03-16_02.png)<!-- -->

The two-step approach is so named because the variance of a variable was estimated first and then entered as a constant in the model. I am uncomfortable, however, with a fixed value being used for an estimated variance. With appropriate constraints, the variance estimation can be built into the model, allowing the estimation of variance and the model in one step. 

To do this in `lavaan`, we must use the `lavaan` function as the defaults invoked by `sem` or `cfa` are incompatible with what needs to be done. Unfortunately, this means that we must build much of the model manually.


```r
mod <- '
# model
  age_f =~ age
  
  stress_f =~ lambda * scstress
  scstress ~~ e * scstress
  
  self =~ esteem + attmar + control

# constraints
  var_scstress := lambda^2 + e
  .90 == lambda^2 / var_scstress
  .10 == e / var_scstress

# originally implied defaults
  age_f ~~ 1 * age_f
  age ~~ 0 * age
  
  stress_f ~~ 1 * stress_f
  
  self ~~ 1 * self
  esteem ~~ esteem
  attmar ~~ attmar
  control ~~ control
'

fit <- lavaan(
  mod, data = d, 
  information = "observed", 
  auto.cov.lv.x = TRUE, 
  meanstructure = TRUE,
  int.ov.free = TRUE
)

summary(fit, rsquare = TRUE)
## lavaan 0.6-3 ended normally after 38 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         17
## 
##   Number of observations                           459
## 
##   Estimator                                         ML
##   Model Fit Test Statistic                      21.792
##   Degrees of freedom                                 4
##   P-value (Chi-square)                           0.000
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard Errors                             Standard
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   age_f =~                                            
##     age               2.226    0.073   30.299    0.000
##   stress_f =~                                         
##     scstrss (lmbd)    1.229    0.041   30.299    0.000
##   self =~                                             
##     esteem            2.711    0.399    6.790    0.000
##     attmar            3.696    0.593    6.228    0.000
##     control           0.622    0.097    6.392    0.000
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   age_f ~~                                            
##     stress_f         -0.306    0.045   -6.780    0.000
##     self             -0.089    0.071   -1.263    0.206
##   stress_f ~~                                         
##     self             -0.018    0.075   -0.242    0.809
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .age               4.364    0.104   42.001    0.000
##    .scstress          2.009    0.060   33.224    0.000
##    .esteem           15.830    0.184   85.925    0.000
##    .attmar           22.730    0.413   54.989    0.000
##    .control           6.743    0.059  114.265    0.000
##     age_f             0.000                           
##     stress_f          0.000                           
##     self              0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .scstress   (e)    0.168    0.011   15.149    0.000
##     age_f             1.000                           
##    .age               0.000                           
##     stress_f          1.000                           
##     self              1.000                           
##    .esteem            8.227    2.055    4.004    0.000
##    .attmar           64.763    5.385   12.026    0.000
##    .control           1.212    0.128    9.487    0.000
## 
## R-Square:
##                    Estimate
##     scstress          0.900
##     age               1.000
##     esteem            0.472
##     attmar            0.174
##     control           0.242
## 
## Defined Parameters:
##                    Estimate  Std.Err  z-value  P(>|z|)
##     var_scstress      1.678    0.111   15.149    0.000
## 
## Constraints:
##                                                |Slack|
##     .90 - (lambda^2/var_scstress)                0.000
##     .10 - (e/var_scstress)                       0.000

semPaths(
  fit, 
  what = "est",
  style = "lisrel", 
  weighted = FALSE,
  rotation = 4, 
  intercepts = FALSE, 
  nCharNodes = 0,  
  edge.label.cex = 1, 
  sizeMan = 10, 
  sizeLat = 10
)
```

![](/fig/2019-03-16_03.png)<!-- -->

An additional degree of freedom was used in the one-step approach and we now have standard errors for the loading and error term of `scstress` owing to the uncertainty of the variance estimation. Little else has changed in the model, however, but the one-step approach seems, in my opinion, more right than the two-step approach. I also wonder whether there are any scenarios or models where there is a profound difference between the two approaches. 

