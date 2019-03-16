---
title: "Contrast scaling"
date: 2019-03-03
tags: ["R", "general linear modeling"]
---



Using a [publicly available auto dataset](https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/), students were asked to set up contrasts to estimate the difference between American and foreign (European and Japenese) cars' miles-per-gallon (mpg) and the difference between European and Japanese cars' mpg. The data are read into R with the code below. Then, the variable "origin" is explicitly converted into a categorical variable from a numeric. 


```r
d <- read.csv(
  "https://raw.githubusercontent.com/jaydennord/data/master/auto.csv",
  as.is  = TRUE
)[, "mpg", "origin"]

d <- transform(
  d, 
  origin = factor(origin, 1:3, c("American", "European", "Japanese"))
)

head(d)
##   mpg   origin
## 1  18 American
## 2  15 American
## 3  18 American
## 4  16 American
## 5  17 American
## 6  15 American
```

Before implementing contrasts, I want to fit a simple dummy-coded version of mpg regressed on origin. Using the resulting model, model-estimated means for each country of origin are calculated. 95% confidence intervals are printed along with the estimate. These are needed for future illustration.


```r
# model fitting
fit <- lm(mpg ~ origin, data = d)
summary(fit)
## 
## Call:
## lm(formula = mpg ~ origin, data = d)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.451  -5.083  -1.034   3.649  18.916 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     20.0835     0.4056  49.517   <2e-16 ***
## originEuropean   7.8079     0.8658   9.018   <2e-16 ***
## originJapanese  10.3671     0.8264  12.544   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.4 on 395 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.3329,	Adjusted R-squared:  0.3295 
## F-statistic: 98.54 on 2 and 395 DF,  p-value: < 2.2e-16

# model-estimated means
newdata <- data.frame(origin = c("American", "European", "Japanese"))

m_ci <- predict(fit, newdata, interval = "c")
m <- m_ci[, 1] # just the mean estimates - used later

cbind(newdata, m_ci)
##     origin      fit      lwr      upr
## 1 American 20.08353 19.28615 20.88092
## 2 European 27.89143 26.38753 29.39533
## 3 Japanese 30.45063 29.03499 31.86628
```

Now, contrasts are included in the model to assess the statistical significance of the differences of interest (American vs foreign; European vs Japanese). The contrasts for these differences (comparing one level to the mean of others, then comparing the second level with the third) are referred to as _Helmert contrasts_. Thus, I use R's built-in `contr.helmert` function.

The initial `contr` matrix below, however, is backwards. Each row is intended to associate with one of the three means while each column is intended to associate with the differences of interest. As the matrix is now, Japenese mpg will be compared to the average of American and European mpg. This issues is resolved with `rev` to reverse the `contr` vector and the `[]<-` assignment operation to maintain the matrix dimensionality of `contr`. 


```r
contr <- contr.helmert(3)
colnames(contr) <- c(".American_vs_foreign", ".European_vs_Japanese")
rownames(contr) <- levels(d$origin)

print(contr)
##          .American_vs_foreign .European_vs_Japanese
## American                   -1                    -1
## European                    1                    -1
## Japanese                    0                     2

contr[] <- rev(contr)
print(contr)
##          .American_vs_foreign .European_vs_Japanese
## American                    2                     0
## European                   -1                     1
## Japanese                   -1                    -1
```

Contrasts can be implemented by creating raw variables (like h1 and h2 in the code below) and then using those variables in the model in place of origin. I forgo this option in favor of `lm`'s `contrasts` argument. Furthermore, I forgo writing and running a new `lm` in favor of the `update` function insofar as I provide the syntax but leave it commented out and unused. 


```r
# d <- transform(
#   d,
#   h1 = contr[origin, 1],
#   h2 = contr[origin, 2]
# )
# 
# head(d)

# fit_contr <- lm(mpg ~ origin, data = d, contrasts = list(origin = contr))
# fit_contr <- lm(mpg ~ h1 + h2, data = d)
fit_contr <- update(fit, contrasts = list(origin = contr))
coef(summary(fit_contr))
##                              Estimate Std. Error    t value      Pr(>|t|)
## (Intercept)                 26.141865  0.3753762  69.641783 6.292610e-224
## origin.American_vs_foreign  -3.029166  0.2212136 -13.693397  3.371848e-35
## origin.European_vs_Japanese -1.279602  0.5252765  -2.436054  1.528964e-02

confint(fit_contr)
##                                 2.5 %     97.5 %
## (Intercept)                 25.403880 26.8798502
## origin.American_vs_foreign  -3.464069 -2.5942623
## origin.European_vs_Japanese -2.312289 -0.2469149
```

The coefficients are now different. The intercept is the mean of the means, `mean(m)` that equals 26.142. Presumably, the next two coefficients should be the difference in mpg between American and foreign and the difference in mpg between European and Japanese. Based on the estimated means above, however, we know that the difference between American and foreign should be 9.087. Meanwhile, the difference between European and Japenese should be 2.559. 

The coefficients for the model above do not reflect the actual differences of interest. The $t$ statistic and $p$ value are nonetheless correct, so statements may be made about the statistical significance of the differences. The coefficients and their associated confidence intervals, however, are not useful for interpretation. Our contrast matrix was not _scaled_ correctly, resulting in improperly scaled coefficients but proper statistical tests and hypothesis decisions.

Finding interpretable Helmert contrasts requires that the three mpg means be directly obtainable from the model. That is, given the mean of the means as the intercept, a pair of contrast codes $c_1$ and $c_2$, and the differences of interest, we should be able to directly calculate the mpg means. Below is the model equation that calculates the American mean mpg, $\mu_A$

<div>$$
\begin{align*}
\mu_{A} & =\bar{\mu}+\left(\mu_{A}-\frac{\mu_{E}+\mu_{J}}{2}\right)c_{1}+\left(\mu_{E}-\mu_{J}\right)c_{2}\\
 & =\frac{\mu_{A}+\mu_{E}+\mu_{J}}{3}+\left(\mu_{A}-\frac{\mu_{E}+\mu_{J}}{2}\right)c_{1}+\left(\mu_{E}-\mu_{J}\right)c_{2}
\end{align*}
$$</div>

where $\mu_E$ and $\mu_J$ are the mean mpg for European and Japenese cars. The goal now is to find a $\{c_1, c_2\}$ pair of codes that correspond with American cars. Realizing that $\mu_E$ and $\mu_J$ must be eventually removed from the equation in order for it to be true, we are free to simplify the above to

<div>$$
\begin{align*}
\mu_{A} & =\mu_{A}\frac{1}{3}+\mu_{A}c_{1}\\
 & =\mu_{A}\left(\frac{1}{3}+c_{1}\right)\\
c_{1} & =\frac{2}{3}
\end{align*}
$$</div>

The first contrast code has been identified. Turning our attention to the second contrast code, recall that $\mu_E$ and $\mu_J$ must be somehow eliminated. Thus, the following must be true.

<div>$$
\begin{matrix}
0 =\mu_{E}\frac{1}{3}-\mu_{E}\frac{1}{2}c_{1}+\mu_{E}c_{2} \\
0 =\mu_{J}\frac{1}{3}-\mu_{J}\frac{1}{2}c_{1}+\mu_{J}c_{2}
\end{matrix}
$$</div>

Focusing on $\mu_E$ and susbtituing $c_1$ with $2 / 3$ as found above, we simplify the following to obtain $c_2$.


<div>$$
\begin{align*}
0 & =\mu_{E}\frac{1}{3}-\mu_{E}\frac{1}{2}c_{1}+\mu_{E}c_{2}\\
 & =\mu_{E}\frac{1}{3}-\mu_{E}\frac{1}{2}\left(\frac{2}{3}\right)+\mu_{E}c_{2}\\
 & =\mu_{E}\left(\frac{1}{3}-\frac{2}{6}+c_{2}\right)\\
 & =\mu_{E}\left(c_{2}\right)\\
c_{2} & =0
\end{align*}
$$</div>

We now have a contrast pair associated with $\mu_A$ that is properly scaled. 

<div>$$
\left\{ c_{1},c_{2}\right\} \text{ for }\mu_{A}=\left\{ \frac{2}{3},0\right\} 
$$</div>

Before with the improperly scaled contrast matrix, the pair was $\{2, 0\}$ but is now $\{2 / 3, 0\}$. A procedure like the one used can be used to find contrast pairs for European and Japenese cars that are also properly scaled. Of course, computers simplify this task. The `codingMatrices` package provides functions to make properly scaled contrast matrices. 


```r
# install.packages("codingMatrices")
library(codingMatrices)

contr <- code_helmert_forward(3)

colnames(contr) <- c(".American_vs_European", ".European_vs_Japanese")
rownames(contr) <- levels(d$origin)

print(contr)
##          .American_vs_European .European_vs_Japanese
## American             0.6666667                   0.0
## European            -0.3333333                   0.5
## Japanese            -0.3333333                  -0.5

fit_contr <- update(fit, contrasts = list(origin = contr))
coef(summary(fit_contr))
##                              Estimate Std. Error    t value      Pr(>|t|)
## (Intercept)                 26.141865  0.3753762  69.641783 6.292610e-224
## origin.American_vs_European -9.087497  0.6636408 -13.693397  3.371848e-35
## origin.European_vs_Japanese -2.559204  1.0505531  -2.436054  1.528964e-02

confint(fit_contr)
##                                  2.5 %     97.5 %
## (Intercept)                  25.403880 26.8798502
## origin.American_vs_European -10.392206 -7.7827869
## origin.European_vs_Japanese  -4.624579 -0.4938298
```

And now the coefficients have interpretational value. Furthermore, we have appropriate confidence intervals for the mean differences of interest.
