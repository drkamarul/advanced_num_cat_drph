---
title: "Practicals: Analysis of Count Data"
author: |
  | Kamarul Imran Musa
  | Assoc Prof (Epidemiology and Statistics
date: "2020-05-27"
output:
  html_document: 
    highlight: tango
    keep_md: yes
    number_sections: yes
    theme: united
    toc: yes
    toc_float: yes
  pdf_document:
    number_sections: yes
    toc: yes
    toc_depth: 3
---

\newpage



# Prepare environment

Load libraries


```r
library(tidyverse)
```

```
## -- Attaching packages ----------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.0.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts -------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(haven)
library(broom)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```


# Data

## Read data

Two datasets for practice


```r
publish <- read_stata('couart2.dta')
visit <- read_csv('NMES1988.csv') %>% clean_names()
```

```
## Parsed with column specification:
## cols(
##   visits = col_double(),
##   nvisits = col_double(),
##   ovisits = col_double(),
##   novisits = col_double(),
##   emergency = col_double(),
##   hospital = col_double(),
##   health = col_character(),
##   chronic = col_double(),
##   adl = col_character(),
##   region = col_character(),
##   age = col_double(),
##   afam = col_character(),
##   gender = col_character(),
##   married = col_character(),
##   school = col_double(),
##   income = col_double(),
##   employed = col_character(),
##   insurance = col_character(),
##   medicaid = col_character()
## )
```

## Transform data

Take a peek of data


```r
glimpse(publish)
```

```
## Rows: 915
## Columns: 6
## $ art  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ fem  <dbl+lbl> 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1...
## $ mar  <dbl+lbl> 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0...
## $ kid5 <dbl> 0, 0, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0,...
## $ phd  <dbl> 2.520, 2.050, 3.750, 1.180, 3.750, 3.590, 3.190, 2.960, 4.620,...
## $ ment <dbl> 7, 6, 6, 3, 26, 2, 3, 4, 6, 0, 14, 13, 3, 4, 0, 1, 7, 13, 7, 9...
```


For variables:

- fem
- mar


```r
publish <- publish %>% mutate_if(is.labelled, ~as_factor(.))
```


# Describe data

## Summarize data

Let us see summary of data


```r
summary(publish)
```

```
##       art            fem           mar           kid5             phd       
##  Min.   : 0.000   Men  :494   Single :309   Min.   :0.0000   Min.   :0.755  
##  1st Qu.: 0.000   Women:421   Married:606   1st Qu.:0.0000   1st Qu.:2.260  
##  Median : 1.000                             Median :0.0000   Median :3.150  
##  Mean   : 1.693                             Mean   :0.4951   Mean   :3.103  
##  3rd Qu.: 2.000                             3rd Qu.:1.0000   3rd Qu.:3.920  
##  Max.   :19.000                             Max.   :3.0000   Max.   :4.620  
##       ment       
##  Min.   : 0.000  
##  1st Qu.: 3.000  
##  Median : 6.000  
##  Mean   : 8.767  
##  3rd Qu.:12.000  
##  Max.   :77.000
```

The outcome variable is **art** : that is the number of publications produced by PhD biochemists. 

## Plots outcome variable


```r
ggplot(publish, aes(x = art)) + geom_histogram() 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](practicals_count_data_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


# Estimation

Let us assume the outcome follows the Poisson regression

- random observations
- independent observations 
- lineariry in parameters
- mean = variance 

## Constant only model

Let us estimate a constant model only. No covariates.


```r
mod_0 <- glm(art ~ 1, family = poisson(link = 'log'), 
             publish)
summary(mod_0)
```

```
## 
## Call:
## glm(formula = art ~ 1, family = poisson(link = "log"), data = publish)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8401  -1.8401  -0.5770   0.2294   7.5677  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.52644    0.02541   20.72   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 1817.4  on 914  degrees of freedom
## Residual deviance: 1817.4  on 914  degrees of freedom
## AIC: 3487.1
## 
## Number of Fisher Scoring iterations: 5
```

The estimated parameters; $\beta_0$ = 0.5264408, hence $\mu$ = 1.6928962 (The estimated mean of `art`). This equals the mean of `art` that is 1.6928962.

## Multivariable model


```r
mod_1 <- glm(art ~ fem + mar + kid5 + phd + ment, family = poisson(link = 'log'), 
             data = publish)
summary(mod_1)
```

```
## 
## Call:
## glm(formula = art ~ fem + mar + kid5 + phd + ment, family = poisson(link = "log"), 
##     data = publish)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5672  -1.5398  -0.3660   0.5722   5.4467  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.304617   0.102981   2.958   0.0031 ** 
## femWomen    -0.224594   0.054613  -4.112 3.92e-05 ***
## marMarried   0.155243   0.061374   2.529   0.0114 *  
## kid5        -0.184883   0.040127  -4.607 4.08e-06 ***
## phd          0.012823   0.026397   0.486   0.6271    
## ment         0.025543   0.002006  12.733  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 1817.4  on 914  degrees of freedom
## Residual deviance: 1634.4  on 909  degrees of freedom
## AIC: 3314.1
## 
## Number of Fisher Scoring iterations: 5
```

Or you can use `tidy()` for nicer output


```r
tidy(mod_1, conf.int = TRUE)
```

```
## # A tibble: 6 x 7
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)   0.305    0.103       2.96  3.10e- 3   0.102     0.505 
## 2 femWomen     -0.225    0.0546     -4.11  3.92e- 5  -0.332    -0.118 
## 3 marMarried    0.155    0.0614      2.53  1.14e- 2   0.0352    0.276 
## 4 kid5         -0.185    0.0401     -4.61  4.08e- 6  -0.264    -0.107 
## 5 phd           0.0128   0.0264      0.486 6.27e- 1  -0.0388    0.0647
## 6 ment          0.0255   0.00201    12.7   3.89e-37   0.0215    0.0294
```

## Interpretation

The expected log of count

- the number of articles for women was on average -0.225 the log of article number compared to men
- the number of articles for married scholar was on average 0.155 the log of article number compared to single scholars

The expected rate:

If we want to interpret using the rate, $\mu$, then for a unit change in $x_k$, the expected count changes by a factor of $exp^{\beta_k}$, holding other other variables constant.  


```r
tidy(mod_1, exponentiate = TRUE, conf.int = TRUE)
```

```
## # A tibble: 6 x 7
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)    1.36    0.103       2.96  3.10e- 3    1.11      1.66 
## 2 femWomen       0.799   0.0546     -4.11  3.92e- 5    0.718     0.889
## 3 marMarried     1.17    0.0614      2.53  1.14e- 2    1.04      1.32 
## 4 kid5           0.831   0.0401     -4.61  4.08e- 6    0.768     0.899
## 5 phd            1.01    0.0264      0.486 6.27e- 1    0.962     1.07 
## 6 ment           1.03    0.00201    12.7   3.89e-37    1.02      1.03
```

So, being a female scientist ($x_{fem}$) decreases the expected number of articles (**art**) by a factor of 0.7988403, holding other other variables constant.  

# Prediction

## Predicted log of count

The predicted log of articles for **art**


```r
augment(mod_1)
```

```
## # A tibble: 915 x 13
##      art fem   mar    kid5   phd  ment .fitted .se.fit .resid    .hat .sigma
##    <dbl> <fct> <fct> <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>   <dbl>  <dbl>
##  1     0 Men   Marr~     0  2.52     7  0.671   0.0495  -1.98 0.00480   1.34
##  2     0 Women Sing~     0  2.05     6  0.260   0.0600  -1.61 0.00467   1.34
##  3     0 Women Sing~     0  3.75     6  0.281   0.0535  -1.63 0.00379   1.34
##  4     0 Men   Marr~     1  1.18     3  0.367   0.0636  -1.70 0.00584   1.34
##  5     0 Women Sing~     0  3.75    26  0.792   0.0601  -2.10 0.00799   1.34
##  6     0 Women Marr~     2  3.59     2 -0.0374  0.0814  -1.39 0.00639   1.34
##  7     0 Women Sing~     0  3.19     3  0.198   0.0527  -1.56 0.00339   1.34
##  8     0 Men   Marr~     2  2.96     4  0.230   0.0653  -1.59 0.00536   1.34
##  9     0 Men   Sing~     0  4.62     6  0.517   0.0665  -1.83 0.00741   1.34
## 10     0 Women Marr~     0  1.25     0  0.251   0.0692  -1.60 0.00615   1.34
## # ... with 905 more rows, and 2 more variables: .cooksd <dbl>, .std.resid <dbl>
```

## Predicted count

The predicted count of **art** for the given covariates for each subject in the data 


```r
exp_count <- predict(mod_1, type = 'response')
head(exp_count)
```

```
##         1         2         3         4         5         6 
## 1.9561384 1.2963665 1.3249355 1.4430175 2.2082905 0.9633092
```

Manually, for the 1st subject, we calculate


```r
# The predicted log of count
0.30461683 - 0.22459423*0 +  0.15524338*1 - 0.18488270*0 + 0.01282258*2.52 + 0.02554275*7
```

```
## [1] 0.6709724
```

```r
# The expected (average) count of art 
exp(0.6709724)
```

```
## [1] 1.956139
```

# References

1.  Long J S, Freese J. Regression Models for Categorical Dependent Variables Using Stata
2. https://cran.r-project.org/web/packages/AER/AER.pdf 

