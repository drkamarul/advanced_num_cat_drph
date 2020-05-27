---
title: |
  | Practicals
  | Logistic Regression for Data with Ordinal Outcome
author: |
  | Kamarul Imran Musa
  | Associate Professor (Epidemiology and Statistics)
date: '2020-05-27'
output:
  html_document: 
    keep_md: yes
    number_sections: yes
    theme: lumen
    toc: yes
    toc_float: yes
  pdf_document:
    number_sections: yes
    toc: yes
    toc_depth: 3
---

\newpage

# Tutorial 1

## Introduction to cumulative link models (Proportional Odds Models) 

First, we will use 'ordinal' package created by Rune Haubo B Christensen. The tutorial can be found here https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf 
Cumulative link models (CLMs) are a powerful model class for such data since observations are treated correctly as categorical, the ordered nature is exploited and the flexible regression framework allows for in-depth analyses

In R, several packages on CRAN implements CLMs. 

- *polr* from **MASS** implements standard CLMs allowing for the 5 standard link functions but no further extensions
- the **VGAM** package (Yee 2010) includes CLMs via the vglm function using the cumulative link.
- the *lrm* and *orm* functions from the **rms** package also implements CLMs with the 5 standard link functions but without scale effects, partial or structured thresholds.

**Note**: On the page 7 in the document, it shows how to test for proportional odds assumption. Read on your own. 

For this part, we will use the *Cumulative Link Models* also known as *Proportional Odds Models* which requires **ordinal** package. Download and install the package if you have not done so.

## Preparation for analysis 

Load 

- ordinal: to use the wine dataset for tutorial
- tidyverse: for data wrangling
- broom: to generate more pleasing results


```r
library(ordinal) # to perform model 
library(tidyverse) # data wrangling
```

```
## -- Attaching packages ---------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.0.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## x dplyr::slice()  masks ordinal::slice()
```

```r
library(haven)
library(broom)
```

## Read data

A dataset named **wine** from **ordinal** package will be used. 

Below:

- We will convert the file to an object of tibble and named it as `data.o`
- Next, we examine the data briefly
- And lastly, summarize the data 


```r
dat.o <- read_dta('wine.dta')
glimpse(dat.o)
```

```
## Rows: 72
## Columns: 6
## $ response <dbl> 36, 48, 47, 67, 77, 60, 83, 90, 17, 22, 14, 50, 30, 51, 90...
## $ rating   <dbl+lbl> 2, 3, 3, 4, 4, 4, 5, 5, 1, 2, 1, 3, 2, 3, 5, 4, 2, 3, ...
## $ temp     <dbl+lbl> 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, ...
## $ contact  <dbl+lbl> 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, ...
## $ bottle   <dbl+lbl> 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, ...
## $ judge    <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, ...
```

```r
summary(dat.o)
```

```
##     response         rating           temp        contact        bottle    
##  Min.   :12.00   Min.   :1.000   Min.   :1.0   Min.   :1.0   Min.   :1.00  
##  1st Qu.:32.00   1st Qu.:2.000   1st Qu.:1.0   1st Qu.:1.0   1st Qu.:2.75  
##  Median :46.00   Median :3.000   Median :1.5   Median :1.5   Median :4.50  
##  Mean   :47.22   Mean   :2.917   Mean   :1.5   Mean   :1.5   Mean   :4.50  
##  3rd Qu.:60.00   3rd Qu.:4.000   3rd Qu.:2.0   3rd Qu.:2.0   3rd Qu.:6.25  
##  Max.   :90.00   Max.   :5.000   Max.   :2.0   Max.   :2.0   Max.   :8.00  
##      judge  
##  Min.   :1  
##  1st Qu.:3  
##  Median :5  
##  Mean   :5  
##  3rd Qu.:7  
##  Max.   :9
```

The outcome variable is `rating` from rating 1 (least bitter) to 5 (most bitter).

We will convert labelled variables to factors variables


```r
dat.o <- dat.o %>%
  mutate_if(is.labelled, as_factor)
str(dat.o)
```

```
## tibble [72 x 6] (S3: tbl_df/tbl/data.frame)
##  $ response: num [1:72] 36 48 47 67 77 60 83 90 17 22 ...
##   ..- attr(*, "format.stata")= chr "%10.0g"
##  $ rating  : Factor w/ 5 levels "1","2","3","4",..: 2 3 3 4 4 4 5 5 1 2 ...
##  $ temp    : Factor w/ 2 levels "cold","warm": 1 1 1 1 2 2 2 2 1 1 ...
##  $ contact : Factor w/ 2 levels "no","yes": 1 1 2 2 1 1 2 2 1 1 ...
##  $ bottle  : Factor w/ 8 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 1 2 ...
##  $ judge   : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 2 2 ...
```


## Estimation

Next, we will estimate the model using *the proportional odds model* or the *cumulative link model*.

In ordinal package we can use the function clm to run the maximum likelihood estimates.

Let's run a model of:

logit$$P(Y_i \leq j) = \theta_j - \beta_1(TEMP) - \beta_2(CONTACT)$$
$i = 1, ... , n$
$j = 1, ... , J - 1$

Now run the *clm()* function to estimate:

1.  regression coefficients 
2.  standard errors 
3.  Wald based p-values (testing the parameters against zero)


```r
o1 <- clm(rating ~ temp + contact, data = dat.o)
summary(o1)
```

```
## formula: rating ~ temp + contact
## data:    dat.o
## 
##  link  threshold nobs logLik AIC    niter max.grad cond.H 
##  logit flexible  72   -86.49 184.98 6(0)  4.02e-12 2.7e+01
## 
## Coefficients:
##            Estimate Std. Error z value Pr(>|z|)    
## tempwarm     2.5031     0.5287   4.735 2.19e-06 ***
## contactyes   1.5278     0.4766   3.205  0.00135 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##     Estimate Std. Error z value
## 1|2  -1.3444     0.5171  -2.600
## 2|3   1.2508     0.4379   2.857
## 3|4   3.4669     0.5978   5.800
## 4|5   5.0064     0.7309   6.850
```

Nice output


```r
tidy(o1)
```

```
## # A tibble: 6 x 6
##   term       estimate std.error statistic  p.value coefficient_type
##   <chr>         <dbl>     <dbl>     <dbl>    <dbl> <chr>           
## 1 1|2           -1.34     0.517     -2.60 9.33e- 3 alpha           
## 2 2|3            1.25     0.438      2.86 4.28e- 3 alpha           
## 3 3|4            3.47     0.598      5.80 6.64e- 9 alpha           
## 4 4|5            5.01     0.731      6.85 7.41e-12 alpha           
## 5 tempwarm       2.50     0.529      4.73 2.19e- 6 beta            
## 6 contactyes     1.53     0.477      3.21 1.35e- 3 beta
```

And STATA also shows similar results for beta coefficents:

![Ordinal logistic regression from STATA](stata_res.PNG)

Variable temp and contact are positive in values. This means

1.  warm temp associates with higher categories of outcome (more bitter wine)
2.  present of contact associates with higher categories of outcome (more bitter wine)
3. The odds ratio of bitterness being rated in category $j$ or above is $exp(\hat{\beta}_2(yes - no)) = 4.61)$ 

Note that there are no p values displayed for the threshold coefficients because it usually does not make sense to test if they equal zero.

Below, the results show the 

1.  the estimated regression coefficients and $95\%$ CI
2.  the estimates the odds ratio and the $95\%$ CI


```r
tidy(o1, conf.int = TRUE)
```

```
## # A tibble: 6 x 8
##   term  estimate std.error statistic  p.value conf.low conf.high
##   <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 1|2      -1.34     0.517     -2.60 9.33e- 3   NA         NA   
## 2 2|3       1.25     0.438      2.86 4.28e- 3   NA         NA   
## 3 3|4       3.47     0.598      5.80 6.64e- 9   NA         NA   
## 4 4|5       5.01     0.731      6.85 7.41e-12   NA         NA   
## 5 cont~     1.53     0.477      3.21 1.35e- 3    0.616      2.49
## 6 temp~     2.50     0.529      4.73 2.19e- 6    1.51       3.60
## # ... with 1 more variable: coefficient_type <chr>
```

```r
tidy(o1, exponentiate = TRUE, conf.int = TRUE)
```

```
## # A tibble: 6 x 8
##   term  estimate std.error statistic  p.value conf.low conf.high
##   <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 1|2      0.261     0.517     -2.60 9.33e- 3    NA         NA  
## 2 2|3      3.49      0.438      2.86 4.28e- 3    NA         NA  
## 3 3|4     32.0       0.598      5.80 6.64e- 9    NA         NA  
## 4 4|5    149.        0.731      6.85 7.41e-12    NA         NA  
## 5 cont~    4.61      0.477      3.21 1.35e- 3     1.85      12.1
## 6 temp~   12.2       0.529      4.73 2.19e- 6     4.53      36.4
## # ... with 1 more variable: coefficient_type <chr>
```

This tells you that:

1.  at warm temperature, the **odds** of bitterness being rated in category *j* or above is 12.22 higher (more bitter) than at cold temperature, when adjusted with CONTACT.
2.  when controlled for TEMP, the CONTACT in the yes category has 4.6 times higher odds of bitterness being rated in category *j* or above (more bitter) then the CONTACT in the no category. 

## Inferences

Perform inferences by:

1.  checking the Wald-based p-values (testing parameters agains 0)
2.  calculating the confidence interval (likelihood based confidence interval)
3. comparison of model using the likelihood ratio test

For example:


```r
o1b <- clm(rating ~ temp, data = dat.o)
anova(o1b, o1, test = 'Chisq')
```

```
## 'test' argument ignored in anova.clm
```

```
## Likelihood ratio tests of cumulative link models:
##  
##     formula:                link: threshold:
## o1b rating ~ temp           logit flexible  
## o1  rating ~ temp + contact logit flexible  
## 
##     no.par    AIC  logLik LR.stat df Pr(>Chisq)    
## o1b      5 194.03 -92.013                          
## o1       6 184.98 -86.492  11.043  1  0.0008902 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## Prediction

To predict the probabilities (*fitted probabilities*) that the *i-th* observation falls in a particular response category, we can use the function `fitted()`.   

Let us make a prediction using the wine dataset. You have to omit variable rating. This is because it an outcome variable.

Note: if you have problem with the function `select()`, then make sure to detach MASS package


```r
#detach("package:MASS", unload = TRUE)
new.data <- dat.o %>% select(-rating) 
prob.o1 <- predict(o1, newdata = new.data)
head(prob.o1$fit) ; tail(prob.o1$fit)
```

```
##            1         2         3          4          5
## 1 0.20679013 0.5706497 0.1922909 0.02361882 0.00665041
## 2 0.20679013 0.5706497 0.1922909 0.02361882 0.00665041
## 3 0.05354601 0.3776461 0.4430599 0.09582084 0.02992711
## 4 0.05354601 0.3776461 0.4430599 0.09582084 0.02992711
## 5 0.02088771 0.2014157 0.5015755 0.20049402 0.07562701
## 6 0.02088771 0.2014157 0.5015755 0.20049402 0.07562701
```

```
##              1          2         3          4          5
## 67 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 68 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 69 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 70 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 71 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
## 72 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
```

And to predict the most likely that the *i-th* observation falls in each of the response category is by using the `predict(model, type = 'class')`: 


```r
class.01 <- predict(o1, type = 'class')
head(class.01)
```

```
## $fit
##  [1] 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3
## [39] 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4 2 2 3 3 3 3 4 4
## Levels: 1 2 3 4 5
```


Another way of making prediction:

To predict the probabilities of i-th falls into each category for model for a new data, we need to create a new data first. We will use `expand.grid()` to create a new data of class `data.frame`

This is the new data


```r
newData <- expand.grid(temp = levels(dat.o$temp), 
                       contact = levels(dat.o$contact))
newData
```

```
##   temp contact
## 1 cold      no
## 2 warm      no
## 3 cold     yes
## 4 warm     yes
```

Now, we will predict the probabilies for the new data. The predicted probabilities are the probabilities that the subjects fall into each category of the outcome.

Remember the categories of the outcome are:


```r
levels(dat.o$rating)
```

```
## [1] "1" "2" "3" "4" "5"
```

The linear predictor and predicted probabilites for these categories:

1.  temp = cold, contact = no
2.  temp = warm, contact = no
3.  temp = cold, contact = yes
4.  temp = warm, contact = yes


```r
lp.newData <- predict(o1, newdata = newData, type = 'linear.predictor')
prob.newData <- predict(o1, newdata = newData, type = 'prob')
lp.newData ; prob.newData
```

```
## $eta1
##           1          2          3         4         5
## 1 -1.344383  1.2508088  3.4668869 5.0064042 100000.00
## 2 -3.847485 -1.2522932  0.9637849 2.5033022  99997.50
## 3 -2.872181 -0.2769889  1.9390893 3.4786065  99998.47
## 4 -5.375283 -2.7800909 -0.5640127 0.9755045  99995.97
## 
## $eta2
##           1         2          3          4         5
## 1 -100000.0 -1.344383  1.2508088  3.4668869 5.0064042
## 2 -100002.5 -3.847485 -1.2522932  0.9637849 2.5033022
## 3 -100001.5 -2.872181 -0.2769889  1.9390893 3.4786065
## 4 -100004.0 -5.375283 -2.7800909 -0.5640127 0.9755045
```

```
## $fit
##             1          2         3          4          5
## 1 0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
## 2 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 3 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 4 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
```

We can use `cbind()` to make a better results.


```r
cbind(newData, prob.newData)
```

```
##   temp contact       fit.1      fit.2     fit.3      fit.4      fit.5
## 1 cold      no 0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
## 2 warm      no 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 3 cold     yes 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 4 warm     yes 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
```


## Checking proportional odds assumption

Let us check if variable CONTACT (which is a numerical var) fulfills the proportional odds assumptions. 

There are a few ways:

First, we run the function `ordinal::nominal_test()`


```r
nominal_test(o1)
```

```
## Tests of nominal effects
## 
## formula: rating ~ temp + contact
##         Df  logLik    AIC    LRT Pr(>Chi)
## <none>     -86.492 184.98                
## temp     3 -84.904 187.81 3.1750   0.3654
## contact  3 -86.209 190.42 0.5667   0.9040
```

The funtion test ordinality one by one covariate. In this example, both temp and contact show that the assumpmtion of proporional odds can not be rejected $p>0.05$. 

Second way:

1.  we will create a model by running a `clm()` function 
2.  then compare the new model (from step 1) with the model estimated using `ordinal()` function earlier in this tutorial. The comparison will be done using the `anova()` function

Specify the numerical variable (CONTACT) in the `clm()` function to estimate the new model


```r
o1.nominal <- clm(rating ~ temp, nominal = ~contact, data = dat.o)
```

then compare the two models using `anova()`


```r
anova(o1, o1.nominal)
```

```
## Likelihood ratio tests of cumulative link models:
##  
##            formula:                nominal: link: threshold:
## o1         rating ~ temp + contact ~1       logit flexible  
## o1.nominal rating ~ temp           ~contact logit flexible  
## 
##            no.par    AIC  logLik LR.stat df Pr(>Chisq)
## o1              6 184.98 -86.492                      
## o1.nominal      9 190.42 -86.209  0.5667  3      0.904
```

The p-value is not significant at $5\%$ level (p = 0.904), hence the proportionality does present for variable CONTACT.

## Prediction 

### Predicted probability

We will discuss two packages:

- ordinal package and 
- polr package

In MASS::polr package


```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
m_polr <- polr(rating ~ temp + contact, data = dat.o)
summary(m_polr)
```

```
## 
## Re-fitting to get Hessian
```

```
## Call:
## polr(formula = rating ~ temp + contact, data = dat.o)
## 
## Coefficients:
##            Value Std. Error t value
## tempwarm   2.503     0.5287   4.735
## contactyes 1.528     0.4766   3.205
## 
## Intercepts:
##     Value   Std. Error t value
## 1|2 -1.3444  0.5171    -2.5998
## 2|3  1.2508  0.4379     2.8565
## 3|4  3.4669  0.5978     5.7998
## 4|5  5.0064  0.7309     6.8496
## 
## Residual Deviance: 172.9838 
## AIC: 184.9838
```

The the probabilities are:


```r
prob_polr <- predict(m_polr, type = 'probs')
head(prob_polr) ; tail(prob_polr)
```

```
##           1         2         3          4           5
## 1 0.2067917 0.5706466 0.1922920 0.02361917 0.006650532
## 2 0.2067917 0.5706466 0.1922920 0.02361917 0.006650532
## 3 0.0535471 0.3776458 0.4430586 0.09582112 0.029927296
## 4 0.0535471 0.3776458 0.4430586 0.09582112 0.029927296
## 5 0.0208885 0.2014185 0.5015746 0.20049218 0.075626257
## 6 0.0208885 0.2014185 0.5015746 0.20049218 0.075626257
```

```
##              1          2         3          4          5
## 67 0.053547101 0.37764585 0.4430586 0.09582112 0.02992730
## 68 0.053547101 0.37764585 0.4430586 0.09582112 0.02992730
## 69 0.020888502 0.20141847 0.5015746 0.20049218 0.07562626
## 70 0.020888502 0.20141847 0.5015746 0.20049218 0.07562626
## 71 0.004608507 0.05380284 0.3042139 0.36359456 0.27378016
## 72 0.004608507 0.05380284 0.3042139 0.36359456 0.27378016
```


### Manual calculation for prediction

Excellent discussion <https://stats.stackexchange.com/questions/41006/predicting-ordered-logit-in-r>

$$\text{logit}(p(Y \leqslant g)) = \ln \frac{p(Y \leqslant g)}{p(Y > g)} = \beta_{0_g} - (\beta_{1} X_{1} + \dots + \beta_{p} X_{p})$$

$$\hat{p}(Y \leqslant g) = \frac{e^{\hat{\beta}_{0_{g}} - (\hat{\beta}_{1} X_{1} + \dots + \hat{\beta}_{p} X_{p})}}{1 + e^{\hat{\beta}_{0_{g}} - (\hat{\beta}_{1} X_{1} + \dots + \hat{\beta}_{p} X_{p})}}$$

Let us go back to the model 


```r
summary(o1) 
```

```
## formula: rating ~ temp + contact
## data:    dat.o
## 
##  link  threshold nobs logLik AIC    niter max.grad cond.H 
##  logit flexible  72   -86.49 184.98 6(0)  4.02e-12 2.7e+01
## 
## Coefficients:
##            Estimate Std. Error z value Pr(>|z|)    
## tempwarm     2.5031     0.5287   4.735 2.19e-06 ***
## contactyes   1.5278     0.4766   3.205  0.00135 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##     Estimate Std. Error z value
## 1|2  -1.3444     0.5171  -2.600
## 2|3   1.2508     0.4379   2.857
## 3|4   3.4669     0.5978   5.800
## 4|5   5.0064     0.7309   6.850
```

And we want to predict these data


```r
newData
```

```
##   temp contact
## 1 cold      no
## 2 warm      no
## 3 cold     yes
## 4 warm     yes
```


And now the predictions


```r
lp.o1 <- predict(o1, newdata = newData, type = 'linear.predictor')
lp.o1
```

```
## $eta1
##           1          2          3         4         5
## 1 -1.344383  1.2508088  3.4668869 5.0064042 100000.00
## 2 -3.847485 -1.2522932  0.9637849 2.5033022  99997.50
## 3 -2.872181 -0.2769889  1.9390893 3.4786065  99998.47
## 4 -5.375283 -2.7800909 -0.5640127 0.9755045  99995.97
## 
## $eta2
##           1         2          3          4         5
## 1 -100000.0 -1.344383  1.2508088  3.4668869 5.0064042
## 2 -100002.5 -3.847485 -1.2522932  0.9637849 2.5033022
## 3 -100001.5 -2.872181 -0.2769889  1.9390893 3.4786065
## 4 -100004.0 -5.375283 -2.7800909 -0.5640127 0.9755045
```

The coefficients for model o.1


```r
coef.o1 <- coef(o1)
coef.o1
```

```
##        1|2        2|3        3|4        4|5   tempwarm contactyes 
##  -1.344383   1.250809   3.466887   5.006404   2.503102   1.527798
```

Putting them inside the equation


```r
lp.o1.bx = coef.o1[5]*0 + coef.o1[6]*0
```


And complete the Eq 8.25 in Hosmer


```r
logit1 <- coef.o1[1] - lp.o1.bx
logit2 <- coef.o1[2] - lp.o1.bx
logit3 <- coef.o1[3] - lp.o1.bx
logit4 <- coef.o1[4] - lp.o1.bx
logit1 ; logit2 ; logit3 ; logit4
```

```
##       1|2 
## -1.344383
```

```
##      2|3 
## 1.250809
```

```
##      3|4 
## 3.466887
```

```
##      4|5 
## 5.006404
```

Then the probabilities are


```r
pLeq1  <- 1 / (1 + exp(-logit1))   # p(Y <= SD)
pLeq2  <- 1 / (1 + exp(-logit2))   # p(Y <= D)
pLeq3  <- 1 / (1 + exp(-logit3))   # p(Y <= A)
pLeq4  <- 1 / (1 + exp(-logit4))   # p(Y <= A)

pMat   <- cbind(p1 = pLeq1, p2 = pLeq2 - pLeq1, p3 = pLeq3 - pLeq2, 
                p4 = pLeq4 - pLeq3, p5 = 1 - pLeq4)
pMat
```

```
##            p1        p2        p3         p4         p5
## 1|2 0.2067901 0.5706497 0.1922909 0.02361882 0.00665041
```

Let us confirm with the prediction made by clm


```r
predict(o1, newdata = newData, type = 'prob')
```

```
## $fit
##             1          2         3          4          5
## 1 0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
## 2 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 3 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 4 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
```

For the forth observation:

Putting them inside the equation


```r
lp.o1.bx4 = coef.o1[5]*1 + coef.o1[6]*1
```

And complete the Eq 8.25 in Hosmer


```r
logit1.4 <- coef.o1[1] - lp.o1.bx4
logit2.4 <- coef.o1[2] - lp.o1.bx4
logit3.4 <- coef.o1[3] - lp.o1.bx4
logit4.4 <- coef.o1[4] - lp.o1.bx4
logit1.4 ; logit2.4 ; logit3.4 ; logit4.4
```

```
##       1|2 
## -5.375283
```

```
##       2|3 
## -2.780091
```

```
##        3|4 
## -0.5640127
```

```
##       4|5 
## 0.9755045
```

Then the probabilities are


```r
pLeq1.4  <- 1 / (1 + exp(-logit1.4))   
pLeq2.4  <- 1 / (1 + exp(-logit2.4))   
pLeq3.4  <- 1 / (1 + exp(-logit3.4))   
pLeq4.4  <- 1 / (1 + exp(-logit4.4))   

pMat.4   <- cbind(p1.4 = pLeq1.4, p2.4 = pLeq2.4 - pLeq1.4, 
                  p3.4 = pLeq3.4 - pLeq2.4, p4.4 = pLeq4.4 - pLeq3.4, 
                  p5.4 = 1 - pLeq4.4)
pMat.4
```

```
##            p1.4       p2.4      p3.4      p4.4      p5.4
## 1|2 0.004608274 0.05380128 0.3042099 0.3635958 0.2737847
```

Let us confirm with the prediction made by clm


```r
predict(o1, newdata = newData, type = 'prob')
```

```
## $fit
##             1          2         3          4          5
## 1 0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
## 2 0.020887709 0.20141572 0.5015755 0.20049402 0.07562701
## 3 0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
## 4 0.004608274 0.05380128 0.3042099 0.36359581 0.27378469
```

# Tutorial 2

## Introduction dataset and models

This example comes from Applied Logistic Regression book. In the book, it describes 3 models:

- Adjacent category (from multinomial or baseline logit) models
- Continuation ratio models
- Proportional odds models

The adjacent-category logistic compares each response to the next larger response. The continuation ratio model compares each response to ALL lower response ($ Y = k$ vs $Y < k $). The proportional odds model compares the probability of an equal or smaller response to a larger response $Y \leq k$ vs $Y > k$. 

## Preparation for data analysis

You can load the **foreign** package to read the `.dta` STATA file. Read the stata file named 'lowbwt.dta'. Summarise all the variables.


```r
library(tidyverse)
library(haven)
data.o3 <- read_dta('lowbwt.dta')
```

Get the overview of data and their summary statistics


```r
summary(data.o3)
```

```
##        id             lbw              age             lwt       
##  Min.   :  4.0   Min.   :0.0000   Min.   :14.00   Min.   : 80.0  
##  1st Qu.: 68.0   1st Qu.:0.0000   1st Qu.:19.00   1st Qu.:110.0  
##  Median :123.0   Median :0.0000   Median :23.00   Median :121.0  
##  Mean   :121.1   Mean   :0.3122   Mean   :23.24   Mean   :129.8  
##  3rd Qu.:176.0   3rd Qu.:1.0000   3rd Qu.:26.00   3rd Qu.:140.0  
##  Max.   :226.0   Max.   :1.0000   Max.   :45.00   Max.   :250.0  
##       race           smoke             ptl             hyper        
##  Min.   :1.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
##  Median :1.000   Median :0.0000   Median :0.0000   Median :0.00000  
##  Mean   :1.847   Mean   :0.3915   Mean   :0.1958   Mean   :0.06349  
##  3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
##  Max.   :3.000   Max.   :1.0000   Max.   :3.0000   Max.   :1.00000  
##      urirr             pvft            weight         agecat     
##  Min.   :0.0000   Min.   :0.0000   Min.   : 709   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:2414   1st Qu.:1.000  
##  Median :0.0000   Median :0.0000   Median :2977   Median :2.000  
##  Mean   :0.1481   Mean   :0.7937   Mean   :2945   Mean   :2.238  
##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:3475   3rd Qu.:3.000  
##  Max.   :1.0000   Max.   :6.0000   Max.   :4990   Max.   :4.000  
##       wcat           anyptl          newpvft      
##  Min.   :1.000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :3.000   Median :0.0000   Median :0.0000  
##  Mean   :2.857   Mean   :0.1587   Mean   :0.6931  
##  3rd Qu.:4.000   3rd Qu.:0.0000   3rd Qu.:1.0000  
##  Max.   :5.000   Max.   :1.0000   Max.   :2.0000
```

```r
glimpse(data.o3)
```

```
## Rows: 189
## Columns: 15
## $ id      <dbl> 85, 86, 87, 88, 89, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100...
## $ lbw     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ age     <dbl> 19, 33, 20, 21, 18, 21, 22, 17, 29, 26, 19, 19, 22, 30, 18,...
## $ lwt     <dbl> 182, 155, 105, 108, 107, 124, 118, 103, 123, 113, 95, 150, ...
## $ race    <dbl> 2, 3, 1, 1, 1, 3, 1, 3, 1, 1, 3, 3, 3, 3, 1, 1, 2, 1, 3, 1,...
## $ smoke   <dbl> 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1,...
## $ ptl     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,...
## $ hyper   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,...
## $ urirr   <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,...
## $ pvft    <dbl> 0, 3, 1, 2, 0, 0, 1, 1, 1, 0, 0, 1, 0, 2, 0, 0, 0, 3, 0, 1,...
## $ weight  <dbl> 2523, 2551, 2557, 2594, 2600, 2622, 2637, 2637, 2663, 2665,...
## $ agecat  <dbl+lbl> 1, 4, 2, 2, 1, 2, 2, 1, 3, 3, 1, 1, 2, 4, 1, 1, 1, 3, 2...
## $ wcat    <dbl+lbl> 5, 5, 1, 2, 2, 3, 2, 1, 3, 2, 1, 4, 1, 2, 1, 1, 1, 2, 2...
## $ anyptl  <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0...
## $ newpvft <dbl+lbl> 0, 2, 1, 2, 0, 0, 1, 1, 1, 0, 0, 1, 0, 2, 0, 0, 0, 2, 0...
```

```r
str(data.o3)
```

```
## tibble [189 x 15] (S3: tbl_df/tbl/data.frame)
##  $ id     : num [1:189] 85 86 87 88 89 91 92 93 94 95 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ lbw    : num [1:189] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ age    : num [1:189] 19 33 20 21 18 21 22 17 29 26 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ lwt    : num [1:189] 182 155 105 108 107 124 118 103 123 113 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ race   : num [1:189] 2 3 1 1 1 3 1 3 1 1 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ smoke  : num [1:189] 0 0 1 1 1 0 0 0 1 1 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ ptl    : num [1:189] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ hyper  : num [1:189] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ urirr  : num [1:189] 1 0 0 1 1 0 0 0 0 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ pvft   : num [1:189] 0 3 1 2 0 0 1 1 1 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ weight : num [1:189] 2523 2551 2557 2594 2600 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##  $ agecat : 'haven_labelled' num [1:189] 1 4 2 2 1 2 2 1 3 3 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
##   .. ..- attr(*, "names")= chr [1:4] "<20" "20-24" "25-29" "30+"
##  $ wcat   : 'haven_labelled' num [1:189] 5 5 1 2 2 3 2 1 3 2 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
##   .. ..- attr(*, "names")= chr [1:5] "<105" "106-120" "121-130" "131-150" ...
##  $ anyptl : 'haven_labelled' num [1:189] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##   ..- attr(*, "labels")= Named num [1:2] 0 1
##   .. ..- attr(*, "names")= chr [1:2] "0" "1+"
##  $ newpvft: 'haven_labelled' num [1:189] 0 2 1 2 0 0 1 1 1 0 ...
##   ..- attr(*, "format.stata")= chr "%9.0g"
##   ..- attr(*, "labels")= Named num [1:3] 0 1 2
##   .. ..- attr(*, "names")= chr [1:3] "0" "1" "2+"
```

Data comes will labelled variable.

We can convert those variables to factor variables:


```r
data.o3 <- data.o3 %>% mutate_if(is.labelled, 
                                 as_factor)
summary(data.o3)
```

```
##        id             lbw              age             lwt       
##  Min.   :  4.0   Min.   :0.0000   Min.   :14.00   Min.   : 80.0  
##  1st Qu.: 68.0   1st Qu.:0.0000   1st Qu.:19.00   1st Qu.:110.0  
##  Median :123.0   Median :0.0000   Median :23.00   Median :121.0  
##  Mean   :121.1   Mean   :0.3122   Mean   :23.24   Mean   :129.8  
##  3rd Qu.:176.0   3rd Qu.:1.0000   3rd Qu.:26.00   3rd Qu.:140.0  
##  Max.   :226.0   Max.   :1.0000   Max.   :45.00   Max.   :250.0  
##       race           smoke             ptl             hyper        
##  Min.   :1.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
##  Median :1.000   Median :0.0000   Median :0.0000   Median :0.00000  
##  Mean   :1.847   Mean   :0.3915   Mean   :0.1958   Mean   :0.06349  
##  3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
##  Max.   :3.000   Max.   :1.0000   Max.   :3.0000   Max.   :1.00000  
##      urirr             pvft            weight       agecat        wcat   
##  Min.   :0.0000   Min.   :0.0000   Min.   : 709   <20  :51   <105   :37  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:2414   20-24:69   106-120:55  
##  Median :0.0000   Median :0.0000   Median :2977   25-29:42   121-130:31  
##  Mean   :0.1481   Mean   :0.7937   Mean   :2945   30+  :27   131-150:30  
##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:3475              151+   :36  
##  Max.   :1.0000   Max.   :6.0000   Max.   :4990                          
##  anyptl   newpvft 
##  0 :159   0 :100  
##  1+: 30   1 : 47  
##           2+: 42  
##                   
##                   
## 
```

We will use `cut()` function to create a categorical (factor) outcome variable. Variable WEIGHT will be categorized into a 4 category variable BWT4. 

Then we use the `label()` function to label the variable.



```r
detach("package:MASS", unload = TRUE)
```

```
## Warning: 'MASS' namespace cannot be unloaded:
##   namespace 'MASS' is imported by 'ordinal' so cannot be unloaded
```

```r
data.o3 <- data.o3 %>% 
  mutate(bwt4 = 
           cut(weight, breaks = c(708,2500, 2999, 3500,
                                  max(weight)),
               labels =
                 c('=<2500','2501-3000','3001-3500','>3500')))
```

Let us check if we have grouped correctly:


```r
data.o3 %>% select(bwt4, weight) %>%
  group_by(bwt4) %>% summarize_at(vars(weight), 
                                  c(min = min, max = max))
```

```
## # A tibble: 4 x 3
##   bwt4        min   max
##   <fct>     <dbl> <dbl>
## 1 =<2500      709  2495
## 2 2501-3000  2523  2992
## 3 3001-3500  3005  3487
## 4 >3500      3544  4990
```


Next, we will used ordered function to create an ordinal variable and define the levels of the variable. 

We will reverse the order of the outcome variable to reproduce results in the book.


```r
lev <- c('>3500','3001-3500','2501-3000','=<2500')
lev
```

```
## [1] ">3500"     "3001-3500" "2501-3000" "=<2500"
```

```r
data.o3 <- data.o3 %>% 
  mutate(bwt4a = fct_relevel(bwt4, lev)) %>%
  mutate(bwt4a = ordered(bwt4a, levels = lev))
```

Let us check if we have done correctly:


```r
str(data.o3$bwt4a)
```

```
##  Ord.factor w/ 4 levels ">3500"<"3001-3500"<..: 3 3 3 3 3 3 3 3 3 3 ...
```

```r
levels(data.o3$bwt4)
```

```
## [1] "=<2500"    "2501-3000" "3001-3500" ">3500"
```

```r
levels(data.o3$bwt4a)
```

```
## [1] ">3500"     "3001-3500" "2501-3000" "=<2500"
```

## Estimation

### Adjacent-category model or multinomial or baseline logit model

Here, we will show how to replicate the baseline logit model (unconstrained) in the Hosmer book (page 294). We could not reproduce the adjancent-category models that is based on constrained logit models.

Generate a new variable but with no ordering


```r
data.o3 <- data.o3 %>% 
  mutate(bwt4b = fct_relevel(bwt4, lev))
```

Run the mlogit and we get similar results to Table 8.14 ALR. I am not sure how to estimate the constrained logit model. 


```r
library(VGAM)
```

```
## Loading required package: stats4
```

```
## Loading required package: splines
```

```
## 
## Attaching package: 'VGAM'
```

```
## The following object is masked from 'package:tidyr':
## 
##     fill
```

```
## The following objects are masked from 'package:ordinal':
## 
##     dgumbel, dlgamma, pgumbel, plgamma, qgumbel, rgumbel, wine
```

```r
table8.16 <- vglm(bwt4 ~ smoke, multinomial, data = data.o3)
summary(table8.16)
```

```
## 
## Call:
## vglm(formula = bwt4 ~ smoke, family = multinomial, data = data.o3)
## 
## Pearson residuals:
##                       Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,4]) -1.535 -0.5301 -0.3490  1.1401 1.671
## log(mu[,2]/mu[,4]) -1.287 -0.3527 -0.2648 -0.2648 1.996
## log(mu[,3]/mu[,4]) -1.309 -0.3747 -0.3202 -0.2947 1.720
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)   
## (Intercept):1  -0.1881     0.2511  -0.749  0.45392   
## (Intercept):2  -0.4643     0.2721  -1.707  0.08791 . 
## (Intercept):3  -0.1881     0.2511  -0.749  0.45392   
## smoke:1         1.1914     0.4328   2.753  0.00591 **
## smoke:2         0.8390     0.4769   1.759  0.07853 . 
## smoke:3         0.6234     0.4613   1.351  0.17658   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,4]), log(mu[,2]/mu[,4]), 
## log(mu[,3]/mu[,4])
## 
## Residual deviance: 510.9718 on 561 degrees of freedom
## 
## Log-likelihood: -255.4859 on 561 degrees of freedom
## 
## Number of Fisher scoring iterations: 4 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  4  of the response
```

And get the RRR


```r
exp(coef(table8.16))
```

```
## (Intercept):1 (Intercept):2 (Intercept):3       smoke:1       smoke:2 
##     0.8285714     0.6285714     0.8285714     3.2915361     2.3140496 
##       smoke:3 
##     1.8652038
```

### Continuation-ratio

This is unconstrained model. We show how to create Table 8.16 on page 296 (Hosmer)
For continuation ratio model we:

1.  compare bwt == >3500 vs bwt== 3000-3500
2.  compare bwt == >3500 and bwt== 3000-3500 vs bwt== 2500-3000
3.  compare bwt == >3500 and bwt== 3000-3500 and bwt== 2500-3000 vs bwt==3 <2500

For comparison bwt == >3500 vs bwt== 3000-3500


```r
table(data.o3$bwt4) ; table(data.o3$bwt4a)
```

```
## 
##    =<2500 2501-3000 3001-3500     >3500 
##        59        38        46        46
```

```
## 
##     >3500 3001-3500 2501-3000    =<2500 
##        46        46        38        59
```

```r
data.o3a <- data.o3 %>% 
  filter(bwt4 == '>3500' | bwt4 == '3001-3500')
cr1 <- glm(bwt4a ~ smoke, family = binomial(link ='logit'),
           data = data.o3a)
summary(cr1)
```

```
## 
## Call:
## glm(formula = bwt4a ~ smoke, family = binomial(link = "logit"), 
##     data = data.o3a)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.36697  -1.09867  -0.04984   1.25824   1.25824  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.1881     0.2511  -0.749    0.454
## smoke         0.6234     0.4613   1.351    0.177
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 127.54  on 91  degrees of freedom
## Residual deviance: 125.68  on 90  degrees of freedom
## AIC: 129.68
## 
## Number of Fisher Scoring iterations: 4
```

For comparison bwt == >3500 and bwt== 3000-3500 vs bwt== 2500-3000

For this we will recode variable bwt using ifelse function. Please, make sure the code will be 0 and 1. using code bigger will not make glm function work



```r
data.o3b <- data.o3 %>% 
  filter(bwt4 == '>3500' | 
         bwt4 == '3001-3500'| 
         bwt4 == '2501-3000')
table(data.o3b$bwt4a)
```

```
## 
##     >3500 3001-3500 2501-3000    =<2500 
##        46        46        38         0
```

```r
data.o3b <- data.o3b %>% 
  mutate(bwt4b = ifelse(bwt4a == ">3500", 0, 
                        ifelse(bwt4a == "3001-3500",0,1)))
table(data.o3b$bwt4a) ; table(data.o3b$bwt4b)
```

```
## 
##     >3500 3001-3500 2501-3000    =<2500 
##        46        46        38         0
```

```
## 
##  0  1 
## 92 38
```

And run the next CR model:


```r
cr2 <- glm(bwt4b ~ smoke, family = binomial(link ='logit'), 
           data = data.o3b)
summary(cr2)
```

```
## 
## Call:
## glm(formula = bwt4b ~ smoke, family = binomial(link = "logit"), 
##     data = data.o3b)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9508  -0.7687  -0.7687   1.4224   1.6512  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.0678     0.2471  -4.321 1.56e-05 ***
## smoke         0.5082     0.3991   1.273    0.203    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 157.09  on 129  degrees of freedom
## Residual deviance: 155.49  on 128  degrees of freedom
## AIC: 159.49
## 
## Number of Fisher Scoring iterations: 4
```

For comparing bwt == >3500 and bwt== 3000-3500 and bwt== 2500-3000 vs bwt==3 <2500


```r
table(data.o3$bwt4) ; table(data.o3$bwt4a)
```

```
## 
##    =<2500 2501-3000 3001-3500     >3500 
##        59        38        46        46
```

```
## 
##     >3500 3001-3500 2501-3000    =<2500 
##        46        46        38        59
```

```r
data.o3b <- data.o3 %>% 
  mutate(bwt4c = fct_recode(bwt4a, 
                            gp0 = '>3500',
                            gp0 = '3001-3500',
                            gp0 = '3001-3500',
                            gp0 = '2501-3000'))
```

We can verify our results:


```r
table(data.o3$bwt4) ; table(data.o3$bwt4a) ; table(data.o3b$bwt4c)       
```

```
## 
##    =<2500 2501-3000 3001-3500     >3500 
##        59        38        46        46
```

```
## 
##     >3500 3001-3500 2501-3000    =<2500 
##        46        46        38        59
```

```
## 
##    gp0 =<2500 
##    130     59
```


And we can obtain logit 3 for Table 8.16 ALR book


```r
cr3 <- glm(bwt4c ~ smoke, family = binomial(link = 'logit'),
           data = data.o3b)
summary(cr3)
```

```
## 
## Call:
## glm(formula = bwt4c ~ smoke, family = binomial(link = "logit"), 
##     data = data.o3b)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0197  -0.7623  -0.7623   1.3438   1.6599  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.0871     0.2147  -5.062 4.14e-07 ***
## smoke         0.7041     0.3196   2.203   0.0276 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 234.67  on 188  degrees of freedom
## Residual deviance: 229.80  on 187  degrees of freedom
## AIC: 233.8
## 
## Number of Fisher Scoring iterations: 4
```


### Conclusion

The three estimates are quite similar (more less 0.6). The estimates indicate that the odds of a birth in the next lower weight category relative to the higher weight categories among women who smoked during pregnancy is about $1.8=exp(0.6)$ times than that of women who did not smoke.


# Tutorial 3

## Data and model for cumulative link logit model

This is also known as the proportional odds model.

Cumulative Logit Models (Proportional Odds Models) Another example of proportional odds models. We use the the variables that code from the lowest weight (=<2500) to the highest weight (>3500)

Recheck the levels to confirm that from smallest category of weight to heavier babies. 

- bwt4 (=<2500) : mean birth weight = 2097.1
- bwt4 (2501 - 3000) : mean birth weight = 2795.7
- bwt4 (3001 - 3500) : mean birth weight = 3236.2
- bwt4 (>=3500) : mean birth weight = 3863.1

In Stata for example, the way we treat cumulative link logits is *similar* (not similar in the estimation process, just the context) to ordinal linear regression. That is we estimate how big the increase in the outcome variable with every unit increase in the predictor. 


```r
levels(data.o3$bwt4)
```

```
## [1] "=<2500"    "2501-3000" "3001-3500" ">3500"
```

## Estimation

The variables:

1.  outcome = bwt4
2.  covariate = lwt

We will use **ordinal::clm** function

- outcome: bwt4
- covariate: lwt


```r
library('ordinal')
o.lwt <- clm(bwt4 ~ lwt, data=data.o3)
summary(o.lwt)
```

```
## formula: bwt4 ~ lwt
## data:    data.o3
## 
##  link  threshold nobs logLik  AIC    niter max.grad cond.H 
##  logit flexible  189  -255.15 518.30 4(0)  2.93e-08 1.0e+06
## 
## Coefficients:
##     Estimate Std. Error z value Pr(>|z|)   
## lwt 0.012737   0.004317   2.951  0.00317 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##                     Estimate Std. Error z value
## =<2500|2501-3000      0.8316     0.5686   1.462
## 2501-3000|3001-3500   1.7070     0.5782   2.952
## 3001-3500|>3500       2.8311     0.6027   4.697
```

These are

- the log odds ratio and 
- the odss ratio


```r
tidy(o.lwt, conf.int = TRUE)
```

```
## # A tibble: 4 x 8
##   term  estimate std.error statistic p.value conf.low conf.high coefficient_type
##   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>           
## 1 =<25~   0.832    0.569        1.46 1.44e-1 NA         NA      alpha           
## 2 2501~   1.71     0.578        2.95 3.15e-3 NA         NA      alpha           
## 3 3001~   2.83     0.603        4.70 2.64e-6 NA         NA      alpha           
## 4 lwt     0.0127   0.00432      2.95 3.17e-3  0.00439    0.0214 beta
```

```r
tidy(o.lwt, exponentiate = TRUE ,conf.int = TRUE)
```

```
## # A tibble: 4 x 8
##   term  estimate std.error statistic p.value conf.low conf.high coefficient_type
##   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>           
## 1 =<25~     2.30   0.569        1.46 1.44e-1    NA        NA    alpha           
## 2 2501~     5.51   0.578        2.95 3.15e-3    NA        NA    alpha           
## 3 3001~    17.0    0.603        4.70 2.64e-6    NA        NA    alpha           
## 4 lwt       1.01   0.00432      2.95 3.17e-3     1.00      1.02 beta
```

## Interpretation

The coefficient $\hat\beta_{lwt} = 0.0127$. This shows that the heavier the lwt, then the value of bwt also increases (remember the coding and Eq 8.25 page 300 ALR. 

The interpretations are for a change of 1 and 10 pound LWT:

- $exp(1 \times -0.013)$
- $exp(10 \times  -0.013)$
- $exp(10 \times -0.013)$

1.  For every 1 pound increase in lwt (mom's weight), the odds to get LIGHTER baby reduced for about $2\%$
2.  For every 10 pound increase in lwt (mom's weight), the odds to get LIGHTER baby reduced for about $12\%$


```r
exp(1 * -0.013)
```

```
## [1] 0.9870841
```

```r
exp(10 * -0.0127)
```

```
## [1] 0.8807337
```

3.  For every 1 pound increase in lwt (mom's weight), the odds to get HEAVIER baby is 1.01 or increased for $1\%$
4.  For every 10 pound increase in lwt (mom's weight), the odds to get HEAVIER baby is 1.14 or increased for $14\%$



```r
exp(1 * 0.013)
```

```
## [1] 1.013085
```

```r
exp(10 * 0.0127)
```

```
## [1] 1.135417
```



## polr in MASS package

Using **MASS::polr** function


```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
polr_cr3 <- polr(bwt4 ~ lwt, data = data.o3, Hess = TRUE)
summary(polr_cr3)
```

```
## Call:
## polr(formula = bwt4 ~ lwt, data = data.o3, Hess = TRUE)
## 
## Coefficients:
##       Value Std. Error t value
## lwt 0.01274   0.004325   2.945
## 
## Intercepts:
##                     Value  Std. Error t value
## =<2500|2501-3000    0.8316 0.5694     1.4606 
## 2501-3000|3001-3500 1.7070 0.5789     2.9488 
## 3001-3500|>3500     2.8311 0.6034     4.6916 
## 
## Residual Deviance: 510.2954 
## AIC: 518.2954
```

## Checking proportional odds assumption


```r
library(brant)
brant(polr_cr3)
```

```
## -------------------------------------------- 
## Test for	X2	df	probability 
## -------------------------------------------- 
## Omnibus		1.13	2	0.57
## lwt		1.13	2	0.57
## -------------------------------------------- 
## 
## H0: Parallel Regression Assumption holds
```

```
##               X2 df probability
## Omnibus 1.132187  2    0.567739
## lwt     1.132187  2    0.567739
```

# Tutorial 4 : Multivariable models 

Source Chapter 5, Regression Models for dependent Categorical variables Using Stata


```r
library(foreign)
warm <- read.dta('ordwarm2b.dta', 
                 convert.factors =  TRUE)
summary(warm)
```

```
##  warm       yr89         male           white           age       
##  SD:297   1977:1379   Women:1227   NotWhite: 283   Min.   :18.00  
##  D :723   1989: 914   Men  :1066   White   :2010   1st Qu.:31.00  
##  A :856                                            Median :42.00  
##  SA:417                                            Mean   :44.94  
##                                                    3rd Qu.:58.00  
##                                                    Max.   :89.00  
##        ed             prst         warmlt2     warmlt3       warmlt4    
##  Min.   : 0.00   Min.   :12.00   D,A,SA:1996   A,SA:1273   SA    : 417  
##  1st Qu.:11.00   1st Qu.:30.00   SD    : 297   D,SD:1020   A,D,SD:1876  
##  Median :12.00   Median :37.00                                          
##  Mean   :12.22   Mean   :39.59                                          
##  3rd Qu.:14.00   3rd Qu.:50.00                                          
##  Max.   :20.00   Max.   :82.00
```

## Model

$$Pr(warm = m | x_i) = F(\tau_m - x\beta) - F(\tau_{m-1}-x\beta)$$
where

$$x\beta = \beta_{yr89}yr89 + \beta_{male}male + \beta_{white}white +
\beta_{age}age + \beta_{ed}ed + \beta_{prst}prst$$

## Estimate

### **MASS** package


```r
library(MASS)
polr_warm <- polr(warm ~ yr89 + male + white + age +
                  ed + prst, data = warm, Hess = TRUE)
```

### **ordinal** package


```r
library(ordinal)
clm_warm <- clm(warm ~ yr89 + male + white + age +
                  ed + prst, data = warm)
```

## Results

### MASS::polr 


```r
summary(polr_warm)
```

```
## Call:
## polr(formula = warm ~ yr89 + male + white + age + ed + prst, 
##     data = warm, Hess = TRUE)
## 
## Coefficients:
##                Value Std. Error t value
## yr891989    0.523912   0.079899   6.557
## maleMen    -0.733309   0.078483  -9.344
## whiteWhite -0.391140   0.118381  -3.304
## age        -0.021666   0.002469  -8.777
## ed          0.067176   0.015975   4.205
## prst        0.006072   0.003293   1.844
## 
## Intercepts:
##      Value    Std. Error t value 
## SD|D  -2.4654   0.2389   -10.3188
## D|A   -0.6309   0.2333    -2.7042
## A|SA   1.2618   0.2340     5.3919
## 
## Residual Deviance: 5689.825 
## AIC: 5707.825
```

### ordinal::clm


```r
summary(clm_warm)
```

```
## formula: warm ~ yr89 + male + white + age + ed + prst
## data:    warm
## 
##  link  threshold nobs logLik   AIC     niter max.grad cond.H 
##  logit flexible  2293 -2844.91 5707.82 5(0)  4.94e-10 4.4e+05
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## yr891989    0.523902   0.079899   6.557 5.49e-11 ***
## maleMen    -0.733300   0.078483  -9.343  < 2e-16 ***
## whiteWhite -0.391159   0.118381  -3.304 0.000952 ***
## age        -0.021666   0.002468  -8.778  < 2e-16 ***
## ed          0.067173   0.015975   4.205 2.61e-05 ***
## prst        0.006073   0.003293   1.844 0.065157 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##      Estimate Std. Error z value
## SD|D  -2.4654     0.2389 -10.319
## D|A   -0.6309     0.2333  -2.704
## A|SA   1.2619     0.2340   5.392
```

$SD|D$ is the first cut point (threshold). $D|A$ is the second cut point (threshold)

## Predict

### Prediict using the current data

polr 

- predict the level 
- predict probability for each outcome level


```r
pred_grp <- predict(polr_warm)
head(pred_grp, 20)
```

```
##  [1] A D A D A D A A A D D D D D A A D A D D
## Levels: SD D A SA
```

```r
pred_prob <- predict(polr_warm, type = 'probs')
head(pred_prob)
```

```
##           SD         D         A         SA
## 1 0.09803658 0.3069328 0.4137917 0.18123883
## 2 0.24674606 0.4255062 0.2593246 0.06842316
## 3 0.10527809 0.3189414 0.4060168 0.16976377
## 4 0.21152068 0.4153110 0.2908628 0.08230550
## 5 0.12883259 0.3519548 0.3792848 0.13992780
## 6 0.17919975 0.3983330 0.3232034 0.09926381
```

clm

In ordinal::clm, prediction must always be used with newdata. And the newdata must NOT contain the outcome variable


```r
detach("package:MASS", unload = TRUE)
```

```
## Warning: 'MASS' namespace cannot be unloaded:
##   namespace 'MASS' is imported by 'brant', 'ordinal' so cannot be unloaded
```

```r
new_data_warm <- warm %>% select(-warm) 
pred_prob_clm <- predict(clm_warm, new_data_warm)
head(pred_prob_clm$fit)
```

```
##           SD         D         A         SA
## 1 0.09803755 0.3069408 0.4137853 0.18123634
## 2 0.24674382 0.4255115 0.2593211 0.06842356
## 3 0.10527870 0.3189487 0.4060106 0.16976204
## 4 0.21151912 0.4153165 0.2908586 0.08230576
## 5 0.12882972 0.3519578 0.3792823 0.13993018
## 6 0.17920094 0.3983401 0.3231963 0.09926256
```


### Predict Using new data

The new data


```r
newData = data.frame(yr89 = '1977' , 
                     male = 'Men', prst = 20,
                     age = 64, ed = 16, 
                     white = 'White' )
```

The predicted probability


```r
predict(polr_warm, newdata = newData, type = 'prob')
```

```
##         SD          D          A         SA 
## 0.24038862 0.42421594 0.26473970 0.07065573
```

```r
predict(clm_warm, newdata = newData)
```

```
## $fit
##          SD         D         A         SA
## 1 0.2403926 0.4242226 0.2647309 0.07065394
```

### Replicate STATA probability (optional reading)

References : 
1.  <https://www.stata.com/manuals13/rologit.pdf>
2.  <https://www.stata.com/manuals13/rologitpostestimation.pdf#rologitpostestimation>

See also section STATA


```r
library(haven)
full_auto <- read_dta('full_auto.dta')
```

estimate using ordinal package


```r
library(ordinal)
full_auto$rep77 <- as_factor(full_auto$rep77)
o_full_auto <- clm(rep77 ~ foreign, link = "logit", data = full_auto)
summary(o_full_auto)
```

```
## formula: rep77 ~ foreign
## data:    full_auto
## 
##  link  threshold nobs logLik AIC    niter max.grad cond.H 
##  logit flexible  66   -85.91 181.82 5(0)  1.23e-07 1.2e+01
## 
## Coefficients:
##         Estimate Std. Error z value Pr(>|z|)   
## foreign   1.4559     0.5309   2.742   0.0061 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##                Estimate Std. Error z value
## Poor|Fair       -2.7656     0.5988  -4.618
## Fair|Average    -0.9964     0.3218  -3.096
## Average|Good     0.9426     0.3136   3.005
## Good|Excellent   3.1234     0.5423   5.759
## (8 observations deleted due to missingness)
```

The estimate produces the same estimates with stata ologit command

for prediction, ordinal predict() gives the value for predicted probability for the observed outcome


```r
pr_o_full_auto <- predict(o_full_auto, type = "prob")
head(pr_o_full_auto, 14)
```

```
## $fit
##  [1] 0.21044388 0.05921375 0.29515398 0.06480985 0.46680958 0.44996999
##  [7] 0.23821810 0.23821810 0.44996999 0.23821810 0.44996999 0.44996999
## [13] 0.21044388 0.44996999 0.44996999 0.23821810 0.44996999 0.44996999
## [19] 0.21044388 0.44996999 0.29515398 0.15876141 0.46680958 0.46680958
## [25] 0.23821810 0.21044388 0.21044388 0.21044388 0.01446518 0.44996999
## [31] 0.15876141 0.46680958 0.23821810 0.23821810 0.44996999 0.46680958
## [37] 0.44996999 0.44996999 0.23821810 0.21044388 0.44996999 0.44996999
## [43] 0.23821810 0.23821810 0.44996999 0.05921375 0.44996999 0.23821810
## [49] 0.44996999 0.23821810 0.21044388 0.23821810 0.21044388 0.44996999
## [55] 0.44996999 0.21044388 0.29515398 0.46680958 0.15876141 0.15876141
## [61] 0.15876141 0.29515398 0.46680958 0.29515398 0.29515398 0.29515398
```

Estimate using MASS::polr


```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
o_auto_polr <- polr(rep77 ~ foreign, Hess = TRUE, method = "logistic", data = full_auto)
summary(o_auto_polr)
```

```
## Call:
## polr(formula = rep77 ~ foreign, data = full_auto, Hess = TRUE, 
##     method = "logistic")
## 
## Coefficients:
##         Value Std. Error t value
## foreign 1.456     0.5309   2.743
## 
## Intercepts:
##                Value   Std. Error t value
## Poor|Fair      -2.7644  0.5984    -4.6198
## Fair|Average   -0.9961  0.3218    -3.0960
## Average|Good    0.9433  0.3137     3.0072
## Good|Excellent  3.1243  0.5424     5.7602
## 
## Residual Deviance: 171.8163 
## AIC: 181.8163 
## (8 observations deleted due to missingness)
```

Similar results with STATA and clm

Prediction


```r
pr_full_polr <- predict(o_auto_polr, type = "prob")
head(pr_full_polr, 14)
```

```
##          Poor       Fair   Average      Good  Excellent
## 1  0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 2  0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 4  0.01447342 0.06477352 0.2951882 0.4668421 0.15872273
## 5  0.01447342 0.06477352 0.2951882 0.4668421 0.15872273
## 6  0.01447342 0.06477352 0.2951882 0.4668421 0.15872273
## 7  0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 8  0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 9  0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 11 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 12 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 13 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 14 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 15 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
## 16 0.05928024 0.21042270 0.4500541 0.2381252 0.04211777
```

Prediction by polr produces predicted probability for each group (similar to STATA)

Manual calculation (pay attention to negative). See formula 8.17 and 8.27 Hosmer


```r
case1_poor <- 1/(1 + exp(-(-2.764 - (1.456*0))))
case1_poor
```

```
## [1] 0.05930084
```

```r
case1_poor <- 1/(1 + exp(-(-2.764 - (1.456*0))))
case1_poor
```

```
## [1] 0.05930084
```

```r
# or can be written as 
exp((-2.764 - (1.456*0))) / (1 + ( exp((-2.764 - (1.456*0)))))
```

```
## [1] 0.05930084
```

```r
case1_fair <- 1/(1 + exp(-(-0.996 - (1.456*0)))) - case1_poor
case1_fair
```

```
## [1] 0.2104278
```

```r
case1_average <- 1/(1 + exp(-(0.942 - (1.456*0)))) - case1_fair - case1_poor
case1_average
```

```
## [1] 0.4497749
```

```r
case1_good <- 1/(1 + exp(-(3.12 - (1.456*0)))) - case1_average  -case1_fair - case1_poor
case1_good
```

```
## [1] 0.2382068
```

```r
case4_poor <- 1/(1 + exp(-(-2.764 - (1.456*1))))
case4_poor
```

```
## [1] 0.01448572
```

```r
case4_fair <- 1/(1 + exp(-(-0.996 - (1.456*1)))) - case4_poor
case4_fair
```

```
## [1] 0.06480669
```


# Software

## STATA:

steps:

1.  data: `datao.dta`
2.  estimation: `ologit apply2 pared public`
3.  prediction: `predict probunlike probsomewhat probverylike, p`
4.  linear prediction: `predict linpred, xb` 

See file `stata_manual_13_handson.do`

### Logit formula

$$ln \frac{P(Y \leq g | X)}{P(Y > g | X)} = \exp{-[\alpha_g - \sum_{j = 1}^{p} \beta_i X_i]}$$

### Probability

$$P(Y \leq g | X) = \frac{1}{1 + \exp{[-(\alpha_g - \sum_{j = 1}^{p} \beta_j X_j)}]}$$
where, $g = 1,2, ... , G-1$, so $\alpha_1 < \alpha2 < \alpha_3 ... < \alpha_{g-1}$


## SAS

### Odds

$$odds_g = odds \ for \ (Y \geq g) = \frac{P(Y \geq g | X)}{P(Y < g | X)} = \exp{[\alpha_g + \sum_{j = 1}^{p} \beta_i X_i}]$$

### Logit formula

$$ln \frac{P(Y \geq g | X)}{P(Y < g | X)} = \alpha_g + \sum_{j = 1}^{p} \beta_i X_i$$



### Probability formula
$$P(Y \geq g | X) = \frac{1}{1 + \exp{[-(\alpha_g + \sum_{j = 1}^{p} \beta_j X_j)}]}$$
where, $g = 1,2, ... , G-1$, so $\alpha_1 > \alpha2 > \alpha_3 ... > \alpha_{g-1}$

# References

1.  Hosmer, D. W., Lemeshow, S. and Sturdivant, R. X. Chapter 8.2 in Applied Logistic Regression. 3rd Edition. Wiley.
2.  Kleinbaum, D. G., Klein M. Chapter 13 in Logistic Regression: A Self Learning Text.
3.  <http://www.stat.ufl.edu/~aa/ordinal/R_examples.pdf>
4.  <https://stats.stackexchange.com/questions/114410/manual-calculation-of-ordinal-logistic-regression-probabilities-starting-at-di>
5. Predicting ordered logit in R <https://stats.stackexchange.com/questions/41006/predicting-ordered-logit-in-r/41025#41025>
6.  <http://data.library.virginia.edu/fitting-and-interpreting-a-proportional-odds-model/>
7.  Prediction <https://www.youtube.com/watch?v=qkivJzjyHoA>




















