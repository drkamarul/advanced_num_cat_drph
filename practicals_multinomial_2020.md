---
title: | 
  | Multinomial Logistic Regression
  | Practicals:
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

# Practicals 1

We will do practicals using the materials available on the ats.ucla website. We will do some modifications in order to simulate result better to suit our needs. The link to source is  <https://stats.idre.ucla.edu/stata/examples/alr2/applied-logistic-regression-second-edition-by-hosmer-and-lemeshowchapter-8-special-topics/m>

You have to be careful with the codes of the variables. Pay attention to the labels and levels of the variable. STATA software provides a more polish interface of results and the coding of variables seems easier to understand too. 

## Data

### Read data


We will use `haven::read_dta()` function to read stata `.dta` data into the memory. Remember, we can also use `foreign::read.dta()` function to read stata `.dta` data. It is your choice.  

The dataset contains these variables:

1.  me: the outcome variable is mammography experience (0 = never, 1 = within a year, 2 = over a year ago)
2.  sympt: "You do not need a mamogram unless you develop symptoms". Codes are, 1 = Strongly Agree, 2 = Agree, 3 = Disagree, 4 = Strongly Disagree
3.  pb: Perveived benefit of mammography. Score between 5 - 20 
4.  HIST: Mother or Sister with a history of breast cancer. Codes are 0 = No, 1 = Yes 
5.  bse: "Has anyone taught you how to examine your own breasts: that is BSE". Codes are 0 = No, 1 = Yes
6.  detc: "How likely is it that a mamogram could find a new case of breast cancer". Codes are 1= Not likely, 2 = Somewhat likely, 3 = Very likely

We examine the first 6 observations. Then we describe the variables in the data.


```r
library(here)
```

```
## here() starts at D:/OneDrive - Universiti Sains Malaysia/3_Statistics/DrPH_Numerical_Categorical_2019
```

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
dat.m1 <- read_dta('mammog9.dta')
```

We can then

1.  check types variables
2.  observe the first 6 observations of data
3.  get summary statistics


```r
glimpse(dat.m1)
```

```
## Rows: 412
## Columns: 7
## $ obs   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18...
## $ me    <dbl> 0, 0, 0, 1, 2, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2...
## $ sympt <dbl> 3, 2, 3, 3, 4, 3, 4, 4, 2, 4, 4, 3, 4, 1, 2, 4, 3, 4, 3, 3, 3...
## $ pb    <dbl> 7, 11, 8, 11, 7, 7, 6, 6, 6, 6, 8, 6, 6, 5, 8, 11, 6, 5, 10, ...
## $ hist  <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0...
## $ bse   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1...
## $ detc  <dbl> 2, 3, 3, 3, 3, 3, 2, 3, 3, 3, 2, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3...
```

```r
summary(dat.m1)
```

```
##       obs              me             sympt             pb        
##  Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Min.   : 5.000  
##  1st Qu.:103.8   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.: 6.000  
##  Median :206.5   Median :0.0000   Median :3.000   Median : 7.000  
##  Mean   :206.5   Mean   :0.6117   Mean   :2.966   Mean   : 7.561  
##  3rd Qu.:309.2   3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.: 9.000  
##  Max.   :412.0   Max.   :2.0000   Max.   :4.000   Max.   :17.000  
##       hist             bse              detc      
##  Min.   :0.0000   Min.   :0.0000   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:2.000  
##  Median :0.0000   Median :1.0000   Median :3.000  
##  Mean   :0.1068   Mean   :0.8689   Mean   :2.658  
##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:3.000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :3.000
```

Variable `me` is a class of numeric. We will convert it to a new variable `me2` as a class of factor variable. 


```r
dat.m1 %>% group_by(me) %>% count()
```

```
## # A tibble: 3 x 2
## # Groups:   me [3]
##      me     n
##   <dbl> <int>
## 1     0   234
## 2     1   104
## 3     2    74
```


To do that, we use `factor()` function. The function will categorize a numerical variable

We will create a new variable me2, then we will verify the categories of the variable me2


```r
dat.m1 <- dat.m1 %>% mutate(me2 = factor(me, 
                                             labels = c("never","within.a.year","over.a.year.ago")))
levels(dat.m1$me2)
```

```
## [1] "never"           "within.a.year"   "over.a.year.ago"
```

```r
dat.m1 %>% group_by(me2) %>% count()
```

```
## # A tibble: 3 x 2
## # Groups:   me2 [3]
##   me2                 n
##   <fct>           <int>
## 1 never             234
## 2 within.a.year     104
## 3 over.a.year.ago    74
```

We will do the same thing to variable `hist`:


```r
dat.m1 <- dat.m1 %>% mutate(hist2 = factor(hist,
                                           labels = c("no","yes")))
dat.m1 %>% group_by(hist2) %>% count()
```

```
## # A tibble: 2 x 2
## # Groups:   hist2 [2]
##   hist2     n
##   <fct> <int>
## 1 no      368
## 2 yes      44
```

### Describe data

**psych** and **summarytools** can provide nicer and more description of data


```r
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
describe(dat.m1)
```

```
##        vars   n   mean     sd median trimmed    mad min max range  skew
## obs       1 412 206.50 119.08  206.5  206.50 152.71   1 412   411  0.00
## me        2 412   0.61   0.77    0.0    0.52   0.00   0   2     2  0.79
## sympt     3 412   2.97   0.95    3.0    3.08   1.48   1   4     3 -0.61
## pb        4 412   7.56   2.10    7.0    7.41   2.97   5  17    12  0.57
## hist      5 412   0.11   0.31    0.0    0.01   0.00   0   1     1  2.54
## bse       6 412   0.87   0.34    1.0    0.96   0.00   0   1     1 -2.18
## detc      7 412   2.66   0.56    3.0    2.75   0.00   1   3     2 -1.39
## me2*      8 412   1.61   0.77    1.0    1.52   0.00   1   3     2  0.79
## hist2*    9 412   1.11   0.31    1.0    1.01   0.00   1   2     1  2.54
##        kurtosis   se
## obs       -1.21 5.87
## me        -0.90 0.04
## sympt     -0.56 0.05
## pb        -0.13 0.10
## hist       4.45 0.02
## bse        2.75 0.02
## detc       0.95 0.03
## me2*      -0.90 0.04
## hist2*     4.45 0.02
```

Below, we can perform cross-tabulation and calculate the frequencies:


```r
dat.m1 %>% count(me2)
```

```
## # A tibble: 3 x 2
##   me2                 n
##   <fct>           <int>
## 1 never             234
## 2 within.a.year     104
## 3 over.a.year.ago    74
```

```r
dat.m1 %>% count(me2, hist2)
```

```
## # A tibble: 6 x 3
##   me2             hist2     n
##   <fct>           <fct> <int>
## 1 never           no      220
## 2 never           yes      14
## 3 within.a.year   no       85
## 4 within.a.year   yes      19
## 5 over.a.year.ago no       63
## 6 over.a.year.ago yes      11
```


## Estimation

### VGAM package 

We will use **VGAM** package and we need to reverse the levels for variable **me2** to replicate the outputs in Applied Logistic Regression book. To do that, we make the group `never` (the last group) as the reference category.

The steps below:

- confirm the order of variable me2
- generate variable me3 that equals to me2
- change order of me3 from over a year to within a year to never
- confirm observations are still correct


```r
summary(dat.m1$me2)
```

```
##           never   within.a.year over.a.year.ago 
##             234             104              74
```

```r
# [1] "never"           "within.a.year"   "over.a.year.ago"
dat.m1 <- dat.m1 %>% mutate(me3 = fct_relevel(me2, 
                                              c("over.a.year.ago", 'within.a.year', 'never')))
table(dat.m1$me) ; summary(dat.m1$me2) ; summary(dat.m1$me3)
```

```
## 
##   0   1   2 
## 234 104  74
```

```
##           never   within.a.year over.a.year.ago 
##             234             104              74
```

```
## over.a.year.ago   within.a.year           never 
##              74             104             234
```

Let us check the order (if you are still anxious)


```r
levels(dat.m1$me2) ; levels(dat.m1$me3)
```

```
## [1] "never"           "within.a.year"   "over.a.year.ago"
```

```
## [1] "over.a.year.ago" "within.a.year"   "never"
```


The function `VGAM::vglm()` runs the multinomial logistic regression. And we want to estimate the model for

1.  group 1 vs group 3 : me = over a year ago vs me = never
2.  group 2 vs group 3 : me = within a year ago vs me = never

So, the reference group is the *never* group. 


The steps below:

- We load the library
- Perform estimation and name the object as fitmlog1
- Return the result of fitmlog1


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
## The following objects are masked from 'package:psych':
## 
##     fisherz, logistic, logit
```

```
## The following object is masked from 'package:tidyr':
## 
##     fill
```

```r
fitmlog1 <- vglm(me3 ~ hist2, multinomial, data = dat.m1)
summary(fitmlog1)
```

```
## 
## Call:
## vglm(formula = me3 ~ hist2, family = multinomial, data = dat.m1)
## 
## Pearson residuals:
##                        Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -0.9287 -0.5410 -0.5410 -0.2162 2.181
## log(mu[,2]/mu[,3]) -1.1315 -0.6165 -0.6165  1.1127 1.812
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1  -1.2505     0.1429  -8.751  < 2e-16 ***
## (Intercept):2  -0.9510     0.1277  -7.446  9.6e-14 ***
## hist2yes:1      1.0093     0.4275   2.361 0.018225 *  
## hist2yes:2      1.2564     0.3747   3.353 0.000798 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 792.3399 on 820 degrees of freedom
## 
## Log-likelihood: -396.17 on 820 degrees of freedom
## 
## Number of Fisher scoring iterations: 4 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  3  of the response
```

Be careful:

1.  (Intercept):1 for outcome me = over a year ago vs never
2.  (Intercept):2 for outcome me =  within a year vs never
3.  hist2yes:1 for outcome me = over a year ago vs never
4.  hist2yes:2 for outcome me =  within a year vs never

Next, to replicate results for **detc** as show on this webpage <http://www.ats.ucla.edu/stat/stata/examples/alr2/alr2stata8.htm>, we will run these codes:



```r
dat.m1 <- dat.m1 %>% mutate(detc2 = factor(detc))
table(dat.m1$me2, dat.m1$detc2)
```

```
##                  
##                     1   2   3
##   never            13  77 144
##   within.a.year     1  12  91
##   over.a.year.ago   4  16  54
```

Now, we will fit the multinomial logit again (with covariate detc2) and name the model as fitmlog2. Remember, the comparisons are:

1.  group 1 vs group 3 : for outcome me = over a year ago vs never
2.  group 2 vs group 3 : for outcome me =  within a year vs never


```r
fitmlog2 <- vglm(me3 ~ detc2, multinomial, data = dat.m1)
summary(fitmlog2)
```

```
## 
## Call:
## vglm(formula = me3 ~ detc2, family = multinomial, data = dat.m1)
## 
## Pearson residuals:
##                        Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -0.6295 -0.6295 -0.4537 -0.2217 2.352
## log(mu[,2]/mu[,3]) -0.7814 -0.7814 -0.3973  1.4583 4.109
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)  
## (Intercept):1  -1.1787     0.5718  -2.061   0.0393 *
## (Intercept):2  -2.5649     1.0377      NA       NA  
## detc22:1       -0.3926     0.6344  -0.619   0.5360  
## detc22:2        0.7061     1.0832   0.652   0.5145  
## detc23:1        0.1978     0.5936   0.333   0.7389  
## detc23:2        2.1060     1.0463   2.013   0.0441 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 778.4011 on 818 degrees of freedom
## 
## Log-likelihood: -389.2005 on 818 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## Warning: Hauck-Donner effect detected in the following estimate(s):
## '(Intercept):2'
## 
## 
## Reference group is level  3  of the response
```

## Inferences

For the inference, we will:

1.  calculate the $95\%$ CI (interval estimates)
2.  calculate the p-values (hypothesis testing)

The codes below will:

1.  return the regression coefficents for all $\hat\beta$ as an object named b_fitmlog2
2.  return the the confidence intervales for all $\hat\beta$ as an object named ci_fitmlog2 
3.  combine the $\hat\beta$ and the corresponding $95\%$ CIs




```r
b_fitmlog2 <- coef(fitmlog2)
ci_fitmlog2 <- confint(fitmlog2)
b_ci_fitmlog2 <- cbind(b_fitmlog2, ci_fitmlog2)
b_ci_fitmlog2
```

```
##               b_fitmlog2       2.5 %      97.5 %
## (Intercept):1 -1.1786550 -2.29930795 -0.05800204
## (Intercept):2 -2.5649494 -4.59888160 -0.53101711
## detc22:1      -0.3925617 -1.63588117  0.85075776
## detc22:2       0.7060506 -1.41689336  2.82899454
## detc23:1       0.1978257 -0.96565093  1.36130242
## detc23:2       2.1059956  0.05519791  4.15679321
```


Afterwards, we will *exponentiate* the coefficients to obtain the **relative-risk ratio**. And combine the results to the previous table. Then we will name the columns of the object tab_fitmlog2. 


```r
rrr_fitmlog2 <- exp(b_ci_fitmlog2)
tab_fitmlog2 <- cbind(b_ci_fitmlog2, rrr_fitmlog2)
colnames(tab_fitmlog2) <- c('b', 'lower b', 'upper b', 
                            'rrr', 'lower rrr', 'upper rrr')
tab_fitmlog2
```

```
##                        b     lower b     upper b        rrr  lower rrr
## (Intercept):1 -1.1786550 -2.29930795 -0.05800204 0.30769231 0.10032825
## (Intercept):2 -2.5649494 -4.59888160 -0.53101711 0.07692308 0.01006308
## detc22:1      -0.3925617 -1.63588117  0.85075776 0.67532468 0.19478066
## detc22:2       0.7060506 -1.41689336  2.82899454 2.02597403 0.24246610
## detc23:1       0.1978257 -0.96565093  1.36130242 1.21875000 0.38073529
## detc23:2       2.1059956  0.05519791  4.15679321 8.21527778 1.05674974
##                upper rrr
## (Intercept):1  0.9436480
## (Intercept):2  0.5880066
## detc22:1       2.3414204
## detc22:2      16.9284313
## detc23:1       3.9012711
## detc23:2      63.8663880
```

# Practicals 2

## Estimation

### VGAM package

Adding more covariates (independent variables) to the model:

1.  sympt (as a factor variable)  
2.  pb 
3.  hist 
4.  bse
5.  detc

We will obtain the estimates (and name the model as fitmlog3). The `factor()` function could easily convert the numeric variables to categorical variables.

The model is multinomial and the data is dat.m1


```r
fitmlog3 <- vglm(me3 ~ factor(sympt) + pb + hist2 + bse + detc2,
                 multinomial, data = dat.m1)
summary(fitmlog3)
```

```
## 
## Call:
## vglm(formula = me3 ~ factor(sympt) + pb + hist2 + bse + detc2, 
##     family = multinomial, data = dat.m1)
## 
## Pearson residuals:
##                      Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -1.47 -0.5286 -0.2936 -0.2263 3.404
## log(mu[,2]/mu[,3]) -1.88 -0.5601 -0.2722  0.6339 8.832
## 
## Coefficients: 
##                  Estimate Std. Error z value Pr(>|z|)   
## (Intercept):1    -0.98609    1.11184  -0.887  0.37513   
## (Intercept):2    -2.99875    1.53905      NA       NA   
## factor(sympt)2:1 -0.29008    0.64406  -0.450  0.65243   
## factor(sympt)2:2  0.11004    0.92273   0.119  0.90508   
## factor(sympt)3:1  0.81731    0.53979   1.514  0.12999   
## factor(sympt)3:2  1.92471    0.77757   2.475  0.01331 * 
## factor(sympt)4:1  1.13224    0.54767   2.067  0.03870 * 
## factor(sympt)4:2  2.45699    0.77530   3.169  0.00153 **
## pb:1             -0.14821    0.07637  -1.941  0.05230 . 
## pb:2             -0.21944    0.07551  -2.906  0.00366 **
## hist2yes:1        1.06544    0.45940   2.319  0.02038 * 
## hist2yes:2        1.36624    0.43752   3.123  0.00179 **
## bse:1             1.05214    0.51499   2.043  0.04105 * 
## bse:2             1.29167    0.52988   2.438  0.01478 * 
## detc22:1         -0.92439    0.71375  -1.295  0.19528   
## detc22:2          0.01702    1.16169   0.015  0.98831   
## detc23:1         -0.69053    0.68712  -1.005  0.31491   
## detc23:2          0.90414    1.12661   0.803  0.42225   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 693.9019 on 806 degrees of freedom
## 
## Log-likelihood: -346.951 on 806 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## Warning: Hauck-Donner effect detected in the following estimate(s):
## '(Intercept):2'
## 
## 
## Reference group is level  3  of the response
```

Variable **sympt** has 4 categories. 


```r
dat.m1 %>% count(sympt)
```

```
## # A tibble: 4 x 2
##   sympt     n
##   <dbl> <int>
## 1     1    40
## 2     2    73
## 3     3   160
## 4     4   139
```

To simulate results as in Applied Logistic Regression book, we recode **sympt** (4 categories) into 2 category factor variable **symptd**. This can be done using the `ifelse()` function


```r
dat.m1 <- dat.m1 %>% mutate(symptd = 
                              ifelse(sympt<3, '<3', '3 or more'))
dat.m1 %>% count(sympt,symptd)
```

```
## # A tibble: 4 x 3
##   sympt symptd        n
##   <dbl> <chr>     <int>
## 1     1 <3           40
## 2     2 <3           73
## 3     3 3 or more   160
## 4     4 3 or more   139
```

Now, we refit the model and name the model as fitmlog4.


```r
fitmlog4 <- vglm(me3 ~ symptd + pb + hist2 + bse + detc2,
                 multinomial, data = dat.m1)
summary(fitmlog4)
```

```
## 
## Call:
## vglm(formula = me3 ~ symptd + pb + hist2 + bse + detc2, family = multinomial, 
##     data = dat.m1)
## 
## Pearson residuals:
##                       Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -1.627 -0.5551 -0.2875 -0.2286 3.621
## log(mu[,2]/mu[,3]) -2.113 -0.5699 -0.2820  0.6939 8.053
## 
## Coefficients: 
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1     -0.99877    1.07197  -0.932 0.351485    
## (Intercept):2     -2.70375    1.43422  -1.885 0.059406 .  
## symptd3 or more:1  1.12136    0.35720   3.139 0.001693 ** 
## symptd3 or more:2  2.09534    0.45739   4.581 4.62e-06 ***
## pb:1              -0.16811    0.07417  -2.266 0.023426 *  
## pb:2              -0.25101    0.07293  -3.442 0.000578 ***
## hist2yes:1         1.01406    0.45381   2.235 0.025446 *  
## hist2yes:2         1.29328    0.43353   2.983 0.002853 ** 
## bse:1              1.02859    0.51398   2.001 0.045366 *  
## bse:2              1.24397    0.52630   2.364 0.018097 *  
## detc22:1          -0.90213    0.71463  -1.262 0.206813    
## detc22:2           0.09028    1.16079   0.078 0.938011    
## detc23:1          -0.66982    0.68759  -0.974 0.329979    
## detc23:2           0.97281    1.12604   0.864 0.387627    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 697.4959 on 810 degrees of freedom
## 
## Log-likelihood: -348.748 on 810 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  3  of the response
```

## Inferences 

We could get the the confidence intervals and the p-values for each covariate. Then we will cbind them together. 


```r
b_fitmlog4 <- coef(fitmlog4)
ci_fitmlog4 <- confint(fitmlog4)
cbind(b_fitmlog4, ci_fitmlog4)
```

```
##                    b_fitmlog4       2.5 %      97.5 %
## (Intercept):1     -0.99876823 -3.09979275  1.10225630
## (Intercept):2     -2.70375032 -5.51476670  0.10726607
## symptd3 or more:1  1.12136493  0.42126810  1.82146176
## symptd3 or more:2  2.09534066  1.19888107  2.99180024
## pb:1              -0.16810623 -0.31348168 -0.02273077
## pb:2              -0.25101212 -0.39395671 -0.10806753
## hist2yes:1         1.01405528  0.12461339  1.90349717
## hist2yes:2         1.29328081  0.44357296  2.14298865
## bse:1              1.02859049  0.02121771  2.03596327
## bse:2              1.24397416  0.21244891  2.27549941
## detc22:1          -0.90213255 -2.30278108  0.49851598
## detc22:2           0.09027536 -2.18484084  2.36539156
## detc23:1          -0.66982206 -2.01747766  0.67783353
## detc23:2           0.97281466 -1.23417339  3.17980271
```

As in the book, we will convert predictor variable `detc` (3 categories) to a binary category variable named as `detcd`. Then, we fit the model with covariate `detcd` and name the model as fitmlog5.


```r
dat.m1 <- dat.m1 %>% mutate(detcd = ifelse(detc<3, '<3', '3 or more'))
fitmlog5 <- vglm(me3 ~ symptd + pb + hist2 + bse + detcd,
                 multinomial, data = dat.m1)
summary(fitmlog5)
```

```
## 
## Call:
## vglm(formula = me3 ~ symptd + pb + hist2 + bse + detcd, family = multinomial, 
##     data = dat.m1)
## 
## Pearson residuals:
##                       Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -1.636 -0.5629 -0.2985 -0.2289 5.004
## log(mu[,2]/mu[,3]) -2.122 -0.5694 -0.2735  0.6927 8.093
## 
## Coefficients: 
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1     -1.82388    0.85509  -2.133 0.032928 *  
## (Intercept):2     -2.62376    0.92639  -2.832 0.004622 ** 
## symptd3 or more:1  1.12742    0.35636   3.164 0.001558 ** 
## symptd3 or more:2  2.09475    0.45742   4.579 4.66e-06 ***
## pb:1              -0.15432    0.07262  -2.125 0.033587 *  
## pb:2              -0.24947    0.07258  -3.437 0.000588 ***
## hist2yes:1         1.06318    0.45284   2.348 0.018885 *  
## hist2yes:2         1.30986    0.43360   3.021 0.002520 ** 
## bse:1              0.95601    0.50734   1.884 0.059515 .  
## bse:2              1.23701    0.52542   2.354 0.018557 *  
## detcd3 or more:1   0.11416    0.31821   0.359 0.719786    
## detcd3 or more:2   0.88518    0.35624   2.485 0.012962 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 699.1326 on 812 degrees of freedom
## 
## Log-likelihood: -349.5663 on 812 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  3  of the response
```

The sequence of codes below will:

1. return the log odds and the RRR 
2. return the $95\%$ CI for the log odds and the CI for RRR
3. combine the objects into a table
4. rename the columns



```r
b_rrr_fitmlog5 <- cbind(coef(fitmlog5), exp(coef(fitmlog5)))
ci_b_rrr.fitmlog5 <- cbind(confint(fitmlog5), exp(confint(fitmlog5)))
res_fitmlog5 <- cbind(b_rrr_fitmlog5, ci_b_rrr.fitmlog5)
colnames(res_fitmlog5) <- c('b', 'rrr', 
                            'lower 95% b','upper 95% b', 
                            'lower 95% rrr' , 'upper b95% rrr')
res_fitmlog5
```

```
##                            b        rrr lower 95% b upper 95% b lower 95% rrr
## (Intercept):1     -1.8238818 0.16139802 -3.49983371 -0.14792981    0.03020241
## (Intercept):2     -2.6237586 0.07252974 -4.43944877 -0.80806848    0.01180244
## symptd3 or more:1  1.1274172 3.08767131  0.42895941  1.82587496    1.53565870
## symptd3 or more:2  2.0947501 8.12341054  1.19821925  2.99128091    3.31420990
## pb:1              -0.1543182 0.85699926 -0.29665201 -0.01198442    0.74330263
## pb:2              -0.2494746 0.77921006 -0.39172650 -0.10722273    0.67588895
## hist2yes:1         1.0631787 2.89556046  0.17562607  1.95073130    1.19199226
## hist2yes:2         1.3098642 3.70567052  0.46002140  2.15970703    1.58410789
## bse:1              0.9560104 2.60129763 -0.03835148  1.95037230    0.96237463
## bse:2              1.2370113 3.44530099  0.20720332  2.26681922    1.23023268
## detcd3 or more:1   0.1141572 1.12092833 -0.50952750  0.73784191    0.60077938
## detcd3 or more:2   0.8851839 2.42343004  0.18697325  1.58339456    1.20559504
##                   upper b95% rrr
## (Intercept):1          0.8624917
## (Intercept):2          0.4457181
## symptd3 or more:1      6.2082246
## symptd3 or more:2     19.9111706
## pb:1                   0.9880871
## pb:2                   0.8983256
## hist2yes:1             7.0338295
## hist2yes:2             8.6685977
## bse:1                  7.0313048
## bse:2                  9.6486617
## detcd3 or more:1       2.0914172
## detcd3 or more:2       4.8714643
```

We can make a *better* table using **knitr** and **kableExtra** packages


```r
library(knitr)
library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

```r
kable(res_fitmlog5, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> b </th>
   <th style="text-align:right;"> rrr </th>
   <th style="text-align:right;"> lower 95% b </th>
   <th style="text-align:right;"> upper 95% b </th>
   <th style="text-align:right;"> lower 95% rrr </th>
   <th style="text-align:right;"> upper b95% rrr </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept):1 </td>
   <td style="text-align:right;"> -1.824 </td>
   <td style="text-align:right;"> 0.161 </td>
   <td style="text-align:right;"> -3.500 </td>
   <td style="text-align:right;"> -0.148 </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> 0.862 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (Intercept):2 </td>
   <td style="text-align:right;"> -2.624 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> -4.439 </td>
   <td style="text-align:right;"> -0.808 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 0.446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> symptd3 or more:1 </td>
   <td style="text-align:right;"> 1.127 </td>
   <td style="text-align:right;"> 3.088 </td>
   <td style="text-align:right;"> 0.429 </td>
   <td style="text-align:right;"> 1.826 </td>
   <td style="text-align:right;"> 1.536 </td>
   <td style="text-align:right;"> 6.208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> symptd3 or more:2 </td>
   <td style="text-align:right;"> 2.095 </td>
   <td style="text-align:right;"> 8.123 </td>
   <td style="text-align:right;"> 1.198 </td>
   <td style="text-align:right;"> 2.991 </td>
   <td style="text-align:right;"> 3.314 </td>
   <td style="text-align:right;"> 19.911 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pb:1 </td>
   <td style="text-align:right;"> -0.154 </td>
   <td style="text-align:right;"> 0.857 </td>
   <td style="text-align:right;"> -0.297 </td>
   <td style="text-align:right;"> -0.012 </td>
   <td style="text-align:right;"> 0.743 </td>
   <td style="text-align:right;"> 0.988 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pb:2 </td>
   <td style="text-align:right;"> -0.249 </td>
   <td style="text-align:right;"> 0.779 </td>
   <td style="text-align:right;"> -0.392 </td>
   <td style="text-align:right;"> -0.107 </td>
   <td style="text-align:right;"> 0.676 </td>
   <td style="text-align:right;"> 0.898 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hist2yes:1 </td>
   <td style="text-align:right;"> 1.063 </td>
   <td style="text-align:right;"> 2.896 </td>
   <td style="text-align:right;"> 0.176 </td>
   <td style="text-align:right;"> 1.951 </td>
   <td style="text-align:right;"> 1.192 </td>
   <td style="text-align:right;"> 7.034 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hist2yes:2 </td>
   <td style="text-align:right;"> 1.310 </td>
   <td style="text-align:right;"> 3.706 </td>
   <td style="text-align:right;"> 0.460 </td>
   <td style="text-align:right;"> 2.160 </td>
   <td style="text-align:right;"> 1.584 </td>
   <td style="text-align:right;"> 8.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bse:1 </td>
   <td style="text-align:right;"> 0.956 </td>
   <td style="text-align:right;"> 2.601 </td>
   <td style="text-align:right;"> -0.038 </td>
   <td style="text-align:right;"> 1.950 </td>
   <td style="text-align:right;"> 0.962 </td>
   <td style="text-align:right;"> 7.031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bse:2 </td>
   <td style="text-align:right;"> 1.237 </td>
   <td style="text-align:right;"> 3.445 </td>
   <td style="text-align:right;"> 0.207 </td>
   <td style="text-align:right;"> 2.267 </td>
   <td style="text-align:right;"> 1.230 </td>
   <td style="text-align:right;"> 9.649 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> detcd3 or more:1 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 1.121 </td>
   <td style="text-align:right;"> -0.510 </td>
   <td style="text-align:right;"> 0.738 </td>
   <td style="text-align:right;"> 0.601 </td>
   <td style="text-align:right;"> 2.091 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> detcd3 or more:2 </td>
   <td style="text-align:right;"> 0.885 </td>
   <td style="text-align:right;"> 2.423 </td>
   <td style="text-align:right;"> 0.187 </td>
   <td style="text-align:right;"> 1.583 </td>
   <td style="text-align:right;"> 1.206 </td>
   <td style="text-align:right;"> 4.871 </td>
  </tr>
</tbody>
</table>


## Prediction

### Predict the log odds

For the model fitmlog1


```r
summary(fitmlog1)
```

```
## 
## Call:
## vglm(formula = me3 ~ hist2, family = multinomial, data = dat.m1)
## 
## Pearson residuals:
##                        Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -0.9287 -0.5410 -0.5410 -0.2162 2.181
## log(mu[,2]/mu[,3]) -1.1315 -0.6165 -0.6165  1.1127 1.812
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1  -1.2505     0.1429  -8.751  < 2e-16 ***
## (Intercept):2  -0.9510     0.1277  -7.446  9.6e-14 ***
## hist2yes:1      1.0093     0.4275   2.361 0.018225 *  
## hist2yes:2      1.2564     0.3747   3.353 0.000798 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 792.3399 on 820 degrees of freedom
## 
## Log-likelihood: -396.17 on 820 degrees of freedom
## 
## Number of Fisher scoring iterations: 4 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  3  of the response
```


The predicted log odds for the first 6 observations:

1.  the predicted log odds for over a year ago vs never in column 1
2.  the predicted log odds for within a year vs never in column 2 


```r
head(predict.vgam(fitmlog1, type = 'link'))
```

```
##   log(mu[,1]/mu[,3]) log(mu[,2]/mu[,3])
## 1         -1.2504928         -0.9509763
## 2         -1.2504928         -0.9509763
## 3         -0.2411621          0.3053816
## 4         -1.2504928         -0.9509763
## 5         -1.2504928         -0.9509763
## 6         -1.2504928         -0.9509763
```

You can verify these prediction manually. For example the calculations for:

1.  the 1st observation log odds
2.  the 3rd observation log odds 


```r
head(dat.m1)[1:3,]
```

```
## # A tibble: 3 x 13
##     obs    me sympt    pb  hist   bse  detc me2   hist2 me3   detc2 symptd detcd
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <fct> <fct> <fct> <fct> <chr>  <chr>
## 1     1     0     3     7     0     1     2 never no    never 2     3 or ~ <3   
## 2     2     0     2    11     0     1     3 never no    never 3     <3     3 or~
## 3     3     0     3     8     1     1     3 never yes   never 3     3 or ~ 3 or~
```

The values for the 

- the 1st observation are hist = 0
- the 3rd observation are hist = 1



```r
# ptn 1: hist2 = no
# logit me3=over a year ago = [1]  vs me3= never =[3]
#-1.2504928 +  1.0093308*0
-1.2504928 +  1.0093308*0
```

```
## [1] -1.250493
```

```r
# logit me3=within a year = [2]  vs me3= never =[3]
# -0.9509763 + 1.2563579 *0
-0.9509763 + 1.2563579 *0
```

```
## [1] -0.9509763
```

```r
# ptn 3: hist2 = yes
# logit me3=over a year ago = [1]  vs me3= never =[3]
# -1.2504928 +  1.0093308*1
-1.2504928 +  1.0093308*1
```

```
## [1] -0.241162
```

```r
# logit me3=within a year = [2]  vs me3= never =[3]
# -0.9509763 + 1.2563579*1
-0.9509763 + 1.2563579*1
```

```
## [1] 0.3053816
```

### Predict the probability 

The predicted probability for the first 6 observation


```r
head(predict.vgam(fitmlog1, type = 'response'))
```

```
##   over.a.year.ago within.a.year     never
## 1       0.1711957     0.2309783 0.5978261
## 2       0.1711957     0.2309783 0.5978261
## 3       0.2500000     0.4318182 0.3181818
## 4       0.1711957     0.2309783 0.5978261
## 5       0.1711957     0.2309783 0.5978261
## 6       0.1711957     0.2309783 0.5978261
```

Manual calculation for probability. Let us take the first observation where,

1.  log odds for group over a year ago: -1.2504928         
2.  log odds for group within a year: -0.9509763


```r
# probability being in the reference group (me3 == never = [3])
# 1/(1 + exp(-1.2504928) + exp(-0.9509763))
1/(1 + exp(-1.2504928) + exp(-0.9509763))
```

```
## [1] 0.5978261
```

```r
# probability being in the over a year ago group (me3 == over a year ago = [2])
# exp(-1.2504928)/(1 + exp(-1.2504928) + exp(-0.9509763))
exp(-1.2504928)/(1 + exp(-1.2504928) + exp(-0.9509763))
```

```
## [1] 0.1711957
```

```r
# probability being in the within a year group (me3 == within a year = [1])
# exp(-0.9509763)/(1 + exp(-1.2504928) + exp(-0.9509763))
exp(-0.9509763)/(1 + exp(-1.2504928) + exp(-0.9509763))
```

```
## [1] 0.2309783
```

# Practical 3 

## Using multinom from nnet package

Unlike VGAM::vglm function - where the reference or the base outcome is the largest group (level) - the nnet::multinom uses the smallest group (level) as the reference or base outcome. 


```r
library(nnet)
mlog_nnet <- multinom(me3 ~ hist2, data = dat.m1)
```

```
## # weights:  9 (4 variable)
## initial  value 452.628263 
## final  value 396.169969 
## converged
```

```r
summary(mlog_nnet)
```

```
## Call:
## multinom(formula = me3 ~ hist2, data = dat.m1)
## 
## Coefficients:
##               (Intercept)   hist2yes
## within.a.year   0.2995173  0.2470274
## never           1.2504941 -1.0093349
## 
## Std. Errors:
##               (Intercept) hist2yes
## within.a.year   0.1662460 0.413737
## never           0.1428933 0.427500
## 
## Residual Deviance: 792.3399 
## AIC: 800.3399
```

## Comparing objects from VGAM::vglm and nnet::multinom

We can use `relevel()` to change the reference category


```r
dat.m1 <- dat.m1 %>%
  mutate(me3_relev = relevel(me3, ref = "never"))
levels(dat.m1$me3_relev)
```

```
## [1] "never"           "over.a.year.ago" "within.a.year"
```

So, running multinom will give this


```r
mlog_nnet_rel <- multinom(me3_relev ~ hist2, data = dat.m1)
```

```
## # weights:  9 (4 variable)
## initial  value 452.628263 
## iter  10 value 396.169969
## iter  10 value 396.169969
## final  value 396.169969 
## converged
```

```r
summary(mlog_nnet_rel)
```

```
## Call:
## multinom(formula = me3_relev ~ hist2, data = dat.m1)
## 
## Coefficients:
##                 (Intercept) hist2yes
## over.a.year.ago  -1.2504803 1.009384
## within.a.year    -0.9509794 1.256531
## 
## Std. Errors:
##                 (Intercept)  hist2yes
## over.a.year.ago   0.1428926 0.4275097
## within.a.year     0.1277115 0.3746633
## 
## Residual Deviance: 792.3399 
## AIC: 800.3399
```

And running vglm, we will get this


```r
summary(fitmlog1)
```

```
## 
## Call:
## vglm(formula = me3 ~ hist2, family = multinomial, data = dat.m1)
## 
## Pearson residuals:
##                        Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -0.9287 -0.5410 -0.5410 -0.2162 2.181
## log(mu[,2]/mu[,3]) -1.1315 -0.6165 -0.6165  1.1127 1.812
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1  -1.2505     0.1429  -8.751  < 2e-16 ***
## (Intercept):2  -0.9510     0.1277  -7.446  9.6e-14 ***
## hist2yes:1      1.0093     0.4275   2.361 0.018225 *  
## hist2yes:2      1.2564     0.3747   3.353 0.000798 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 792.3399 on 820 degrees of freedom
## 
## Log-likelihood: -396.17 on 820 degrees of freedom
## 
## Number of Fisher scoring iterations: 4 
## 
## No Hauck-Donner effect found in any of the estimates
## 
## 
## Reference group is level  3  of the response
```

```r
confint(fitmlog1)
```

```
##                    2.5 %     97.5 %
## (Intercept):1 -1.5305586 -0.9704270
## (Intercept):2 -1.2012851 -0.7006674
## hist2yes:1     0.1714464  1.8472152
## hist2yes:2     0.5220375  1.9906784
```

## Geeting p-values from nnet::multinom


```r
z.test <- summary(mlog_nnet_rel)$coefficients/summary(mlog_nnet_rel)$standard.errors
# 2-tailed
p.val <- (1 - pnorm(abs(z.test), 0, 1)) * 2
library(readxl)
colnames(p.val) <- c('p-val intercept', 'p-val hist')
p.val
```

```
##                 p-val intercept   p-val hist
## over.a.year.ago    0.000000e+00 0.0182218315
## within.a.year      9.592327e-14 0.0007972127
```

## CI for nnet::multinom


```r
confint(mlog_nnet_rel, level=0.95)
```

```
## , , over.a.year.ago
## 
##                  2.5 %     97.5 %
## (Intercept) -1.5305447 -0.9704159
## hist2yes     0.1714807  1.8472880
## 
## , , within.a.year
## 
##                  2.5 %     97.5 %
## (Intercept) -1.2012893 -0.7006696
## hist2yes     0.5222045  1.9908576
```


# Practical 4 (Optional)

## Data

Next, analyse data from <https://onlinecourses.science.psu.edu/stat504/book/export/html/171> 

The dataset in a `.txt` file is named **gator.txt**. Some facts about the data - Researchers classified the stomach contents of 219 captured alligators into five categories: 

1. Fish (the most common primary food choice)
2. Invertebrate (snails, insects, crayfish, etc.)
3. Reptile (turtles, alligators)
4. Bird
5. and Other (amphibians, plants, household pets, stones, and other debris).

Because the usual primary food choice of alligators appears to be fish, we'll use fish as the baseline category; the four logit equations will then describe the log-odds that alligators select other primary food types instead of fish.

Now, we will use the **gator** data. WARNING, the data are in the wide format. 


```r
gator <- read.table('gator.txt', header=TRUE)
head(gator)
```

```
##   profile Gender Size    Lake Fish Invertebrate Reptile Bird Other
## 1       1      f <2.3  george    3            9       1    0     1
## 2       2      m <2.3  george   13           10       0    2     2
## 3       3      f >2.3  george    8            1       0    0     1
## 4       4      m >2.3  george    9            0       0    1     2
## 5       5      f <2.3 hancock   16            3       2    2     3
## 6       6      m <2.3 hancock    7            1       0    0     5
```

One of the main predictor (covariate/independent variable) is **Lake**


```r
gator <- gator %>% mutate(Lake = factor(Lake))
levels(gator$Lake)
```

```
## [1] "george"   "hancock"  "oklawaha" "trafford"
```

Different software might treat different level as its baseline level. For example, let's examine *Lake* variable

In this above function, the baseline category is *george*. To replicate the results on the webpage, we assign *hancock* as the baseline category (level) for the predictor variable *Lake*. To do that, we:


```r
contrasts(gator$Lake) <- contr.treatment(levels(gator$Lake), base=1)
contrasts(gator$Lake)
```

```
##          hancock oklawaha trafford
## george         0        0        0
## hancock        1        0        0
## oklawaha       0        1        0
## trafford       0        0        1
```

```r
levels(gator$Lake)
```

```
## [1] "george"   "hancock"  "oklawaha" "trafford"
```

## Estimation

from VGAM package

For the outcome, we can specify the level for the baseline in the **vglm** arguments


```r
fit.gat<-vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Gender, data=gator, family=multinomial)
summary(fit.gat)
```

```
## 
## Call:
## vglm(formula = cbind(Bird, Invertebrate, Reptile, Other, Fish) ~ 
##     Lake + Size + Gender, family = multinomial, data = gator)
## 
## Pearson residuals:
##                        Min      1Q   Median     3Q   Max
## log(mu[,1]/mu[,5]) -1.1985 -0.5478 -0.22421 0.3678 3.478
## log(mu[,2]/mu[,5]) -1.3218 -0.4611  0.01054 0.3810 1.866
## log(mu[,3]/mu[,5]) -0.7033 -0.5751 -0.35511 0.2610 2.064
## log(mu[,4]/mu[,5]) -1.6945 -0.2893 -0.10807 1.1236 1.367
## 
## Coefficients: 
##                Estimate Std. Error z value Pr(>|z|)   
## (Intercept):1  -2.43211    0.77066      NA       NA   
## (Intercept):2   0.16902    0.37875   0.446  0.65541   
## (Intercept):3  -3.41604    1.08513      NA       NA   
## (Intercept):4  -1.43073    0.53809  -2.659  0.00784 **
## Lakehancock:1   0.57527    0.79522   0.723  0.46943   
## Lakehancock:2  -1.78051    0.62321  -2.857  0.00428 **
## Lakehancock:3   1.12946    1.19280   0.947  0.34369   
## Lakehancock:4   0.76658    0.56855   1.348  0.17756   
## Lakeoklawaha:1 -0.55035    1.20980  -0.455  0.64917   
## Lakeoklawaha:2  0.91318    0.47612   1.918  0.05511 . 
## Lakeoklawaha:3  2.53026    1.12211   2.255  0.02414 * 
## Lakeoklawaha:4  0.02606    0.77776   0.034  0.97327   
## Laketrafford:1  1.23699    0.86610   1.428  0.15322   
## Laketrafford:2  1.15582    0.49279   2.345  0.01900 * 
## Laketrafford:3  3.06105    1.12972   2.710  0.00674 **
## Laketrafford:4  1.55776    0.62567   2.490  0.01278 * 
## Size>2.3:1      0.73024    0.65228   1.120  0.26292   
## Size>2.3:2     -1.33626    0.41119  -3.250  0.00116 **
## Size>2.3:3      0.55704    0.64661   0.861  0.38898   
## Size>2.3:4     -0.29058    0.45993  -0.632  0.52751   
## Genderm:1      -0.60643    0.68884  -0.880  0.37867   
## Genderm:2      -0.46296    0.39552  -1.171  0.24180   
## Genderm:3      -0.62756    0.68528  -0.916  0.35978   
## Genderm:4      -0.25257    0.46635  -0.542  0.58810   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,5]), log(mu[,2]/mu[,5]), 
## log(mu[,3]/mu[,5]), log(mu[,4]/mu[,5])
## 
## Residual deviance: 50.2637 on 40 degrees of freedom
## 
## Log-likelihood: -73.3221 on 40 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## Warning: Hauck-Donner effect detected in the following estimate(s):
## '(Intercept):1', '(Intercept):3'
## 
## 
## Reference group is level  5  of the response
```

The results tell us that the four logit equations predict the log-odds of:

1.  birds versus fish (and from the R output: 1 vs 5),
2.  invertebrates versus fish (2 vs 5),
3.  other versus fish (4 vs 5), and
4.  reptiles versus fish (3 vs 5).

## Interpretation

The intercepts give the estimated log-odds for the reference group that is:
lake = Hancock, size = small, sex = male. 

Now, we see how three stat packages work

1.  vglm::VGAM
2.  multinom::nnet
3.  mlogit in STATA


```r
library(foreign)
dat.m2 <- read.dta('APS.dta', convert.factors = TRUE)
names(dat.m2)
```

```
##  [1] "id"      "place"   "place3"  "age"     "race"    "gender"  "neuro"  
##  [8] "emot"    "danger"  "elope"   "los"     "behav"   "custd"   "viol"   
## [15] "LOS5"    "lc"      "dangerd"
```

```r
summary(dat.m2)
```

```
##        id            place        place3         age             race       
##  Min.   :  1.0   Min.   :0.000   zero:259   Min.   :11.06   Min.   :0.0000  
##  1st Qu.:127.8   1st Qu.:0.000   one :130   1st Qu.:12.92   1st Qu.:0.0000  
##  Median :254.5   Median :1.000   two :119   Median :14.18   Median :1.0000  
##  Mean   :254.5   Mean   :1.419              Mean   :14.27   Mean   :0.5472  
##  3rd Qu.:381.2   3rd Qu.:2.000              3rd Qu.:15.51   3rd Qu.:1.0000  
##  Max.   :508.0   Max.   :3.000              Max.   :17.92   Max.   :1.0000  
##      gender           neuro             emot            danger     
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :2.000  
##  Mean   :0.4606   Mean   :0.5571   Mean   :0.2343   Mean   :2.006  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:3.000  
##  Max.   :1.0000   Max.   :3.0000   Max.   :1.0000   Max.   :3.000  
##      elope            los             behav           custd      
##  Min.   :0.000   Min.   :  1.00   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:  6.00   1st Qu.:5.000   1st Qu.:0.000  
##  Median :0.000   Median :  8.00   Median :6.000   Median :0.000  
##  Mean   :0.378   Mean   : 21.83   Mean   :5.878   Mean   :0.376  
##  3rd Qu.:1.000   3rd Qu.: 17.00   3rd Qu.:7.000   3rd Qu.:1.000  
##  Max.   :1.000   Max.   :305.00   Max.   :9.000   Max.   :1.000  
##       viol             LOS5              lc             dangerd      
##  Min.   :0.0000   Min.   : 1.000   Min.   :  0.000   Min.   :0.0000  
##  1st Qu.:1.0000   1st Qu.: 2.449   1st Qu.:  0.000   1st Qu.:1.0000  
##  Median :1.0000   Median : 2.828   Median :  0.000   Median :1.0000  
##  Mean   :0.7618   Mean   : 3.873   Mean   :  8.065   Mean   :0.8937  
##  3rd Qu.:1.0000   3rd Qu.: 4.123   3rd Qu.:  7.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :17.464   Max.   :230.000   Max.   :1.0000
```

```r
levels(dat.m2$place3)
```

```
## [1] "zero" "one"  "two"
```

Now, we will change levels to one,two VS zero (base outcome)


```r
library(VGAM)
dat.m2$place3b <- factor(dat.m2$place3, c("one","two","zero"))
levels(dat.m2$place3b)
```

```
## [1] "one"  "two"  "zero"
```

```r
fit.m2 <- vglm(place3b ~ age + race + dangerd + behav +
                 custd, multinomial, data = dat.m2)
summary(fit.m2)
```

```
## 
## Call:
## vglm(formula = place3b ~ age + race + dangerd + behav + custd, 
##     family = multinomial, data = dat.m2)
## 
## Pearson residuals:
##                       Min      1Q  Median      3Q   Max
## log(mu[,1]/mu[,3]) -3.181 -0.3779 -0.2438  0.5179 4.221
## log(mu[,2]/mu[,3]) -2.990 -0.5043 -0.3083 -0.1014 4.828
## 
## Coefficients: 
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1 -7.14964    1.52405  -4.691 2.72e-06 ***
## (Intercept):2 -8.80496    1.45015  -6.072 1.27e-09 ***
## age:1          0.20014    0.08722   2.295  0.02175 *  
## age:2          0.26049    0.07938   3.281  0.00103 ** 
## race:1         0.52611    0.29135   1.806  0.07095 .  
## race:2         0.43590    0.26105   1.670  0.09496 .  
## dangerd:1      1.37313    0.59737   2.299  0.02152 *  
## dangerd:2      0.40490    0.61781   0.655  0.51222    
## behav:1        0.07010    0.08433   0.831  0.40586    
## behav:2        0.50722    0.08601   5.898 3.69e-09 ***
## custd:1        3.84509    0.32439  11.853  < 2e-16 ***
## custd:2        2.12246    0.30463   6.967 3.23e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
## 
## Residual deviance: 751.5588 on 1004 degrees of freedom
## 
## Log-likelihood: -375.7794 on 1004 degrees of freedom
## 
## Number of Fisher scoring iterations: 5 
## 
## Warning: Hauck-Donner effect detected in the following estimate(s):
## '(Intercept):2'
## 
## 
## Reference group is level  3  of the response
```

To obtain the rrr


```r
exp(coef(fit.m2))
```

```
## (Intercept):1 (Intercept):2         age:1         age:2        race:1 
##  7.851470e-04  1.499871e-04  1.221574e+00  1.297572e+00  1.692345e+00 
##        race:2     dangerd:1     dangerd:2       behav:1       behav:2 
##  1.546347e+00  3.947705e+00  1.499150e+00  1.072611e+00  1.660674e+00 
##       custd:1       custd:2 
##  4.676288e+01  8.351698e+00
```

## Estimation from the nnet package

Look, how the results we obtained from nnet package. Of all, I guess the results from multinom::nnet are the easiest.


```r
library(nnet)
fit.m3<-multinom(place3 ~ age + race + dangerd + behav +
                 custd, data = dat.m2)
```

```
## # weights:  21 (12 variable)
## initial  value 558.095043 
## iter  10 value 397.312630
## iter  20 value 375.779379
## iter  20 value 375.779377
## iter  20 value 375.779377
## final  value 375.779377 
## converged
```

```r
summary(fit.m3)
```

```
## Call:
## multinom(formula = place3 ~ age + race + dangerd + behav + custd, 
##     data = dat.m2)
## 
## Coefficients:
##     (Intercept)       age      race  dangerd      behav    custd
## one   -7.149391 0.2001143 0.5261018 1.373146 0.07011865 3.845105
## two   -8.805023 0.2604802 0.4359178 0.404981 0.50725083 2.122484
## 
## Std. Errors:
##     (Intercept)        age      race   dangerd      behav     custd
## one    1.524047 0.08722094 0.2913487 0.5973691 0.08433085 0.3243931
## two    1.450168 0.07938364 0.2610523 0.6178308 0.08600733 0.3046367
## 
## Residual Deviance: 751.5588 
## AIC: 775.5588
```

# Comparisons between packages and software

1.  Results from VGAM::vglm and nnet::multinom are similar. 
2.  In Stata, we run "mlogit place3 age race dangerd behav los custd". The results are easier to look at
3.  **mlogit** package in R requires data to be in wide format
4.  `nnet::multinom` does not provide p-values 

# References

1.  <https://stats.idre.ucla.edu/stata/examples/alr2/applied-logistic-regression-second-edition-by-hosmer-and-lemeshowchapter-8-special-topics/m>
2.  <https://onlinecourses.science.psu.edu/stat504/node/171>     
3.  <https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/>
4.  <https://cran.r-project.org/web/packages/mlogit/vignettes/mlogit.pdf>
5.  Long JS, Freese J. Models for Nominal Outcomes In Regression Models for Categorical Dependent Variables Using Stata
6.  Hosmer DW Jr, Lemeshow S, Sturdivant RX. Special Topics In Applied Logistic Regression

