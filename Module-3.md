Risk Management with R - Module 3 - Non-Normal Distributions
================

The code from the course was adapted using the
[**tidyquant**](https://business-science.github.io/tidyquant/index.html)
package as a way to better understand its workflow and functions.

## Libraries

``` r
library(tidyverse)
library(quantmod)
library(tidyquant)
library(skimr)
library(moments)
library(metRology)
library(MASS)
```

## 1\. Non-normal Distributions

Loading Data

``` r
#security = "GOLDPMGBD228NLBM"

security = "WILL5000IND"

wilsh <- tq_get(security, get = "economic.data", from = "1979-12-31", to  = "2017-12-31")

wilsh <- wilsh %>% 
  na.omit()

log_returns <- wilsh %>%
    group_by(symbol) %>%
    tq_transmute(select = price, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 col_rename = "d_return")
```

Analyzing returns:

``` r
skim(log_returns)
```

|                                                  |              |
| :----------------------------------------------- | :----------- |
| Name                                             | log\_returns |
| Number of rows                                   | 9584         |
| Number of columns                                | 3            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |              |
| Column type frequency:                           |              |
| Date                                             | 1            |
| numeric                                          | 1            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |              |
| Group variables                                  | symbol       |

Data summary

**Variable type:
Date**

| skim\_variable | symbol      | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | :---------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| date           | WILL5000IND |          0 |              1 | 1979-12-31 | 2017-12-29 | 1998-12-14 |      9584 |

**Variable type:
numeric**

| skim\_variable | symbol      | n\_missing | complete\_rate | mean |   sd |     p0 | p25 | p50 |  p75 | p100 | hist  |
| :------------- | :---------- | ---------: | -------------: | ---: | ---: | -----: | --: | --: | ---: | ---: | :---- |
| d\_return      | WILL5000IND |          0 |              1 |    0 | 0.01 | \-0.17 |   0 |   0 | 0.01 | 0.11 | ▁▁▅▇▁ |

``` r
#mean
(mu = mean(log_returns$d_return))
```

    ## [1] 0.0004930988

``` r
#standard deviation
(sig = sd(log_returns$d_return))
```

    ## [1] 0.01067945

``` r
log_returns %>% 
  ggplot(aes(d_return)) +
  geom_density() +
  labs(title = "Distribution of Returns") + 
  theme_tq()
```

![](Module-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### 1.1. Skewness

  - sk = 0 : symmetric (normal)
  - sk \< 0 : left-skewed (left tail \> right tail)
  - sk \> 0 : right-skewed

<!-- end list -->

``` r
rvec <- as.vector(log_returns$d_return)

(skewness(rvec))
```

    ## [1] -0.6113245

### 1.2 Kurtosis

  - Heavy-tailed distributions are called **leptokurtic**, and have
    fatter tails than the normal distribution.

  - Thin-tailed distributions are called **platykurtic**, and aren’t
    very typical.

  - krt = 3 : normal

  - sk \< 3 : thin-tailed

  - sk \> 3 : heavy-tailed

<!-- end list -->

``` r
(kurtosis(rvec))
```

    ## [1] 18.79145

### 1.3 Tests for normality

``` r
jarque.test(rvec) #p-value = 0: reject normality
```

    ## 
    ##  Jarque-Bera Normality Test
    ## 
    ## data:  rvec
    ## JB = 100179, p-value < 2.2e-16
    ## alternative hypothesis: greater

``` r
#qqplot()
```

## 2\. Rescaled t-distribution

``` r
n_out <- 100000

alpha <- 0.05

set.seed(123789)

t.fit <- fitdistr(rvec,"t")
```

    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced

    ## Warning in log(s): NaNs produced

    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced

    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced

    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced

    ## Warning in log(s): NaNs produced

    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced

    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced
    
    ## Warning in log(s): NaNs produced

    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced
    
    ## Warning in dt((x - m)/s, df, log = TRUE): NaNs produced

``` r
rvec <- rt.scaled(n_out, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3])

var <- quantile(rvec, alpha)

es <- mean(rvec[rvec < var])
```

## 3\. VaR and ES

### 3.1. Student-t Simulation - Sum of 10 1-day return

``` r
set.seed(123789)

rvec <- rep(0,n_out)

for (i in 1:10){
  
  rvec <- rvec + rt.scaled(n_out, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3])
  
}

(var <- quantile(rvec, alpha))
```

    ##          5% 
    ## -0.04484815

``` r
(es <- mean(rvec[rvec < var]))
```

    ## [1] -0.06390778

### 3.2. IID Simulation - Sum of 10 1-day return

Draw results from the data for simulation

``` r
set.seed(123789)

rvec <- rep(0,n_out)



for (i in 1:10){
  rvec <- rvec + sample( as.vector(log_returns$d_return), n_out, replace = TRUE)
}

(var <- quantile(rvec, alpha))
```

    ##          5% 
    ## -0.05004264

``` r
(es <- mean(rvec[rvec < var]))
```

    ## [1] -0.07066367

### 3.3. Block Simulation - Sum of 10 consecutive 1-day return

Draw results from the data for simulation

``` r
n_out <- 100000

alpha <- 0.05

set.seed(123789)

rvec <- rep(0,n_out)

rdat <-  as.vector(log_returns$d_return)

posn <- seq(from = 1, to = length(rdat)-9, by = 1)

rpos <- sample(posn, n_out, replace = TRUE)


for (i in 1:10){
  rvec <- rvec + rdat[rpos]
  rpos = rpos + 1
}

(var <- quantile(rvec, alpha))
```

    ##          5% 
    ## -0.04564817

``` r
(es <- mean(rvec[rvec < var]))
```

    ## [1] -0.07471419
