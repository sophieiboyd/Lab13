Lab 13 - Colonizing Mars
================
Sophie Boyd
3-27-26

### Load packages and data

Having trouble installing MASS… come back later if needed

``` r
library(tidyverse) 
library(ggplot2)
```

### Exercise 1: Simulating our colonists

#### 1.1

``` r
set.seed(123)
age <- rnorm(100, mean = 30, sd = 5)
```

``` r
df_colonists <- data.frame(
  id = 1:100,
  age = rnorm(100, mean = 30, sd = 5)
)
```

``` r
df_colonists %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth=5) +
  labs(x = 'Age', 
       y = 'Frequency')
```

![](lab-13_files/figure-gfm/age-dist-1.png)<!-- -->

Age follows a normal distribution, centered at around 30 years old. The
spread of the distribution is the same regardless of seed because the
standard deviation was set at the same value each time.

#### 1.2

``` r
set.seed(1)
roleA <- rep(c("engineer", "scientist", "medic"),
  length.out = 100
)
# this works if we want to set a specific number of each role
roleB <- rep(c("engineer", "scientist", "medic"),
  each = 34,
  length.out = 100
)

roleC <- rep(c("engineer", "scientist", "medic"),
  times = c(33, 33, 33),
  length.out = 100
)

# if you want to use sampling weights
roleD <- sample(c("engineer", "scientist", "medic"),
  replace = TRUE,
  size = 100, prob = c(1, 1, 1)
)

# if you want to randomly shuffle

roleE <- sample(roleB, size = 100, replace = FALSE)
```

#### 1.3

I wanted about equal numbers of engineers, medics, and scientists, so I
used method C.

#### 1.4

``` r
set.seed(123)

df_colonists$marsgar <- runif(n = 100, min = 0, max = 100)
```

``` r
df_colonists %>%
  ggplot(aes(x = age, y = marsgar)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-13_files/figure-gfm/marsgar-by-age-1.png)<!-- -->

No relationship between age and MARSGAR score.

### Exercise 2: Growing our colonists
