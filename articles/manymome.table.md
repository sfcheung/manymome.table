# manymome.table

## Introduction

This article is a brief illustration of how convert some results from
the package [manymome](https://sfcheung.github.io/manymome/index.html)
([Cheung & Cheung, 2024](https://doi.org/10.3758/s13428-023-02224-z)) to
publication-ready tables using the functions from
[manymome.table](https://sfcheung.github.io/manymome.table/index.html).
It assumes readers have used `manymome`. This guide will focus on
converting the results using the
[`as_flextable()`](https://davidgohel.github.io/flextable/reference/as_flextable.html)
method.

## Several Indirect Effects

The example from [this
article](https://sfcheung.github.io/manymome/articles/manymome.html)
will be used, with some modifications.

This is the sample data set from `manymome`:

``` r

library(manymome)
dat <- data_serial
print(head(dat), digits = 3)
#>       x   m1    m2     y        c1   c2
#> 1 12.12 20.6  9.33  9.00  0.109262 6.01
#> 2  9.81 18.2  9.47 11.56 -0.124014 6.42
#> 3 10.11 20.3 10.05  9.35  4.278608 5.34
#> 4 10.07 19.7 10.17 11.41  1.245356 5.59
#> 5 11.91 20.5 10.05 14.26 -0.000932 5.34
#> 6  9.13 16.5  8.93 10.01  1.802727 5.91
```

Th following model is fitted in `lavaan`:

``` r

library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
mod_med <- "
m1 ~ x
m2 ~ m1 + x
y ~ m2 + m1 + x
"
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)
```

Use
[`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.html)
to identify all indirect paths:

``` r

all_paths <- all_indirect_paths(fit = fit_med,
                                x = "x",
                                y = "y")
all_paths
#> Call: 
#> all_indirect_paths(fit = fit_med, x = "x", y = "y")
#> Path(s): 
#>   path              
#> 1 x -> m1 -> m2 -> y
#> 2 x -> m1 -> y      
#> 3 x -> m2 -> y
```

Estimate the indirect effects, with bootstrap confidence intervals.

``` r

# R set to 100 just for illustration.
# Use 5000 or 10000 and set parallel to TRUE in real research.
out_all <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE,
                                 boot_ci = TRUE,
                                 R = 100,
                                 seed = 12345,
                                 parallel = FALSE,
                                 progress = FALSE)
out_all
#> 
#> == Indirect Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
#> 
#>                       std  CI.lo  CI.hi Sig
#> x -> m1 -> m2 -> y  0.119  0.016  0.223 Sig
#> x -> m1 -> y       -0.163 -0.307 -0.039 Sig
#> x -> m2 -> y       -0.058 -0.208  0.024    
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 100 samples.
#>  - std: The standardized indirect effect(s).
#> 
```

The method
[`as_flextable()`](https://davidgohel.github.io/flextable/reference/as_flextable.html)
can then be used to convert the output to a flextable. To use this
method, we need to load the package `manymome.table` first.

``` r

library(manymome.table)
ft_all <- as_flextable(out_all)
ft_all
```

| Path | Effect | Std. Effect | 95% CI |  | SE |
|----|----|----|----|----|----|
| x → m1 → m2 → y | 0.25 | 0.12 | \[0.02 | , 0.22\] | 0.05 |
| x → m1 → y | -0.34 | -0.16 | \[-0.31 | , -0.04\] | 0.07 |
| x → m2 → y | -0.12 | -0.06 | \[-0.21 | , 0.02\] | 0.05 |
| x → .. → y | -0.21 | -0.10 | \[-0.29 | , 0.04\] | 0.07 |
| Note: CI = confidence interval; paths with '..' are total indirect effects; Std. Effect is completely standardized effect. |  |  |  |  |  |

By default, if standardized effects are requested, the unstandardized
effects will also be printed when converting to a flextable.

Not demonstrated here due to speed concern, it also supports output with
confidence intervals.

See
[`help(as_flextable.indirect_list)`](https://sfcheung.github.io/manymome.table/reference/as_flextable.indirect_list.md)
for more information on the options available in the conversion.

## Conditional Indirect Effects

The example from [this
article](https://sfcheung.github.io/manymome/articles/manymome.html)
will be used, with some modifications.

This is the sample data set from \`manymome:

``` r

dat <- data_med_mod_ab
print(head(dat), digits = 3)
#>       x   w1   w2    m    y    c1   c2
#> 1  9.27 4.97 2.66 3.46 8.80  9.26 3.14
#> 2 10.79 4.13 3.33 4.05 7.37 10.71 5.80
#> 3 11.10 5.91 3.32 4.04 8.24 10.60 5.45
#> 4  9.53 4.78 2.32 3.54 8.37  9.22 3.83
#> 5 10.00 4.38 2.95 4.65 8.39  9.58 4.26
#> 6 12.25 5.81 4.04 4.73 9.65  9.51 4.01
```

For illustration, OLS regression is used instead of structural equation
modeling to fit the model:

``` r

m ~ x + w1 + w1x + c1 + c2
#> m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
#> y ~ m + w2 + w2m + x + c1 + c2
lm_m <- lm(m ~ x*w1, dat)
lm_y <- lm(y ~ m*w2 + x, dat)
lm_out <- lm2list(lm_m, lm_y)
```

Compute conditional indirect effects:

``` r

# R set to 100 just for illustration.
# Use 5000 or 10000 and set parallel to TRUE in real research.
out_cond <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                  x = "x",
                                  y = "y",
                                  m = "m",
                                  fit = lm_out,
                                  standardized_x = TRUE,
                                  standardized_y = TRUE,
                                  boot_ci = TRUE,
                                  R = 100,
                                  seed = 12345,
                                  parallel = FALSE,
                                  progress = FALSE)
out_cond
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w1, w2
#>  Moderator(s) represented by: w1, w2
#> 
#>      [w1]    [w2]  (w1)  (w2)   std  CI.lo CI.hi Sig   m~x   y~m   ind
#> 1 M+1.0SD M+1.0SD 6.173 4.040 0.412  0.110 0.685 Sig 0.599 0.685 0.410
#> 2 M+1.0SD M-1.0SD 6.173 2.055 0.182 -0.021 0.437     0.599 0.302 0.181
#> 3 M-1.0SD M+1.0SD 4.038 4.040 0.119 -0.089 0.393     0.173 0.685 0.118
#> 4 M-1.0SD M-1.0SD 4.038 2.055 0.052 -0.042 0.203     0.173 0.302 0.052
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 100 samples.
#>  - std: The standardized conditional indirect effects. 
#>  - ind: The unstandardized conditional indirect effects.
#>  - 'm~x','y~m' is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
```

The method
[`as_flextable()`](https://davidgohel.github.io/flextable/reference/as_flextable.html)
can then be used to convert the output to a flextable.

``` r

library(manymome.table)
ft_cond <- as_flextable(out_cond)
ft_cond
```

| Path: x → m → y |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|
| \[w1\] | \[w2\] | (w1) | (w2) | Effect | Std. Effect | 95% CI |  | SE |
| M+1.0SD | M+1.0SD | 6.17 | 4.04 | 0.41 | 0.41 | \[0.11 | , 0.68\] | 0.15 |
| M+1.0SD | M-1.0SD | 6.17 | 2.06 | 0.18 | 0.18 | \[-0.02 | , 0.44\] | 0.11 |
| M-1.0SD | M+1.0SD | 4.04 | 4.04 | 0.12 | 0.12 | \[-0.09 | , 0.39\] | 0.12 |
| M-1.0SD | M-1.0SD | 4.04 | 2.06 | 0.05 | 0.05 | \[-0.04 | , 0.20\] | 0.07 |
| Note: \[w\] is the meaning of a level of moderator 'w'; (w) is the value of a level of moderator 'w': CI = confidence interval; Std. Effect is completely standardized effect. |  |  |  |  |  |  |  |  |

By default, if standardized effects are requested, the unstandardized
effects will also be printed when converting to a flextable.

Not demonstrated here due to speed concern, it also supports output with
confidence intervals.

See
[`help(as_flextable.cond_indirect_effects)`](https://sfcheung.github.io/manymome.table/reference/as_flextable.cond_indirect_effects.md)
for more information on the options available in the conversion.

## Other Features

### Further Processing by `flextable`

The output of both methods is a flextable object. Therefore, it can be
further modified by functions for flextable. Load the package
`flextable` first to use its functions.

For example:

``` r

library(flextable)
ft_cond2 <- ft_cond |>
              bold(part = "header") |>
              bg(i = c(1, 2), bg = "lightblue", part = "body") |>
              bg(i = c(3, 4), bg = "lightgreen", part = "body")
ft_cond2
```

| Path: x → m → y |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|
| \[w1\] | \[w2\] | (w1) | (w2) | Effect | Std. Effect | 95% CI |  | SE |
| M+1.0SD | M+1.0SD | 6.17 | 4.04 | 0.41 | 0.41 | \[0.11 | , 0.68\] | 0.15 |
| M+1.0SD | M-1.0SD | 6.17 | 2.06 | 0.18 | 0.18 | \[-0.02 | , 0.44\] | 0.11 |
| M-1.0SD | M+1.0SD | 4.04 | 4.04 | 0.12 | 0.12 | \[-0.09 | , 0.39\] | 0.12 |
| M-1.0SD | M-1.0SD | 4.04 | 2.06 | 0.05 | 0.05 | \[-0.04 | , 0.20\] | 0.07 |
| Note: \[w\] is the meaning of a level of moderator 'w'; (w) is the value of a level of moderator 'w': CI = confidence interval; Std. Effect is completely standardized effect. |  |  |  |  |  |  |  |  |

The export functions from `flextable` can also be used to export one or
more tables to an external file, such as a Word file:

``` r

save_as_docx(ft_cond, "conditional_effects.docx")
```

Please refer to the documentation of `flextable` for further
information.

### Other Options

Both
[`as_flextable.indirect_list()`](https://sfcheung.github.io/manymome.table/reference/as_flextable.indirect_list.md)
and
[`as_flextable.cond_indirect_effects()`](https://sfcheung.github.io/manymome.table/reference/as_flextable.cond_indirect_effects.md)
have options for customize the generation of the table. For example, if
the list of indirect paths have different predictors (x-variables)
and/or different outcome variables (y-variables), the estimates caN be
grouped by x- and/or y-variables. Please refer to the help pages for
further details.
