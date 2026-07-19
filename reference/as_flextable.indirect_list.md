# Convert an 'indirect_list' Object to a 'flextable' Object

The 'as_flextable' method for the output of
'manymome::many_indirect_effects()'.

## Usage

``` r
# S3 method for class 'indirect_list'
as_flextable(
  x,
  pvalue = FALSE,
  se = TRUE,
  var_labels = NULL,
  digits = 2,
  pval_digits = 3,
  use_arrow = TRUE,
  indirect_raw = TRUE,
  indirect_raw_ci = indirect_raw,
  indirect_raw_se = indirect_raw,
  group_by_x = TRUE,
  group_by_y = TRUE,
  y_first = TRUE,
  total_indirect = TRUE,
  footnote = TRUE,
  pcut = 0.001,
  ...
)
```

## Arguments

- x:

  The object to be converted. Should be of the class `indirect_list`
  from the package `manymome`.

- pvalue:

  If bootstrap confidence intervals are stored, whether asymmetric
  *p*-values are reported. Default is `FALSE`. See
  [`manymome::print.indirect_list()`](https://sfcheung.github.io/manymome/reference/print.indirect_list.html)
  for the computational details.

- se:

  Whether standard errors are reported if confidence intervals are
  stored. Default is `TRUE`. See
  [`manymome::print.indirect_list()`](https://sfcheung.github.io/manymome/reference/print.indirect_list.html)
  for the computation details.

- var_labels:

  A named vectors. Used to replace variable names by other names when
  generating the table. For example, `c(x = "I.V", y = "D.V.")` replaces
  `x` by `"I.V"` and `y` by `"D.V."` in the output.

- digits:

  The number of digits to be displayed for most numerical columns, such
  as effect estimates, standard errors, and confidence intervals.
  Default is 2.

- pval_digits:

  The number of digits to be displayed for the *p*-value column, if
  present. Default is 3.

- use_arrow:

  If `TRUE`, the default, use the arrow symbol in the paths.

- indirect_raw:

  If `TRUE`, the default, report unstandardized effects even if
  standardization was done.

- indirect_raw_ci:

  If `TRUE`, report the confidence intervals of unstandardized effects
  even if standardization was done and confidence intervals were stored.
  Default to be equal to `indirect_raw`. NOTE: Not used for now. Always
  `FALSE`.

- indirect_raw_se:

  If `TRUE`, report the standard errors of unstandardized effects even
  if standardization was done and confidence intervals were stored.
  Default to be equal to `indirect_raw`. NOTE: Not used for now. Always
  `FALSE`.

- group_by_x:

  If `TRUE`, the default, the rows will be grouped by x-variables if the
  paths have more than one x-variable. Default is `TRUE`.

- group_by_y:

  If `TRUE`, the default, the rows will be grouped by y-variables if the
  paths have more than one y-variable. Default is `TRUE`.

- y_first:

  If group by both x- and y-variables, group by y-variables first if
  `TRUE`, the default. Otherwise, group by x-variables.

- total_indirect:

  If `TRUE`, the default, total indirect effect will be computed and
  added to the output.

- footnote:

  If `TRUE`, the default, add footnote(s) regarding the results to the
  bottom of the table.

- pcut:

  Any *p*-value less than `pcut` will be displayed as `<[pcut]`,
  `"[pcut]"` replaced by the value of `pcut`. Default is .001.

- ...:

  Additional arguments. To be passed to
  [`flextable::autofit()`](https://davidgohel.github.io/flextable/reference/autofit.html)
  in preparing the final table. For example, if some lines are too lone
  and wrapped, try adding `add_w = .2`.

## Value

A `flextable` object.

## Details

It converts an `indirect_list` object, which is usually created by
[`manymome::many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.html),
to a `flextable` object. The output can be further modified by functions
from the package `flextable`.

## Examples

``` r

library(flextable)
library(manymome)

data(data_med_complicated)
lm_m11 <- lm(m11 ~ x1 + x2, data_med_complicated)
lm_m2 <- lm(m2 ~ x1 + x2, data_med_complicated)
lm_y1 <- lm(y1 ~ m11 + m2 + x1 + x2, data_med_complicated)
fit <- lm2list(lm_m11, lm_m2, lm_y1)

# All indirect paths
paths <- all_indirect_paths(fit,
                           x = c("x1", "x2"),
                           y = c("y1"))

# Indirect paths from x1 to y1
paths_x1y1 <- all_indirect_paths(fit,
                           x = c("x1"),
                           y = c("y1"))

# Indirect effect estimates
ind <- many_indirect_effects(paths,
                             fit = fit)
ft_ind <- as_flextable(ind)
ft_ind


.cl-3deb56a2{}.cl-3de32b08{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3de32b1c{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3de64eb4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3de64ec8{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3de6789e{width:0.974in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678a8{width:1.454in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678a9{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678b2{width:0.974in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678b3{width:1.454in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678bc{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678bd{width:0.974in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678be{width:1.454in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678c6{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678c7{width:0.974in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678c8{width:1.454in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3de678d0{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Predictor
```
