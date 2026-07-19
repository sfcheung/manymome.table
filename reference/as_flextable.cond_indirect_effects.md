# Convert an 'cond_indirect_effects' Object to a 'flextable' Object

The 'as_flextable' method for the output of
'manymome::many_indirect_effects()'.

## Usage

``` r
# S3 method for class 'cond_indirect_effects'
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
  footnote = TRUE,
  show_wvalues = TRUE,
  show_indicators = FALSE,
  show_path = TRUE,
  pcut = 0.001,
  level = 0.95,
  ...
)
```

## Arguments

- x:

  The object to be converted. Should be of the class
  `cond_indirect_effects` from the package `manymome`.

- pvalue:

  If bootstrap confidence intervals are stored, whether asymmetric
  *p*-values are reported. Default is `FALSE`. See
  [`manymome::print.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_effects.html)
  for the computational details.

- se:

  Whether standard errors are reported if confidence intervals are
  stored. Default is `TRUE`. See
  [`manymome::print.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_effects.html)
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

- footnote:

  If `TRUE`, the default, add footnote(s) regarding the results to the
  bottom of the table.

- show_wvalues:

  Whether the values of moderators will be shown. If `FALSE`, no values
  will be shown, even for categorical moderators. Default is `TRUE`.

- show_indicators:

  Whether the values of indicators (dummy variables) will be shown for
  categorical moderators. Default is `FALSE`.

- show_path:

  Whether the paths being moderated will be displayed. Default is
  `TRUE`.

- pcut:

  Any *p*-value less than `pcut` will be displayed as `<[pcut]`,
  `"[pcut]"` replaced by the value of `pcut`. Default is .001.

- level:

  The level of confidence for the confidence intervals computed from the
  original standard errors (e.g., the standard errors in
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or `lavaan`). Used
  only for paths without mediators and both x- and y-variables are not
  standardized. Default is .95.

- ...:

  Additional arguments. To be passed to
  [`flextable::autofit()`](https://davidgohel.github.io/flextable/reference/autofit.html)
  in preparing the final table. For example, if some lines are too lone
  and wrapped, try adding `add_w = .2`.

## Value

A `flextable` object.

## Details

It converts an `cond_indirect_effects` object, which is usually created
by
[`manymome::cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.html),
to a `flextable` object. The output can be further modified by functions
from the `flextable` package.

## Examples

``` r

library(manymome)
library(flextable)

# List of indirect effects

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)

# Should set R to 5000 or 10000 in real research
boot_out_lm <- do_boot(fit_lm,
                       R = 100,
                       seed = 54532,
                       parallel = FALSE,
                       progress = FALSE)

out_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm)

std_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm,
                                      standardized_x = TRUE,
                                      standardized_y = TRUE)

ft1 <- as_flextable(out_xmy_on_w,
                    var_labels = c(w = "Moderator"))
ft1


.cl-3d43151e{}.cl-3d39c78e{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3d39c7a2{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-3d3e0538{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3d3e054c{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3d3e054d{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3d3e0556{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:0;line-height: 1;background-color:transparent;}.cl-3d3e0557{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:0;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-3d3e3e90{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3e9a{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3e9b{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ea4{width:0.74in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ea5{width:0.782in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3eae{width:0.625in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3eaf{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3eb0{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3eb8{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3eb9{width:0.74in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ec2{width:0.782in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ec3{width:0.625in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ec4{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ecc{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ecd{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ece{width:0.74in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ed6{width:0.782in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ed7{width:0.625in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ed8{width:1.193in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ed9{width:0.727in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ee0{width:0.74in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ee1{width:0.782in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3d3e3ee2{width:0.625in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Path:  x → m → y
```
