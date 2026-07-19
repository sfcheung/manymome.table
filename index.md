# manymome.table

(Version 0.4.1, updated on 2026-07-19, [release
history](https://sfcheung.github.io/manymome.table/news/index.html))

A collection of helper functions for converting selected results of
[`manymome`](https://sfcheung.github.io/manymome/) ([Cheung & Cheung,
2024](https://doi.org/10.3758/s13428-023-02224-z)) to publication-ready
tables.

It currently supports the `flextable` format from the [`flextable`
package](https://davidgohel.github.io/flextable/). Results from
[`manymome::many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.html)
and
[`manymome::cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.html)
can be converted to `flextable` objects using the method
[`as_flextable()`](https://davidgohel.github.io/flextable/reference/as_flextable.html).
They can then be exported to other formats, such as Word.

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/manymome.table/>

# Installation

The stable CRAN version can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r

install.packages("manymome.table")
```

The latest developmental version of this package can be installed by
`remotes::install_github`:

``` r

remotes::install_github("sfcheung/manymome.table")
```

# Reference

- Cheung, S. F., & Cheung, S.-H. (2024). *manymome*: An R package for
  computing the indirect effects, conditional effects, and conditional
  indirect effects, standardized or unstandardized, and their bootstrap
  confidence intervals, in many (though not all) models. *Behavior
  Research Methods, 56*(5), 4862–4882.
  <https://doi.org/10.3758/s13428-023-02224-z>

# Issues

If you have any suggestions and found any bugs, please feel free to open
a GitHub issue. Thanks.
