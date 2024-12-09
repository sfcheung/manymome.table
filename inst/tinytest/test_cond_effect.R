if (requireNamespace("officer", quietly = TRUE) &&
    length(unclass(packageVersion("manymome.table"))[[1]]) == 4) {

library(tinytest)
library(manymome)
library(officer)
library(flextable)

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)

out_xm_on_w <- cond_effects(wlevels = "w",
                            x = "x",
                            y = "m",
                            fit = fit_lm)

std_xm_on_w <- cond_effects(wlevels = "w",
                            x = "x",
                            y = "m",
                            standardized_x = TRUE,
                            fit = fit_lm)

tmp1 <- as_flextable(out_xm_on_w,
                     var_labels = c(w = "Moderator"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 3,
                     level = .90)

smp1 <- as_flextable(std_xm_on_w,
                     var_labels = c(w = "Moderator"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)

tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 7)
expect_match(tmp, "[Moderator]")
expect_match(tmp, "(Moderator)")
expect_match(tmp, "90% CI")
expect_match(tmp, "SE")
expect_match(tmp, formatC(coef(out_xm_on_w)[1], digits = 3, format = "f"))
expect_match(tmp, formatC(confint(out_xm_on_w, level = .90)[2, 2], digits = 3, format = "f"))

smp <- to_html(smp1)
expect_equal(ncol_keys(smp1), 4)
expect_match(smp, "[Moderator]")
expect_match(smp, "(Moderator)")
expect_false(grepl("90% CI", smp, fixed = TRUE))
expect_false(grepl("SE", smp, fixed = TRUE))
expect_match(smp, formatC(coef(std_xm_on_w)[1], digits = 3, format = "f"))

}
