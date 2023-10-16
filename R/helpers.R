
#' @noRd

format_ci <- function(x,
                      level = NULL,
                      digits = 3,
                      cilo_name = "CI.lo",
                      cihi_name = "CI.hi",
                      brackets = c("[", "]"),
                      sep = ", ") {
    cilo <- x[, cilo_name, drop = TRUE]
    cihi <- x[, cihi_name, drop = TRUE]
    cilo <- formatC(cilo,
                    digits = digits,
                    format = "f")
    cihi <- formatC(cihi,
                    digits = digits,
                    format = "f")
    out <- paste0(brackets[1],
                  cilo,
                  sep,
                  cihi,
                  brackets[2])
    out
  }

#' @noRd

set_path_names <- function(object,
                           var_labels = NULL) {
    out0 <- lapply(object,
                   set_path_names_i,
                   var_labels = var_labels)
    out1 <- sapply(out0,
                   function(xx) {
                      paste(c(xx$x, xx$m, xx$y),
                            collapse = " -> ")
                     })
    unname(out1)
  }

#' @noRd

set_path_names_i <- function(object,
                             var_labels = NULL) {
    out_x <- gsub_table(object$x, var_labels = var_labels)
    out_m <- gsub_table(object$m, var_labels = var_labels)
    out_y <- gsub_table(object$y, var_labels = var_labels)
    list(x = out_x,
         m = out_m,
         y = out_y)
  }

#' @noRd

gsub_table <- function(x,
                       var_labels = NULL) {
    var_labels_names <- names(var_labels)
    for (i in seq_along(var_labels)) {
        x <- gsub(var_labels_names[i],
                  var_labels[i],
                  x,
                  fixed = TRUE)
      }
    x
  }

#' @noRd

format_pvalue <- function(pv,
                          pcut = .001) {
    digits <- abs(floor(log10(pcut)))
    if (pv < pcut) {
        out <- paste0("<", formatC(pcut, digits))
        out <- gsub("<0.", "<.", out, fixed = TRUE)
      } else {
        out <- formatC(pv,
                       digits = digits,
                       format = "f")
        out <- gsub("0.", ".", out, fixed = TRUE)
      }
    return(out)
  }