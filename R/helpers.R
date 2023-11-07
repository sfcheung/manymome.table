
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
    if (isTRUE(object$is_total)) {
        out_m <- ".."
      }
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

#' @noRd

group_ind_df <- function(object,
                         ind_list,
                         group_by_x = FALSE,
                         group_by_y = FALSE,
                         y_first = TRUE) {

    vars_x <- unique(sapply(ind_list, function(xx) xx$x))
    vars_y <- unique(sapply(ind_list, function(xx) xx$y))
    p_x <- length(vars_x)
    p_y <- length(vars_y)
    if (p_x == 1) group_by_x <- FALSE
    if (p_y == 1) group_by_y <- FALSE

    if (group_by_x && p_x > 1) {
        object$x <- sapply(ind_list, function(xx) xx$x)
      }
    if (group_by_y && p_y > 1) {
        object$y <- sapply(ind_list, function(xx) xx$y)
      }
    if (group_by_x && group_by_y) {
        if (y_first) {
            object <- object[order(object$y, object$x), ]
            object <- flextable::as_grouped_data(object,
                                                groups = c("y", "x"))
          } else {
            object <- object[order(object$x, object$y), ]
            object <- flextable::as_grouped_data(object,
                                                 groups = c("x", "y"))
          }
      }
    if (group_by_x && !group_by_y) {
            object <- object[order(object$x), ]
            object <- flextable::as_grouped_data(object,
                                                groups = c("x"))
      }
    if (!group_by_x && group_by_y) {
            object <- object[order(object$y), ]
            object <- flextable::as_grouped_data(object,
                                                groups = c("y"))
      }
    object
  }

#' @noRd

all_total_indirect_effects <- function(object) {
    vars_x <- sapply(object, function(xx) xx$x)
    vars_y <- sapply(object, function(xx) xx$y)
    vars_xy <- unique(cbind(vars_x, vars_y))
    row.names(vars_xy) <- NULL
    vars_x <- vars_xy[, "vars_x"]
    vars_y <- vars_xy[, "vars_y"]
    out <- mapply(manymome::total_indirect_effect,
                  x = vars_x,
                  y = vars_y,
                  MoreArgs = list(object = object),
                  SIMPLIFY = FALSE)
    for (i in seq_along(out)) {
        out[[i]]$is_total <- TRUE
      }
    out_names <- paste(vars_x, "-->..-->", vars_y)
    names(out) <- out_names
    out
  }

#' @noRd

list2indirect_list <- function(object) {
    out <- object
    class(out) <- c("indirect_list", class(object))
    out
  }

#' @noRd

get_indicators <- function(object) {
    wlevels <- attr(object, "wlevels")
    wvars <- attr(wlevels, "wvars")
    if (is.null(wvars)) {
        wvars <- list(colnames(wlevels))
        names(wvars) <- attr(wlevels, "wname")
      }
    w_types <- attr(wlevels, "w_types")
    if (is.null(w_types)) {
        w_types <- attr(wlevels, "w_type")
      }
    p_cat <- w_types %in% "categorical"
    if (!any(p_cat)) {
        return(NULL)
      }
    wvars_cat <- wvars[p_cat]
    out <- unlist(wvars_cat, use.names = FALSE)
    out
  }

#' @noRd

get_wvars <- function(object) {
    wlevels <- attr(object, "wlevels")
    wvars <- c(attr(wlevels, "wvars"))
    if (is.null(wvars)) {
        out <- attr(wlevels, "wname")
      } else {
        out <- names(wvars)
      }
    out
  }

#' @noRd

get_wvalues <- function(object) {
    wlevels <- attr(object, "wlevels")
    wvars <- attr(wlevels, "wvars")
    if (is.null(wvars)) {
        out <- colnames(wlevels)
      } else {
        out <- unlist(wvars, use.names = FALSE)
      }
    out
  }

#' @noRd

format_p <- function(pvals,
                     pcut = .001,
                     pval_digits = 3) {
  pd <- max(abs(floor(log10(pcut))), pval_digits)
  pvals_out <- as.character(formatC(pvals,
                                    digits = pd,
                                    format = "f",
                                    flag = "#"))
  pvals_out <- gsub("^0.", ".", pvals_out)
  pcut_str <- formatC(pcut,
                      digits = pd,
                      format = "f",
                      flag = "#")
  pcut_str <- gsub("^0.", "<.", pcut_str)
  pvals_out[pvals < pcut] <- pcut_str
  pvals_out[is.na(pvals)] <- ""
  return(pvals_out)
}

#' @noRd

all_w_numeric <- function(object) {
    wlevels <- attr(object, "wlevels")
    w_types <- attr(wlevels, "w_types")
    if (is.null(w_types)) {
        w_types <- attr(wlevels, "w_type")
      }
    return(all(w_types == "numeric"))
  }

#' @noRd

all_w_categorical <- function(object) {
    wlevels <- attr(object, "wlevels")
    w_types <- attr(wlevels, "w_types")
    if (is.null(w_types)) {
        w_types <- attr(wlevels, "w_type")
      }
    return(all(w_types == "categorical"))
  }