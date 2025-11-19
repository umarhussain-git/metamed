#' Publication ready table for binary outcome in meta-analysis
#'
#' @param data A data frame containing study-level summary statistics for binary outcomes.
#' @param event.e Column name or vector of events in the experimental/treatment group.
#' @param event.c Column name or vector of events in the control group.
#' @param n.e Column name or vector of sample sizes in the experimental/treatment group.
#' @param n.c Column name or vector of sample sizes in the control group.
#' @param outcome Character. Effect measure for the binary outcome, e.g., "OR" or "RR".
#' @param model Character. Meta-analysis model: "Random-effects" or "Fixed-effects". Default is "Random-effects".
#' @param estimator Character. Estimator for the random-effects model, e.g., "REML". Default is "REML".
#' @param hkn Logical. Whether to use Hartung-Knapp adjustment for random-effects model. Default is TRUE.
#' @param measure Character. Effect size measure to compute. Default is "OR".
#' @param Comparison Character. Optional description of comparison groups.
#' @param results Logical. Whether to return detailed meta-analysis results. Default is TRUE.
#' @import flextable
#' @import dplyr
#'
#' @return A data frame (or table) summarizing study-level results and effect estimates
#' @export
#'
#' @examples tab.binary(dat)
tab.binary <- function(
    data,
    event.e = "events_t",
    event.c = "events_c",
    n.e = "n_t",
    n.c = "n_c",
    outcome = "Event Rate",
    model = "RE",
    estimator = "REML",
    hkn = TRUE,
    measure = "RR",
    Comparison = "Treatment vs Control",
    results = "both"
) {

  # Pull columns by string names
  events_t <- data[[event.e]]
  n_t      <- data[[n.e]]
  events_c <- data[[event.c]]
  n_c      <- data[[n.c]]

  # Validate inputs
  model <- toupper(model)
  if(!model %in% c("RE","FE")) stop("Model must be 'RE' or 'FE'.")
  measure <- toupper(measure)
  if(!measure %in% c("RR","OR","RD")) stop("Measure must be 'RR', 'OR', or 'RD'.")
  results <- tolower(results)
  if(!results %in% c("pooled","study.level","both")) stop("results must be 'pooled', 'study.level', or 'both'")

  message("Selected model: ", model,
          ifelse(model=="RE", paste0(" (estimator=", estimator, ", Hartung-Knapp=", hkn, ")"), ""),
          "; Measure: ", measure)

  # Method
  method <- ifelse(model == "RE", estimator, "FE")

  # Meta-analysis
  meta_res <- metafor::rma(measure = measure,
                           ai = events_t, n1i = n_t,
                           ci = events_c, n2i = n_c,
                           data = data, method = method,
                           test = ifelse(hkn & model=="RE", "knha", "z"))

  # Study-level table
  study_tbl <- data %>%
    dplyr::mutate(
      `Events (T)` = paste0(events_t, "/", n_t),
      `Events (C)` = paste0(events_c, "/", n_c),
      yi = log((events_t / n_t)/(events_c / n_c)),
      sei = sqrt(1/events_t - 1/n_t + 1/events_c - 1/n_c),
      `Effect (95% CI)` = paste0(round(exp(yi),2), " (",
                                 round(exp(yi - 1.96*sei),2), "-",
                                 round(exp(yi + 1.96*sei),2), ")"),
      P = round(2 * (1 - stats::pnorm(abs(yi/sei))), 3),
      P = ifelse(P == 0, "<0.001", as.character(P))
    ) %>%
    dplyr::select(Study, `Events (T)`, `Events (C)`, `Effect (95% CI)`, P)

  ft_study <- flextable::flextable(study_tbl) %>%
    flextable::bold(part = "header") %>%
    flextable::autofit() %>%
    flextable::theme_vanilla() %>%
    flextable::fontsize(size = 11) %>%
    flextable::align(align = "center", part = "all")

  # Pooled table
  effect_ci <- paste0(round(exp(meta_res$beta),2), " (",
                      round(exp(meta_res$ci.lb),2), "-",
                      round(exp(meta_res$ci.ub),2), ")")

  if(model=="RE") {
    tau2 <- round(meta_res$tau2, 2)
    tau2_CI <- paste0(tau2, " (", round(stats::confint(meta_res)$random[1],2), "-",
                      round(stats::confint(meta_res)$random[2],2), ")")
    I2 <- round(meta_res$I2, 0)
    I2_CI <- paste0(I2, "% (", round(max(0, ((meta_res$QE - meta_res$k + 1)/meta_res$QE)*100),0),
                    "%-", round(min(100, ((meta_res$QE * stats::qchisq(0.975, meta_res$k-1)/(meta_res$k-1) - meta_res$k+1)/
                                            (meta_res$QE * stats::qchisq(0.975, meta_res$k-1)/(meta_res$k-1)))*100),0), "%)")
    pred <- predict(meta_res, transf = exp)  # <- Generic predict() works
    pred_text <- paste0(round(pred$pi.lb,2), ", ", round(pred$pi.ub,2))
  } else {
    tau2_CI <- "0"
    I2_CI <- "0"
    pred_text <- "NA"
  }

  n_studies <- meta_res$k

  pooled_tbl <- data.frame(
    Comparison = Comparison,
    Outcome = outcome,
    n = n_studies,
    `Effect size (95% CI)` = effect_ci,
    P = signif(meta_res$QEp,3),
    `τ² (95% CI)` = tau2_CI,
    `I² (95% CI)` = I2_CI,
    `95% prediction` = pred_text,
    check.names = FALSE
  )

  ft_pooled <- flextable::flextable(pooled_tbl) %>%
    flextable::bold(part = "header") %>%
    flextable::autofit() %>%
    flextable::theme_vanilla() %>%
    flextable::fontsize(size = 11) %>%
    flextable::align(align = "center", part = "all")

  # Return
  if(results=="pooled") return(ft_pooled)
  if(results=="study.level") return(ft_study)
  if(results=="both") {
    message("Both study-level and pooled tables are displayed.")
    print(ft_study)
    print(ft_pooled)
    return(invisible(list(Study_Level = ft_study, Pooled = ft_pooled)))
  }
}

