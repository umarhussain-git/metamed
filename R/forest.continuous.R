#' Contour-Enhanced Forest Plot for Continuous Outcomes
#'
#' @param dat A data frame containing study-level summary statistics for continuous outcomes.
#' @param measure Effect size measure to compute (e.g., "SMD", "MD"). Default is "SMD".
#' @param method Method used for pooling effects (e.g., "REML", "DL").
#' @param xlab Label for the x-axis of the forest plot.
#' @param title Title of the forest plot.
#' @param model Model type to fit ("Random-effects" or "Fixed-effect").
#' @param estimator Estimator used for between-study variance (τ²). Default is "REML".
#' @param m_c_col Column name for the control group mean.
#' @param sd_c_col Column name for the control group standard deviation.
#' @param n_c_col Column name for the control group sample size.
#' @param m_t_col Column name for the treatment group mean.
#' @param sd_t_col Column name for the treatment group standard deviation.
#' @param n_t_col Column name for the treatment group sample size.
#' @param diamond.col Color of the pooled effect diamond.
#' @param study.col Color of the study-specific points/squares.
#' @param CI.col Color of the confidence interval lines.
#' @param Pred.Inter.col Color of the prediction interval line.
#' @param square.size Size of the study effect squares representing weight.
#' @param contour_fill Logical. Whether to include contour shading behind the plot.
#' @param text_size Font size for study labels, statistics, and axis text.
#' @param pred Logical. Whether to display the prediction interval.
#' @return
#' @export
#'
#' @examples
forest.continuous <- function(dat,
                              measure = "SMD",
                              method = "REML",
                              xlab = "",
                              title = NULL,
                              model = "Random-effects",
                              estimator = "REML",
                              m_c_col = "mean_c",
                              sd_c_col = "sd_c",
                              n_c_col = "n_c",
                              m_t_col = "mean_t",
                              sd_t_col = "sd_t",
                              n_t_col = "n_t",
                              diamond.col = "red",
                              study.col = "blue",
                              CI.col = "blue",
                              Pred.Inter.col = "black",
                              square.size = 10,
                              contour_fill = c("gray95","gray80","gray60","gray40"),
                              text_size = 3.5,
                              pred = TRUE) {

  # Compute effect size
  escalc_res <- metafor::escalc(measure=measure,
                                m1i=dat[[m_t_col]], sd1i=dat[[sd_t_col]], n1i=dat[[n_t_col]],
                                m2i=dat[[m_c_col]], sd2i=dat[[sd_c_col]], n2i=dat[[n_c_col]],
                                data=dat)

  # Random-effects meta-analysis
  meta_res <- metafor::rma(yi, vi, data=escalc_res, method=method)
  pi <- predict(meta_res, digits=3)

  # Prepare data for plotting
  dat_plot <- escalc_res %>%
    mutate(weight = 100 * (1/vi)/sum(1/vi),
           ci.lb = yi - 1.96*sqrt(vi),
           ci.ub = yi + 1.96*sqrt(vi),
           y = rev(seq_len(nrow(.))),
           Effect = sprintf("%.2f [%.2f - %.2f]", yi, ci.lb, ci.ub),
           WeightText = sprintf("%.1f%%", weight),
           MeanT = sprintf("%.2f [%.2f]", dat[[m_t_col]], dat[[sd_t_col]]),
           MeanC = sprintf("%.2f [%.2f]", dat[[m_c_col]], dat[[sd_c_col]])) %>%
    arrange(desc(yi))

  # Diamond for pooled effect
  pooled <- data.frame(
    x = c(meta_res$ci.lb, meta_res$beta, meta_res$ci.ub, meta_res$beta, meta_res$ci.lb),
    y = c(0, 0.5, 0, -0.5, 0)
  )
  pooled_text <- sprintf("%.2f [%.2f - %.2f]", meta_res$beta, meta_res$ci.lb, meta_res$ci.ub)

  # Heterogeneity
  tau_ci <- confint(meta_res)$random   # 95% CI for tau
  tau2 <- meta_res$tau2
  I2 <- meta_res$I2                     # point estimate of I²

  # Compute approximate 95% CI for I² (Higgins & Thompson method)
  Q <- meta_res$QE
  df <- meta_res$k - 1

  I2_lb <- max(0, (Q/df - 1)/Q * 100)                # lower bound, min 0%
  I2_ub <- min(100, ((Q * qchisq(0.975, df)/df) - 1)/Q * 100)  # upper bound, max 100%

  hetero_text <- paste0(
    model, " (", estimator, ")\n",
    "Tau² = ", round(tau2,3), " [", round(tau_ci[1],3), " - ", round(tau_ci[2],3), "]\n",
    "I² = ", round(I2,1), "% [", round(I2_lb,1), " - ", round(I2_ub,1), "%]\n",
    "Q-test p = ", signif(meta_res$QEp,3)
  )


  if(is.null(title)) title <- paste0("Contour Enhanced Forest Plot (", measure, ")")

  # Contour shading
  ymax_plot <- max(dat_plot$y) + 2
  ymin_plot <- -2
  effect_max <- max(abs(c(dat_plot$ci.lb, dat_plot$ci.ub, meta_res$ci.lb, meta_res$ci.ub)))
  effect_min <- min(abs(c(dat_plot$ci.lb, dat_plot$ci.ub, meta_res$ci.lb, meta_res$ci.ub)))

  contour_left <- data.frame(
    xmin = c(-effect_max-0.1, -0.8, -0.5, -0.2),
    xmax = c(-0.8, -0.5, -0.2, 0),
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = factor(c("Very Large","Large","Medium","Small"),
                  levels = c("Small","Medium","Large","Very Large"))
  )

  contour_right <- data.frame(
    xmin = c(0, 0.2, 0.5, 0.8),
    xmax = c(0.2, 0.5, 0.8, pi$pi.ub+.2),
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = factor(c("Small","Medium","Large","Very Large"),
                  levels = c("Small","Medium","Large","Very Large"))
  )

  contour_all <- rbind(contour_left, contour_right)

  # Table positions

  xpos_left <- list(
    Study   = effect_min - 5,   # Study names slightly left of leftmost CI
    MeanT   = effect_min - 4, # Treatment mean[SD] next to study
    MeanC   = effect_min - 3, # Control mean[SD] next to treatment
    PredInt = effect_min - 1  # Prediction Interval label slightly above study column
  )
  xpos_right <- list(Effect = effect_max + 0.2, Weight = effect_max + 1.2, PredInt = effect_max + 2.2)
  xlim <- c(-5.5, effect_max + 3)

  # Prediction interval y-position
  pred_y <- -0.6

  # Right-side table aligned to pi$pi.ub + 0.3
  x_smd_wt <- pi$pi.ub + 0.3  # new x-position for SMD and weight

  p <- ggplot() +
    geom_rect(data = contour_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.7) +
    scale_fill_manual(values = contour_fill, name = "", guide = guide_legend(override.aes = list(alpha = 1))) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +

    # Study CIs
    geom_errorbarh(data = dat_plot, aes(y = y, xmin = ci.lb, xmax = ci.ub), height = 0, color = CI.col) +
    geom_point(data = dat_plot, aes(y = y, x = yi, size = weight), shape = 15, color = study.col, show.legend = FALSE) +

    # Pooled diamond
    geom_polygon(data = pooled, aes(x = x, y = y), fill = diamond.col, alpha = 0.9) +

    # Prediction interval
    geom_errorbarh(data = data.frame(y=pred_y, xmin=pi$pi.lb, xmax=pi$pi.ub),
                   aes(y=y, xmin=xmin, xmax=xmax),
                   height = 0, size = 1.5, color = Pred.Inter.col) +
    annotate("text", x = xpos_right$PredInt-1, y = pred_y,
             label = paste0("[", round(pi$pi.lb,2), "-", round(pi$pi.ub,2), "]"),
             fontface="bold", hjust=0, size=text_size) +
    annotate("text", x = xpos_left$Study+.9, y = pred_y,
             label = "Pred. Int.", fontface="bold", hjust=0, size=text_size) +

    # Left-side table
    annotate("text", x=xpos_left$Study, y=dat_plot$y, label=dat_plot$Study, hjust=0, size=text_size, family="mono") +
    annotate("text", x=xpos_left$MeanT, y=dat_plot$y, label=dat_plot$MeanT, hjust=0, size=text_size, family="mono") +
    annotate("text", x=xpos_left$MeanC, y=dat_plot$y, label=dat_plot$MeanC, hjust=0, size=text_size, family="mono") +

    # Right-side table (moved)
    annotate("text", x=x_smd_wt, y=dat_plot$y, label=dat_plot$Effect, hjust=0, size=text_size, family="mono") +
    annotate("text", x=x_smd_wt + 1.9, y=dat_plot$y, label=dat_plot$WeightText, hjust=0, size=text_size, family="mono") +

    # Headers
    annotate("text", x=xpos_left$Study, y=max(dat_plot$y)+1, label="Study", fontface="bold", hjust=0, size=text_size+.3) +
    annotate("text", x=xpos_left$MeanT, y=max(dat_plot$y)+1, label="Treatment \nmean[sd]", fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=xpos_left$MeanC, y=max(dat_plot$y)+1, label="Control \nmean[sd]", fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=x_smd_wt, y=max(dat_plot$y)+1, label=paste0(measure," (95% CI)"), fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=x_smd_wt + 1.9, y=max(dat_plot$y)+1, label="Weight (%)", fontface="bold", hjust=0, size=text_size) +

    # Pooled effect
    annotate("text", x=xpos_left$Study+.9, y=0.3, label="Pooled effect", fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=x_smd_wt, y=0.4, label=pooled_text, fontface="bold", hjust=0, size=text_size) +

    # Heterogeneity
    annotate("text", x=xpos_left$Study-1, y=-1.1, label=hetero_text, hjust=0, fontface="italic", size=text_size) +

    scale_y_continuous(breaks = dat_plot$y, labels = rep("", length(dat_plot$y))) +
    labs(x=xlab, y=NULL, title=title) +
    theme_minimal(base_size=14, base_family="mono") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top") +
    coord_cartesian(xlim=c(-5.5, x_smd_wt + 4), ylim=c(pred_y-0.5, max(dat_plot$y)+1.5))

  return(p)
}


