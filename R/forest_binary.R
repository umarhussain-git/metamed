#' Contour-Enhanced Forest Plot for Binary Outcomes (OR or RR)
#'
#' Generates a contour-enhanced forest plot for meta-analysis of binary outcomes
#' using Odds Ratios (OR) or Risk Ratios (RR). Includes contour shading,
#' study-level effects, pooled effect diamond, heterogeneity statistics,
#' prediction interval, and formatted text columns.
#'
#' @param dat A data frame containing study-level binary data. Must include event
#'   counts and total sample size for both treatment and control groups.
#' @param measure Character. Effect measure, either "OR" (default) or "RR".
#' @param method Character. Method for meta-analysis (default "REML").
#' @param xlab Character. x-axis label.
#' @param title Character. Plot title. If NULL, an automatic title is created.
#' @param model Character. Model description used in heterogeneity text.
#' @param estimator Character. Estimator used for heterogeneity text.
#' @param nc_col Column name for control group sample size.
#' @param ne_col Column name for treatment group sample size.
#' @param event_c_col Column name for control group events.
#' @param event_t_col Column name for treatment group events.
#' @param diamond.col Color of pooled-effect diamond.
#' @param study.col Color of study squares.
#' @param CI.col Color for study confidence interval bars.
#' @param Pred.Inter.col Color of prediction interval line.
#' @param square.size Numeric. Maximum square size for study weights.
#' @param contour_fill Character vector of fill colors for contour shading.
#' @param text_size Numeric. Font size for text labels.
#' @param xlim Numeric vector of length 2. x-axis limits.
#' @param pred Logical. If TRUE, displays prediction interval.
#' @param xpos A named list defining x-positions for text columns:
#'   \code{EventsT}, \code{EventsC}, \code{Effect}, \code{Weight}.
#' @param study_x Numeric. Horizontal position for study names.
#' @param hetero_x Numeric. Horizontal position for heterogeneity text.
#'
#' @return A ggplot forest plot object.
#'
#' @details
#' Contour shading visually represents magnitude of effects on both sides of 1.
#' The prediction interval represents the expected range for a new study.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom dplyr %>%
#' @import grid
#' @import gridExtra
#' @importFrom metafor rma predict confint
#' @importFrom stats qt
#' @export
#'
#' @examples
#' \donttest{
#' dat <- data.frame(
#'   Study = c("Shell 2021","Khalid 2020","Daniel 2009","Albert 2018","Khan 2011"),
#'   events_t = c(10,25,5,30,18),
#'   n_t = c(100,120,90,140,110),
#'   events_c = c(15,20,22,28,25),
#'   n_c = c(100,115,95,130,120)
#' )
#'
#' # Basic example using only supported arguments
#' forest.binary(
#'   dat,
#'   measure = "OR"
#' )
#' }
forest.binary <- function(dat,
                          measure = "OR",
                          method = "REML",
                          xlab = "",
                          title = NULL,
                          model = "Random-effects",
                          estimator = "REML",
                          nc_col = "n_c",
                          ne_col = "n_t",
                          event_c_col = "events_c",
                          event_t_col = "events_t",
                          diamond.col = "red",
                          study.col = "blue",
                          CI.col = "blue",
                          Pred.Inter.col = "black",
                          square.size = 10,
                          contour_fill = c("gray95","gray80","gray60","gray40"),
                          text_size = 3.5,
                          xlim = c(-1.7, 3.5),
                          pred = TRUE,
                          xpos = list(
                            EventsT = -0.9,
                            EventsC = -0.3,
                            Effect = 2.6,
                            Weight = 3.1
                          ),
                          study_x = -1.5,
                          hetero_x = -1.7) {

  events_t <- dat[[event_t_col]]
  n_t <- dat[[ne_col]]
  events_c <- dat[[event_c_col]]
  n_c <- dat[[nc_col]]

  # Meta-analysis
  meta_res <- metafor::rma(measure=measure, ai=events_t, n1i=n_t,
                           ci=events_c, n2i=n_c, data=dat, method=method)
  pi <- predict(meta_res, digits=3, transf=exp)

  # Study-level log(effect) & SE
  dat <- dat %>%
    mutate(
      yi = if(measure=="OR") {
        log((events_t/(n_t - events_t)) / (events_c/(n_c - events_c)))
      } else {
        log((events_t/n_t) / (events_c/n_c))
      },
      sei = sqrt(1/events_t + 1/(n_t - events_t) + 1/events_c + 1/(n_c - events_c)),
      weight = 100 * (1/sei^2) / sum(1/sei^2),
      ci.lb = yi - 1.96*sei,
      ci.ub = yi + 1.96*sei
    ) %>%
    arrange(desc(yi)) %>%
    mutate(y = rev(seq_len(nrow(.))),
           Effect = paste0(round(exp(yi),2), " [", round(exp(ci.lb),2), "-", round(exp(ci.ub),2), "]"),
           WeightText = paste0(round(weight,1), "%"),
           EventsT = paste0(events_t,"/",n_t),
           EventsC = paste0(events_c,"/",n_c))

  # Diamond for pooled effect
  pooled <- data.frame(
    x = c(exp(meta_res$ci.lb), exp(meta_res$beta), exp(meta_res$ci.ub), exp(meta_res$beta), exp(meta_res$ci.lb)),
    y = c(0, 0.5, 0, -0.5, 0)
  )
  pooled_text <- paste0(round(exp(meta_res$beta),2), " [", round(exp(meta_res$ci.lb),2), "-", round(exp(meta_res$ci.ub),2), "]")

  # Heterogeneity
  tau_ci <- confint(meta_res)$random
  tau2 <- meta_res$tau2
  I2 <- meta_res$I2

  # 95% CI for I²
  Q <- meta_res$QE
  df <- meta_res$k - 1
  alpha <- 0.05
  I2_lower <- max(0, (Q - qchisq(1 - alpha/2, df))/Q * 100)
  I2_upper <- min(100, (Q - qchisq(alpha/2, df))/Q * 100)

  hetero_text <- paste0(
    model, " (", estimator, ")\n",
    "Tau² = ", round(tau2,3), " [", round(tau_ci[1],3), "-", round(tau_ci[2],3), "]\n",
    "I² = ", round(I2,1), "% [", round(I2_lower,1), "-", round(I2_upper,1), "%]\n",
    "Q-test p = ", signif(meta_res$QEp,3)
  )
  # Contour shading (same breaks for simplicity)
  ymax_plot <- max(dat$y) + 2
  ymin_plot <- -2
  contour_left <- data.frame(
    xmin = c(0, 1/2.0, 1/1.5, 1/1.2),
    xmax = c(1/2.0, 1/1.5, 1/1.2, 1),
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = factor(c("Very Large", "Large", "Medium", "Small"),
                  levels=c("Small","Medium","Large","Very Large"))
  )
  contour_right <- data.frame(
    xmin = c(1, 1.2, 1.5, 2.0),
    xmax = c(1.2, 1.5, 2.0, 2.5),
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = factor(c("Small","Medium","Large","Very Large"),
                  levels=c("Small","Medium","Large","Very Large"))
  )
  contour_all <- rbind(contour_left, contour_right)

  if(is.null(title)) title <- paste0("Contour Enhanced Forest Plot (", measure, ")")

  # Final plot
  p <- ggplot() +
    geom_rect(data=contour_all, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill), alpha=0.8) +
    scale_fill_manual(values = contour_fill, name="", guide = guide_legend(override.aes = list(alpha=1))) +
    geom_vline(xintercept = 1, linetype = "solid", alpha=.7, color = "black", size = .5) +
    geom_point(data=dat, aes(y=y, x=exp(yi), size=weight), shape=15, color=study.col, show.legend=FALSE) +
    scale_size_continuous(range=c(2, square.size)) +
    geom_errorbarh(data=dat, aes(y=y, xmin=exp(ci.lb), xmax=exp(ci.ub)), height=0, color=CI.col) +
    geom_polygon(data=pooled, aes(x=x, y=y), fill=diamond.col, alpha=0.9) +
    geom_text(data=dat, aes(x=xpos$EventsT, y=y, label=EventsT), hjust=0, size=text_size) +
    geom_text(data=dat, aes(x=xpos$EventsC, y=y, label=EventsC), hjust=0, size=text_size) +
    geom_text(data=dat, aes(x=xpos$Effect, y=y, label=Effect), hjust=0, size=text_size) +
    geom_text(data=dat, aes(x=xpos$Weight, y=y, label=WeightText), hjust=0, size=text_size) +
    annotate("text", x= study_x, y=dat$y, label=dat$Study, hjust=0, size=text_size) +
    scale_y_continuous(breaks = dat$y, labels = rep("", length(dat$y))) +
    labs(x=xlab, y=NULL, title=title) +
    theme_minimal(base_size=14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top") +
    annotate("text", x=c(xpos$EventsT, xpos$EventsC, xpos$Effect, xpos$Weight),
             y=rep(max(dat$y)+1,4),
             label=c("Events (T)", "Events (C)", paste0(measure," (95% CI)"), "Weight (%)"),
             fontface="bold", hjust=0, size=text_size) +
    annotate("text", x= study_x, y=max(dat$y)+1, label="Study", fontface="bold", hjust=0, size=text_size+.3)+
    annotate("text", x= study_x, y=.4, label="Pooled effect", fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=xpos$Effect, y=.4, label=pooled_text, fontface="bold", hjust=0, size=text_size) +
    annotate("text", x=hetero_x, y=-1.1, label=hetero_text, hjust=0, fontface="italic", size=text_size)

  if(pred){
    p <- p +
      geom_errorbarh(aes(xmin=pi$pi.lb, xmax=pi$pi.ub, y=-0.6), height=0, size=1.9, color=Pred.Inter.col) +
      annotate("text", x=-1, y=-0.5, label="Prediction interval", fontface="bold", hjust=0, size=text_size) +
      annotate("text", x=xpos$Effect, y=-0.5,
               label=paste0("[", round(pi$pi.lb,2), "-", round(pi$pi.ub,2), "]"),
               fontface="bold", color="black", hjust=0, size=text_size)
  }

  p <- p + coord_cartesian(xlim=xlim, ylim=c(-1.5, max(dat$y)+1.5))
  return(p)
}
