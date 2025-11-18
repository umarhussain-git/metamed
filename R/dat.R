#' Hypothetical dataset: Depression by Socioeconomic Status
#'
#' Counts of depression events in high SES (experimental) vs low SES (control) groups across 5 studies.
#'
#' @format A data frame with 5 rows and 6 variables:
#' \describe{
#'   \item{Study}{Study name or year}
#'   \item{events_t}{Depression events in high SES group}
#'   \item{n_t}{Total in high SES group}
#'   \item{events_c}{Depression events in low SES group}
#'   \item{n_c}{Total in low SES group}
#'   \item{subgroup}{Subgroup label, e.g., region}
#' }
#' @source Hypothetical data
#' @export
dat <- data.frame(
  Study = c("Shell 2021","Khalid 2020","Daniel 2009","Albert 2018",
            "Khan 2011","Ali 2016","Sara 2019","Omar 2022"),
  events_t = c(10,25,5,30,18,8,22,12),
  n_t      = c(100,120,90,140,110,90,130,100),
  events_c = c(15,20,22,28,25,10,20,18),
  n_c      = c(100,115,95,130,120,95,120,100),
  subgroup = c("Europe","Europe","Europe","Asia","Asia","Asia","Africa","Africa")
)

