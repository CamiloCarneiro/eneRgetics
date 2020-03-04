#' Calculates the Basal Metabolic Rate of shorebirds
#'
#' Calculates the Basal Metabolic Rate of shorebirds in temperate or tropical
#' sites, following Kersten & Piersma (1987) and Kersten et al. (1998).
#'
#' Kersten, M., Bruinzeel, L., Wiersma, P. & Piersma, T.
#' 1998. Reduced basal metabolic rate of migratory waders wintering in coastal
#' Africa. - Ardea 86: 71-80.\cr
#' Kersten, M. &  Piersma, T. 1987. High Levels of Energy Expenditure in
#' Shorebirds; Metabolic Adaptations to an Energetically Expensive Way of Life.
#' - Ardea 75: 175-187.
#'
#' @param body_mass body mass in grams.
#' @param region "temp" or "trop", for temperate or tropical, respectively.
#' @return Returns the basal metabolic rate in W.
#' @examples
#' bmr(0.46, "trop")
#' @export


bmr <- function(body_mass, region) {

  if (checkmate::test_numeric(body_mass) == F)
  {stop("body_mass must be a number", call. = FALSE)
  }

  if (checkmate::test_subset(region, choices = c("temp", "trop")) == F)
  {stop("region must be temp or trop")
  }

  body_mass_kg<-body_mass/1000
  if (region == "temp") {
    5.06*(body_mass_kg)^0.729

  } else {
    4.02*(body_mass_kg)^0.724
  }

}
