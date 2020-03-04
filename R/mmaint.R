#' Calculates maintenance metabolism
#'
#' Calculates the maintenance metabolism, in watt, for a shorebird following the
#' model of Wiersma & Piersma (1994); the thermal conductance used is calculated
#' following Kendeigth et al. (1977). Thermoregulatory costs will only occur when
#' maintenance metabolism is above the Basal Metabolic Rate.
#'
#' Wiersma, P. and Piersma, T. 1994. Effects of microhabitat, flocking, climate
#' and migratory goal on energy expenditure in the annual cycle of red knots. -
#' Condor 96: 257 - 279.\cr Kendeigh, S. C., V. R. Dol'nik, and V. M. Gavrilov.
#' 1977. Avian Energetics. Pp. 127 - 204 in J. Pinowski and S. C. Kendeigh,
#' editors. Granivorous birds in ecosystems. Cambridge University Press,
#' Cambridge, UK.
#'
#' Microhabitats:\cr h1 - Densely vegetated salt marsh\cr h2 - Vegetated salt
#' marsh\cr h3 - Mudflat and bare salt marsh\cr h4 - On snow on tundra\cr h5 -
#' Behind tundra hillock\cr h6 - On tundra hillock\cr h7 - Dense group\cr h8 -
#' Open group\cr h9 - Solitary\cr h10 - Head in wind\cr h11 - Flank in wind\cr
#'
#' @param weatherData a data frame with wind speed (named "wind_speed") in m/s,
#'   temperature (named "temperature") in celsius degrees, global horizontal
#'   irradiance (named "GHI") in Wh/m^2, and datetime (named "date") in POSIXct
#'   format.
#' @param tide_table a tide table based on which the tide codes will be calculated;
#'   these codes will be used to merge with timeBudget; must have a column named
#'   "date" with datetime info in POSIXct format and another one named "type"
#'   with values 'High' or 'Low'.
#' @param timeBudget a data frame with tidal codes (column named "tide_code")
#'   and the proportion of time spent on each microhabitat in columns 'h1', 'h2',
#'   'h3' ... 'h11', corresponding to the microhabitats in Wiersma & Piersma
#'   (1994; table 1). See correspondence under Details. If there is no variation
#'   in relation to tides, provide a data frame with only one row and the
#'   proportion per microhabitat
#' @param breast_height breast height, in meters.
#' @param body_mass body mass, in grams.
#' @return a data frame with the date, weather data, tide codes (if used), time
#'   budget and a column with the maintenance metabolism in W; prints a plot
#'   with the initial 15 days of data and the range of m_maint in the data
#' @examples
#' mmaint(weather, tides, time_budget, 0.17, 420)
#' @export

mmaint<- function(weatherData, tide_table, timeBudget, breast_height, body_mass){

  if (missing(tide_table)) {

    if (nrow(timeBudget) !=1)
    {stop("TimeBudget must have only one entry per microhabitat", call. = FALSE)
    }
    
    # check data is correcly provided
    if (missing(weatherData) | missing(timeBudget) |
        missing(breast_height) | missing(body_mass) )
    {stop("weatherData, timeBudget, breast_height and/or body_mass is missing")
    }
    
    if (checkmate::testDataFrame(weatherData) == F)
    {stop("weatherData data must be a data frame", call. = FALSE)
    }
    
    if (checkmate::testDataFrame(timeBudget) == F)
    {stop("timeBudget must be a data frame", call. = FALSE)
    }
    
    if (checkmate::test_numeric(breast_height) == F)
    {stop("breast_height must be a number", call. = FALSE)
    }
    
    if (checkmate::test_numeric(body_mass) == F)
    {stop("body_mass must be a number", call. = FALSE)
    }
    
    if("h1" %in% names(timeBudget) == F) timeBudget$h1 <- 0
    if("h2" %in% names(timeBudget) == F) timeBudget$h2 <- 0
    if("h3" %in% names(timeBudget) == F) timeBudget$h3 <- 0
    if("h4" %in% names(timeBudget) == F) timeBudget$h4 <- 0
    if("h5" %in% names(timeBudget) == F) timeBudget$h5 <- 0
    if("h6" %in% names(timeBudget) == F) timeBudget$h6 <- 0
    if("h7" %in% names(timeBudget) == F) timeBudget$h7 <- 0
    if("h8" %in% names(timeBudget) == F) timeBudget$h8 <- 0
    if("h9" %in% names(timeBudget) == F) timeBudget$h9 <- 0
    if("h10" %in% names(timeBudget) == F) timeBudget$h10 <- 0
    if("h11" %in% names(timeBudget) == F) timeBudget$h11 <- 0
    
    # merge weather and time budget
    dt <- merge(weatherData, timeBudget, all.x = T)

    # calculate thermal condutance (from Kendeigth et al 1977)
    Kes <- 0.0022 * body_mass^0.5886

    # Experienced wind speed (from Appendix 2)
    ExpU <- log10(breast_height / 0.005) / log10(0.09 / 0.005)

    # define condutance per microhabitat
    cond_h1 <- 0.00294
    cond_h2 <- 0.00478
    cond_h3 <- 0.00809
    cond_h4 <- 0.00899
    cond_h5 <- 0.00707
    cond_h6 <- 0.01164
    cond_h7 <- 0.00358
    cond_h8 <- 0.00455
    cond_h9 <- 0.00614
    cond_h10 <- 0.00829
    cond_h11 <- 0.00952

    # define radiative condutance per microhabitat
    rad_cond_h1 <- 0.00103
    rad_cond_h2 <- 0.00139
    rad_cond_h3 <- 0.00080
    rad_cond_h4 <- 0.00197
    rad_cond_h5 <- 0.00140
    rad_cond_h6 <- 0.00107
    rad_cond_h7 <- 0.00052
    rad_cond_h8 <- 0.00063
    rad_cond_h9 <- 0.00048
    rad_cond_h10 <- 0.00067
    rad_cond_h11 <- 0.00062


    # calculate the heatloss for each microhabitat
    dt$Hsm_h1 <- (0.045 + cond_h1 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h1 * dt$GHI
    dt$Hsm_h2 <- (0.045 + cond_h2 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h2 * dt$GHI
    dt$Hsm_h3 <- (0.045 + cond_h3 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h3 * dt$GHI
    dt$Hsm_h4 <- (0.045 + cond_h4 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h4 * dt$GHI
    dt$Hsm_h5 <- (0.045 + cond_h5 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h5 * dt$GHI
    dt$Hsm_h6 <- (0.045 + cond_h6 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h6 * dt$GHI
    dt$Hsm_h7 <- (0.045 + cond_h7 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h7 * dt$GHI
    dt$Hsm_h8 <- (0.045 + cond_h8 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h8 * dt$GHI
    dt$Hsm_h9 <- (0.045 + cond_h9 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h9 * dt$GHI
    dt$Hsm_h10 <- (0.045 + cond_h10 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h10 * dt$GHI
    dt$Hsm_h11 <- (0.045 + cond_h11 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h11 * dt$GHI


    # calculate the STANDARD OPERATIVE TEMPERATURE
    dt$Tes_h1 <- 41 - dt$Hsm_h1 / 0.055
    dt$Tes_h2 <- 41 - dt$Hsm_h2 / 0.055
    dt$Tes_h3 <- 41 - dt$Hsm_h3 / 0.055
    dt$Tes_h4 <- 41 - dt$Hsm_h4 / 0.055
    dt$Tes_h5 <- 41 - dt$Hsm_h5 / 0.055
    dt$Tes_h6 <- 41 - dt$Hsm_h6 / 0.055
    dt$Tes_h7 <- 41 - dt$Hsm_h7 / 0.055
    dt$Tes_h8 <- 41 - dt$Hsm_h8 / 0.055
    dt$Tes_h9 <- 41 - dt$Hsm_h9 / 0.055
    dt$Tes_h10 <- 41 - dt$Hsm_h10 / 0.055
    dt$Tes_h11 <- 41 - dt$Hsm_h11 / 0.055

    # calculate the temporary Mmaint for each microhabitat
    dt$m_maint_h1 <- Kes * (41 - dt$Tes_h1)
    dt$m_maint_h2 <- Kes * (41 - dt$Tes_h2)
    dt$m_maint_h3 <- Kes * (41 - dt$Tes_h3)
    dt$m_maint_h4 <- Kes * (41 - dt$Tes_h4)
    dt$m_maint_h5 <- Kes * (41 - dt$Tes_h5)
    dt$m_maint_h6 <- Kes * (41 - dt$Tes_h6)
    dt$m_maint_h7 <- Kes * (41 - dt$Tes_h7)
    dt$m_maint_h8 <- Kes * (41 - dt$Tes_h8)
    dt$m_maint_h9 <- Kes * (41 - dt$Tes_h9)
    dt$m_maint_h10 <- Kes * (41 - dt$Tes_h10)
    dt$m_maint_h11 <- Kes * (41 - dt$Tes_h11)

    # final maintenance metabolism
    dt$m_maint <- (dt$m_maint_h1 * dt$h1) +
              (dt$m_maint_h2 * dt$h2) +
              (dt$m_maint_h3 * dt$h3) +
              (dt$m_maint_h4 * dt$h4) +
              (dt$m_maint_h5 * dt$h5) +
              (dt$m_maint_h6 * dt$h6) +
              (dt$m_maint_h7 * dt$h7) +
              (dt$m_maint_h8 * dt$h8) +
              (dt$m_maint_h9 * dt$h9) +
              (dt$m_maint_h10 * dt$h10) +
              (dt$m_maint_h11 * dt$h11)

    # remove the columns created to calculate the SMR and return data with final Mmaint
    dt <- dt[, !(grepl("Hsm_h", names(dt)) | grepl("Tes_h", names(dt)) | grepl("m_maint_h", names(dt)))]
    
    if (!exists("BMR")) {
      with(na.omit(dt[c(1:360),]), plot(date, m_maint, pch=20, xlab = "Date", ylab = "Maintenance metabolism (W)"))
      with(na.omit(dt[c(1:360),]), lines(date[order(date)], m_maint[order(date)]))
      
    } else {
      with(na.omit(dt[c(1:360),]), plot(date, m_maint, pch=20, xlab = "Date", ylab = "Maintenance metabolism (W)",
                                        ylim= c(min(m_maint), max(m_maint)+0.2)))
      with(na.omit(dt[c(1:360),]), lines(date[order(date)], m_maint[order(date)]))
      abline(h=BMR, col = "blue")
      with(na.omit(dt[c(1:360),]), text(x = min(date), y = max(m_maint)+0.1, "BMR", adj = c(0,0), col = "blue"))
    }
    print(paste("m_maint range:",round(min(dt$m_maint, na.rm = T), 3), "-", round(max(dt$m_maint, na.rm = T),3)))
    
    return(dt)

  } else {

    # check data is correcly provided
    if (missing(weatherData) | missing(timeBudget) |
        missing(breast_height) | missing(body_mass) )
    {stop("weatherData, timeBudget, breast_height and/or body_mass is missing")
    }
    
    if (checkmate::testDataFrame(weatherData) == F)
    {stop("weatherData data must be a data frame", call. = FALSE)
    }
    
    if (checkmate::testDataFrame(tide_table) == F)
    {stop("Tide table must be a data frame", call. = FALSE)
    }
    
    if (checkmate::testDataFrame(timeBudget) == F)
    {stop("timeBudget must be a data frame", call. = FALSE)
    }
    
    if (checkmate::test_numeric(breast_height) == F)
    {stop("breast_height must be a number", call. = FALSE)
    } # maybe here we can set limits - see paper to find appropriate values
    
    if (checkmate::test_numeric(body_mass) == F)
    {stop("body_mass must be a number", call. = FALSE)
    }
    
    if("h1" %in% names(timeBudget) == F) timeBudget$h1 <- 0
    if("h2" %in% names(timeBudget) == F) timeBudget$h2 <- 0
    if("h3" %in% names(timeBudget) == F) timeBudget$h3 <- 0
    if("h4" %in% names(timeBudget) == F) timeBudget$h4 <- 0
    if("h5" %in% names(timeBudget) == F) timeBudget$h5 <- 0
    if("h6" %in% names(timeBudget) == F) timeBudget$h6 <- 0
    if("h7" %in% names(timeBudget) == F) timeBudget$h7 <- 0
    if("h8" %in% names(timeBudget) == F) timeBudget$h8 <- 0
    if("h9" %in% names(timeBudget) == F) timeBudget$h9 <- 0
    if("h10" %in% names(timeBudget) == F) timeBudget$h10 <- 0
    if("h11" %in% names(timeBudget) == F) timeBudget$h11 <- 0
    
    
    # create a tide code column to be used to merge with time budget
    weatherData <- tidal_codes(weatherData, tide_table)

    # merge weather and time budget
    dt <- merge(weatherData, timeBudget, by = "tide_code", all.x = T)

    # calculate thermal condutance (from Kendeigth et al 1977)
    Kes <- 0.0022 * body_mass^0.5886

    # Experienced wind speed (from Appendix 2)
    ExpU <- log10(breast_height / 0.005) / log10(0.09 / 0.005)

    # define condutance per microhabitat
    cond_h1 <- 0.00294
    cond_h2 <- 0.00478
    cond_h3 <- 0.00809
    cond_h4 <- 0.00899
    cond_h5 <- 0.00707
    cond_h6 <- 0.01164
    cond_h7 <- 0.00358
    cond_h8 <- 0.00455
    cond_h9 <- 0.00614
    cond_h10 <- 0.00829
    cond_h11 <- 0.00952

    # define radiative condutance per microhabitat
    rad_cond_h1 <- 0.00103
    rad_cond_h2 <- 0.00139
    rad_cond_h3 <- 0.00080
    rad_cond_h4 <- 0.00197
    rad_cond_h5 <- 0.00140
    rad_cond_h6 <- 0.00107
    rad_cond_h7 <- 0.00052
    rad_cond_h8 <- 0.00063
    rad_cond_h9 <- 0.00048
    rad_cond_h10 <- 0.00067
    rad_cond_h11 <- 0.00062


    # calculate the heatloss for each microhabitat
    dt$Hsm_h1 <- (0.045 + cond_h1 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h1 * dt$GHI
    dt$Hsm_h2 <- (0.045 + cond_h2 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h2 * dt$GHI
    dt$Hsm_h3 <- (0.045 + cond_h3 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h3 * dt$GHI
    dt$Hsm_h4 <- (0.045 + cond_h4 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h4 * dt$GHI
    dt$Hsm_h5 <- (0.045 + cond_h5 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h5 * dt$GHI
    dt$Hsm_h6 <- (0.045 + cond_h6 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h6 * dt$GHI
    dt$Hsm_h7 <- (0.045 + cond_h7 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h7 * dt$GHI
    dt$Hsm_h8 <- (0.045 + cond_h8 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h8 * dt$GHI
    dt$Hsm_h9 <- (0.045 + cond_h9 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h9 * dt$GHI
    dt$Hsm_h10 <- (0.045 + cond_h10 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h10 * dt$GHI
    dt$Hsm_h11 <- (0.045 + cond_h11 * ((dt$wind_speed * ExpU)^0.75)) * (41 - dt$temperature) - rad_cond_h11 * dt$GHI


    # calculate the STANDARD OPERATIVE TEMPERATURE
    dt$Tes_h1 <- 41 - dt$Hsm_h1 / 0.055
    dt$Tes_h2 <- 41 - dt$Hsm_h2 / 0.055
    dt$Tes_h3 <- 41 - dt$Hsm_h3 / 0.055
    dt$Tes_h4 <- 41 - dt$Hsm_h4 / 0.055
    dt$Tes_h5 <- 41 - dt$Hsm_h5 / 0.055
    dt$Tes_h6 <- 41 - dt$Hsm_h6 / 0.055
    dt$Tes_h7 <- 41 - dt$Hsm_h7 / 0.055
    dt$Tes_h8 <- 41 - dt$Hsm_h8 / 0.055
    dt$Tes_h9 <- 41 - dt$Hsm_h9 / 0.055
    dt$Tes_h10 <- 41 - dt$Hsm_h10 / 0.055
    dt$Tes_h11 <- 41 - dt$Hsm_h11 / 0.055

    # calculate the temporary Mmaint for each microhabitat
    dt$m_maint_h1 <- Kes * (41 - dt$Tes_h1)
    dt$m_maint_h2 <- Kes * (41 - dt$Tes_h2)
    dt$m_maint_h3 <- Kes * (41 - dt$Tes_h3)
    dt$m_maint_h4 <- Kes * (41 - dt$Tes_h4)
    dt$m_maint_h5 <- Kes * (41 - dt$Tes_h5)
    dt$m_maint_h6 <- Kes * (41 - dt$Tes_h6)
    dt$m_maint_h7 <- Kes * (41 - dt$Tes_h7)
    dt$m_maint_h8 <- Kes * (41 - dt$Tes_h8)
    dt$m_maint_h9 <- Kes * (41 - dt$Tes_h9)
    dt$m_maint_h10 <- Kes * (41 - dt$Tes_h10)
    dt$m_maint_h11 <- Kes * (41 - dt$Tes_h11)

    # final maintenance metabolism
    dt$m_maint <- (dt$m_maint_h1 * dt$h1) +
              (dt$m_maint_h2 * dt$h2) +
              (dt$m_maint_h3 * dt$h3) +
              (dt$m_maint_h4 * dt$h4) +
              (dt$m_maint_h5 * dt$h5) +
              (dt$m_maint_h6 * dt$h6) +
              (dt$m_maint_h7 * dt$h7) +
              (dt$m_maint_h8 * dt$h8) +
              (dt$m_maint_h9 * dt$h9) +
              (dt$m_maint_h10 * dt$h10) +
              (dt$m_maint_h11 * dt$h11)

    # remove the columns created to calculate the SMR and return data with final Mmaint
    dt <- dt[, !(grepl("Hsm_h", names(dt)) | grepl("Tes_h", names(dt)) | grepl("m_maint_h", names(dt)))]
    
    if (!exists("BMR")) {
      with(na.omit(dt[c(1:360),]), plot(date, m_maint, pch=20, xlab = "Date", ylab = "Maintenance metabolism (W)"))
      with(na.omit(dt[c(1:360),]), lines(date[order(date)], m_maint[order(date)]))
      
    } else {
      with(na.omit(dt[c(1:360),]), plot(date, m_maint, pch=20, xlab = "Date", ylab = "Maintenance metabolism (W)",
                                        ylim= c(min(m_maint), max(m_maint)+0.2)))
      with(na.omit(dt[c(1:360),]), lines(date[order(date)], m_maint[order(date)]))
      abline(h=BMR, col = "blue")
      with(na.omit(dt[c(1:360),]), text(x = min(date), y = max(m_maint)+0.1, "BMR", adj = c(0,0), col = "blue"))
    }
    print(paste("m_maint range:",round(min(dt$m_maint, na.rm = T), 3), "-", round(max(dt$m_maint, na.rm = T),3)))
    
    return(dt)
  }
}
