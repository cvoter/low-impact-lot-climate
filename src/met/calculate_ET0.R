#' calculate_ET0.R
#' 
#' Calculates reference ET (ET0) using the FAO Penman Montieth equations
#' 
#' This function calculates FAO Penman Monteith reference evapotranspiration 
#' (ET0) in mm/hr for hourly time steps. Note that daily ET0 requires a 
#' different equation.
#' 
#' REFERENCES
#' Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop
#' evapotranspiration: Guidelines for computing crop water requirements. Rome:
#' FAO. Retrieved from http://www.fao.org/docrep/X0490E/x0490e00.htm
#' 
#' REQUIRE
#' NISTunits (package)
#' 
#' INPUTS
#' elevation:             elevation above sea level (m)
#' latitude.degrees:      latitude of city (degrees)
#' longitude.timezone:    longitude of timing info time zone, default: 0
#' longitude.measurement: longitude of city (degrees west of Greenwich)
#' julian.day:            julian day of every hour in the time series
#' met.pressure:          pressure (kPa)
#' wind.measured:         wind speed (m/s)
#' temperature.K:         air temperature (K)
#' Rs:                    incoming shortwave radiation (W/m^2)
#' humidity:              specific humidity (kg/kg)
#' nhours:                number of hours in time series
#' 
#' OUTPUTS
#' ET0: vector with ET0 rate (mm/hr) for each input time step
calculate_ET0 <- function(elevation,
                          latitude.degrees,
                          longitude.timezone = 0, 
                          longitude.measurement,
                          julian.day,
                          met.pressure, 
                          wind.measured,
                          temperature.K,
                          Rs,
                          humidity, 
                          nhours) {
  # INITIALIZE ----------------------------------------------------------------
  library(NISTunits)
  ET0 <- NULL

  # define additional parameters not specific to location
  albedo        <- 0.23  # albedo for green grass reference crop (p.43)
  wind.height   <- 10  # distance above ground at which wind measured (m)
  temperature.C <- temperature.K - 273.15  # temperature in Celsius
  latitude      <- NISTdegTOradian(latitude.degrees)
  
  # LOOP THROUGH TIME SERIES --------------------------------------------------
  for (i in 1:nhours) {
    # day
    this.julian <- julian.day[i]
    
    # inverse relative distance Earth to Sun (Eq.23, p.46)
    inverse.distance <- 1 + 0.033*cos(2*pi*this.julian/365)
    
    # seasonal correction for solar time (hour) (Eq.32-33, p.48)
    b                   <- 2*pi*(this.julian - 81)/364
    seasonal.correction <- 0.1645*sin(2*b) - 0.1255*cos(b) - 0.025*sin(b)
    
    # solar time angles (radians) (Eq.29-31, p.48)
    omega            <- (pi/12)*(((i %% 24 + 0.5) + 
                        0.06667*(longitude.timezone - longitude.measurement) + 
                        seasonal.correction) - 12)  # @ midpoint of time period
    omega.start      <- omega - pi*1/24  # @ beginning of time period
    omega.end        <- omega + pi*1/24  # @ end of time period
    
    # solar declination (radians) (Eq.24, p.46)
    solar.declination <- 0.409*sin(2*pi*this.julian/365 - 1.39)
    
    # sunset solar time angle (radians) (Eq. 25, p.46)
    omega.solar       <- acos(-tan(latitude)*tan(solar.declination))
    omega.solar.start <- omega.solar - 0.79  # p.75
    omega.solar.end   <- omega.solar - 0.52  # p.75
    
    # determine if it is day or night
    if ( (omega > -omega.solar) & (omega < omega.solar) ) {
      # daytime
      day = 1
      night = 0
    } else {
      # nighttime
      day = 0
      night = 1
    }
    
    # AIR AND HUMIDITY PARAMETERS ---------------------------------------------
    # atmospheric pressure (kPa) (Eq.7, p.31)
    atmospheric.pressure <- 101.3*((293 - 0.0065*elevation)/293)^5.26
    
    # psychrometric constant (kPa/degC) (Eq.8, p.32)
    gamma <- (0.665e-3)*atmospheric.pressure
    
    # saturation vapor pressure (kPa) (Eq.11, p.36)
    vp.saturation <- 0.6108*exp(17.27*temperature.C[i]/(temperature.C[i]+237.3))
    
    # slope of saturation vapor pressure curve (Eq.13, p.37)
    delta <- 4098*vp.saturation/((temperature.C[i] + 237.3)^2)
    
    # actual vapor pressure (kPa), based on Bolton, 1980 (Eq.16)
    # assume: (specific humidity, kg/kg)*(1000 g/kg) = (mixing ratio, g/kg)
    vp.actual <- met.pressure[i]*humidity[i]/(0.622 + humidity[i])
    
    # vapor pressure deficit (kPa)
    vpd <- max(c(vp.saturation - vp.actual, 0))
    
    # RADIATION --------------------------------------------------------------
    # extraterrestrial radiation (MJ/m^2*hr) (Eq.28,p.47, also see note p.75)
    Ra.day <- 12*60/pi*0.0820*inverse.distance*(
              (omega.end - omega.start)*sin(latitude)*
                sin(solar.declination) + 
                cos(latitude)*cos(solar.declination)*
                (sin(omega.end)-sin(omega.start)))
    Ra.night <- 12*60/pi*0.0820*inverse.distance*(
              (omega.solar.end-omega.solar.start)*sin(latitude)*
                sin(solar.declination) + 
                  cos(latitude)*cos(solar.declination)*
                  (sin(omega.solar.end)-sin(omega.solar.start)))
    
    # clear sky radiation (MJ/m^2*hr) (Eq.37, p.51)
    Rso <- (0.75 + (2e-5)*elevation)*(Ra.day*day + Ra.night*night)
    
    # Stefan-Boltzman constant, converted to (MJ/K^4*m^2*hr) (p.74)
    stefan.boltzman <- (4.903e-9)/24
    
    # net longwave radiation (MJ/m^2*hr) (Eq.39, p.52)
    Rnl <- stefan.boltzman*temperature.K[i]^4*
      (0.34 - 0.14*sqrt(vp.actual))*(1.35*min(Rs[i]/Rso,1) - 0.35)
    
    # net shortwave radiation (MJ/m^2*hr) (Eq.38, p.51)
    Rns <- (1 - albedo)*Rs[i]
    
    # net radiation (MJ/m^2*hr) (Eq.40, p.53)
    Rn <- Rns - Rnl
    
    # Ground heat (MJ/m^2*hr) (Eq.45-46, p.55)
    G <- 0.1*Rn*day + 0.5*Rn*night
    
    # Wind speed at 2m off the ground (m/s) (Eq.47, p.56)
    wind.2m <- wind.measured[i]*4.87/(log(67.8*wind.height - 5.42))
    
    # FAO Penman-Monteith reference evapotranspiration (mm/hr) (Eq.53, p.74)
    ET0[i]<-(0.408*delta*(Rn - G) + gamma*(37/temperature.K[i])*wind.2m*vpd)/
             (delta + gamma*(1 + 0.34*wind.2m))
  }
  return(ET0)
}