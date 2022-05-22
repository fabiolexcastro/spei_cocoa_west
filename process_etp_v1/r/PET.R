


ETo<-function(lat, z, TN, TX, RHmax, RHmin, FF, Rs, doy, albedo=0.23, method_ea="allRH", method_ETo="PM") {
  
  
  # lat = 13.9
  # z = 292
  # TN = 11.1 
  # TX = 27.5 
  # Rs = 13.2 
  # doy = 12 
  # method_ETo = 'Hs'
  
  ### Function to calculate Penman-Monteith and Hargreaves-Samani Potential Evapo-Transpiration, implementing the equations in Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. 'Crop evapotranspiration-Guidelines for Computing Crop Water requirements-FAO Irrigation and Drainage Paper 56'. FAO, Rome 300: 6541.
  
  ### Modified from Crespo, O, not dated (https://r-forge.r-project.org/scm/viewvc.php/*checkout*/convertBruceFormat/agriParameters.r?revision=280&root=xpos-r)
  
  ### For Penman-Monteith ETo, choice is given between the 4 methods described in Allen & al 1998 to compute the actual vapour pressure: using both RHmax and RHmin, RHmax only, RHmean or the minimum temperature to estimate dew temperature.
  
  ### required parameters:
  
  ## for all options:
  
  # lat: latitude of the site in decimal degrees ([?])
  
  # z: elevation above sea level [m]
  
  # TN: daily minimum temperature at 2m height [?C]
  
  # TX: daily maximum temperature at 2m height [?C]
  
  # method_ETo: the equation used to compute ETo. Default is FAO Penman-Monteith (method_ETo="PM"). method_ETo="HS" gives the Hargreaves-Samani equation.
  
  # data: a daily multivariate zoo time-series containing the required variables
  
  ## depending on the computation method chosen, see below:
  
  # RHmax maximum relative humidity [%] and RHmin minimum relative humidity [%]
  
  # or
  
  # RHmax maximum relative humidity [%]
  
  # or
  
  # RHmean [%] = (RHmax+RHmin)/2
  
  # Rs: incoming solar or shortwave radiation [MJ.m^(-2).day^(-1)]
  
  # FF: Wind speed
  
  # alb: albedo [dimensionless]. Default is alb=0.23 for the FAO hypothetical reference crop
  
  # method_ea: method used to compute the actual vapour pressure in the Penman-Monteith equation. A choice is given between using both RHmax and RHmin (default, method_ea="allRH", RHmax only (method_ea="RHmax"), RHmean (method_ea="RHmean") or the minimum temperature to estimate dew temperature (method_ea=="Tdew").
  
  ##################
  
  alb<-albedo
  
  # Tmean
  
  Tmean=(TN+TX)/2  ## Eq. 9 Allen & al. 1998.
  
  # RHmean
  
  if (method_ea=="RHmean") RHmean=(RHmax+RHmin)/2
  
  #Slope of saturation vapour pressure curve
  
  slopevap<-4098*0.6108*exp(17.27*Tmean/(Tmean+237.3))/((Tmean+237.3)^2) ## Eq. 13 Allen & al. 1998 [kPa/?C].
  
  # calculation of psychometric constant
  
  P <- 101.3*(((293-0.0065*z)/293)^5.26) ## Eq. 7 Allen & al. 1998 [kPa].
  Cp <- 0.001013 ## Allen & al. 1998 p. 32.
  epsilon <- 0.622 ## Allen & al. 1998 p. 32.
  lambda <- 2.45 ## Allen & al. 1998 p. 31. O. Cresto uses lambda<-2.501-(0.002361*Tmean)
  psychCon <- Cp*P/(epsilon*lambda) ## Eq. 8 Allen & al. 1998. but rounds values up.
  
  # Mean saturation vapour pressure
  eTmin <- 0.6108*exp(17.27*TN/(TN+237.3)) ## min saturation vapour pressure [kPa] Eq. 11 Allen & al. 1998.
  eTmax <- 0.6108*exp(17.27*TX/(TX+237.3)) ## max saturation vapour pressure [kPa] Eq. 11 Allen & al. 1998.
  es <- (eTmax+eTmin)/2 ## Eq. 12 Allen & al. 1998.
  
  ## Actual vapour pressure
  if(method_ETo=="PM") {
    # for RHmax and RHmin
    if(method_ea=="allRH") ea<-(eTmin*RHmax+eTmax*RHmin)/200 ## Eq. 17 Allen & al. 1998.
    # for RHmax
    if(method_ea=="RHmax") ea<-eTmin*RHmax/100 ## Eq. 18 Allen & al. 1998
    # for RH mean
    if(method_ea=="RHmean") ea<-RHmean*(eTmax+eTmin)/200 ## Eq. 19 Allen & al. 1998
    # In the absence of reliable relative humidity measurements
    if(method_ea=="Tdew") ea<-0.6108*exp(17.27*TN/(TN+237.3)) ## Eq. 48 Allen & al. 1998, but see limitations p 58. Should be checked against measured
    if(!method_ea %in% c("allRH", "RHmax", "RHmean", "Tdew")) print("Method for actual vapour pressure wrongly specified")
  }
  
  ## Extraterrestrial radiation
  # solar constant = 0.0820 MJ.m^(-2).min^(-1)
  Gsc <- 0.0820
  # convert site latitude to radians
  phi <- pi*lat/180            # Eq. 23 Allen & al. 1998.
  # julian day of the year
  #J <- as.numeric(format(index(data), "%j"))
  J <- doy
  # inverse relative distance Earth-Sun
  Dr <- 1+0.033*cos(2*pi*J/365)      # Eq. 23 Allen & al. 1998.
  # solar decimation [rad]
  delta <- 0.409*sin((2*pi*J/365)-1.39) # Eq. 24 Allen & al. 1998.
  # sunset hour angle [rad]
  Ws <- acos(-tan(phi)*tan(delta))        # Eq. 25 Allen & al. 1998.
  
  # Extraterrestrial radiation for daily periods [MJ.m^(-2).day^(-1)]
  eRad <- (24*60/pi)*Gsc*Dr*(Ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(Ws)) # Eq. 21 Allen & al. 1998.
  ## eRad <- zoo((24*60/pi)*Gsc*Dr*(Ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(Ws)), date1)
  
  if(method_ETo=="PM") {
    # net solar radiation
    Rns <- (1-albedo)*Rs ## Eq. 38 Allen & al. 1998. albedo = 0.23 for the FAO hypothetical crop reference
    # clear-sky solar radiation
    Rso <- (0.75+2*10^(-5)*z)*eRad ## Eq. 37 Allen & al. 1998
    # net longwave radiation
    SteBolCon <- 4.903*10^(-9) # Stefan-Boltzmann constant [4.903.10^(-9) MJ.K^(-4).m^(-2).day^(-1)]
    Rnl <- SteBolCon*(((TX+273.16)^4+(TN+273.16)^4)/2)*(0.34 - 0.14*sqrt(ea))*((1.35*ifelse((Rs/Rso)>1,1,Rs/Rso))-0.35) ## Eq. 39 Allen & al. 1998.
    # net radiation at the crop surface
    Rn <- Rns-Rnl ## Eq. 40 Allen & al. 1998.
  }
  
  # Soil heat flux
  G <-0 ## Eq. 42 Allen & al. 1998. G can be ignored for daily computations
  
  # FAO Penman-Monteith equation for reference evapotranspiration
  if(method_ETo=="PM"){
    result <- (0.408*slopevap*(Rn-G)+(psychCon*900*FF*(es-ea)/(Tmean+273)))/(slopevap+psychCon*(1+0.34*FF))  ## Eq. 6 Allen & al. 1998.
  }
  # Hargreaves ETo
  if(method_ETo=="HS"){
    result <- 0.0023*(Tmean+17.8)*sqrt(TX-TN)*eRad/lambda ## Eq. 52 Allen & al. 1998; multiplied by 1/lambda to convert from MJ.m^(-2).day^(-1) to mm.day.day^(-1)
  }
  # if(!method_ETo %in% c("PM", "HS")) print("ETo method wrongly specified")
  
  return(result)
}
