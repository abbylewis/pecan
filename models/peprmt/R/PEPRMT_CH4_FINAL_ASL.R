###################################################
###  PEPRMT-Tidal Methane Module v2.0### 
###################################################

#Re-written by A. S. L. Lewis
#Originally developed by Patty Oikawa
#patty.oikawa@gmail.com

#About the model:
#1. Originally PEPRMT (the Peatland Ecosystem Photosynthesis and Methane Transport) model 
#was parameterized for restored freshwater wetlands in the Sacramento-San Joaquin River Delta, CA USA
# Oikawa et al. 2017 https://doi.org/10.1002/2016JG003438
#Presented here is an updated version that works for tidal wetlands 
#and inhibits CH4 production in response to Salinity and nitrate (Oikawa et al. 2023)
#2. All PEPRMT modules use the same input structure (data) however not all models use all variables in the structure;
#3. All variables are at the daily time step.
#4. Modules are run in succession, first GPP, then Reco and last CH4


#Inputs: 
#1. Theta: a vector of 7 parameter values that were determined via MCMC Bayesian fitting 
#theta= <- c( 14.9025078, 0.4644174, 16.7845002, 0.4359649, 15.8857612,0.5120464, 486.4106939, 0.1020278 )
#See Oikawa et al. 2023 

#2. Data: a data frame containing 18 variables described at start of function.

#3. Wetland_type
# - "wetland_type == 1" corresponds to a "freshwater peatland"
# - "wetland_type == 2" corresponds to a "tidal wetland" 

#Outputs:
# a data frame containing
# 1. pulse_emission_total: total amount of methane emitted, which is the sum of plant-mediated + diffusive water fluxes.
# 2. F_plant: net amount of methane released from plants.
# 3. Hydro_flux: net amount of methane that transfer from water to atm.
# 4. CH4prod_SOC: pool of methane produced from soil carbon pool 1, the labile pool
# 5. CH4prod_labile: pool of methane produced from soil carbon pool 2, the SOC pool
# 6. GPP_fract: fraction of CH4 released via plant-mediated transport

#units of output
# 1. pulse_emission_total: g C methane m^-2 day^-1
# 2. F_plant: g C methane m^-2 day^-1
# 3. Hydro_flux: g C methane m^-2 day^-1
# 4. CH4prod_SOC: g C methane m^-3 (includes top m3 of soil+water)
# 5. CH4prod_labile: g C methane m^-3 (includes top m3 of soil+water)
# 6. GPP_fract: unitless

library(tidyverse)

PEPRMT_CH4_FINAL <- function(theta,
                             data,
                             wetland_type){
  #CH4 PARAMETERS
  #SOC pool
  M_alpha1 <- 6.2e13 # gC m-3 d-1 
  Ea_CH4_SOC <- theta[1]*1000 #parameter in kJ mol-1 multiplied by 1000 = J mol-1
  kM_CH4_SOC <- theta[2] #g C m-3 
  
  #Labile C pool
  M_alpha2 <- 6.2e14 # gC m-3 d-1 
  Ea_CH4_labile <- theta[3]*1000 #J mol-1
  kM_CH4_labile <- theta[4] #g C m-3 
  
  #CH4 oxidation parameters
  M_alpha3 <- 6.2e13 # gC m-3 d-1 
  Ea_CH4_oxi <- theta[5]*1000 #J mol-1
  kM_CH4oxi <- theta[6] #g C m-3 
  
  #Salinity sulfate parameters
  kI_SO4 <- theta[7] #mg L^-1 
  kI_NO3 <- theta[8] #mg L^-1 
  
  #Parameters for hydrodynamic flux
  k_hydro_max <- 0.04 #gas transfer velocity (m day-1)
  
  #Parameters for plant-mediated transport
  k_plant <- 0.24 #gas transfer velocity through plants(m d-1)
  V_oxi_plant <- 0.35 #percent oxidized during transport
  
  #Abby's new parameters
  oxi_depth_sens <- 10 #this parameter changes how sensitive oxidation is to depth below the soil surface
  #lower = more sensitive
  fract_outflow <- 0.6 #When water level declines, 60% of the methane in that volume of water is exported
  bottom_bound_m = 1
  
  #---CREATE A SPACE TO COLLECT RESULTS---#
  q <- unique(as.integer(data$site))
  outcome_lst <- vector('list', length(q))   
  
  #---LOOP TO RUN THE MODEL ACROSS DIFFERENT SITES---#
  for(i in 1:length(q)){
    #  subset your data here, then create the exogenous variables here
    d <- subset(data, site == i)
    
    #Time Invariant
    WTD_cm_adj <- d$WTD_cm/100 + bottom_bound_m #convert to m and adjust because we are modeling from 1m below the sediment to the atmosphere
    WTD_cm_adj[WTD_cm_adj < 0] <- 0.000001 #make sure WTD_cm_adj is never negative or 0 (WTD < bottom bound)
    R <- 8.314 #J K mol-1
    RT <- R * (d$TA_C + 274.15) #T in Kelvin-all units cancel out
    Vmax_CH4_SOC <- M_alpha1 * exp(-Ea_CH4_SOC / RT) #g C m-2 d-1 
    Vmax_CH4_labile <- M_alpha2 * exp(-Ea_CH4_labile / RT) #gC m-2 d-1 
    Vmax_CH4oxi <- M_alpha3 * exp(-Ea_CH4_oxi / RT) #gC m-2 d-1 
    k_hydro <- ifelse(WTD_cm_adj > bottom_bound_m, k_hydro_max, 
                      0) #if WTD is below soil surface, cut off diffusion
    
    #Calculate sulfate from Salinity
    conc_so4AV <- 0.074 * d$Salinity_daily_ave_ppt * 1000 # ppm or mg L-1
    
    #Calculate the gpp-dependent parameter for plant-mediated transport at each timestep
    GPP_fract <- (d$EVI - min(d$EVI))/(max(d$EVI) - min(d$EVI))
    #GPP_fract <- -d$V16 / max(-d$V16) #GPP values are negative- convert to positive
    
    ### Calculate CH4prod at each timestep from SOC pools
    #CH4 from total SOC pool
    CH4prod_SOC <- (Vmax_CH4_SOC * (d$SOM_total /(kM_CH4_SOC + d$SOM_total))) * 
      (kI_NO3 / (kI_NO3 + d$NO3_mg_L)) *
      (kI_SO4 / (kI_SO4 + conc_so4AV)) #gC CH4 prod d-1 
    #CH4 from labile SOC pool
    CH4prod_labile <- (Vmax_CH4_labile * (d$SOM_labile /(kM_CH4_labile + d$SOM_labile)))  * 
      (kI_NO3 / (kI_NO3 + d$NO3_mg_L)) *
      (kI_SO4 / (kI_SO4 + conc_so4AV)) #gC CH4 prod d-1 
    CH4prod <- CH4prod_SOC + CH4prod_labile #total CH4 produced at this time step in gC m-3 soil day-1
    
    #preallocating space for loop
    CH4water <- vector('numeric', length(d$DOY))
    Hydro_flux <- vector('numeric', length(d$DOY))
    Plant_flux <- vector('numeric', length(d$DOY))
    F_plant <- vector('numeric', length(d$DOY))
    CH4water_store <- vector('numeric', length(d$DOY))
    
    #--METHANE TRANSPORT ACROSS DATA---
    for(t in 1:length(d$DOY)) {
      
      #Initialize methane
      if (t == 1) { 
        CH4water_init <- 0 # Assume no methane in the water to start
      } else {
        CH4water_g_prev <- CH4water_store[t-1] * WTD_cm_adj[t-1]
        #If water level decreased, assume some methane left with it
        if(WTD_cm_adj[t-1] > WTD_cm_adj[t]) { 
          CH4_exported <- CH4water_g_prev *
            ((WTD_cm_adj[t-1] - WTD_cm_adj[t]) / WTD_cm_adj[t-1]) * #fractional water loss
            fract_outflow #fraction of methane exported
        } else {
          CH4_exported <- 0
        }
        #account for any changes in concentration of CH4 due to any change in WTD_cm_adj height
        CH4water_init <- CH4water_g_prev - CH4_exported # gC
      } 
      
      #Total methane prior to oxidation 
      # methane produced / water volume (gC CH4 m-3)
      CH4water_pre_oxi <- (CH4prod[t] + CH4water_init) / WTD_cm_adj[t]
      
      if(WTD_cm_adj[t] > bottom_bound_m){
        CH4_oxi <- 0 #No oxidation when water is above the soil
      } else {
        Vmax_CH4oxi[t] <- Vmax_CH4oxi[t] + Vmax_CH4oxi[t] * (bottom_bound_m - WTD_cm_adj[t])
        CH4_oxi <- Vmax_CH4oxi[t] * CH4water_pre_oxi / (kM_CH4oxi + CH4water_pre_oxi)  #gC CH4 m-3 Reaction velocity
        }
      
      CH4water[t] <- CH4water_pre_oxi - CH4_oxi  #gC CH4 m-3
      
      #based on the concentrations in the soil and water, calculate hydro and plant-mediated fluxes
      Hydro_flux[t] <- k_hydro[t] * CH4water[t]  #gC CH4 m-2 day-1  Hydrodynamic flux 
      Plant_flux[t] <- k_plant * CH4water[t] * GPP_fract[t]  #gC CH4 m-2 day-1  Bulk Plant mediated transport 
      F_plant[t] <- Plant_flux[t] * V_oxi_plant  ##gC CH4 m-2 day-1  Plant mediated transport after oxidation
      
      #subtract the moles of methane lost from the pools (soil and water) to the atm
      CH4water_store[t] <- CH4water[t] - Hydro_flux[t] - Plant_flux[t]  #gC CH4 m-3 stored in the system
      
      # If you have water, then the CH4 should mix between the 2 layers and concentrations should be the same in water and soil
    }
    
    w <- (data.frame(pulse_emission_total = F_plant+Hydro_flux, #gC CH4 m-2 day-1 total CH4 flux to atm
                     Plant_flux_net = F_plant,
                     Hydro_flux,
                     CH4prod_SOC,
                     CH4prod_labile,
                     GPP_fract,
                     Time_2 = d$DOY,
                     site = rep(i, length(F_plant))))
    
    # store d in a vector  
    outcome_lst[[i]] <- (w) 
  }
  
  # combine iterations of loop and return all results
  peprmtCH4 <- do.call('rbind', outcome_lst) %>% 
    as.data.frame(.)
  
  return(peprmtCH4)
}
