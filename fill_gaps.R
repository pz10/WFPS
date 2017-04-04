# fill gaps at event edges to perform a proppper interpolation
tobind.0 <- ABCDfluxes[days.adjusted <0, .(
        days.adjusted = 0,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]
tobind.15 <- ABCDfluxes[days.adjusted>5 & days.adjusted <15, .(
        days.adjusted = 15,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]
tobind.30 <- ABCDfluxes[days.adjusted>20 & days.adjusted <30, .(
        days.adjusted = 30,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]

tobind.14 <- ABCDfluxes[days.adjusted>5 & days.adjusted <15, .(
        days.adjusted = 14,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]
tobind.29 <- ABCDfluxes[days.adjusted>20 & days.adjusted <30, .(
        days.adjusted = 29,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]
tobind.44 <- ABCDfluxes[days.adjusted>35 & days.adjusted <45, .(
        days.adjusted = 44,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]

tobind.minus3 <- ABCDfluxes[days.adjusted>(-3) & days.adjusted <(-1), .(
        days.adjusted = -3,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = NO[!is.na(NO)][1],
        N2O = N2O[!is.na(N2O)][1],
        CO2 = CO2[!is.na(CO2)][1],
        CH4 = CH4[!is.na(CH4)][1],
        WFPS = WFPS[!is.na(WFPS)][1],
        WFPS_0 = WFPS_0[!is.na(WFPS_0)][1],
        WFPS_f = WFPS_f[!is.na(WFPS_f)][1],
        WFPSg = WFPSg[!is.na(WFPSg)][1]
), by=.(treatment, incubation)]

tobind.minus1 <- ABCDfluxes[days.adjusted>(-3) & days.adjusted <(-1), .(
        days.adjusted = -1,
        fertilizer = unique(fertilizer),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        NO = tail(NO[!is.na(NO)],1),
        N2O = tail(N2O[!is.na(N2O)],1),
        CO2 = tail(CO2[!is.na(CO2)],1),
        CH4 = tail(CH4[!is.na(CH4)],1),
        WFPS = tail(WFPS[!is.na(WFPS)],1),
        WFPS_0 = tail(WFPS_0[!is.na(WFPS_0)],1),
        WFPS_f = tail(WFPS_f[!is.na(WFPS_f)],1),
        WFPSg = tail(WFPSg[!is.na(WFPSg)],1)
), by=.(treatment, incubation)]

# remove edge values (to attach them in next step)
ABCDfluxes <- ABCDfluxes[!days.adjusted %in% c(-3,-1,0,14,15,29,30,44),]

ABCDfluxes <- rbindlist(list(ABCDfluxes,
                             tobind.minus3, tobind.minus1,
                             tobind.0, tobind.15, tobind.30,
                             tobind.14, tobind.29, tobind.44),
                        use.names=T)
setkey(ABCDfluxes, treatment, incubation, days.adjusted)
# ABCDfluxes <- ABCDfluxes[days.adjusted>=0]
