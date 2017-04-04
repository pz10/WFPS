
setkey(data, treatment)
setkey(treatments, treatment)
data <- treatments[data]

data[, event:= as.character(NA)]
data[days.adjusted<0, event:= "0_pre"]
data[days.adjusted>=0 & days.adjusted<=14, event:= "1st"]
data[days.adjusted>=15 & days.adjusted<=29, event:= "2nd"]
data[days.adjusted>=30 & days.adjusted<=44, event:= "3rd"]

data[, NO:= as.numeric(NA)]
data[, N2O:= as.numeric(NA)]
data[, CO2:= as.numeric(NA)]
data[, CH4:= as.numeric(NA)]
data[, WFPS:= as.numeric(NA)]
data[, WFPSg:= as.numeric(NA)]
data[, WFPS_0:= as.numeric(NA)]
data[, WFPS_f:= as.numeric(NA)]

ABCDfluxes[incubation=="D" , CH4:=0]
for(i in 1:18){
        t <- treat[i]
        for(j in LETTERS[1:4]){
                inc <- j
                myNO <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(NO)]],
                               y = ABCDfluxes[incubation==inc & treatment== t, NO[!is.na(NO)]],
                               xout = days,
                               rule = 2)                
                myN2O <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(N2O)]],
                                y = ABCDfluxes[incubation==inc & treatment== t, N2O[!is.na(N2O)]],
                                xout = days,
                                rule = 2)
                myCO2 <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(CO2)]],
                                y = ABCDfluxes[incubation==inc & treatment== t, CO2[!is.na(CO2)]],
                                xout = days,
                                rule = 2)
                myCH4 <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(CH4)]],
                                y = ABCDfluxes[incubation==inc & treatment== t, CH4[!is.na(CH4)]],
                                xout = days,
                                rule = 2)
                myWFPS <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(WFPS)]],
                                   y = ABCDfluxes[incubation==inc & treatment== t, WFPS[!is.na(WFPS)]],
                                   xout = days,
                                   rule = 2)
                myWFPSg <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(WFPSg)]],
                                 y = ABCDfluxes[incubation==inc & treatment== t, WFPSg[!is.na(WFPSg)]],
                                 xout = days,
                                 rule = 2)
                myWFPS_0 <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(WFPS_0)]],
                                   y = ABCDfluxes[incubation==inc & treatment== t, WFPS_0[!is.na(WFPS_0)]],
                                   xout = days,
                                   rule = 2)
                myWFPS_f <- approx(x = ABCDfluxes[incubation==inc & treatment== t, days.adjusted[!is.na(WFPS_f)]],
                                   y = ABCDfluxes[incubation==inc & treatment== t, WFPS_f[!is.na(WFPS_f)]],
                                   xout = days,
                                   rule = 2)
                myNO <- myNO$y
                myN2O <- myN2O$y
                myCO2 <- myCO2$y
                myCH4 <- myCH4$y
                myWFPS <- myWFPS$y
                myWFPSg <- myWFPSg$y
                myWFPS_0 <- myWFPS_0$y
                myWFPS_f <- myWFPS_f$y
                
                data[incubation==inc & treatment == t, NO:= myNO]
                data[incubation==inc & treatment == t, N2O:= myN2O]
                data[incubation==inc & treatment == t, CO2:= myCO2]
                data[incubation==inc & treatment == t, CH4:= myCH4]
                data[incubation==inc & treatment == t, WFPS:= myWFPS]
                data[incubation==inc & treatment == t, WFPSg:= myWFPSg]
                data[incubation==inc & treatment == t, WFPS_0:= myWFPS_0]
                data[incubation==inc & treatment == t, WFPS_f:= myWFPS_f]
        }
}
data[incubation=="D" , CH4:=NA]
setnames(data, "fertilizer", "fertilization")

# 
# 
# write file
mydata <- copy (data)
setkey(mydata, incubation, fertilization, precipitation, tillage, days.adjusted)
to.format4 <- c("NO", "N2O", "CO2", "CH4", "WFPS", "WFPSg", "WFPS_0", "WFPS_f")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfolder <- paste0(output_path, "/WFPS")
dir.create(myfolder, recursive=T)
myfile <- paste0(myfolder, "/3h_interpolation.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)