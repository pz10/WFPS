myplots <- paste0(output_path, "/WFPS/WFPS_vs_N/plots")
dir.create(myplots, recursive=T)
folderout <- paste0(output_path, "/WFPS/WFPS_vs_N")
dir.create(folderout, recursive=T)

myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

data <- fread(myfile)

wide <- 0.5
window <- 4 #in WFPS % units
################################################################################
# get mean cummulative fluxes by WFPS 'wide' stretches; WFPS_0 is used
# wide <- 0.5
cuts <- seq(from=wide/2, to= 110+wide/2, by=wide)
label.cut <- cuts[-1]-wide/2
data[, WFPS := cut(WFPS_0, breaks = cuts, labels=factor(label.cut))]
setkey(data, WFPS)

data[days.adjusted<45, event:=3]
data[days.adjusted<30, event:=2]
data[days.adjusted<15, event:=1]

##
cum.till <- data[,.(
        count.WFPS = .N,
        count.event1 = sum(event==1), # for later partition of density by events
        count.event2 = sum(event==2),
        count.event3 = sum(event==3),
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18,
        CO2 = sum(CO2, na.rm=T)*3/18,
        CH4.microgCm2core = sum(CH4, na.rm=T)*3/18
        
), by= .(WFPS, tillage)]
setkey(cum.till, WFPS, tillage)

cum.fert <- data[,.(
        count.WFPS = .N,
        count.event1 = sum(event==1), # for later partition of density by events
        count.event2 = sum(event==2),
        count.event3 = sum(event==3),
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18,
        CO2 = sum(CO2, na.rm=T)*3/18,
        CH4.microgCm2core = sum(CH4, na.rm=T)*3/18
        
), by= .(WFPS, fertilization)]
setkey(cum.fert, WFPS, fertilization)

cum.rain <- data[,.(
        count.WFPS = .N,
        count.event1 = sum(event==1), # for later partition of density by events
        count.event2 = sum(event==2),
        count.event3 = sum(event==3),
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18,
        CO2 = sum(CO2, na.rm=T)*3/18,
        CH4.microgCm2core = sum(CH4, na.rm=T)*3/18
        
), by= .(WFPS, precipitation)]
setkey(cum.rain, WFPS, precipitation)

##
cum <- data[,.(
        count.WFPS = .N,
        count.event1 = sum(event==1), # for later partition of density by events
        count.event2 = sum(event==2),
        count.event3 = sum(event==3),
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18,
        CO2 = sum(CO2, na.rm=T)*3/18,
        CH4.microgCm2core = sum(CH4, na.rm=T)*3/18
        
), by= WFPS]
setkey(cum, WFPS)
cum[, dens.event1:= count.event1/count.WFPS]
cum[, dens.event2:= count.event2/count.WFPS]
cum[, dens.event3:= count.event3/count.WFPS]

# get density curves
total <- cum[,sum(NO+N2O, na.rm=T)]
cum[, NO.dens:= 100*NO/total]
cum[, N2O.dens:= 100*N2O/total]
cum[, CO2.dens:= 100*CO2/sum(CO2, na.rm=T)]

# WFPS density curves
d <- density(data$WFPS_0, adjust = 1/2)
myd <- data.table(WFPS=d$x, y=d$y)

mywfps <- approx(x=d$x, y=d$y, xout=cum$WFPS)

cum[, WFPS.dens:= 100 * mywfps$y]

cum[, WFPS.dens.1:= WFPS.dens * dens.event1]
cum[, WFPS.dens.2:= WFPS.dens * dens.event2]
cum[, WFPS.dens.3:= WFPS.dens * dens.event3]
# cum[, WFPS.dens.3:= WFPS.dens.3 + WFPS.dens.2 + WFPS.dens.1]
# cum[, WFPS.dens.2:= WFPS.dens.2 + WFPS.dens.1]

# rearrange
myorder <- c("WFPS", "count.WFPS", "count.event1", "count.event2", "count.event3",
             "WFPS.dens", "WFPS.dens.1", "WFPS.dens.2", "WFPS.dens.3",
             "dens.event1", "dens.event2", "dens.event3",
             "NO", "N2O", "CO2", "CH4.microgCm2core",  "NO.dens" ,"N2O.dens", "CO2.dens")
setcolorder(cum, myorder)


################################################################################
# get rolling means over WFPS values  (rolling window == 'window') to smoothen the curves a bit
s.cum <- copy(cum)
# window <- 3 #in WFPS % units

myNO <- rollapply(cum$NO, FUN= mean, na.rm=T, width=window/wide+1, fill=NA) #fill="extend"
myN2O <- rollapply(cum$N2O, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
myCO2 <- rollapply(cum$CO2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
myCH4 <- rollapply(cum$CH4.microgCm2core, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)

s.cum[, NO:= myNO]
s.cum[, N2O:= myN2O]
s.cum[, CO2:= myCO2]
s.cum[, CH4:= myCH4]
s.cum[, WFPS:= as.numeric(as.character(WFPS))]

# get density curves
total <- s.cum[,sum(NO+N2O, na.rm=T)]
s.cum[, NO:= 100*NO/total]
s.cum[, N2O:= 100*N2O/total]
s.cum[, CO2:= 100*CO2/sum(CO2, na.rm=T)]

# WFPS density curves
d <- density(data$WFPS_0, adjust = 1/2)
myd <- data.table(WFPS=d$x, y=d$y)

mywfps <- approx(x=d$x, y=d$y, xout=s.cum$WFPS)

s.cum[, WFPS.dens:= 100 * mywfps$y]

s.cum[, WFPS.dens.1:= WFPS.dens * dens.event1]
s.cum[, WFPS.dens.2:= WFPS.dens * dens.event2]
s.cum[, WFPS.dens.3:= WFPS.dens * dens.event3]
# s.cum[, WFPS.dens.3:= WFPS.dens.3 + WFPS.dens.2 + WFPS.dens.1]
# s.cum[, WFPS.dens.2:= WFPS.dens.2 + WFPS.dens.1]

# smoothen WFPS density curves
my1 <- rollapply(s.cum$WFPS.dens.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
s.cum[,aux := WFPS.dens.1]
s.cum[, WFPS.dens.1:= my1]
s.cum[dens.event1 %in% c(0,1), WFPS.dens.1:= aux]

my2 <- rollapply(s.cum$WFPS.dens.2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
s.cum[,aux := WFPS.dens.2]
s.cum[, WFPS.dens.2:= my2]
s.cum[dens.event2 %in% c(0,1), WFPS.dens.2:= aux]
s.cum[, aux:=NULL]

################################################################################
################################################################################
################################################################################
####
# 100%-normalized s.cum(NO or N2O) by WFPS and event; 
`%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}

cum.event.till <- data[,.(
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18        
), by= .(event, WFPS, tillage)]
setkey(cum.event.till, event, WFPS, tillage)

cum.event.fert <- data[,.(
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18        
), by= .(event, WFPS, fertilization)]
setkey(cum.event.fert, event, WFPS, fertilization)

cum.event.rain <- data[,.(
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18        
), by= .(event, WFPS, precipitation)]
setkey(cum.event.rain, event, WFPS, precipitation)

###
cum.event <- data[,.(
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18        
), by= .(event, WFPS)]
setkey(cum.event, event, WFPS)

myNO <- dcast.data.table(cum.event, WFPS ~ event, value.var= "NO")
setnames(myNO, c("1", "2", "3"), c("NO.1", "NO.2", "NO.3"))
myN2O <- dcast.data.table(cum.event, WFPS ~ event, value.var= "N2O")
setnames(myN2O, c("1", "2", "3"), c("N2O.1", "N2O.2", "N2O.3"))

cum100 <- myN2O [myNO]

cum100[,NO:=rowSums(.SD, na.rm = TRUE), .SDcols = c("NO.1", "NO.2", "NO.3")]
cum100[,Ndens.NO.1:= 100 * NO.1 / NO]
cum100[,Ndens.NO.2:= 100 * NO.2 / NO]
cum100[,Ndens.NO.3:= 100 * NO.3 / NO]
# cum100[,Ndens.NO.3:= Ndens.NO.1 %+na% Ndens.NO.2 %+na% Ndens.NO.3]
# cum100[,Ndens.NO.2:= Ndens.NO.1 %+na% Ndens.NO.2]

cum100[,N2O:=rowSums(.SD, na.rm = TRUE), .SDcols = c("N2O.1", "N2O.2", "N2O.3")]
cum100[,Ndens.N2O.1:= 100 * N2O.1 / N2O]
cum100[,Ndens.N2O.2:= 100 * N2O.2 / N2O]
cum100[,Ndens.N2O.3:= 100 * N2O.3 / N2O]
# cum100[,Ndens.N2O.3:= N2O.1 %+na% N2O.2 %+na% N2O.3]
# cum100[,Ndens.N2O.2:= N2O.1 %+na% N2O.2]

# add WFPS densities by event
w100 <- cum[,.(WFPS,
               Ndens.event.1 = 100*dens.event1,
               Ndens.event.2 = 100*dens.event2,
               Ndens.event.3 = 100*dens.event3
)]
# w100[, Ndens.event.2:= Ndens.event.1 + Ndens.event.2]
setkey(cum100, WFPS)
setkey(w100, WFPS)
cum100 <- w100[cum100]

# my1 <- rollapply(cum100$event.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
# cum100[,aux := event.1]
# cum100[, event.1:= my1]
# cum100[aux %in% c(0,100), event.1:= aux]
# 
# my2 <- rollapply(cum100$event.2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
# cum100[,aux := event.2]
# cum100[, event.2:= my2]
# cum100[aux %in% c(0,100), event.2:= aux]
# cum100[, aux:=NULL]

cum100[,WFPS:= as.numeric(as.character(WFPS))]


################################################################################
s.cum100 <- copy(cum100)
# smoothing
a.1 <- is.na(s.cum100$NO.1)
a.2 <- is.na(s.cum100$NO.2)
a.3 <- is.na(s.cum100$NO.3)
b.1 <- is.na(s.cum100$N2O.1)
b.2 <- is.na(s.cum100$N2O.2)
b.3 <- is.na(s.cum100$N2O.3)
# wide <- 0.5
# window <- 3 #in WFPS % units
myNO.1 <- rollapply(s.cum100$NO.1, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myNO.2 <- rollapply(s.cum100$NO.2, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myNO.3 <- rollapply(s.cum100$NO.3, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.1 <- rollapply(s.cum100$N2O.1, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.2 <- rollapply(s.cum100$N2O.2, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.3 <- rollapply(s.cum100$N2O.3, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
s.cum100[, NO.1:= myNO.1]
s.cum100[, NO.2:= myNO.2]
s.cum100[, NO.3:= myNO.3]
s.cum100[, N2O.1:= myN2O.1]
s.cum100[, N2O.2:= myN2O.2]
s.cum100[, N2O.3:= myN2O.3]

s.cum100[a.1, NO.1:= NA]
s.cum100[a.2, NO.2:= NA]
s.cum100[a.3, NO.3:= NA]
s.cum100[b.1, N2O.1:= NA]
s.cum100[b.2, N2O.2:= NA]
s.cum100[b.3, N2O.3:= NA]
#
s.cum100[,NO:=rowSums(.SD, na.rm = TRUE), .SDcols = c("NO.1", "NO.2", "NO.3")]
s.cum100[,Ndens.NO.1:= 100 * NO.1 / NO]
s.cum100[,Ndens.NO.2:= 100 * NO.2 / NO]
s.cum100[,Ndens.NO.3:= 100 * NO.3 / NO]
# s.cum100[,Ndens.NO.3:= NO.1 %+na% NO.2 %+na% NO.3]
# s.cum100[,Ndens.NO.2:= NO.1 %+na% NO.2]

s.cum100[,N2O:=rowSums(.SD, na.rm = TRUE), .SDcols = c("N2O.1", "N2O.2", "N2O.3")]
s.cum100[,Ndens.N2O.1:= 100 * N2O.1 / N2O]
s.cum100[,Ndens.N2O.2:= 100 * N2O.2 / N2O]
s.cum100[,Ndens.N2O.3:= 100 * N2O.3 / N2O]
# s.cum100[,Ndens.N2O.3:= N2O.1 %+na% N2O.2 %+na% N2O.3]
# s.cum100[,Ndens.N2O.2:= N2O.1 %+na% N2O.2]


# add WFPS densities by event
w100 <- s.cum[,.(WFPS, Ndens.event.1 = 100*dens.event1, Ndens.event.2 = 100*dens.event2)]
w100[, Ndens.event.2:= Ndens.event.1 + Ndens.event.2]
setkey(s.cum100, WFPS)
setkey(w100, WFPS)
s.cum100[, Ndens.event.1:= NULL]
s.cum100[, Ndens.event.2:= NULL]
s.cum100 <- w100[s.cum100]
s.cum100[, Ndens.event.3:= 100 - Ndens.event.2 - Ndens.event.1]

my1 <- rollapply(s.cum100$Ndens.event.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
s.cum100[,aux := Ndens.event.1]
s.cum100[, Ndens.event.1:= my1]
s.cum100[aux %in% c(0,100), Ndens.event.1:= aux]

my2 <- rollapply(s.cum100$Ndens.event.2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
s.cum100[,aux := Ndens.event.2]
s.cum100[, Ndens.event.2:= my2]
s.cum100[aux %in% c(0,100), Ndens.event.2:= aux]
s.cum100[, aux:=NULL]
#
cum100[,WFPS:= as.numeric(as.character(WFPS))]
s.cum100[,WFPS:= as.numeric(as.character(WFPS))]

myorder <- c("WFPS",
             "NO", "NO.1", "NO.2", "NO.3",
             "N2O", "N2O.1", "N2O.2", "N2O.3",
             "Ndens.event.1", "Ndens.event.2", "Ndens.event.3",
             "Ndens.NO.1", "Ndens.NO.2", "Ndens.NO.3",
             "Ndens.N2O.1", "Ndens.N2O.2", "Ndens.N2O.3")
setcolorder(cum100, myorder)
setcolorder(s.cum100, myorder)

# write file
mydata <- copy (cum)
setkey(mydata, WFPS)
no.format <- c("WFPS", "count.WFPS", "count.event1", "count.event2", "count.event3")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
# write file
mydata <- copy (s.cum)
setkey(mydata, WFPS)
no.format <- c("WFPS", "count.WFPS", "count.event1", "count.event2", "count.event3")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps_roll_", window,".dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write file
mydata <- copy (cum100)
setkey(mydata, WFPS)
no.format <- c("WFPS")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/100normalized_meanflux_by_wfps.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
# write file
mydata <- copy (s.cum100)
setkey(mydata, WFPS)
no.format <- c("WFPS")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/100normalized_meanflux_by_wfps_roll_", window,".dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


# write file
mydata <- copy (cum.event)
setkey(mydata, event, WFPS)
no.format <- c("event", "WFPS")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

####
# write file
mydata <- copy (cum.till)
no.format <- c("event", "WFPS", "fertilization", "tillage", "count.WFPS", "count.event1", "count.event2", "count.event3")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps__till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (cum.fert)
no.format <- c("event", "WFPS", "fertilization", "tillage", "count.WFPS", "count.event1", "count.event2", "count.event3")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps__fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (cum.rain)
no.format <- c("event", "WFPS", "fertilization", "tillage", "precipitation", "count.WFPS", "count.event1", "count.event2", "count.event3")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps__rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

###
# write file
mydata <- copy (cum.event.till)
no.format <- c("event", "WFPS", "fertilization", "tillage", "precipitation")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps_event__till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write file
mydata <- copy (cum.event.fert)
no.format <- c("event", "WFPS", "fertilization", "tillage", "precipitation")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps_event__fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write file
mydata <- copy (cum.event.rain)
no.format <- c("event", "WFPS", "fertilization", "tillage", "precipitation")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/meanflux_by_wfps_event__rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


source("plots_WFPS_N.R")