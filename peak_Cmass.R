myfolder <- paste0(output_path, "/WFPS")
folder_out <- paste0(output_path, "/WFPS/peaks")
dir.create(folder_out, recursive=T)

file <- paste0(myfolder, "/3h_interpolation.dat")
data <- fread(file)

setkey(data, incubation, fertilization, precipitation, tillage, days.adjusted)

data[days.adjusted<45, event:=3]
data[days.adjusted<30, event:=2]
data[days.adjusted<15, event:=1]

data[, cumNO:= 3*cumsum(NO)/1000, by=.(incubation, treatment,event)] #*3h; /1000 µg to mg
data[, cumN2O:= 3*cumsum(N2O)/1000, by=.(incubation, treatment,event)]
data[, cumCO2:= 3*cumsum(CO2)/1000, by=.(incubation, treatment,event)]

# NO
mydata <- copy(data)
mydata[, height := max(NO), by=.(incubation, treatment, event)] #get height
mydata[NO==height, span.height:= days.adjusted]

mydata[,before:= cumNO -max(cumNO)/2, by=.(incubation, treatment, event)] # get center of mass span
mydata[before>0, before:=NA]
mydata[,after:= cumNO -max(cumNO)/2, by=.(incubation, treatment, event)]
mydata[after<0, after:=NA]
mydata[, max:= max(before), by=.(incubation, treatment, event)]
mydata[, max:= max(before, na.rm=T), by=.(incubation, treatment, event)]
mydata[, min:= min(after, na.rm=T), by=.(incubation, treatment, event)]
mydata[max==before, span.Cmass:= days.adjusted]
mydata[, span.Cmass:= span.Cmass + 0.125*abs(max)/(min-max)]

NO <- mydata[,.(
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        height = max(NO),
        span.height = max(span.height, na.rm=T) - min(days.adjusted) +0.125,
        span.Cmass = max(span.Cmass, na.rm=T) - min(days.adjusted) +0.125,

        cumNO.mgNm2 = max(cumNO)
),by=.(incubation, treatment, event)]
setkey(NO, event, fertilization, precipitation, tillage, incubation)

NO.se <- NO[,.(
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumNO.mgNm2 = mean(cumNO.mgNm2),
        se.cumNO.mgNm2 = sd(cumNO.mgNm2)/sqrt(sum(!is.na((cumNO.mgNm2)))),
        count.cumNO.mgNm2 = sum(!is.na((cumNO.mgNm2)))
        ), by=.(event, treatment)]

NO.se.F <- NO[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumNO.mgNm2 = mean(cumNO.mgNm2),
        se.cumNO.mgNm2 = sd(cumNO.mgNm2)/sqrt(sum(!is.na((cumNO.mgNm2)))),
        count.cumNO.mgNm2 = sum(!is.na((cumNO.mgNm2)))
), by=.(event, fertilization)]

NO.se.P <- NO[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumNO.mgNm2 = mean(cumNO.mgNm2),
        se.cumNO.mgNm2 = sd(cumNO.mgNm2)/sqrt(sum(!is.na((cumNO.mgNm2)))),
        count.cumNO.mgNm2 = sum(!is.na((cumNO.mgNm2)))
), by=.(event, precipitation)]

NO.se.T <- NO[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumNO.mgNm2 = mean(cumNO.mgNm2),
        se.cumNO.mgNm2 = sd(cumNO.mgNm2)/sqrt(sum(!is.na((cumNO.mgNm2)))),
        count.cumNO.mgNm2 = sum(!is.na((cumNO.mgNm2)))
), by=.(event, tillage)]


# N2O
mydata <- copy(data)
mydata[, height := max(N2O), by=.(incubation, treatment, event)] #get height
mydata[N2O==height, span.height:= days.adjusted]

mydata[,before:= cumN2O -max(cumN2O)/2, by=.(incubation, treatment, event)] # get center of mass span
mydata[before>0, before:=NA]
mydata[,after:= cumN2O -max(cumN2O)/2, by=.(incubation, treatment, event)]
mydata[after<0, after:=NA]
mydata[, max:= max(before), by=.(incubation, treatment, event)]
mydata[, max:= max(before, na.rm=T), by=.(incubation, treatment, event)]
mydata[, min:= min(after, na.rm=T), by=.(incubation, treatment, event)]
mydata[max==before, span.Cmass:= days.adjusted]
mydata[, span.Cmass:= span.Cmass + 0.125*abs(max)/(min-max)]

N2O <- mydata[,.(
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        height = max(N2O),
        span.height = max(span.height, na.rm=T) - min(days.adjusted) +0.125,
        span.Cmass = max(span.Cmass, na.rm=T) - min(days.adjusted) +0.125,
        
        cumN2O.mgNm2 = max(cumN2O)
),by=.(incubation, treatment, event)]
setkey(N2O, event, fertilization, precipitation, tillage, incubation)

N2O.se <- N2O[,.(
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumN2O.mgNm2 = mean(cumN2O.mgNm2),
        se.cumN2O.mgNm2 = sd(cumN2O.mgNm2)/sqrt(sum(!is.na((cumN2O.mgNm2)))),
        count.cumN2O.mgNm2 = sum(!is.na((cumN2O.mgNm2)))
), by=.(event, treatment)]

N2O.se.F <- N2O[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumN2O.mgNm2 = mean(cumN2O.mgNm2),
        se.cumN2O.mgNm2 = sd(cumN2O.mgNm2)/sqrt(sum(!is.na((cumN2O.mgNm2)))),
        count.cumN2O.mgNm2 = sum(!is.na((cumN2O.mgNm2)))
), by=.(event, fertilization)]

N2O.se.P <- N2O[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumN2O.mgNm2 = mean(cumN2O.mgNm2),
        se.cumN2O.mgNm2 = sd(cumN2O.mgNm2)/sqrt(sum(!is.na((cumN2O.mgNm2)))),
        count.cumN2O.mgNm2 = sum(!is.na((cumN2O.mgNm2)))
), by=.(event, precipitation)]

N2O.se.T <- N2O[,.(
        span.Cmass = mean(span.Cmass),
        se.span.Cmass = sd(span.Cmass)/sqrt(sum(!is.na((span.Cmass)))),
        count.Cmass = sum(!is.na((span.Cmass))),
        
        height = mean(height),
        se.height = sd(height)/sqrt(sum(!is.na((height)))),
        count.height = sum(!is.na((height))),
        
        span.height = mean(span.height),
        se.span.height = sd(span.height)/sqrt(sum(!is.na((span.height)))),
        count.span.height = sum(!is.na((span.height))),
        
        cumN2O.mgNm2 = mean(cumN2O.mgNm2),
        se.cumN2O.mgNm2 = sd(cumN2O.mgNm2)/sqrt(sum(!is.na((cumN2O.mgNm2)))),
        count.cumN2O.mgNm2 = sum(!is.na((cumN2O.mgNm2)))
), by=.(event, tillage)]

# CO2
mydata <- copy(data)
mydata[, height := max(CO2), by=.(incubation, treatment, event)] #get height
mydata[CO2==height, span.height:= days.adjusted]

mydata[,before:= cumCO2 -max(cumCO2)/2, by=.(incubation, treatment, event)] # get center of mass span
mydata[before>0, before:=NA]
mydata[,after:= cumCO2 -max(cumCO2)/2, by=.(incubation, treatment, event)]
mydata[after<0, after:=NA]
mydata[, max:= max(before), by=.(incubation, treatment, event)]
mydata[, max:= max(before, na.rm=T), by=.(incubation, treatment, event)]
mydata[, min:= min(after, na.rm=T), by=.(incubation, treatment, event)]
mydata[max==before, span.Cmass:= days.adjusted]
mydata[, span.Cmass:= span.Cmass + 0.125*abs(max)/(min-max)]

CO2 <- mydata[,.(
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        height = max(CO2),
        span.height = max(span.height, na.rm=T) - min(days.adjusted) +0.125,
        span.Cmass = max(span.Cmass, na.rm=T) - min(days.adjusted) +0.125,
        
        cumCO2.gCm2 = max(cumCO2)
),by=.(incubation, treatment, event)]
setkey(CO2, event, fertilization, precipitation, tillage, incubation)



### write summary-files
mydata <- copy (NO)
to.format4 <- c("span.Cmass", "height", "cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_NO_byevent.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (N2O)
to.format4 <- c("span.Cmass", "height", "cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_N2O_byevent.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (CO2)
to.format4 <- c("span.Cmass", "height", "cumCO2.gCm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_CO2_byevent.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

##
mydata <- copy (NO.se)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumNO.mgNm2", "se.cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seNO_byevent.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (NO.se.F)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumNO.mgNm2", "se.cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seNO_byevent_byfert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (NO.se.P)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumNO.mgNm2", "se.cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seNO_byevent_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (NO.se.T)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumNO.mgNm2", "se.cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seNO_byevent_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

##
mydata <- copy (N2O.se)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumN2O.mgNm2", "se.cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seN2O_byevent.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (N2O.se.F)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumN2O.mgNm2", "se.cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seN2O_byevent_byfert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (N2O.se.P)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumN2O.mgNm2", "se.cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seN2O_byevent_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (N2O.se.T)
to.format4 <- c("span.Cmass", "se.span.Cmass", "height", "se.height",
                "span.height", "se.span.height", "cumN2O.mgNm2", "se.cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/Cmass_seN2O_byevent_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


