folder_out <- paste0(output_path, "/WFPS/peaks")
dir.create(folder_out, recursive=T)

myfolder <- paste0(output_path, "/WFPS")
file <- paste0(myfolder, "/3h_interpolation.dat")
data <- fread(file)


# m <- ggplot(data[NO<100], aes(x = NO))
# m + geom_density()
# m <- ggplot(data[N2O<100], aes(x = N2O))
# m + geom_density()
# m <- ggplot(data, aes(x = CO2))
# m + geom_density()


thr <- 20
data[, is.NO:= 0]
data[, is.N2O:= 0]
data[, is.CO2:= 0]
data[NO > thr, is.NO:= 1]
data[N2O > thr, is.N2O:= 1]
data[CO2 > thr, is.CO2:= 1]

# dd <- copy(data)
# 
# dd[incubation=="B", is.NO:= 2*is.NO]
# dd[incubation=="B", is.N2O:= 2*is.N2O]
# dd[incubation=="B", is.CO2:= 2*is.CO2]
# 
# dd[incubation=="C", is.NO:= 3*is.NO]
# dd[incubation=="C", is.N2O:= 3*is.N2O]
# dd[incubation=="C", is.CO2:= 3*is.CO2]
# 
# dd[incubation=="D", is.NO:= 4*is.NO]
# dd[incubation=="D", is.N2O:= 4*is.N2O]
# dd[incubation=="D", is.CO2:= 4*is.CO2]
# 
# dd[tillage=="TT", is.NO:= 0.25+is.NO]
# dd[tillage=="TT", is.N2O:= 0.25+is.N2O]
# dd[tillage=="TT", is.CO2:= 0.25+is.CO2]
# 
# # NO
# p <- qplot(days.adjusted, is.NO, data=dd, color=tillage, facets=precipitation ~ fertilization, size=I(2),
#            xlab="", ylab="")
# p +
#         theme_bw() +
#         scale_colour_manual(values = c("black","red")) +
#         scale_shape_manual(values = c(6,1)) +
#         theme(legend.position = c(1, 1))+
#         scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))
# 
# 
# # N2O
# p <- qplot(days.adjusted, is.N2O, data=dd, color=tillage, facets=precipitation ~ fertilization, size=I(2),
#            xlab="", ylab="")
# p +
#         theme_bw() +
#         scale_colour_manual(values = c("black","red")) +
#         scale_shape_manual(values = c(6,1)) +
#         theme(legend.position = c(1, 1))+
#         scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))
# 
# # CO2
# p <- qplot(days.adjusted, is.CO2, data=dd, color=tillage, facets=precipitation ~ fertilization, size=I(2),
#            xlab="", ylab="")
# p +
#         theme_bw() +
#         scale_colour_manual(values = c("black","red")) +
#         scale_shape_manual(values = c(6,1)) +
#         theme(legend.position = c(1, 1))+
#         scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))


setkey(data, incubation, fertilization, precipitation, tillage, days.adjusted)
data[,order:=1:.N]

### find peaks
# NO
data[, aux:= is.NO[-1][1:nrow(data)] ]
data[, aux:= is.NO - aux]
data[days.adjusted==14, aux:= 1]
data[days.adjusted==29, aux:= 1]
data[days.adjusted==44, aux:= 1]

mycut <- c(0, data[aux>0, order])
data[, sNO:= cut(order, breaks = mycut, labels = FALSE, include.lowest = TRUE)]
data[is.NO==0 , sNO:= NA]
mylevels <- length(unique(data[!is.na(sNO), sNO]))
data[, sNO:= factor(sNO, labels= 1: mylevels)]
data[,cumNO:= 3*cumsum(NO)/1000, by=sNO] #*3h; /1000 µg to mg

# N2O
data[, aux:= is.N2O[-1][1:nrow(data)] ]
data[, aux:= is.N2O - aux]
data[days.adjusted==14, aux:= 1]
data[days.adjusted==29, aux:= 1]
data[days.adjusted==44, aux:= 1]

mycut <- c(0, data[aux>0, order])
data[, sN2O:= cut(order, breaks = mycut, labels = FALSE, include.lowest = TRUE)]
data[is.N2O==0 , sN2O:= NA]
mylevels <- length(unique(data[!is.na(sN2O), sN2O]))
data[, sN2O:= factor(sN2O, labels= 1: mylevels)]
data[,cumN2O:= 3*cumsum(N2O)/1000, by=sN2O] #*3h; /1000 µg to mg

### perform summaries
# NO
mydata <- copy(data)
mydata <- mydata[!is.na(sNO)]
mydata[, height := max(NO), by=sNO] #get height
mydata[NO==height, span.height:= days.adjusted]

mydata[,before:= cumNO -max(cumNO)/2, by=sNO] # get center of mass span
mydata[before>0, before:=NA]
mydata[,after:= cumNO -max(cumNO)/2, by=sNO]
mydata[after<0, after:=NA]
mydata[, max:= max(before), by=sNO]
mydata[, max:= max(before, na.rm=T), by=sNO]
mydata[, min:= min(after, na.rm=T), by=sNO]
mydata[max==before, span.Cmass:= days.adjusted]
mydata[, span.Cmass:= span.Cmass + 0.125*abs(max)/(min-max)]


NO <- mydata[,.(
        incubation = unique(incubation),
        treatment = unique(treatment),
        
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        start = min(days.adjusted) - 0.125,
        span = max(days.adjusted) - min(days.adjusted) +0.125,
        height = max(NO),
        span.height = max(span.height, na.rm=T) - min(days.adjusted) +0.125,
        span.Cmass = max(span.Cmass, na.rm=T) - min(days.adjusted) +0.125,

        cumNO.mgNm2 = max(cumNO)
),by=.(peak.NO=sNO)]
NO <- NO[span>=0.5]

NO.1st <- copy(NO)

NO.1st[,min:= min(start), by=.(incubation, treatment)]
NO.1st <- NO.1st[start==min,]
NO.1st[,min:= NULL]
setkey(NO.1st, fertilization, precipitation, tillage, incubation)

# N2O
mydata <- copy(data)
mydata <- mydata[!is.na(sN2O)]
mydata[, height := max(N2O), by=sN2O] #get height
mydata[N2O==height, span.height:= days.adjusted]

mydata[,before:= cumN2O -max(cumN2O)/2, by=sN2O] # get center of mass span
mydata[before>0, before:=NA]
mydata[,after:= cumN2O -max(cumN2O)/2, by=sN2O]
mydata[after<0, after:=NA]
mydata[, max:= max(before), by=sN2O]
mydata[, max:= max(before, na.rm=T), by=sN2O]
mydata[, min:= min(after, na.rm=T), by=sN2O]
mydata[max==before, span.Cmass:= days.adjusted]
mydata[, span.Cmass:= span.Cmass + 0.125*abs(max)/(min-max)]

N2O <- mydata[,.(
        incubation = unique(incubation),
        treatment = unique(treatment),
        
        fertilization = unique(fertilization),
        precipitation = unique(precipitation),
        tillage = unique(tillage),
        
        start = min(days.adjusted) - 0.125,
        span = max(days.adjusted) - min(days.adjusted) +0.125,
        height = max(N2O),
        span.height = max(span.height, na.rm=T) - min(days.adjusted) +0.125,
        span.Cmass = max(span.Cmass, na.rm=T) - min(days.adjusted) +0.125,

        cumN2O.mgNm2 = max(cumN2O)
),by=.(peak.N2O=sN2O)]
N2O <- N2O[span>=0.5]

# N2O.1st <- copy(N2O)
# N2O.1st[,min:= min(start), by=.(incubation, treatment)]
# N2O.1st <- N2O.1st[start==min,]
# N2O.1st[,min:= NULL]
# setkey(NO.1st, fertilization, precipitation, tillage, incubation)

### write summary-files
mydata <- copy (NO)
setkey(mydata, peak.NO)
to.format4 <- c("span.Cmass", "height", "cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NO_", thr, "_peaks.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (N2O)
setkey(mydata, peak.N2O)
to.format4 <- c("span.Cmass", "height", "cumN2O.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/N2O_", thr, "_peaks.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (NO.1st)
to.format4 <- c("span.Cmass", "height", "cumNO.mgNm2")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NO_", thr, "_1st_peak.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
