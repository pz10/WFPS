# folder out
folderout <- paste0(output_path, "/WFPS/WFPS_summaries")
dir.create(folderout, recursive=T)

# file in (balanced 3h interpolated values)
myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

# read data file
data <- fread(myfile)

# select just incubation-stretches 1 to 3
data <- data[! event %in% c("0_pre"),]

setkey(data, event, precipitation, tillage)

################################################################################
# compute summary by = event, incubation, precipitation, tillage, fertilization
sum.216 <- data[,.(
        mean.WFPS = mean(WFPS_0, na.rm=T),
        sd.WFPS = sd(WFPS_0, na.rm=T),
        
        median.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS_0, na.rm = T),
        max.WFPS = max(WFPS_0, na.rm = T)
), by=.(event, incubation, precipitation, tillage, fertilization)]

# now get mean values for the rest of summaries
sum216.event.rain.till <- sum.216[,.(
        mean.WFPS = mean(mean.WFPS, na.rm=T),
        sd.WFPS = mean(sd.WFPS, na.rm=T),
        
        median.WFPS = quantile(median.WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(q25.WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(q75.WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(q09.WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(q91.WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(min.WFPS, na.rm = T),
        max.WFPS = max(max.WFPS, na.rm = T)
), by=.(event, precipitation, tillage)]

sum216.event.till <- sum.216[,.(
        mean.WFPS = mean(mean.WFPS, na.rm=T),
        sd.WFPS = mean(sd.WFPS, na.rm=T),
        
        median.WFPS = quantile(median.WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(q25.WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(q75.WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(q09.WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(q91.WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(min.WFPS, na.rm = T),
        max.WFPS = max(max.WFPS, na.rm = T)
), by=.(event, tillage)]

sum216.rain.till <- sum.216[,.(
        mean.WFPS = mean(mean.WFPS, na.rm=T),
        sd.WFPS = mean(sd.WFPS, na.rm=T),
        
        median.WFPS = quantile(median.WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(q25.WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(q75.WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(q09.WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(q91.WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(min.WFPS, na.rm = T),
        max.WFPS = max(max.WFPS, na.rm = T)
), by=.(precipitation, tillage)]

sum216.till <- sum.216[,.(
        mean.WFPS = mean(mean.WFPS, na.rm=T),
        sd.WFPS = mean(sd.WFPS, na.rm=T),
        
        median.WFPS = quantile(median.WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(q25.WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(q75.WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(q09.WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(q91.WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(min.WFPS, na.rm = T),
        max.WFPS = max(max.WFPS, na.rm = T)
), by=.(tillage)]

# write summary files
mydata <- copy (sum.216)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary216.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum216.event.rain.till)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary216_by_event_rain_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum216.event.till)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary216_by_event_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum216.rain.till)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary216_by_rain_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum216.till)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary216_by_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


################################################################################
# get summaries from all data together

sum.event.rain.till <- data[,.(
        mean.WFPS = mean(WFPS_0, na.rm=T),
        sd.WFPS = sd(WFPS_0, na.rm=T),
        
        median.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS_0, na.rm = T),
        max.WFPS = max(WFPS_0, na.rm = T)
), by=.(event, precipitation, tillage)]

sum.event.till <- data[,.(
        mean.WFPS = mean(WFPS_0, na.rm=T),
        sd.WFPS = sd(WFPS_0, na.rm=T),
        
        median.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS_0, na.rm = T),
        max.WFPS = max(WFPS_0, na.rm = T)
), by=.(event, tillage)]

sum.rain.till <- data[,.(
        mean.WFPS = mean(WFPS_0, na.rm=T),
        sd.WFPS = sd(WFPS_0, na.rm=T),
        
        median.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS_0, na.rm = T),
        max.WFPS = max(WFPS_0, na.rm = T)
), by=.(precipitation, tillage)]

sum.till <- data[,.(
        mean.WFPS = mean(WFPS_0, na.rm=T),
        sd.WFPS = sd(WFPS_0, na.rm=T),
        
        median.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS_0, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS_0, na.rm = T),
        max.WFPS = max(WFPS_0, na.rm = T)
), by=.(tillage)]

# write summary files
mydata <- copy (sum.event.rain.till)
no.format <- c("event", "precipitation", "tillage")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary_by_event_rain_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum.event.till)
no.format <- c("event", "precipitation", "tillage")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary_by_event_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum.rain.till)
no.format <- c("event", "precipitation", "tillage")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary_by_rain_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (sum.till)
no.format <- c("event", "precipitation", "tillage")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/WFPS_0_summary_by_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

################################################################################

# priming effects

data.event <- data[,.(
        NO = mean(NO, na.rm=T),
        N2O = mean(N2O, na.rm=T),
        CO2 = mean(CO2, na.rm=T)
), by=.(event)]
data.e1 <- data.event[1]
data.e2 <- data.event[2]
data.e3 <- data.event[3]

p.e.1.2 <- data.e1/data.e2
p.e.1.23 <- data.e1/(data.e2 + data.e3)
###

data.ev.till <- data[,.(
        NO = mean(NO, na.rm=T),
        N2O = mean(N2O, na.rm=T),
        CO2 = mean(CO2, na.rm=T)
), by=.(event, tillage)]

p.ev.tt.1.2 <- data.ev.till[2]/data.ev.till[4]
p.ev.nt.1.2 <- data.ev.till[1]/data.ev.till[3]

p.ev.tt.1.23 <- data.ev.till[2]/(data.ev.till[4] + data.ev.till[6])
p.ev.nt.1.23 <- data.ev.till[1]/(data.ev.till[3] + data.ev.till[5])


###
data.ev.fert <- data[,.(
        NO = mean(NO, na.rm=T),
        N2O = mean(N2O, na.rm=T),
        CO2 = mean(CO2, na.rm=T)
), by=.(event, fertilization)]

p.ev.F0.1.2 <- data.ev.fert[1]/data.ev.fert[4]
p.ev.F50.1.2 <- data.ev.fert[2]/data.ev.fert[5]
p.ev.F100.1.2 <- data.ev.fert[3]/data.ev.fert[6]


###
data.ev.rain <- data[,.(
        NO = mean(NO, na.rm=T),
        N2O = mean(N2O, na.rm=T),
        CO2 = mean(CO2, na.rm=T)
), by=.(event, precipitation)]


p.ev.C.1.2 <- data.ev.rain[1]/data.ev.rain[4]
p.ev.D.1.2 <- data.ev.rain[2]/data.ev.rain[5]
p.ev.I.1.2 <- data.ev.rain[3]/data.ev.rain[6]



data[event=="1st", sum(NO, na.rm=T)]/data[event=="2nd", sum(NO, na.rm=T)]
data[event=="1st", sum(N2O, na.rm=T)]/data[event=="2nd", sum(N2O, na.rm=T)]
data[event=="1st", sum(CO2, na.rm=T)]/data[event=="2nd", sum(CO2, na.rm=T)]


################################################################################
data[,tillage:= factor(tillage)]
data[,event:= factor(event)]
data[,precipitation:= factor(precipitation)]

setkey(data, event, incubation, precipitation, tillage, fertilization)
setkey(sum.216, event, incubation, precipitation, tillage, fertilization)
dataIQR <- data[sum.216]
dataIQR[WFPS_0<q25.WFPS | WFPS_0>q75.WFPS, WFPS_0:= NA]
dataIQR <- dataIQR[!is.na(WFPS_0)]

### by=.(event, precipitation)
mytest <- dataIQR[,.(
        p.oneway = as.numeric(NA),
        p.adj.oneway = as.numeric(NA),
        
        p.wilcox = as.numeric(NA),
        p.adj.wilcox = as.numeric(NA)
), by=.(event, precipitation)]

for(i in unique(mytest$event)){
        for(j in mytest$precipitation){
                d <- dataIQR[event==i & precipitation == j,]
                
                myp <- with(d, oneway_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i & precipitation == j, p.oneway:= myp]
                
                myp <- with(d, wilcox_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i & precipitation == j, p.wilcox:= myp]
        }
}
mytest[, p.adj.oneway:= p.adjust(p.oneway, method="holm")]
mytest[, p.adj.wilcox:= p.adjust(p.wilcox, method="holm")]

pairtest.till.byevent.rain <- copy(mytest)


### by=.(event)
mytest <- dataIQR[,.(
        p.oneway = as.numeric(NA),
        p.adj.oneway = as.numeric(NA),
        
        p.wilcox = as.numeric(NA),
        p.adj.wilcox = as.numeric(NA)
), by=.(event)]

for(i in unique(mytest$event)){
                d <- dataIQR[event==i,]
                
                myp <- with(d, oneway_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i, p.oneway:= myp]
                
                myp <- with(d, wilcox_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i, p.wilcox:= myp]
}
mytest[, p.adj.oneway:= p.adjust(p.oneway, method="holm")]
mytest[, p.adj.wilcox:= p.adjust(p.wilcox, method="holm")]

pairtest.till.byevent <- copy(mytest)

### by=.(precipitation)
mytest <- dataIQR[,.(
        p.oneway = as.numeric(NA),
        p.adj.oneway = as.numeric(NA),
        
        p.wilcox = as.numeric(NA),
        p.adj.wilcox = as.numeric(NA)
), by=.(precipitation)]


for(j in mytest$precipitation){
        d <- dataIQR[precipitation == j,]
        
        myp <- with(d, oneway_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
        myp<- pvalue(myp)[1]
        mytest[precipitation == j, p.oneway:= myp]
        
        myp <- with(d, wilcox_test(WFPS_0~tillage, distribution=approximate(B = 9999)))
        myp<- pvalue(myp)[1]
        mytest[precipitation == j, p.wilcox:= myp]
}
mytest[, p.adj.oneway:= p.adjust(p.oneway, method="holm")]
mytest[, p.adj.wilcox:= p.adjust(p.wilcox, method="holm")]

pairtest.till.byrain <- copy(mytest)

################################################################################
mydata <- dataIQR[precipitation != "d"]
mydata[, precipitation:= factor(precipitation)]
with(mydata, oneway_test(WFPS_0~precipitation, distribution=approximate(B = 9999)))
################################################################################






one.way.1 <- oneway_test(WFPS_0~tillage, data=data, distribution=approximate(B = 9999))
wilcox <- wilcox_test(WFPS_0~tillage, data=data, distribution=approximate(B=9999))





pairwise.wilcox.test(data$WFPS_0, g = c("event", "precipitation", "tillage"), p.adjust.method = "holm")

with(data, pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))

with(data[event=="1st" & precipitation == "c",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="1st" & precipitation == "i",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="1st" & precipitation == "d",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))

with(data[event=="2nd" & precipitation == "c",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="2nd" & precipitation == "i",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="2nd" & precipitation == "d",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))

with(data[event=="2nd" & precipitation == "c",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="3rd" & precipitation == "i",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))
with(data[event=="3rd" & precipitation == "d",], pairwise.wilcox.test(WFPS_0, tillage, p.adjust.method = "holm"))


one.way.1c <- with(data[event=="1st" & precipitation == "c",], oneway_test(WFPS_0~tillage, distribution=approximate(B = 9999)))






#

library(coin)

mydata <- copy(data)
mydata <- copy(data[event=="1st",])
mydata <- copy(data[event=="1st",])
mydata <- copy(data[event=="2nd",])
mydata <- copy(data[event=="1st" & precipitation == "i",])

t.test(WFPS_0~tillage, data=mydata, var.equal=TRUE)
oneway_test(WFPS_0~tillage, data=mydata, distribution=approximate(B = 10000))

# Wilcoxon Mann-Whitney U test
library(MASS)
wilcox_test(WFPS_0~tillage, data=mydata, distribution=approximate(B=9999))

mytest <- function(data){
        t_test <- t.test(WFPS_0~tillage, data=data, var.equal=TRUE, conf.level=0.99)
        one.way <- oneway_test(WFPS_0~tillage, data=data, distribution=approximate(B = 9999), conf.level=0.99)
        wilcox <- wilcox_test(WFPS_0~tillage, data=data, distribution=approximate(B=9999), conf.level=0.99)
        return(list(t.test = t_test, one.way.test = one.way, Rank.SumTest = wilcox))
}
mytest(copy(data[event=="1st",]))
mytest(copy(data[event=="2nd",]))
mytest(copy(data[event=="3rd",]))

mytest(copy(data[event=="1st" & precipitation == "c",]))
mytest(copy(data[event=="2nd" & precipitation == "c",]))
mytest(copy(data[event=="3rd" & precipitation == "c",]))

mytest(copy(data[event=="1st" & precipitation == "d",]))
mytest(copy(data[event=="2nd" & precipitation == "d",]))
mytest(copy(data[event=="3rd" & precipitation == "d",]))

mytest(copy(data[event=="1st" & precipitation == "i",]))
mytest(copy(data[event=="2nd" & precipitation == "i",]))
mytest(copy(data[event=="3rd" & precipitation == "i",]))

mytest <- function(data){
#         t_test <- t.test(WFPS_f~tillage, data=data, var.equal=TRUE, conf.level=0.99)
#         one.way <- oneway_test(WFPS_f~tillage, data=data, distribution=approximate(B = 9999), conf.level=0.99)
        wilcox <- wilcox_test(WFPS_f~tillage, data=data, distribution=approximate(B=9999), conf.level=0.99)
        # return(list(t.test = t_test, one.way.test = one.way, Rank.SumTest = wilcox))
        return(list(Rank.SumTest = wilcox))
}
mytest(copy(data[event=="1st",]))
mytest(copy(data[event=="2nd",]))
mytest(copy(data[event=="3rd",]))

mytest(copy(data[event=="1st" & precipitation == "d",]))
mytest(copy(data[event=="2nd" & precipitation == "d",]))
mytest(copy(data[event=="3rd" & precipitation == "d",]))




attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
## These give warnings because of ties :
pairwise.wilcox.test(Ozone, Month)
pairwise.wilcox.test(Ozone, Month, p.adj = "bonf")
detach()
