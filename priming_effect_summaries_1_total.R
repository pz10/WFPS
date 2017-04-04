folderout.1.123 <- paste0(output_path, "/priming/summaries.1.123.ratio")
dir.create(folderout.1.123, recursive=T)

first <- copy(data)

# total
first[, NO:= NO.1 + NO.2 + NO.3]
first[, N2O:= N2O.1 + N2O.2 + N2O.3]
first[, NO.N2O:= NO.N2O.1 + NO.N2O.2 + NO.N2O.3]
first[, NO.N2O.NH3:= NO.N2O.NH3.1 + NO.N2O.NH3.2 + NO.N2O.NH3.3]
first[, CO2:= CO2.1 + CO2.2 + CO2.3]


first[, NO.1.123:= NO.1 / NO * 100]
first[, N2O.1.123:= N2O.1 / N2O * 100]
first[, NO.N2O.1.123:= NO.N2O.1 / NO.N2O * 100]
first[, NO.N2O.NH3.1.123:= NO.N2O.NH3.1 / NO.N2O.NH3 * 100]
first[, CO2.1.123:= CO2.1 / CO2 * 100]


# by tillage
p.1.123.till <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=tillage]


# by precipitation
p.1.123.rain <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=precipitation]


# by fertilization
p.1.123.fert <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=fertilization]

# by (tillage, rain)
p.1.123.till.rain <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=.(tillage, precipitation)]

# by (tillage, fert)
p.1.123.till.fert <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=.(tillage, fertilization)]

# by (rain, fert)
p.1.123.rain.fert <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=.(precipitation, fertilization)]

# by treatment
p.1.123.treat <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=.(fertilization, precipitation, tillage)]

# by CORE
p.1.123.core <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
), by=.(incubation, fertilization, precipitation, tillage)]


# mean values
p.1.123.mean <- first[,.(
        NO = mean(NO, na.rm=T),
        NO.1.123 = mean(NO.1.123, na.rm=T),
        sd.NO.1.123 = sd(NO.1.123, na.rm=T),
        se.NO.1.123 = sd(NO.1.123, na.rm=T)/sqrt(sum(!is.na(NO.1.123))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.1.123 = mean(N2O.1.123, na.rm=T),
        sd.N2O.1.123 = sd(N2O.1.123, na.rm=T),
        se.N2O.1.123 = sd(N2O.1.123, na.rm=T)/sqrt(sum(!is.na(N2O.1.123))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.1.123 = mean(NO.N2O.1.123, na.rm=T),
        sd.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T),
        se.NO.N2O.1.123 = sd(NO.N2O.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.123))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.1.123 = mean(NO.N2O.NH3.1.123, na.rm=T),
        sd.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T),
        se.NO.N2O.NH3.1.123 = sd(NO.N2O.NH3.1.123, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.123))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.1.123 = mean(CO2.1.123, na.rm=T),
        sd.CO2.1.123 = sd(CO2.1.123, na.rm=T),
        se.CO2.1.123 = sd(CO2.1.123, na.rm=T)/sqrt(sum(!is.na(CO2.1.123)))
)]
################################################################################################################################################################
################################################################################
# write summary files
# tillage
################################################################################
mydata <- copy (p.1.123.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_tillage_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# fertiliztion
################################################################################
mydata <- copy (p.1.123.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_fert_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# rain
################################################################################
mydata <- copy (p.1.123.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_rain_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# treatment
################################################################################
mydata <- copy (p.1.123.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_treat_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)




# tillage, rain
################################################################################
mydata <- copy (p.1.123.till.rain)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_till_rain_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# tillage, fert
################################################################################
mydata <- copy (p.1.123.till.fert)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_till_fert_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# rain, fert
################################################################################
mydata <- copy (p.1.123.rain.fert)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_rain_fert_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# CORE
################################################################################
mydata <- copy (p.1.123.core)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_by_CORE_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# mean
################################################################################
mydata <- copy (p.1.123.mean)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1.123, "/priming_mean_1_123.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)