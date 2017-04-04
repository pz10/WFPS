folderout.1 <- paste0(output_path, "/priming/summaries.2.1.ratio")
dir.create(folderout.1, recursive=T)
# by tillage
p.21.till <- priming[,.(
        NO.2.1 = mean(NO.2.1, na.rm=T),
        sd.NO.2.1 = sd(NO.2.1, na.rm=T),
        se.NO.2.1 = sd(NO.2.1, na.rm=T)/sqrt(sum(!is.na(NO.2.1))),
        
        N2O.2.1 = mean(N2O.2.1, na.rm=T),
        sd.N2O.2.1 = sd(N2O.2.1, na.rm=T),
        se.N2O.2.1 = sd(N2O.2.1, na.rm=T)/sqrt(sum(!is.na(N2O.2.1))),
        
        NO.N2O.2.1 = mean(NO.N2O.2.1, na.rm=T),
        sd.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T),
        se.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.2.1))),
        
        NO.N2O.NH3.2.1 = mean(NO.N2O.NH3.2.1, na.rm=T),
        sd.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T),
        se.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.2.1))),
        
        CO2.2.1 = mean(CO2.2.1, na.rm=T),
        sd.CO2.2.1 = sd(CO2.2.1, na.rm=T),
        se.CO2.2.1 = sd(CO2.2.1, na.rm=T)/sqrt(sum(!is.na(CO2.2.1)))
), by=tillage]

p.31.till <- priming[,.(
        NO.3.1 = mean(NO.3.1, na.rm=T),
        sd.NO.3.1 = sd(NO.3.1, na.rm=T),
        se.NO.3.1 = sd(NO.3.1, na.rm=T)/sqrt(sum(!is.na(NO.3.1))),
        
        N2O.3.1 = mean(N2O.3.1, na.rm=T),
        sd.N2O.3.1 = sd(N2O.3.1, na.rm=T),
        se.N2O.3.1 = sd(N2O.3.1, na.rm=T)/sqrt(sum(!is.na(N2O.3.1))),
        
        NO.N2O.3.1 = mean(NO.N2O.3.1, na.rm=T),
        sd.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T),
        se.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.3.1))),
        
        NO.N2O.NH3.3.1 = mean(NO.N2O.NH3.3.1, na.rm=T),
        sd.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T),
        se.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.3.1))),
        
        CO2.3.1 = mean(CO2.3.1, na.rm=T),
        sd.CO2.3.1 = sd(CO2.3.1, na.rm=T),
        se.CO2.3.1 = sd(CO2.3.1, na.rm=T)/sqrt(sum(!is.na(CO2.3.1)))
), by=tillage]

p.23.1.till <- priming[,.(
        NO.23.1 = mean(NO.23.1, na.rm=T),
        sd.NO.23.1 = sd(NO.23.1, na.rm=T),
        se.NO.23.1 = sd(NO.23.1, na.rm=T)/sqrt(sum(!is.na(NO.23.1))),
        
        N2O.23.1 = mean(N2O.23.1, na.rm=T),
        sd.N2O.23.1 = sd(N2O.23.1, na.rm=T),
        se.N2O.23.1 = sd(N2O.23.1, na.rm=T)/sqrt(sum(!is.na(N2O.23.1))),
        
        NO.N2O.23.1 = mean(NO.N2O.23.1, na.rm=T),
        sd.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T),
        se.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.23.1))),
        
        NO.N2O.NH3.23.1 = mean(NO.N2O.NH3.23.1, na.rm=T),
        sd.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T),
        se.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.23.1))),
        
        CO2.23.1 = mean(CO2.23.1, na.rm=T),
        sd.CO2.23.1 = sd(CO2.23.1, na.rm=T),
        se.CO2.23.1 = sd(CO2.23.1, na.rm=T)/sqrt(sum(!is.na(CO2.23.1)))
), by=tillage]

# by precipitation
p.21.rain <- priming[,.(
        NO.2.1 = mean(NO.2.1, na.rm=T),
        sd.NO.2.1 = sd(NO.2.1, na.rm=T),
        se.NO.2.1 = sd(NO.2.1, na.rm=T)/sqrt(sum(!is.na(NO.2.1))),
        
        N2O.2.1 = mean(N2O.2.1, na.rm=T),
        sd.N2O.2.1 = sd(N2O.2.1, na.rm=T),
        se.N2O.2.1 = sd(N2O.2.1, na.rm=T)/sqrt(sum(!is.na(N2O.2.1))),
        
        NO.N2O.2.1 = mean(NO.N2O.2.1, na.rm=T),
        sd.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T),
        se.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.2.1))),
        
        NO.N2O.NH3.2.1 = mean(NO.N2O.NH3.2.1, na.rm=T),
        sd.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T),
        se.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.2.1))),
        
        CO2.2.1 = mean(CO2.2.1, na.rm=T),
        sd.CO2.2.1 = sd(CO2.2.1, na.rm=T),
        se.CO2.2.1 = sd(CO2.2.1, na.rm=T)/sqrt(sum(!is.na(CO2.2.1)))
), by=precipitation]

p.31.rain <- priming[,.(
        NO.3.1 = mean(NO.3.1, na.rm=T),
        sd.NO.3.1 = sd(NO.3.1, na.rm=T),
        se.NO.3.1 = sd(NO.3.1, na.rm=T)/sqrt(sum(!is.na(NO.3.1))),
        
        N2O.3.1 = mean(N2O.3.1, na.rm=T),
        sd.N2O.3.1 = sd(N2O.3.1, na.rm=T),
        se.N2O.3.1 = sd(N2O.3.1, na.rm=T)/sqrt(sum(!is.na(N2O.3.1))),
        
        NO.N2O.3.1 = mean(NO.N2O.3.1, na.rm=T),
        sd.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T),
        se.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.3.1))),
        
        NO.N2O.NH3.3.1 = mean(NO.N2O.NH3.3.1, na.rm=T),
        sd.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T),
        se.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.3.1))),
        
        CO2.3.1 = mean(CO2.3.1, na.rm=T),
        sd.CO2.3.1 = sd(CO2.3.1, na.rm=T),
        se.CO2.3.1 = sd(CO2.3.1, na.rm=T)/sqrt(sum(!is.na(CO2.3.1)))
), by=precipitation]

p.23.1.rain <- priming[,.(
        NO.23.1 = mean(NO.23.1, na.rm=T),
        sd.NO.23.1 = sd(NO.23.1, na.rm=T),
        se.NO.23.1 = sd(NO.23.1, na.rm=T)/sqrt(sum(!is.na(NO.23.1))),
        
        N2O.23.1 = mean(N2O.23.1, na.rm=T),
        sd.N2O.23.1 = sd(N2O.23.1, na.rm=T),
        se.N2O.23.1 = sd(N2O.23.1, na.rm=T)/sqrt(sum(!is.na(N2O.23.1))),
        
        NO.N2O.23.1 = mean(NO.N2O.23.1, na.rm=T),
        sd.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T),
        se.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.23.1))),
        
        NO.N2O.NH3.23.1 = mean(NO.N2O.NH3.23.1, na.rm=T),
        sd.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T),
        se.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.23.1))),
        
        CO2.23.1 = mean(CO2.23.1, na.rm=T),
        sd.CO2.23.1 = sd(CO2.23.1, na.rm=T),
        se.CO2.23.1 = sd(CO2.23.1, na.rm=T)/sqrt(sum(!is.na(CO2.23.1)))
), by=precipitation]


# by fertilization
p.21.fert <- priming[,.(
        NO.2.1 = mean(NO.2.1, na.rm=T),
        sd.NO.2.1 = sd(NO.2.1, na.rm=T),
        se.NO.2.1 = sd(NO.2.1, na.rm=T)/sqrt(sum(!is.na(NO.2.1))),
        
        N2O.2.1 = mean(N2O.2.1, na.rm=T),
        sd.N2O.2.1 = sd(N2O.2.1, na.rm=T),
        se.N2O.2.1 = sd(N2O.2.1, na.rm=T)/sqrt(sum(!is.na(N2O.2.1))),
        
        NO.N2O.2.1 = mean(NO.N2O.2.1, na.rm=T),
        sd.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T),
        se.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.2.1))),
        
        NO.N2O.NH3.2.1 = mean(NO.N2O.NH3.2.1, na.rm=T),
        sd.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T),
        se.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.2.1))),
        
        CO2.2.1 = mean(CO2.2.1, na.rm=T),
        sd.CO2.2.1 = sd(CO2.2.1, na.rm=T),
        se.CO2.2.1 = sd(CO2.2.1, na.rm=T)/sqrt(sum(!is.na(CO2.2.1)))
), by=fertilization]

p.31.fert <- priming[,.(
        NO.3.1 = mean(NO.3.1, na.rm=T),
        sd.NO.3.1 = sd(NO.3.1, na.rm=T),
        se.NO.3.1 = sd(NO.3.1, na.rm=T)/sqrt(sum(!is.na(NO.3.1))),
        
        N2O.3.1 = mean(N2O.3.1, na.rm=T),
        sd.N2O.3.1 = sd(N2O.3.1, na.rm=T),
        se.N2O.3.1 = sd(N2O.3.1, na.rm=T)/sqrt(sum(!is.na(N2O.3.1))),
        
        NO.N2O.3.1 = mean(NO.N2O.3.1, na.rm=T),
        sd.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T),
        se.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.3.1))),
        
        NO.N2O.NH3.3.1 = mean(NO.N2O.NH3.3.1, na.rm=T),
        sd.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T),
        se.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.3.1))),
        
        CO2.3.1 = mean(CO2.3.1, na.rm=T),
        sd.CO2.3.1 = sd(CO2.3.1, na.rm=T),
        se.CO2.3.1 = sd(CO2.3.1, na.rm=T)/sqrt(sum(!is.na(CO2.3.1)))
), by=fertilization]

p.23.1.fert <- priming[,.(
        NO.23.1 = mean(NO.23.1, na.rm=T),
        sd.NO.23.1 = sd(NO.23.1, na.rm=T),
        se.NO.23.1 = sd(NO.23.1, na.rm=T)/sqrt(sum(!is.na(NO.23.1))),
        
        N2O.23.1 = mean(N2O.23.1, na.rm=T),
        sd.N2O.23.1 = sd(N2O.23.1, na.rm=T),
        se.N2O.23.1 = sd(N2O.23.1, na.rm=T)/sqrt(sum(!is.na(N2O.23.1))),
        
        NO.N2O.23.1 = mean(NO.N2O.23.1, na.rm=T),
        sd.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T),
        se.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.23.1))),
        
        NO.N2O.NH3.23.1 = mean(NO.N2O.NH3.23.1, na.rm=T),
        sd.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T),
        se.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.23.1))),
        
        CO2.23.1 = mean(CO2.23.1, na.rm=T),
        sd.CO2.23.1 = sd(CO2.23.1, na.rm=T),
        se.CO2.23.1 = sd(CO2.23.1, na.rm=T)/sqrt(sum(!is.na(CO2.23.1)))
), by=fertilization]

# by treatment
p.21.treat <- priming[,.(
        NO.2.1 = mean(NO.2.1, na.rm=T),
        sd.NO.2.1 = sd(NO.2.1, na.rm=T),
        se.NO.2.1 = sd(NO.2.1, na.rm=T)/sqrt(sum(!is.na(NO.2.1))),
        
        N2O.2.1 = mean(N2O.2.1, na.rm=T),
        sd.N2O.2.1 = sd(N2O.2.1, na.rm=T),
        se.N2O.2.1 = sd(N2O.2.1, na.rm=T)/sqrt(sum(!is.na(N2O.2.1))),
        
        NO.N2O.2.1 = mean(NO.N2O.2.1, na.rm=T),
        sd.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T),
        se.NO.N2O.2.1 = sd(NO.N2O.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.2.1))),
        
        NO.N2O.NH3.2.1 = mean(NO.N2O.NH3.2.1, na.rm=T),
        sd.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T),
        se.NO.N2O.NH3.2.1 = sd(NO.N2O.NH3.2.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.2.1))),
        
        CO2.2.1 = mean(CO2.2.1, na.rm=T),
        sd.CO2.2.1 = sd(CO2.2.1, na.rm=T),
        se.CO2.2.1 = sd(CO2.2.1, na.rm=T)/sqrt(sum(!is.na(CO2.2.1)))
), by=.(fertilization, precipitation, tillage)]

p.31.treat <- priming[,.(
        NO.3.1 = mean(NO.3.1, na.rm=T),
        sd.NO.3.1 = sd(NO.3.1, na.rm=T),
        se.NO.3.1 = sd(NO.3.1, na.rm=T)/sqrt(sum(!is.na(NO.3.1))),
        
        N2O.3.1 = mean(N2O.3.1, na.rm=T),
        sd.N2O.3.1 = sd(N2O.3.1, na.rm=T),
        se.N2O.3.1 = sd(N2O.3.1, na.rm=T)/sqrt(sum(!is.na(N2O.3.1))),
        
        NO.N2O.3.1 = mean(NO.N2O.3.1, na.rm=T),
        sd.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T),
        se.NO.N2O.3.1 = sd(NO.N2O.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.3.1))),
        
        NO.N2O.NH3.3.1 = mean(NO.N2O.NH3.3.1, na.rm=T),
        sd.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T),
        se.NO.N2O.NH3.3.1 = sd(NO.N2O.NH3.3.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.3.1))),
        
        CO2.3.1 = mean(CO2.3.1, na.rm=T),
        sd.CO2.3.1 = sd(CO2.3.1, na.rm=T),
        se.CO2.3.1 = sd(CO2.3.1, na.rm=T)/sqrt(sum(!is.na(CO2.3.1)))
), by=.(fertilization, precipitation, tillage)]

p.23.1.treat <- priming[,.(
        NO.23.1 = mean(NO.23.1, na.rm=T),
        sd.NO.23.1 = sd(NO.23.1, na.rm=T),
        se.NO.23.1 = sd(NO.23.1, na.rm=T)/sqrt(sum(!is.na(NO.23.1))),
        
        N2O.23.1 = mean(N2O.23.1, na.rm=T),
        sd.N2O.23.1 = sd(N2O.23.1, na.rm=T),
        se.N2O.23.1 = sd(N2O.23.1, na.rm=T)/sqrt(sum(!is.na(N2O.23.1))),
        
        NO.N2O.23.1 = mean(NO.N2O.23.1, na.rm=T),
        sd.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T),
        se.NO.N2O.23.1 = sd(NO.N2O.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.23.1))),
        
        NO.N2O.NH3.23.1 = mean(NO.N2O.NH3.23.1, na.rm=T),
        sd.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T),
        se.NO.N2O.NH3.23.1 = sd(NO.N2O.NH3.23.1, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.23.1))),
        
        CO2.23.1 = mean(CO2.23.1, na.rm=T),
        sd.CO2.23.1 = sd(CO2.23.1, na.rm=T),
        se.CO2.23.1 = sd(CO2.23.1, na.rm=T)/sqrt(sum(!is.na(CO2.23.1)))
), by=.(fertilization, precipitation, tillage)]
################################################################################################################################################################
################################################################################
# write summary files
# tillage
################################################################################
mydata <- copy (p.21.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_tillage_2_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.31.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_tillage_3_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.23.1.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_tillage_23_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# fertiliztion
################################################################################
mydata <- copy (p.21.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_fert_2_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.31.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_fert_3_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.23.1.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_fert_23_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# rain
################################################################################
mydata <- copy (p.21.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_rain_2_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.31.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_rain_3_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.23.1.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_rain_23_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# treatment
################################################################################
mydata <- copy (p.21.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_treat_2_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.31.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_treat_3_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.23.1.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.1, "/priming_by_treat_23_1.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
