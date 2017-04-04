# folder out
folderout <- paste0(output_path, "/priming/summaries.1.2.ratio")
dir.create(folderout, recursive=T)

# file in (balanced 3h interpolated values)
myfolder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/cum_fluxes_14"
s1 <- paste0(myfolder, "/1st_event_cum14_fluxes.dat")
s2 <- paste0(myfolder, "/2nd_event_cum14_fluxes.dat")
s3 <- paste0(myfolder, "/3rd_event_cum14_fluxes.dat")
ammonia <- paste0(input_path,"/_all_fluxes/AMMONIAtrial/NH3.dat")

# read files
s1 <- fread(input = s1)
s2 <- fread(input = s2)
s3 <- fread(input = s3)
amm <- fread(input = ammonia)
setnames(s1, "fertilizer", "fertilization")
setnames(s2, "fertilizer", "fertilization")
setnames(s3, "fertilizer", "fertilization")
setnames(amm, "fertilizer", "fertilization")

amm <- amm[,.(incubation, fertilization, precipitation, tillage, treatment, NH3)]

# dcast to long format (a pelo)
s1.a <- s1[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="A",
        NO.1 = aNO,
        N2O.1 = aN2O,
        CO2.1 = aCO2,
        CH4.1 = aCH4
)]
s1.b <- s1[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="B",
        NO.1 = bNO,
        N2O.1 = bN2O,
        CO2.1 = bCO2,
        CH4.1 = bCH4
)]
s1.c <- s1[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="C",
        NO.1 = cNO,
        N2O.1 = cN2O,
        CO2.1 = cCO2,
        CH4.1 = cCH4
)]
s1.d <- s1[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="D",
        NO.1 = dNO,
        N2O.1 = dN2O,
        CO2.1 = dCO2,
        CH4.1 = dCH4
)]

####
s2.a <- s2[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="A",
        NO.2 = aNO,
        N2O.2 = aN2O,
        CO2.2 = aCO2,
        CH4.2 = aCH4
)]
s2.b <- s2[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="B",
        NO.2 = bNO,
        N2O.2 = bN2O,
        CO2.2 = bCO2,
        CH4.2 = bCH4
)]
s2.c <- s2[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="C",
        NO.2 = cNO,
        N2O.2 = cN2O,
        CO2.2 = cCO2,
        CH4.2 = cCH4
)]
s2.d <- s2[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="D",
        NO.2 = dNO,
        N2O.2 = dN2O,
        CO2.2 = dCO2,
        CH4.2 = dCH4
)]

####
s3.a <- s3[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="A",
        NO.3 = aNO,
        N2O.3 = aN2O,
        CO2.3 = aCO2,
        CH4.3 = aCH4
)]
s3.b <- s3[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="B",
        NO.3 = bNO,
        N2O.3 = bN2O,
        CO2.3 = bCO2,
        CH4.3 = bCH4
)]
s3.c <- s3[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="C",
        NO.3 = cNO,
        N2O.3 = cN2O,
        CO2.3 = cCO2,
        CH4.3 = cCH4
)]
s3.d <- s3[,.(
        treatment, fertilization, precipitation, tillage,
        incubation ="D",
        NO.3 = dNO,
        N2O.3 = dN2O,
        CO2.3 = dCO2,
        CH4.3 = dCH4
)]

s1 <- rbindlist(list(s1.a, s1.b, s1.c, s1.d))
s2 <- rbindlist(list(s2.a, s2.b, s2.c, s2.d))
s3 <- rbindlist(list(s3.a, s3.b, s3.c, s3.d))

setkey(s1, incubation, fertilization, precipitation, tillage, treatment)
setkey(s2, incubation, fertilization, precipitation, tillage, treatment)
setkey(s3, incubation, fertilization, precipitation, tillage, treatment)

data <- s1[s2[s3]]
data[incubation=="D", CH4.1:=NA]
data[incubation=="D", CH4.2:=NA]
data[incubation=="D", CH4.3:=NA]

# merge NH3 data
setkey(data, incubation, fertilization, precipitation, tillage, treatment)
setkey(amm, incubation, fertilization, precipitation, tillage, treatment)
data <- amm[data]

# create sum N columns
data[, NO.N2O.1:= NO.1+N2O.1]
data[, NO.N2O.2:= NO.2+N2O.2]
data[, NO.N2O.3:= NO.3+N2O.3]

data[, NO.N2O.NH3.1:= NO.1+N2O.1+NH3]
data[, NO.N2O.NH3.2:= NO.2+N2O.2]
data[, NO.N2O.NH3.3:= NO.3+N2O.3]

source("priming_effect_summaries_1_total.R")
################################################################################
# calculate priming effects
### 1/2(3)
data[,NO.1.2:= NO.1/NO.2]
data[,NO.1.3:= NO.1/NO.3]
data[,NO.1.23:= 2*NO.1/(NO.2+NO.3)]

data[,N2O.1.2:= N2O.1/N2O.2]
data[,N2O.1.3:= N2O.1/N2O.3]
data[,N2O.1.23:= 2*N2O.1/(N2O.2+N2O.3)]

data[,NO.N2O.1.2:= NO.N2O.1/NO.N2O.2]
data[,NO.N2O.1.3:= NO.N2O.1/NO.N2O.3]
data[,NO.N2O.1.23:= 2*NO.N2O.1/(NO.N2O.2+NO.N2O.3)]

data[,NO.N2O.NH3.1.2:= NO.N2O.NH3.1/NO.N2O.NH3.2]
data[,NO.N2O.NH3.1.3:= NO.N2O.NH3.1/NO.N2O.NH3.3]
data[,NO.N2O.NH3.1.23:= 2*NO.N2O.NH3.1/(NO.N2O.NH3.2+NO.N2O.NH3.3)]

data[,CO2.1.2:= CO2.1/CO2.2]
data[,CO2.1.3:= CO2.1/CO2.3]
data[,CO2.1.23:= 2*CO2.1/(CO2.2+CO2.3)]

data[,CH4.1.2:= CH4.1/CH4.2]
data[,CH4.1.3:= CH4.1/CH4.3]
data[,CH4.1.23:= 2*CH4.1/(CH4.2+CH4.3)]

priming <- data[,.(treatment, fertilization, precipitation, tillage, incubation,
                   NO.1.2, NO.1.3, NO.1.23,
                   N2O.1.2, N2O.1.3, N2O.1.23,
                   NO.N2O.1.2, NO.N2O.1.3, NO.N2O.1.23,
                   NO.N2O.NH3.1.2, NO.N2O.NH3.1.3, NO.N2O.NH3.1.23,
                   CO2.1.2, CO2.1.3, CO2.1.23,
                   CH4.1.2, CH4.1.3, CH4.1.23)]

### 2(3)/1
priming[, NO.2.1:= 1/NO.1.2]
priming[, NO.3.1:= 1/NO.1.3]
priming[, NO.23.1:= 1/NO.1.23]

priming[, N2O.2.1:= 1/N2O.1.2]
priming[, N2O.3.1:= 1/N2O.1.3]
priming[, N2O.23.1:= 1/N2O.1.23]

priming[, CO2.2.1:= 1/CO2.1.2]
priming[, CO2.3.1:= 1/CO2.1.3]
priming[, CO2.23.1:= 1/CO2.1.23]

priming[, CH4.2.1:= 1/CH4.1.2]
priming[, CH4.3.1:= 1/CH4.1.3]
priming[, CH4.23.1:= 1/CH4.1.23]

priming[, NO.N2O.2.1:= 1/NO.N2O.1.2]
priming[, NO.N2O.3.1:= 1/NO.N2O.1.3]
priming[, NO.N2O.23.1:= 1/NO.N2O.1.23]


priming[, NO.N2O.NH3.2.1:= 1/NO.N2O.NH3.1.2]
priming[, NO.N2O.NH3.3.1:= 1/NO.N2O.NH3.1.3]
priming[, NO.N2O.NH3.23.1:= 1/NO.N2O.NH3.1.23]


# 
# # priming effects quick views
# plot(sort(data$NO.1.23))
# plot(sort(data$N2O.1.23))
# plot(sort(data$NO.N2O.1.23))
# plot(sort(data$CO2.1.23))
# abline(h=1, col="red")
# 
# plot(sort(data$NO.1.2))
# plot(sort(data$N2O.1.2))
# plot(sort(data$NO.N2O.1.2))
# plot(sort(data$CO2.1.2))
# abline(h=1, col="red")

# priming Summaries
setkey(priming, incubation, fertilization, precipitation, tillage, treatment)

### 2(3)/1 summaries
source("priming_effect_summaries_2_1.R")

### summaries 1/2(3)
# by tillage
p.12.till <- priming[,.(
        NO.1.2 = mean(NO.1.2, na.rm=T),
        sd.NO.1.2 = sd(NO.1.2, na.rm=T),
        se.NO.1.2 = sd(NO.1.2, na.rm=T)/sqrt(sum(!is.na(NO.1.2))),
        
        N2O.1.2 = mean(N2O.1.2, na.rm=T),
        sd.N2O.1.2 = sd(N2O.1.2, na.rm=T),
        se.N2O.1.2 = sd(N2O.1.2, na.rm=T)/sqrt(sum(!is.na(N2O.1.2))),
        
        NO.N2O.1.2 = mean(NO.N2O.1.2, na.rm=T),
        sd.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T),
        se.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.2))),
        
        NO.N2O.NH3.1.2 = mean(NO.N2O.NH3.1.2, na.rm=T),
        sd.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T),
        se.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.2))),
        
        CO2.1.2 = mean(CO2.1.2, na.rm=T),
        sd.CO2.1.2 = sd(CO2.1.2, na.rm=T),
        se.CO2.1.2 = sd(CO2.1.2, na.rm=T)/sqrt(sum(!is.na(CO2.1.2)))
), by=tillage]

p.13.till <- priming[,.(
        NO.1.3 = mean(NO.1.3, na.rm=T),
        sd.NO.1.3 = sd(NO.1.3, na.rm=T),
        se.NO.1.3 = sd(NO.1.3, na.rm=T)/sqrt(sum(!is.na(NO.1.3))),
        
        N2O.1.3 = mean(N2O.1.3, na.rm=T),
        sd.N2O.1.3 = sd(N2O.1.3, na.rm=T),
        se.N2O.1.3 = sd(N2O.1.3, na.rm=T)/sqrt(sum(!is.na(N2O.1.3))),
        
        NO.N2O.1.3 = mean(NO.N2O.1.3, na.rm=T),
        sd.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T),
        se.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.3))),
        
        NO.N2O.NH3.1.3 = mean(NO.N2O.NH3.1.3, na.rm=T),
        sd.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T),
        se.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.3))),
        
        CO2.1.3 = mean(CO2.1.3, na.rm=T),
        sd.CO2.1.3 = sd(CO2.1.3, na.rm=T),
        se.CO2.1.3 = sd(CO2.1.3, na.rm=T)/sqrt(sum(!is.na(CO2.1.3)))
), by=tillage]

p.1.23.till <- priming[,.(
        NO.1.23 = mean(NO.1.23, na.rm=T),
        sd.NO.1.23 = sd(NO.1.23, na.rm=T),
        se.NO.1.23 = sd(NO.1.23, na.rm=T)/sqrt(sum(!is.na(NO.1.23))),
        
        N2O.1.23 = mean(N2O.1.23, na.rm=T),
        sd.N2O.1.23 = sd(N2O.1.23, na.rm=T),
        se.N2O.1.23 = sd(N2O.1.23, na.rm=T)/sqrt(sum(!is.na(N2O.1.23))),
        
        NO.N2O.1.23 = mean(NO.N2O.1.23, na.rm=T),
        sd.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T),
        se.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.23))),
        
        NO.N2O.NH3.1.23 = mean(NO.N2O.NH3.1.23, na.rm=T),
        sd.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T),
        se.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.23))),
        
        CO2.1.23 = mean(CO2.1.23, na.rm=T),
        sd.CO2.1.23 = sd(CO2.1.23, na.rm=T),
        se.CO2.1.23 = sd(CO2.1.23, na.rm=T)/sqrt(sum(!is.na(CO2.1.23)))
), by=tillage]

# by precipitation
p.12.rain <- priming[,.(
        NO.1.2 = mean(NO.1.2, na.rm=T),
        sd.NO.1.2 = sd(NO.1.2, na.rm=T),
        se.NO.1.2 = sd(NO.1.2, na.rm=T)/sqrt(sum(!is.na(NO.1.2))),
        
        N2O.1.2 = mean(N2O.1.2, na.rm=T),
        sd.N2O.1.2 = sd(N2O.1.2, na.rm=T),
        se.N2O.1.2 = sd(N2O.1.2, na.rm=T)/sqrt(sum(!is.na(N2O.1.2))),
        
        NO.N2O.1.2 = mean(NO.N2O.1.2, na.rm=T),
        sd.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T),
        se.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.2))),
        
        NO.N2O.NH3.1.2 = mean(NO.N2O.NH3.1.2, na.rm=T),
        sd.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T),
        se.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.2))),
        
        CO2.1.2 = mean(CO2.1.2, na.rm=T),
        sd.CO2.1.2 = sd(CO2.1.2, na.rm=T),
        se.CO2.1.2 = sd(CO2.1.2, na.rm=T)/sqrt(sum(!is.na(CO2.1.2)))
), by=precipitation]

p.13.rain <- priming[,.(
        NO.1.3 = mean(NO.1.3, na.rm=T),
        sd.NO.1.3 = sd(NO.1.3, na.rm=T),
        se.NO.1.3 = sd(NO.1.3, na.rm=T)/sqrt(sum(!is.na(NO.1.3))),
        
        N2O.1.3 = mean(N2O.1.3, na.rm=T),
        sd.N2O.1.3 = sd(N2O.1.3, na.rm=T),
        se.N2O.1.3 = sd(N2O.1.3, na.rm=T)/sqrt(sum(!is.na(N2O.1.3))),
        
        NO.N2O.1.3 = mean(NO.N2O.1.3, na.rm=T),
        sd.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T),
        se.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.3))),
        
        NO.N2O.NH3.1.3 = mean(NO.N2O.NH3.1.3, na.rm=T),
        sd.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T),
        se.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.3))),
        
        CO2.1.3 = mean(CO2.1.3, na.rm=T),
        sd.CO2.1.3 = sd(CO2.1.3, na.rm=T),
        se.CO2.1.3 = sd(CO2.1.3, na.rm=T)/sqrt(sum(!is.na(CO2.1.3)))
), by=precipitation]

p.1.23.rain <- priming[,.(
        NO.1.23 = mean(NO.1.23, na.rm=T),
        sd.NO.1.23 = sd(NO.1.23, na.rm=T),
        se.NO.1.23 = sd(NO.1.23, na.rm=T)/sqrt(sum(!is.na(NO.1.23))),
        
        N2O.1.23 = mean(N2O.1.23, na.rm=T),
        sd.N2O.1.23 = sd(N2O.1.23, na.rm=T),
        se.N2O.1.23 = sd(N2O.1.23, na.rm=T)/sqrt(sum(!is.na(N2O.1.23))),
        
        NO.N2O.1.23 = mean(NO.N2O.1.23, na.rm=T),
        sd.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T),
        se.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.23))),
        
        NO.N2O.NH3.1.23 = mean(NO.N2O.NH3.1.23, na.rm=T),
        sd.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T),
        se.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.23))),
        
        CO2.1.23 = mean(CO2.1.23, na.rm=T),
        sd.CO2.1.23 = sd(CO2.1.23, na.rm=T),
        se.CO2.1.23 = sd(CO2.1.23, na.rm=T)/sqrt(sum(!is.na(CO2.1.23)))
), by=precipitation]


# by fertilization
p.12.fert <- priming[,.(
        NO.1.2 = mean(NO.1.2, na.rm=T),
        sd.NO.1.2 = sd(NO.1.2, na.rm=T),
        se.NO.1.2 = sd(NO.1.2, na.rm=T)/sqrt(sum(!is.na(NO.1.2))),
        
        N2O.1.2 = mean(N2O.1.2, na.rm=T),
        sd.N2O.1.2 = sd(N2O.1.2, na.rm=T),
        se.N2O.1.2 = sd(N2O.1.2, na.rm=T)/sqrt(sum(!is.na(N2O.1.2))),
        
        NO.N2O.1.2 = mean(NO.N2O.1.2, na.rm=T),
        sd.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T),
        se.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.2))),
        
        NO.N2O.NH3.1.2 = mean(NO.N2O.NH3.1.2, na.rm=T),
        sd.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T),
        se.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.2))),
        
        CO2.1.2 = mean(CO2.1.2, na.rm=T),
        sd.CO2.1.2 = sd(CO2.1.2, na.rm=T),
        se.CO2.1.2 = sd(CO2.1.2, na.rm=T)/sqrt(sum(!is.na(CO2.1.2)))
), by=fertilization]

p.13.fert <- priming[,.(
        NO.1.3 = mean(NO.1.3, na.rm=T),
        sd.NO.1.3 = sd(NO.1.3, na.rm=T),
        se.NO.1.3 = sd(NO.1.3, na.rm=T)/sqrt(sum(!is.na(NO.1.3))),
        
        N2O.1.3 = mean(N2O.1.3, na.rm=T),
        sd.N2O.1.3 = sd(N2O.1.3, na.rm=T),
        se.N2O.1.3 = sd(N2O.1.3, na.rm=T)/sqrt(sum(!is.na(N2O.1.3))),
        
        NO.N2O.1.3 = mean(NO.N2O.1.3, na.rm=T),
        sd.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T),
        se.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.3))),
        
        NO.N2O.NH3.1.3 = mean(NO.N2O.NH3.1.3, na.rm=T),
        sd.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T),
        se.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.3))),
        
        CO2.1.3 = mean(CO2.1.3, na.rm=T),
        sd.CO2.1.3 = sd(CO2.1.3, na.rm=T),
        se.CO2.1.3 = sd(CO2.1.3, na.rm=T)/sqrt(sum(!is.na(CO2.1.3)))
), by=fertilization]

p.1.23.fert <- priming[,.(
        NO.1.23 = mean(NO.1.23, na.rm=T),
        sd.NO.1.23 = sd(NO.1.23, na.rm=T),
        se.NO.1.23 = sd(NO.1.23, na.rm=T)/sqrt(sum(!is.na(NO.1.23))),
        
        N2O.1.23 = mean(N2O.1.23, na.rm=T),
        sd.N2O.1.23 = sd(N2O.1.23, na.rm=T),
        se.N2O.1.23 = sd(N2O.1.23, na.rm=T)/sqrt(sum(!is.na(N2O.1.23))),
        
        NO.N2O.1.23 = mean(NO.N2O.1.23, na.rm=T),
        sd.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T),
        se.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.23))),
        
        NO.N2O.NH3.1.23 = mean(NO.N2O.NH3.1.23, na.rm=T),
        sd.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T),
        se.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.23))),
        
        CO2.1.23 = mean(CO2.1.23, na.rm=T),
        sd.CO2.1.23 = sd(CO2.1.23, na.rm=T),
        se.CO2.1.23 = sd(CO2.1.23, na.rm=T)/sqrt(sum(!is.na(CO2.1.23)))
), by=fertilization]

# by treatment
p.12.treat <- priming[,.(
        NO.1.2 = mean(NO.1.2, na.rm=T),
        sd.NO.1.2 = sd(NO.1.2, na.rm=T),
        se.NO.1.2 = sd(NO.1.2, na.rm=T)/sqrt(sum(!is.na(NO.1.2))),
        
        N2O.1.2 = mean(N2O.1.2, na.rm=T),
        sd.N2O.1.2 = sd(N2O.1.2, na.rm=T),
        se.N2O.1.2 = sd(N2O.1.2, na.rm=T)/sqrt(sum(!is.na(N2O.1.2))),
        
        NO.N2O.1.2 = mean(NO.N2O.1.2, na.rm=T),
        sd.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T),
        se.NO.N2O.1.2 = sd(NO.N2O.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.2))),
        
        NO.N2O.NH3.1.2 = mean(NO.N2O.NH3.1.2, na.rm=T),
        sd.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T),
        se.NO.N2O.NH3.1.2 = sd(NO.N2O.NH3.1.2, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.2))),
        
        CO2.1.2 = mean(CO2.1.2, na.rm=T),
        sd.CO2.1.2 = sd(CO2.1.2, na.rm=T),
        se.CO2.1.2 = sd(CO2.1.2, na.rm=T)/sqrt(sum(!is.na(CO2.1.2)))
), by=.(fertilization, precipitation, tillage)]

p.13.treat <- priming[,.(
        NO.1.3 = mean(NO.1.3, na.rm=T),
        sd.NO.1.3 = sd(NO.1.3, na.rm=T),
        se.NO.1.3 = sd(NO.1.3, na.rm=T)/sqrt(sum(!is.na(NO.1.3))),
        
        N2O.1.3 = mean(N2O.1.3, na.rm=T),
        sd.N2O.1.3 = sd(N2O.1.3, na.rm=T),
        se.N2O.1.3 = sd(N2O.1.3, na.rm=T)/sqrt(sum(!is.na(N2O.1.3))),
        
        NO.N2O.1.3 = mean(NO.N2O.1.3, na.rm=T),
        sd.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T),
        se.NO.N2O.1.3 = sd(NO.N2O.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.3))),
        
        NO.N2O.NH3.1.3 = mean(NO.N2O.NH3.1.3, na.rm=T),
        sd.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T),
        se.NO.N2O.NH3.1.3 = sd(NO.N2O.NH3.1.3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.3))),
        
        CO2.1.3 = mean(CO2.1.3, na.rm=T),
        sd.CO2.1.3 = sd(CO2.1.3, na.rm=T),
        se.CO2.1.3 = sd(CO2.1.3, na.rm=T)/sqrt(sum(!is.na(CO2.1.3)))
), by=.(fertilization, precipitation, tillage)]

p.1.23.treat <- priming[,.(
        NO.1.23 = mean(NO.1.23, na.rm=T),
        sd.NO.1.23 = sd(NO.1.23, na.rm=T),
        se.NO.1.23 = sd(NO.1.23, na.rm=T)/sqrt(sum(!is.na(NO.1.23))),
        
        N2O.1.23 = mean(N2O.1.23, na.rm=T),
        sd.N2O.1.23 = sd(N2O.1.23, na.rm=T),
        se.N2O.1.23 = sd(N2O.1.23, na.rm=T)/sqrt(sum(!is.na(N2O.1.23))),
        
        NO.N2O.1.23 = mean(NO.N2O.1.23, na.rm=T),
        sd.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T),
        se.NO.N2O.1.23 = sd(NO.N2O.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.1.23))),
        
        NO.N2O.NH3.1.23 = mean(NO.N2O.NH3.1.23, na.rm=T),
        sd.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T),
        se.NO.N2O.NH3.1.23 = sd(NO.N2O.NH3.1.23, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.1.23))),
        
        CO2.1.23 = mean(CO2.1.23, na.rm=T),
        sd.CO2.1.23 = sd(CO2.1.23, na.rm=T),
        se.CO2.1.23 = sd(CO2.1.23, na.rm=T)/sqrt(sum(!is.na(CO2.1.23)))
), by=.(fertilization, precipitation, tillage)]
################################################################################################################################################################
################################################################################
# write summary files
# tillage
################################################################################
mydata <- copy (p.12.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_tillage_1_2.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.13.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_tillage_1_3.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.1.23.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_tillage_1_23.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# fertiliztion
################################################################################
mydata <- copy (p.12.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_fert_1_2.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.13.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_fert_1_3.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.1.23.fert)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_fert_1_23.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# rain
################################################################################
mydata <- copy (p.12.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_rain_1_2.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.13.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_rain_1_3.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.1.23.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_rain_1_23.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# treatment
################################################################################
mydata <- copy (p.12.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_treat_1_2.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.13.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_treat_1_3.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (p.1.23.treat)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout, "/priming_by_treat_1_23.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by core
################################################################################
myfolderout <- paste0(output_path, "/priming")
mydata <- copy (priming)
no.format <- c("precipitation", "tillage", "fertilization", "treatment", "incubation")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(myfolderout, "/priming_by_core.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

