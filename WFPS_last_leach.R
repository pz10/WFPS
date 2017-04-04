# 
folderout <- paste0(output_path, "/WFPS/last_leaching_WFPS_summaries")
dir.create(folderout, recursive=T)

# file in (balanced 3h interpolated values)
myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

# file leaching
myfolder <- paste0(output_path, "/leaching")
myfile1 <- paste0(myfolder, "/4_leaching.dat")

# read data files
data <- fread(myfile)
leach <- fread(myfile1)

# get last leaching event per core
leach[days>30 & is.na(days.adjusted), days.adjusted:= 43.9]
leach <- leach[ ,tail(.SD,1), by=.(incubation, tillage, fertilization, precipitation), .SDcols= c("days.adjusted","est.leaching", "meas.leaching")]
setnames(leach, "days.adjusted", "days.last.leach")
# get WFPS at this days
data <- data[,.(
        days.adjusted = days.adjusted,
        WFPS = WFPS_0
), by=.(incubation, tillage, fertilization, precipitation)]

setkey(data, incubation, tillage, fertilization, precipitation)
setkey(leach, incubation, tillage, fertilization, precipitation)
data <- data[leach]
data <- data[days.adjusted > days.last.leach,
        head(.SD,1) 
, by=.(incubation, tillage, fertilization, precipitation)]

with(data,boxplot(WFPS~precipitation))
with(data,boxplot(WFPS~tillage))
with(data,boxplot(WFPS~fertilization))

setkey(data, fertilization, precipitation, tillage)

mysummary <- data[,.(
        mean.WFPS = mean(WFPS, na.rm=T),
        sd.WFPS = sd(WFPS, na.rm=T),
        se.WFPS = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        
        median.WFPS = quantile(WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS, na.rm = T),
        max.WFPS = max(WFPS, na.rm = T),
        
        count = .N
), by=.(precipitation, tillage, fertilization)]

sum.till <- data[,.(
        mean.WFPS = mean(WFPS, na.rm=T),
        sd.WFPS = sd(WFPS, na.rm=T),
        se.WFPS = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        
        median.WFPS = quantile(WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS, na.rm = T),
        max.WFPS = max(WFPS, na.rm = T),
        
        count = .N
), by=.(tillage)]

sum.rain <- data[,.(
        mean.WFPS = mean(WFPS, na.rm=T),
        sd.WFPS = sd(WFPS, na.rm=T),
        se.WFPS = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        
        median.WFPS = quantile(WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS, na.rm = T),
        max.WFPS = max(WFPS, na.rm = T),
        
        count = .N
), by=.(precipitation)]

sum.fert <- data[,.(
        mean.WFPS = mean(WFPS, na.rm=T),
        sd.WFPS = sd(WFPS, na.rm=T),
        se.WFPS = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        
        median.WFPS = quantile(WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS, na.rm = T),
        max.WFPS = max(WFPS, na.rm = T),
        
        count = .N
), by=.(fertilization)]

sum.total <- data[,.(
        mean.WFPS = mean(WFPS, na.rm=T),
        sd.WFPS = sd(WFPS, na.rm=T),
        se.WFPS = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        
        median.WFPS = quantile(WFPS, na.rm = T, probs = 0.50),
        
        q25.WFPS = quantile(WFPS, na.rm = T, probs = 0.25),
        q75.WFPS = quantile(WFPS, na.rm = T, probs = 0.75),
        
        q09.WFPS = quantile(WFPS, na.rm = T, probs = 0.09),
        q91.WFPS = quantile(WFPS, na.rm = T, probs = 0.91),
        
        min.WFPS = min(WFPS, na.rm = T),
        max.WFPS = max(WFPS, na.rm = T),
        
        count = .N
)]

# write summary files
mydata <- copy (mysummary)
no.format <- c("precipitation", "tillage", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/last_leaching_WFPS_0_summary_by_treatment.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write summary files
mydata <- copy (sum.fert)
no.format <- c("precipitation", "tillage", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/last_leaching_WFPS_0_summary_by_fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write summary files
mydata <- copy (sum.rain)
no.format <- c("precipitation", "tillage", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/last_leaching_WFPS_0_summary_by_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write summary files
mydata <- copy (sum.till)
no.format <- c("precipitation", "tillage", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/last_leaching_WFPS_0_summary_by_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# write summary files
mydata <- copy (sum.total)
no.format <- c("precipitation", "tillage", "fertilization")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/last_leaching_WFPS_0_summary.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
################################################################################
col.rain <- c("white", "deepskyblue","dodgerblue4")
col.fert <- c("white","olivedrab2", "olivedrab4")
col.till <- c("black","red")

g.1a <- ggplot(data, aes(x = tillage, y=WFPS, fill=tillage))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("WFPS [%]")
         + xlab("tillage")
         # + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.till)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a

g.1a <- ggplot(data, aes(x = tillage, y=WFPS, fill=tillage))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("WFPS [%]")
         + xlab("tillage")
         # + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.till)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a


g.2 <- ggplot(data, aes(x = precipitation, y=WFPS, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ precipitation, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Rain pattern")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2