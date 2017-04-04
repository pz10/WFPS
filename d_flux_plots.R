# folder out
# folderout <- paste0(output_path, "/WFPS/WFPS_summaries")
# dir.create(folderout, recursive=T)

# file in (balanced 3h interpolated values)
myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

# read data file
data <- fread(myfile)

# select just incubation-stretches 1 to 3
data <- data[! event %in% c("0_pre"),]

# id columns
data[,id:= paste(incubation, treatment, sep= "-")]
data[, order:= 1:.N, by = id]
with(data, table(id))

# calculate dX/dt for X in :NO, N2O, CO2
setkey(data, id, days.adjusted)

data[, pre.1 := c(NA, data$CO2)[1:nrow(data)]]
data[, pre.2 := c(NA, NA, data$CO2)[1:nrow(data)]]
data[,dCO2:= (CO2 - pre.1)/3]# /3 h
data[,d2.CO2:= (CO2 - 2*pre.1 + pre.2)/3]
data[order==1, dCO2:=NA]
data[order <= 2, d2.CO2:=NA]


data[, pre.1 := c(NA, data$NO)[1:nrow(data)]]
data[, pre.2 := c(NA, NA, data$NO)[1:nrow(data)]]
data[,dNO:= (NO - pre.1)/3]# /3 h
data[,d2.NO:= (NO - 2*pre.1 + pre.2)/3]
data[order==1, dNO:=NA]
data[order <= 2, d2.NO:=NA]


data[, pre.1 := c(NA, data$N2O)[1:nrow(data)]]
data[, pre.2 := c(NA, NA, data$N2O)[1:nrow(data)]]
data[,dN2O:= (N2O - pre.1)/3]# /3 h
data[,d2.N2O:= (N2O - 2*pre.1 + pre.2)/3]
data[order==1, dN2O:=NA]
data[order <= 2, d2.N2O:=NA]

data[, pre.1:= NULL]
data[, pre.2:= NULL]

################################################################################
################################################################################
# NO
################################################################################
g.1 <- ggplot(data[NO>30], aes(dCO2, NO))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("NO [µg-N/m2/h]")
        + xlab("dCO2 [mg-C/m2/h2]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1

################################################################################
g.1 <- ggplot(data[NO>30], aes(d2.CO2, NO))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("NO [µg-N/m2/h]")
        + xlab("dCO2/dt2 [mg-C/m2/h3]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1
################################################################################
g.1 <- ggplot(data[NO>30], aes(dCO2, dNO))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("dNO [µg-N/m2/h2]")
        + xlab("dCO2/dt2 [mg-C/m2/h2]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1

################################################################################
################################################################################
# N2O
################################################################################
g.1 <- ggplot(data[N2O>30], aes(dCO2, N2O))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N2O [µg-N/m2/h]")
        + xlab("dCO2 [mg-C/m2/h2]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1

################################################################################
g.1 <- ggplot(data[N2O>30], aes(d2.CO2, N2O))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N2O [µg-N/m2/h]")
        + xlab("dCO2/dt2 [mg-C/m2/h3]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1

################################################################################
g.1 <- ggplot(data[N2O>30], aes(dCO2, dN2O))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("dN2O [µg-N/m2/h2]")
        + xlab("dCO2 [mg-C/m2/h2]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1

################################################################################
################################################################################
# NO vs N2O
################################################################################
g.1 <- ggplot(data, aes(N2O, NO))
g.1 <- (g.1
        + facet_grid(fertilization ~ precipitation, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("dN2O [µg-N/m2/h2]")
        + xlab("dNO [mg-C/m2/h2]")
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
)
g.1