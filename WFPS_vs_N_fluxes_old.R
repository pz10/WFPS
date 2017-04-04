myplots <- paste0(output_path, "/WFPS/WFPS_vs_N/plots")
dir.create(myplots, recursive=T)
folderout <- paste0(output_path, "/WFPS/WFPS_vs_N")
dir.create(folderout, recursive=T)

myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

data <- fread(myfile)

# get mean cummulative fluxes by WFPS 'wide' stretches
wide <- 0.5
cuts <- seq(from=wide/2, to= 110+wide/2, by=wide)
label.cut <- cuts[-1]-wide/2
data[, WFPS := cut(WFPS_0, breaks = cuts, labels=factor(label.cut))]
setkey(data, WFPS)

data[days.adjusted<45, event:=3]
data[days.adjusted<30, event:=2]
data[days.adjusted<15, event:=1]

cum <- data[,.(
        count.WFPS = .N,
        count.event1 = sum(event==1), # for later partition of density by events
        count.event2 = sum(event==2),
        count.event3 = sum(event==3),
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18,
        CO2 = sum(CO2, na.rm=T)*3/18,
        CH4 = sum(CH4, na.rm=T)*3/18
        
), by= WFPS]
setkey(cum, WFPS)
cum[, dens.event1:= count.event1/count.WFPS]
cum[, dens.event2:= count.event2/count.WFPS]
cum[, dens.event3:= count.event3/count.WFPS]

# get rolling means over WFPS values to smoothen the curve a bit
window <- 3 #in WFPS % units
# myNO <- rollmean(cum$NO, k=window/wide+1, fill="extend")
# myN2O <- rollmean(cum$N2O, k=window/wide+1, fill="extend")
# myCO2 <- rollmean(cum$CO2, k=window/wide+1, fill="extend")
# myCH4 <- rollmean(cum$CH4, k=window/wide+1, fill="extend")
# mycount <- rollmean(cum$count.WFPS, k=window/wide+1, fill="extend")

myNO <- rollapply(cum$NO, FUN= mean, na.rm=T, width=window/wide+1, fill=NA) #fill="extend"
myN2O <- rollapply(cum$N2O, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
myCO2 <- rollapply(cum$CO2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
myCH4 <- rollapply(cum$CH4, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
mycount <- rollapply(cum$count.WFPS, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)


cum.w <- copy(cum)
cum.w[, NO:= myNO]
cum.w[, N2O:= myN2O]
cum.w[, CO2:= myCO2]
cum.w[, CH4:= myCH4]
# cum.w[, count.WFPS:= mycount]
cum.w[, WFPS:= as.numeric(as.character(WFPS))]


# get density curves
cum.dens <- copy(cum.w)
total <- cum.w[,sum(NO+N2O, na.rm=T)]
cum.dens[, NO:= 100*NO/total]
cum.dens[, N2O:= 100*N2O/total]
cum.dens[, CO2:= 100*CO2/sum(CO2, na.rm=T)]


# WFPS density curves
d <- density(data$WFPS_0, adjust = 1/2)
myd <- data.table(WFPS=d$x, y=d$y)

mywfps <- approx(x=d$x, y=d$y, xout=cum.dens$WFPS)

cum.dens[, WFPS.dens:= 100 * mywfps$y]

cum.dens[, WFPS.dens.1:= WFPS.dens * dens.event1]
cum.dens[, WFPS.dens.2:= WFPS.dens * dens.event2]
cum.dens[, WFPS.dens.3:= WFPS.dens * dens.event3]

cum.dens[, WFPS.dens.3:= WFPS.dens.3 + WFPS.dens.2 + WFPS.dens.1]
cum.dens[, WFPS.dens.2:= WFPS.dens.2 + WFPS.dens.1]

# smoothen WFPS density curves
my1 <- rollapply(cum.dens$WFPS.dens.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
cum.dens[,aux := WFPS.dens.1]
cum.dens[, WFPS.dens.1:= my1]
cum.dens[dens.event1 %in% c(0,1), WFPS.dens.1:= aux]

my2 <- rollapply(cum.dens$WFPS.dens.2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
cum.dens[,aux := WFPS.dens.2]
cum.dens[, WFPS.dens.2:= my2]
cum.dens[dens.event2 %in% c(0,1), WFPS.dens.2:= aux]
cum.dens[, aux:=NULL]

####
# 100%-normalized cum.dens(NO or N2O) by WFPS and event; 
`%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
cum.event <- data[,.(
        NO = sum(NO, na.rm=T)*3/18, #*3 hours (µgN/m2/h --> µgN/m2); /18 treatments
        N2O = sum(N2O, na.rm=T)*3/18        
), by= .(event, WFPS)]
setkey(cum.event, event, WFPS)


myNO <- dcast.data.table(cum.event, WFPS ~ event, value.var= "NO")
setnames(myNO, c("1", "2", "3"), c("NO.1", "NO.2", "NO.3"))
# myNO[,NO:=rowSums(.SD, na.rm = TRUE), .SDcols = c("NO.1", "NO.2", "NO.3")]

myN2O <- dcast.data.table(cum.event, WFPS ~ event, value.var= "N2O")
setnames(myN2O, c("1", "2", "3"), c("N2O.1", "N2O.2", "N2O.3"))
# myN2O[,N2O:=rowSums(.SD, na.rm = TRUE), .SDcols = c("N2O.1", "N2O.2", "N2O.3")]

events <- myN2O [myNO]
# smoothing
a.1 <- is.na(events$NO.1)
a.2 <- is.na(events$NO.2)
a.3 <- is.na(events$NO.3)
b.1 <- is.na(events$N2O.1)
b.2 <- is.na(events$N2O.2)
b.3 <- is.na(events$N2O.3)
wide <- 0.5
window <- 3 #in WFPS % units
myNO.1 <- rollapply(events$NO.1, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myNO.2 <- rollapply(events$NO.2, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myNO.3 <- rollapply(events$NO.3, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.1 <- rollapply(events$N2O.1, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.2 <- rollapply(events$N2O.2, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
myN2O.3 <- rollapply(events$N2O.3, FUN= mean, na.rm=T, width=window/wide+1, fill="extend")
events[, NO.1:= myNO.1]
events[, NO.2:= myNO.2]
events[, NO.3:= myNO.3]
events[, N2O.1:= myN2O.1]
events[, N2O.2:= myN2O.2]
events[, N2O.3:= myN2O.3]

events[a.1, NO.1:= NA]
events[a.2, NO.2:= NA]
events[a.3, NO.3:= NA]
events[b.1, N2O.1:= NA]
events[b.2, N2O.2:= NA]
events[b.3, N2O.3:= NA]
#
events[,NO:=rowSums(.SD, na.rm = TRUE), .SDcols = c("NO.1", "NO.2", "NO.3")]
events[,NO.1:= 100 * NO.1 / NO]
events[,NO.2:= 100 * NO.2 / NO]
events[,NO.3:= 100 * NO.3 / NO]
events[,NO.3:= NO.1 %+na% NO.2 %+na% NO.3]
events[,NO.2:= NO.1 %+na% NO.2]

events[,N2O:=rowSums(.SD, na.rm = TRUE), .SDcols = c("N2O.1", "N2O.2", "N2O.3")]
events[,N2O.1:= 100 * N2O.1 / N2O]
events[,N2O.2:= 100 * N2O.2 / N2O]
events[,N2O.3:= 100 * N2O.3 / N2O]
events[,N2O.3:= N2O.1 %+na% N2O.2 %+na% N2O.3]
events[,N2O.2:= N2O.1 %+na% N2O.2]

events[,WFPS:= as.numeric(as.character(WFPS))]

# add WFPS densities by event
event100 <- cum.dens[,.(WFPS, event.1 = 100*dens.event1, event.2 = 100*dens.event2)]
event100[, event.2:= event.1 + event.2]
setkey(events, WFPS)
setkey(event100, WFPS)
events <- event100[events]

my1 <- rollapply(events$event.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
events[,aux := event.1]
events[, event.1:= my1]
events[aux %in% c(0,100), event.1:= aux]

my2 <- rollapply(events$event.2, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
events[,aux := event.2]
events[, event.2:= my2]
events[aux %in% c(0,100), event.2:= aux]
events[, aux:=NULL]

# 
# # smoothing
# a <- events$NO.1
# b <- rollapply(events$NO.1, FUN= mean, na.rm=T, width=window/wide+1, fill=NA)
# test <- data.table(WFPS = events$WFPS, original =a, rolled = b)
# test[original<1, original := 0]
# test[rolled<1, rolled := 0]
# 
# 
# 





# write file
mydata <- copy (cum)
setkey(mydata, WFPS)
to.format4 <- c("NO", "N2O", "CO2", "CH4")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
setnames(mydata, "CH4", "CH4.microgNm2")
myfile <- paste0(folderout, "/meanflux_by_wfps.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
# write file
mydata <- copy (cum.w)
setkey(mydata, WFPS)
to.format4 <- c("NO", "N2O", "CO2", "CH4")
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
setnames(mydata, "CH4", "CH4.microgNm2")
myfile <- paste0(folderout, "/meanflux_by_wfps_roll_", window,".dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# plots
g.1 <- ggplot(cum.dens, aes(x= WFPS, y = NO/wide))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [mg-N/m2]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
                + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens), fill="grey92")
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens.2), fill="grey85")
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens.1), fill="grey78")
        
#         + geom_ribbon(data=myd,aes(x= WFPS + 1, y =y, ymin=0, ymax=100*y), fill="grey90")
#         + geom_ribbon(data=myd,aes(x= WFPS.2 + 1, y =y.2, ymin=0, ymax=100*y.2), fill="grey85")
#         + geom_ribbon(data=myd,aes(x= WFPS.1 + 1, y =y.1, ymin=0, ymax=100*y.1), fill="grey80")
        + geom_line(size=0.25, color= "#74276D")
        + geom_line(aes(x= WFPS, y=N2O/wide),size=0.25, color= "#8EA336")

        
#         + geom_density(data= wfps.dens, aes(x= WFPS_0))
        #                 + geom_line(aes(x= WFPS, y=CO2/wide/1000),size=0.25)
        #         + geom_line(aes(x= WFPS, y=CH4/wide/1000),size=0.25, col="red")
)
g.1



data[days.adjusted<45, event:=3]
data[days.adjusted<30, event:=2]
data[days.adjusted<15, event:=1]
summary <- data[,.(
        NO = 3*sum(NO)/1000/18/4, #*3h; /1000 µg to mg; /18treatments/4replicates
        N2O = 3*sum(N2O)/1000/18/4
#         CO2 = 3*sum(CO2)/1000/18/4
        ), by=.(event)]

summ.dens <- copy(summary)

myNO <- sum(summary$NO)
myN2O <- sum(summary$N2O)
myNON2O <- myNO+ myN2O

##
df <- data.frame(segment = c("NO", "N2O"),
                 segpct = c(100*myNO/(myNO+myN2O), 100-100*myNO/(myNO+myN2O)),
                 event_1 = c(87,32.9),
                 event_2 = c(11.9, 22),
                 event_3 = c(1.1, 45.1)
)

df$xmax <- cumsum(df$segpct)
df$xmin <- df$xmax - df$segpct
df$segpct <- NULL
dfm <- melt(df, id = c("segment", "xmin", "xmax"))


library(plyr)
dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(value))
dfm1 <- ddply(dfm1, .(segment), transform,
              ymin = ymax - value)

dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)

col.events <- c("#DBF27D", "#8EA336", "#47550D", "#AB59A4", "#74276D", "#3C0938")

# col.events <- c("#8EA336", "#AB59A4", "#DBF27D", "#3C0938", "#47550D", "#74276D")
dfm1$color <- LETTERS[1:6]

g.2 <- ggplot(dfm1, aes(ymin = ymin, ymax = ymax,
                        xmin = xmin, xmax = xmax, fill = color))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("relative abundance [%]")
        + xlab("species")
                + labs(title = "(b)")
        #         + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)
g.2 <- (g.2
        + geom_rect()
        + scale_fill_manual(values=col.events)
        + geom_text(aes(x = xtext, y = ytext,
                        label = ifelse(segment == "A", paste(variable,
                                                             " - ", value, "%", sep = ""), paste(value,
                                                                                                 "%", sep = ""))), size = 3.5)
        + geom_text(aes(x = xtext, y = 103,
                        label = paste(segment)), size = 4)
)
g.2

#
myplot <- paste0(myplots,"/WFPS_vs_NO_N2O_wfpsbyevent.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1:6))
print(g.2, vp = vplayout(1, 7:10))

dev.off()

################################################################################
# plots
col.events <- c("#DBF27D", "#8EA336", "#47550D", "#AB59A4", "#74276D", "#3C0938")
col.events <- c("#DBF27D", "#8EA336", "#47550D", "#F8AFF2", "#AB59A4", "#74276D")
# col.events <- c("#F0FEB3", "#DBF27D", "#8EA336", "#F8AFF2", "#AB59A4", "#74276D")


g.1a <- ggplot(events, aes(x= WFPS, y = NO.3))
g.1a <- (g.1a
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("NO emission share by rain event [%]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
                + coord_cartesian(xlim= c(0, 110), ylim=c(0,100))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1a <- (g.1a
         + geom_ribbon(aes(ymin=0, ymax=NO.3), fill=col.events[4])
         + geom_ribbon(aes(ymin=0, ymax=NO.2), fill=col.events[5])
         + geom_ribbon(aes(ymin=0, ymax=NO.1), fill=col.events[6])
         
         + geom_hline(yintercept=c(25,50,75), col="grey90", size = 0.2* 8/12, alpha=0.3)
         + geom_hline(yintercept=c(25,50,75,100)-12.5, col="grey98", size = 0.5* 8/12, alpha=0.05)
         + geom_vline(xintercept=c(25,50,75), col="grey90", size = 0.2* 8/12, alpha=0.3)
         + geom_vline(xintercept=c(25,50,75,100)-12.5, col="grey98", size = 0.5* 8/12, alpha=0.05)
         
         +geom_line(aes(x= WFPS, y = event.1), col="white")
         +geom_line(aes(x= WFPS, y = event.2))
)
g.1a

####
g.1b <- ggplot(events, aes(x= WFPS, y = N2O.3))
g.1b <- (g.1b
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin =       unit(-0.4, "lines"),
                 strip.background =   element_blank(),
                 strip.text.x =       element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("N2O emission share by rain event [%]")
         + xlab("WFPS  [%]")
         #         + labs(title = "(a)")
         + coord_cartesian(xlim= c(0, 110), ylim=c(0,100))
         + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
         #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1b <- (g.1b
         + geom_ribbon(aes(ymin=0, ymax=N2O.3), fill=col.events[1])
         + geom_ribbon(aes(ymin=0, ymax=N2O.2), fill=col.events[2])
         + geom_ribbon(aes(ymin=0, ymax=N2O.1), fill=col.events[3])
         
         + geom_hline(yintercept=c(25,50,75), col="grey90", size = 0.2* 8/12, alpha=0.3)
         + geom_hline(yintercept=c(25,50,75,100)-12.5, col="grey98", size = 0.5* 8/12, alpha=0.05)
         + geom_vline(xintercept=c(25,50,75), col="grey90", size = 0.2* 8/12, alpha=0.3)
         + geom_vline(xintercept=c(25,50,75,100)-12.5, col="grey98", size = 0.5* 8/12, alpha=0.05)
         
         
         +geom_line(aes(x= WFPS, y = event.1), col="white")
         +geom_line(aes(x= WFPS, y = event.2))
)
g.1b




################################################################################

limit <- 0.0005
cum.w[,wfps.dens:= count.WFPS/sum(count.WFPS)]
cum.w[wfps.dens < limit, wfps.dens:= limit]
# plot
g.1 <- ggplot(cum.w, aes(x= WFPS, y = NO/wide/1000))
# g.1 <- ggplot(cum.w, aes(x= WFPS, y = NO/wide/1000/wfps.dens))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [mg-N/m2]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_line(size=0.25, color= "#74276D")
        + geom_line(aes(x= WFPS, y=N2O/wide/1000),size=0.25, color= "#8EA336")
        #         + geom_line(aes(x= WFPS, y=CO2/wide/1000),size=0.25)
        #         + geom_line(aes(x= WFPS, y=CH4/wide/1000),size=0.25, col="red")
)
g.1

cum.dens <- copy(cum.w)
total <- cum.w[,sum(NO+N2O)]
cum.dens[, NO:= 100*NO/total]
cum.dens[, N2O:= 100*N2O/total]


wfps.dens  <- data[,.(WFPS_0)]
wfps.dens

# plot
g.1 <- ggplot(cum.dens, aes(x= WFPS, y = NO/wide))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [mg-N/m2]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_line(size=0.25, color= "#74276D")
        + geom_line(aes(x= WFPS, y=N2O/wide),size=0.25, color= "#8EA336")
        + geom_density(data= wfps.dens, aes(x= WFPS_0))
        #                 + geom_line(aes(x= WFPS, y=CO2/wide/1000),size=0.25)
        #         + geom_line(aes(x= WFPS, y=CH4/wide/1000),size=0.25, col="red")
)
g.1







# plot
g.1 <- ggplot(data, aes(as.numeric(WFPS_0), NO))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [mg-N/m2]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
        + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_line(size=0.25, color= "#74276D")
        + geom_line(aes(as.numeric(WFPS_0), N2O),size=0.25, color= "#8EA336")
)
g.1







tobind.NO <- data[,.(days.adjusted, incubation,
                     treatment, fertilizer, precipitation, tillage,
                     species = "NO", N.emission = NO, WFPS_0)]
tobind.N2O <- data[,.(days.adjusted, incubation,
                      treatment, fertilizer, precipitation, tillage,
                      species = "N2O", N.emission = N2O, WFPS_0)]
myN <- rbindlist(list(tobind.NO, tobind.N2O))
myN[, WFPS := cut(WFPS_0, breaks = seq(from=0.5, to= 110.5, by=1), labels=1:110)]
setkey(myN,species, WFPS)
n <- myN[,.(
        N.cum = sum(N.emission, na.rm=T)/18,
        count.WFPS = .N
), by= .(WFPS, species)]
setkey(n,species, WFPS)
n[,WFPS.dens := count.WFPS/sum(count.WFPS), by=species]

limit <- 0.1
n[,WFPS.dens.corr := WFPS.dens]
n[WFPS.dens < limit, WFPS.dens.corr := limit]
n[, N.WFPScorrected := N.cum/WFPS.dens.corr]

# plot
g.1 <- ggplot(n, aes(as.numeric(WFPS), N.WFPScorrected))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [µg-N/m2/h]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
        + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_line(aes(colour = species), size=0.25)
        + scale_colour_manual(values = c("#74276D","#8EA336"))
)
g.1



g.1 <- ggplot(data, aes(as.numeric(WFPS_0), CH4))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("N-emission (NO or N2O)  [µg-N/m2/h]")
        + xlab("WFPS  [%]")
        #         + labs(title = "(a)")
        + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("#74276D","#8EA336"))
)
g.1


g.1 <- ggplot(data, aes(CO2, N2O))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
)

g.1 <- (g.1
        + geom_point(size=2.25, alpha=0.25)
        #         + scale_colour_manual(values = c("#74276D","#8EA336"))
)
g.1




tobindNO <- copy(ABCDfluxes[,.(days.adjusted, treatment, fertilizer, precipitation, tillage,
                               NO, N2O, CO2, CH4, WFPS_0)])
tobindN2O <- copy(ABCDfluxes[,.(days.adjusted, treatment, fertilizer, precipitation, tillage,
                                NO, N2O, CO2, CH4, WFPS_0)])
tobindNO[, species:= "NO"]
tobindNO[,NOandN2O:=NO]

tobindN2O[, species:= "N2O"]
tobindN2O[,NOandN2O:=N2O]

data <- rbind(tobindNO, tobindN2O)
data <- data[days.adjusted>0]
data[NOandN2O<0, NOandN2O:=0]


data[,min(days.adjusted), by=treatment]
data[days.adjusted>15 ,min(days.adjusted), by=treatment]
data[days.adjusted>30 ,min(days.adjusted), by=treatment]

data[days.adjusted<15,max(days.adjusted), by=treatment]
data[days.adjusted<30,max(days.adjusted), by=treatment]
data[,max(days.adjusted), by=treatment]

ABCDfluxes[,min(days.adjusted), by=treatment]
ABCDfluxes[days.adjusted>15 ,min(days.adjusted), by=treatment]
ABCDfluxes[days.adjusted>30 ,min(days.adjusted), by=treatment]

ABCDfluxes[days.adjusted<15,max(days.adjusted), by=treatment]
ABCDfluxes[days.adjusted<30,max(days.adjusted), by=treatment]
ABCDfluxes[days.adjusted<45,max(days.adjusted), by=treatment]





title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]")
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O.png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()

# from WFPS_vs_mean_N_plots.R
title <- paste("WFPS [%] vs NO ~ N2O relative abundance [%]")
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O_relative_abundance.png")
png(filename = myplot, width = 1920, height = 400, units = "px")

p <- ggplot(mybind, aes(x=WFPS, y=rel_abundance, color=species))
p +
        geom_point(alpha=1, na.rm=TRUE, size=5) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set1") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        xlim(0,100) +
        ylim(0,100) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = c(0.85,0.5)) +
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))