myplots <- paste0(output_path, "/WFPS/WFPS_vs_N/plots")
dir.create(myplots, recursive=T)

folderin <- paste0(output_path, "/WFPS/WFPS_vs_N")
window <- 4 #in WFPS % units
s.cum <- paste0(folderin, "/meanflux_by_wfps_roll_", window,".dat")
s.cum100 <- paste0(folderin, "/100normalized_meanflux_by_wfps_roll_", window,".dat")

s.cum <- fread(s.cum)
s.cum100 <- fread(s.cum100)

# labels <- data.table(x = c(22, 84, 50, 15, 45, 75),
#                      y= c(3.8, 2, 1.5, 0.5, 0.9, 0.9),
#                      label = c("NO", "N2O", "WFPS", "1st", "2nd", "3rd"),
#                      color = c("#E69F00", "#12329C", "grey50", "white", "white", "grey 68" ))
# labels <- data.table(x = c(22, 84, 50),
#                      y= c(3.8, 2, 1.5),
#                      label = c("NO", "N2O", "WFPS"),
#                      color = c("#E69F00", "#12329C", "grey50"))
labels <- data.table(x = c(22, 84, 50),
                     y= c(3.8, 2, 1.5),
                     label = c("NO", "N2O", "WFPS"),
                     color = c("#E69F00", "#12329C", "grey50"))

# plots
g.1 <- ggplot(s.cum, aes(x= WFPS, y = NO/wide))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, -1, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                axis.text.x = element_blank(),
                strip.text.x = element_blank(),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("density of occurrence \n [%]")
        + xlab("")
                # + labs(title = "(a)")
        + coord_cartesian(xlim= c(0, 100))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens), fill="grey92")
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens.1 + WFPS.dens.2), fill="grey85")
        + geom_ribbon(aes(x= WFPS, ymin = 0, ymax = WFPS.dens.1), fill="grey78")
        
        #         + geom_ribbon(data=myd,aes(x= WFPS + 1, y =y, ymin=0, ymax=100*y), fill="grey90")
        #         + geom_ribbon(data=myd,aes(x= WFPS.2 + 1, y =y.2, ymin=0, ymax=100*y.2), fill="grey85")
        #         + geom_ribbon(data=myd,aes(x= WFPS.1 + 1, y =y.1, ymin=0, ymax=100*y.1), fill="grey80")
#         + geom_line(size=0.25, color= "#74276D")
#         + geom_line(aes(x= WFPS, y=N2O/wide),size=0.25, color= "#8EA336")
        + geom_line(size=0.5, color= "#E69F00")#B72559
        + geom_line(aes(x= WFPS, y=N2O/wide),size=0.5, color= "#12329C")#72BC26
#         + coord_flip()

+ geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2.3, color=labels$color)
+ geom_vline(xintercept= 62.5, col= "black", size=.25, lty=5)
)

s.cum100[,myNO.2:=rowSums(.SD, na.rm = TRUE), .SDcols = c("Ndens.NO.1", "Ndens.NO.2")]
s.cum100[WFPS>92.5, myNO.2:=NA]
s.cum100[is.na(Ndens.NO.1), Ndens.NO.1:=0]


s.cum100[,myN2O.2:=rowSums(.SD, na.rm = TRUE), .SDcols = c("Ndens.N2O.1", "Ndens.N2O.2")]
s.cum100[is.na(Ndens.N2O.1), Ndens.NO.1:=0]
# s.cum100[myN2O.2 < 25 & WFPS >75, .(WFPS, myN2O.2)]
s.cum100[WFPS >90 & myN2O.2<0.1, myN2O.2:=NA]

labels <- data.table(x = c(16, 40, 78),
                     y= c(10, 10, 80),
                     label = c("1st", "2nd", "3rd"),
                     color = c("white", "grey50", "grey50"))
         
g.1a <- ggplot(s.cum100, aes(x= WFPS, y = Ndens.event.1))
g.1a <- (g.1a
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0, 0.5, 0, 0.1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin =       unit(-0.4, "lines"),
                 strip.background =   element_blank(),
                 strip.text.x =       element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("occurrence by incubation phase [%]")
         + xlab("WFPS  [%]")
         #         + labs(title = "(a)")
         + coord_cartesian(xlim= c(0, 100), ylim=c(0,100))
         + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
)

g.1a <- (g.1a
         + geom_ribbon(aes(ymin=0, ymax=100), fill="grey92")
         + geom_ribbon(aes(ymin=0, ymax=Ndens.event.2), fill="grey85")
         + geom_ribbon(aes(ymin=0, ymax=Ndens.event.1), fill="grey68")
         
         
#          + geom_line(aes(x= WFPS, y = Ndens.NO.1), col="#74276D")
#          + geom_line(aes(x= WFPS, y = myNO.2), col="#74276D", linetype="dotted")         
#          + geom_line(aes(x= WFPS, y = Ndens.N2O.1), col="#8EA336")
#          + geom_line(aes(x= WFPS, y = myN2O.2), col="#8EA336", linetype="dotted")
         + geom_line(aes(x= WFPS, y = Ndens.NO.1), col="#E69F00")
         + geom_line(aes(x= WFPS, y = myNO.2), col="#E69F00", linetype="dotted")         
         + geom_line(aes(x= WFPS, y = Ndens.N2O.1), col="#12329C")
         + geom_line(aes(x= WFPS, y = myN2O.2), col="#12329C", linetype="dotted")
         
#          + geom_line(aes(x= WFPS, y = Ndens.event.2))
         
         + geom_hline(yintercept=c(25,50,75,100), col="white", size = 0.2* 8/12, alpha=0.3)
         + geom_hline(yintercept=c(25,50,75,100)-12.5, col="white", size = 0.5* 8/12, alpha=0.1)
         + geom_vline(xintercept=c(25,50,75,100), col="white", size = 0.2* 8/12, alpha=0.3)
         + geom_vline(xintercept=c(25,50,75,100)-12.5, col="white", size = 0.5* 8/12, alpha=0.1)
         
+ geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2.8, color=labels$color)
+ geom_vline(xintercept= 62.5, col= "black", size=.25, lty=5)
)
g.1a







data[days.adjusted<45, event:=3]
data[days.adjusted<30, event:=2]
data[days.adjusted<15, event:=1]
summary <- data[,.(
        NO = 3*sum(NO)/1000/18/4, #*3h; /1000 µg to mg; /18treatments/4replicates
        N2O = 3*sum(N2O)/1000/18/4
        #         CO2 = 3*sum(CO2)/1000/18/4
), by=.(event)]
mysumm <- copy(summary)
mysumm <- 100*mysumm/mysumm[,sum(NO, N2O)]

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
col.events <- c("#B1E074", "#97C858", "#6A982E", "#F5857F", "#DB6761", "#A73933")
col.events <- c("#CAF1A2", "#B1E879", "#8FCD4F", "#EE9EBA", "#E27098", "#B52759")

col.events <- c("#8FCD4F", "#B1E879", "#CAF1A2", 
                "#B52759", "#E27098", "#EE9EBA")
col.events <- c("#8FCD4F", "#B1E879", "#CAF1A2", 
                "#C74976", "#E27098", "#EE9EBA")

col.events <- c("#2E4CAD", "#4B65BB", "#7288D0", 
                "#FFBD2C", "#FFCA53", "#FFD67D")
col.events <- c("#2E4CAD", "#6177BE", "#AEBAE2", 
                "#FFBD2C", "#FFD371", "#FFEABC")
# E69F00


dfm1$color <- LETTERS[1:6]
dfm1$value1 <- c(24,12,18,40,5,1)
dfm1$ytext[6] <- 101.5

g.2 <- ggplot(dfm1, aes(ymin = ymin, ymax = ymax,
                        xmin = xmin, xmax = xmax, fill = color))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.1, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("occurrence by rain event [%]")
        + xlab("occurrence by species [%] ")
        + labs(title = "(b)")
        #         + coord_cartesian(xlim= c(0, 110))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))
        + scale_y_continuous(breaks = c(0, 25, 50, 75, 100))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)
g.2 <- (g.2
        + geom_rect()
        + scale_fill_manual(values=col.events)
        + geom_text(aes(x = xtext, y = ytext,
                        label = ifelse(segment == "A", paste(variable,
                                                             " - ", value1, "%", sep = ""), paste(value1,
                                                                                                 "%", sep = ""))), size = 2)
        + geom_text(aes(x = xtext, y = 107,
                        label = paste(segment)), size = 4)
)
g.2

#
myplot <- paste0(myplots,"/WFPS_vs_NO_N2O_wfpsbyevent_w", window,".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 12)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1:8))
print(g.1a, vp = vplayout(2, 1:8))
print(g.2, vp = vplayout(1:2, 9:12))

dev.off()

#
myplot <- paste0(myplots,"/WFPS_vs_NO_N2O_wfpsbyevent_w", window,"simplified.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1))
print(g.1a, vp = vplayout(2, 1))

dev.off()