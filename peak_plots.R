myplots <- paste0(output_path, "/WFPS/peaks/plots")
dir.create(myplots, recursive=T)
### plots
# plot color parameters
col.rain <- c("white", "deepskyblue","dodgerblue4")
col.fert <- c("white","olivedrab2", "olivedrab4")
col.till <- c("grey","red")
# col.inc <- c("red", "black", "darkorange2")
col.inc <- c("red", "black", "darkorange2", "grey")

data <- copy(NO)

data[, labelT:=paste(fertilization, precipitation)]
data[, labelP:=paste(fertilization, tillage)]
data[, labelF:=paste(precipitation, tillage)]
# data[, labelINC:=paste(incubation, tillage)]

data[, precipitation:= factor(precipitation, levels=c("c", "i", "d"), labels= c("c", "i", "d"))]
data[, fertilization:= factor(fertilization, levels=c("0", "50", "100"), labels= c("0", "50", "100"))]
data[, labelP:= factor(labelP, levels=c("0 TT", "0 NT", "50 NT", "50 TT", "100 NT", "100 TT"),
                       labels= c("0 NT", "0 TT", "50 NT", "50 TT", "100 NT", "100 TT"))]
data[, labelF:= factor(labelF, levels=c("c NT", "c TT", "i NT", "i TT", "d NT",  "d TT"),
                       labels= c("c NT", "c TT", "i NT", "i TT", "d NT",  "d TT"))]
data[, labelT:= factor(labelT, levels=c("0 c", "0 i", "0 d", "50 c", "50 i", "50 d", "100 c", "100 i", "100 d"),
                       labels= c("0 c", "0 i", "0 d", "50 c", "50 i", "50 d", "100 c", "100 i", "100 d"))]
# data[, labelINC:= factor(labelINC, levels=c("A TT", "B TT", "C TT", "A NT", "B NT", "C NT"),
#                          labels= c("A TT", "B TT", "C TT", "A NT", "B NT", "C NT"))]

data <- data[event==1]
source("peak_bpNO.R")


data <- copy(N2O)

data[, labelT:=paste(fertilization, precipitation)]
data[, labelP:=paste(fertilization, tillage)]
data[, labelF:=paste(precipitation, tillage)]
# data[, labelINC:=paste(incubation, tillage)]

data[, precipitation:= factor(precipitation, levels=c("c", "i", "d"), labels= c("c", "i", "d"))]
data[, fertilization:= factor(fertilization, levels=c("0", "50", "100"), labels= c("0", "50", "100"))]
data[, labelP:= factor(labelP, levels=c("0 TT", "0 NT", "50 NT", "50 TT", "100 NT", "100 TT"),
                       labels= c("0 NT", "0 TT", "50 NT", "50 TT", "100 NT", "100 TT"))]
data[, labelF:= factor(labelF, levels=c("c NT", "c TT", "i NT", "i TT", "d NT",  "d TT"),
                       labels= c("c NT", "c TT", "i NT", "i TT", "d NT",  "d TT"))]
data[, labelT:= factor(labelT, levels=c("0 c", "0 i", "0 d", "50 c", "50 i", "50 d", "100 c", "100 i", "100 d"),
                       labels= c("0 c", "0 i", "0 d", "50 c", "50 i", "50 d", "100 c", "100 i", "100 d"))]
# data[, labelINC:= factor(labelINC, levels=c("A TT", "B TT", "C TT", "A NT", "B NT", "C NT"),
#                          labels= c("A TT", "B TT", "C TT", "A NT", "B NT", "C NT"))]

data <- data[event==1]
source("peak_bpN2O.R")