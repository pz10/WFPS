folder_in <- paste0(output_path, "/WFPS/WFPS_vs_N")
folderout <- paste0(output_path, "/WFPS/WFPS_vs_N")

myfile <- paste0(folder_in, "/meanflux_by_wfps.dat")
cum <- fread(myfile)

myfile <- paste0(folder_in, "/meanflux_by_wfps_event.dat")
cum.event <- fread(myfile)
# get common WFPS for 1 and 2 (and 3)
U.12 <- cum[count.event1!=0 & count.event2 != 0, WFPS]
# U.123 <- cum[count.event1!=0 & count.event2 != 0 & count.event3 != 0, WFPS]
################################################################################
### event 1 vs event 2
count1 <- cum[WFPS %in% U.12, sum(count.event1, na.rm=T)]
count2 <- cum[WFPS %in% U.12, sum(count.event2, na.rm=T)]

NO.1 <- cum.event[WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
NO.2 <- cum.event[WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]

N2O.1 <- cum.event[WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
N2O.2 <- cum.event[WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]


primming.NO <- NO.1/count1 / (NO.2/count2)
primming.N2O <- N2O.1/count1 / (N2O.2/count2)

## divide event 1 between its two major peaks
U.12.a <- U.12[U.12<41]
U.12.b <- U.12[U.12>41]

count1.a <- cum[WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
count2.a <- cum[WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
NO.1.a <- cum.event[WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
NO.2.a <- cum.event[WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
N2O.1.a <- cum.event[WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
N2O.2.a <- cum.event[WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]
primming.NO.a <- NO.1.a/count1.a / (NO.2.a/count2.a)
primming.N2O.a <- N2O.1.a/count1.a / (N2O.2.a/count2.a)


count1.b <- cum[WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
count2.b <- cum[WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
NO.1.b <- cum.event[WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
NO.2.b <- cum.event[WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
N2O.1.b <- cum.event[WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
N2O.2.b <- cum.event[WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]
primming.NO.b <- NO.1.b/count1.b / (NO.2.b/count2.b)
primming.N2O.b <- N2O.1.b/count1.b / (N2O.2.b/count2.b)

### event 1 vs (event 2 & event 3) (b)
count1 <- cum[WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
count2 <- cum[WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
count3 <- cum[WFPS %in% U.12.b, sum(count.event3, na.rm=T)]

count23 <- count2 +count3

NO.1 <- cum.event[WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
NO.23 <- cum.event[WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]

N2O.1 <- cum.event[WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
N2O.23 <- cum.event[WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]


primming.NO.23.b <- NO.1/count1 / (NO.23/count23)
primming.N2O.23.b <- N2O.1/count1 / (N2O.23/count23)

####
################################################################################
# by tillage
folder_in <- paste0(output_path, "/WFPS/WFPS_vs_N")

myfile <- paste0(folder_in, "/meanflux_by_wfps__till.dat")
cum <- fread(myfile)

myfile <- paste0(folder_in, "/meanflux_by_wfps_event__till.dat")
cum.event <- fread(myfile)


### event 1 vs event 2
nt.count1 <- cum[tillage == "NT" & WFPS %in% U.12, sum(count.event1, na.rm=T)]
nt.count2 <- cum[tillage == "NT" & WFPS %in% U.12, sum(count.event2, na.rm=T)]
nt.NO.1 <- cum.event[tillage == "NT" & WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
nt.NO.2 <- cum.event[tillage == "NT" & WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]
nt.N2O.1 <- cum.event[tillage == "NT" & WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
nt.N2O.2 <- cum.event[tillage == "NT" & WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]

tt.count1 <- cum[tillage == "TT" & WFPS %in% U.12, sum(count.event1, na.rm=T)]
tt.count2 <- cum[tillage == "TT" & WFPS %in% U.12, sum(count.event2, na.rm=T)]
tt.NO.1 <- cum.event[tillage == "TT" & WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
tt.NO.2 <- cum.event[tillage == "TT" & WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]
tt.N2O.1 <- cum.event[tillage == "TT" & WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
tt.N2O.2 <- cum.event[tillage == "TT" & WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]


nt.primming.NO <- nt.NO.1/nt.count1 / (nt.NO.2/nt.count2)
nt.primming.N2O <- nt.N2O.1/nt.count1 / (nt.N2O.2/nt.count2)

tt.primming.NO <- tt.NO.1/tt.count1 / (tt.NO.2/tt.count2)
tt.primming.N2O <- tt.N2O.1/tt.count1 / (tt.N2O.2/tt.count2)

### event 1 vs (event 2 & event 3) (b)
nt.count1 <- cum[tillage == "NT" & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
nt.count2 <- cum[tillage == "NT" & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
nt.count3 <- cum[tillage == "NT" & WFPS %in% U.12.b, sum(count.event3, na.rm=T)]
nt.count23 <- nt.count2 + nt.count3
nt.NO.1 <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
nt.NO.23 <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]
nt.N2O.1 <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
nt.N2O.23 <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]

tt.count1 <- cum[tillage == "TT" & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
tt.count2 <- cum[tillage == "TT" & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
tt.count3 <- cum[tillage == "TT" & WFPS %in% U.12.b, sum(count.event3, na.rm=T)]
tt.count23 <- tt.count2 + tt.count3
tt.NO.1 <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
tt.NO.23 <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]
tt.N2O.1 <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
tt.N2O.23 <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]


nt.primming.NO.23.b <- nt.NO.1/nt.count1 / (nt.NO.23/nt.count23)
nt.primming.N2O.23.b <- nt.N2O.1/nt.count1 / (nt.N2O.23/nt.count23)
tt.primming.NO.23.b <- tt.NO.1/tt.count1 / (tt.NO.23/tt.count23)
tt.primming.N2O.23.b <- tt.N2O.1/tt.count1 / (tt.N2O.23/tt.count23)

## divide event 1 between its two major peaks
U.12.a <- U.12[U.12<41]
U.12.b <- U.12[U.12>41]

nt.count1.a <- cum[tillage == "NT" & WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
nt.count2.a <- cum[tillage == "NT" & WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
nt.NO.1.a <- cum.event[tillage == "NT" & WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
nt.NO.2.a <- cum.event[tillage == "NT" & WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
nt.N2O.1.a <- cum.event[tillage == "NT" & WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
nt.N2O.2.a <- cum.event[tillage == "NT" & WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]

tt.count1.a <- cum[tillage == "TT" & WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
tt.count2.a <- cum[tillage == "TT" & WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
tt.NO.1.a <- cum.event[tillage == "TT" & WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
tt.NO.2.a <- cum.event[tillage == "TT" & WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
tt.N2O.1.a <- cum.event[tillage == "TT" & WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
tt.N2O.2.a <- cum.event[tillage == "TT" & WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]


nt.primming.NO.a <- nt.NO.1.a/nt.count1.a / (nt.NO.2.a/nt.count2.a)
tt.primming.NO.a <- tt.NO.1.a/tt.count1.a / (tt.NO.2.a/tt.count2.a)
nt.primming.N2O.a <- nt.N2O.1.a/nt.count1.a / (nt.N2O.2.a/nt.count2.a)
tt.primming.N2O.a <- tt.N2O.1.a/tt.count1.a / (tt.N2O.2.a/tt.count2.a)
##

nt.count1.b <- cum[tillage == "NT" & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
nt.count2.b <- cum[tillage == "NT" & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
nt.NO.1.b <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
nt.NO.2.b <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
nt.N2O.1.b <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
nt.N2O.2.b <- cum.event[tillage == "NT" & WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]

tt.count1.b <- cum[tillage == "TT" & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
tt.count2.b <- cum[tillage == "TT" & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
tt.NO.1.b <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
tt.NO.2.b <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
tt.N2O.1.b <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
tt.N2O.2.b <- cum.event[tillage == "TT" & WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]

nt.primming.NO.b <- nt.NO.1.b/nt.count1.b / (nt.NO.2.b/nt.count2.b)
tt.primming.NO.b <- tt.NO.1.b/tt.count1.b / (tt.NO.2.b/tt.count2.b)
nt.primming.N2O.b <- nt.N2O.1.b/nt.count1.b / (nt.N2O.2.b/nt.count2.b)
tt.primming.N2O.b <- tt.N2O.1.b/tt.count1.b / (tt.N2O.2.b/tt.count2.b)


################################################################################
# by fert
folder_in <- paste0(output_path, "/WFPS/WFPS_vs_N")

myfile <- paste0(folder_in, "/meanflux_by_wfps__fert.dat")
cum <- fread(myfile)

myfile <- paste0(folder_in, "/meanflux_by_wfps_event__fert.dat")
cum.event <- fread(myfile)


### event 1 vs event 2
f.0.count1 <- cum[fertilization == 0 & WFPS %in% U.12, sum(count.event1, na.rm=T)]
f.0.count2 <- cum[fertilization == 0 & WFPS %in% U.12, sum(count.event2, na.rm=T)]
f.0.NO.1 <- cum.event[fertilization == 0 & WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
f.0.NO.2 <- cum.event[fertilization == 0 & WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]
f.0.N2O.1 <- cum.event[fertilization == 0 & WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
f.0.N2O.2 <- cum.event[fertilization == 0 & WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]

f.50.count1 <- cum[fertilization == 50 & WFPS %in% U.12, sum(count.event1, na.rm=T)]
f.50.count2 <- cum[fertilization == 50 & WFPS %in% U.12, sum(count.event2, na.rm=T)]
f.50.NO.1 <- cum.event[fertilization == 50 & WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
f.50.NO.2 <- cum.event[fertilization == 50 & WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]
f.50.N2O.1 <- cum.event[fertilization == 50 & WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
f.50.N2O.2 <- cum.event[fertilization == 50 & WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]

f.100.count1 <- cum[fertilization == 100 & WFPS %in% U.12, sum(count.event1, na.rm=T)]
f.100.count2 <- cum[fertilization == 100 & WFPS %in% U.12, sum(count.event2, na.rm=T)]
f.100.NO.1 <- cum.event[fertilization == 100 & WFPS %in% U.12 & event ==1, sum(NO, na.rm=T)]
f.100.NO.2 <- cum.event[fertilization == 100 & WFPS %in% U.12 & event ==2, sum(NO, na.rm=T)]
f.100.N2O.1 <- cum.event[fertilization == 100 & WFPS %in% U.12 & event ==1, sum(N2O, na.rm=T)]
f.100.N2O.2 <- cum.event[fertilization == 100 & WFPS %in% U.12 & event ==2, sum(N2O, na.rm=T)]


f.0.primming.NO <- f.0.NO.1/f.0.count1 / (f.0.NO.2/f.0.count2)
f.0.primming.N2O <- f.0.N2O.1/f.0.count1 / (f.0.N2O.2/f.0.count2)

f.50.primming.NO <- f.50.NO.1/f.50.count1 / (f.50.NO.2/f.50.count2)
f.50.primming.N2O <- f.50.N2O.1/f.50.count1 / (f.50.N2O.2/f.50.count2)

f.100.primming.NO <- f.100.NO.1/f.100.count1 / (f.100.NO.2/f.100.count2)
f.100.primming.N2O <- f.100.N2O.1/f.100.count1 / (f.100.N2O.2/f.100.count2)

### event 1 vs (event 2 & event 3) (b)
f.0.count1 <- cum[fertilization == 0 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.0.count2 <- cum[fertilization == 0 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.0.count3 <- cum[fertilization == 0 & WFPS %in% U.12.b, sum(count.event3, na.rm=T)]
f.0.count23 <- f.0.count2 + f.0.count3
f.0.NO.1 <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.0.NO.23 <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]
f.0.N2O.1 <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.0.N2O.23 <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]

f.50.count1 <- cum[fertilization == 50 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.50.count2 <- cum[fertilization == 50 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.50.count3 <- cum[fertilization == 50 & WFPS %in% U.12.b, sum(count.event3, na.rm=T)]
f.50.count23 <- f.50.count2 + f.50.count3
f.50.NO.1 <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.50.NO.23 <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]
f.50.N2O.1 <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.50.N2O.23 <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]

f.100.count1 <- cum[fertilization == 100 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.100.count2 <- cum[fertilization == 100 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.100.count3 <- cum[fertilization == 100 & WFPS %in% U.12.b, sum(count.event3, na.rm=T)]
f.100.count23 <- f.100.count2 + f.100.count3
f.100.NO.1 <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.100.NO.23 <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event %in% c(2,3), sum(NO, na.rm=T)]
f.100.N2O.1 <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.100.N2O.23 <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event %in% c(2,3), sum(N2O, na.rm=T)]


f.0.primming.NO.23.b <- f.0.NO.1/f.0.count1 / (f.0.NO.23/f.0.count23)
f.0.primming.N2O.23.b <- f.0.N2O.1/f.0.count1 / (f.0.N2O.23/f.0.count23)
f.50.primming.NO.23.b <- f.50.NO.1/f.50.count1 / (f.50.NO.23/f.50.count23)
f.50.primming.N2O.23.b <- f.50.N2O.1/f.50.count1 / (f.50.N2O.23/f.50.count23)
f.100.primming.NO.23.b <- f.100.NO.1/f.100.count1 / (f.100.NO.23/f.100.count23)
f.100.primming.N2O.23.b <- f.100.N2O.1/f.100.count1 / (f.100.N2O.23/f.100.count23)

## divide event 1 between its two major peaks
U.12.a <- U.12[U.12<41]
U.12.b <- U.12[U.12>41]

f.0.count1.a <- cum[fertilization == 0 & WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
f.0.count2.a <- cum[fertilization == 0 & WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
f.0.NO.1.a <- cum.event[fertilization == 0 & WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
f.0.NO.2.a <- cum.event[fertilization == 0 & WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
f.0.N2O.1.a <- cum.event[fertilization == 0 & WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
f.0.N2O.2.a <- cum.event[fertilization == 0 & WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]

f.50.count1.a <- cum[fertilization == 50 & WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
f.50.count2.a <- cum[fertilization == 50 & WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
f.50.NO.1.a <- cum.event[fertilization == 50 & WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
f.50.NO.2.a <- cum.event[fertilization == 50 & WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
f.50.N2O.1.a <- cum.event[fertilization == 50 & WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
f.50.N2O.2.a <- cum.event[fertilization == 50 & WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]

f.100.count1.a <- cum[fertilization == 100 & WFPS %in% U.12.a, sum(count.event1, na.rm=T)]
f.100.count2.a <- cum[fertilization == 100 & WFPS %in% U.12.a, sum(count.event2, na.rm=T)]
f.100.NO.1.a <- cum.event[fertilization == 100 & WFPS %in% U.12.a & event ==1, sum(NO, na.rm=T)]
f.100.NO.2.a <- cum.event[fertilization == 100 & WFPS %in% U.12.a & event ==2, sum(NO, na.rm=T)]
f.100.N2O.1.a <- cum.event[fertilization == 100 & WFPS %in% U.12.a & event ==1, sum(N2O, na.rm=T)]
f.100.N2O.2.a <- cum.event[fertilization == 100 & WFPS %in% U.12.a & event ==2, sum(N2O, na.rm=T)]


f.0.primming.NO.a <- f.0.NO.1.a/f.0.count1.a / (f.0.NO.2.a/f.0.count2.a)
f.50.primming.NO.a <- f.50.NO.1.a/f.50.count1.a / (f.50.NO.2.a/f.50.count2.a)
f.100.primming.NO.a <- f.100.NO.1.a/f.100.count1.a / (f.100.NO.2.a/f.100.count2.a)
f.0.primming.N2O.a <- f.0.N2O.1.a/f.0.count1.a / (f.0.N2O.2.a/f.0.count2.a)
f.50.primming.N2O.a <- f.50.N2O.1.a/f.50.count1.a / (f.50.N2O.2.a/f.50.count2.a)
f.100.primming.N2O.a <- f.100.N2O.1.a/f.100.count1.a / (f.100.N2O.2.a/f.100.count2.a)
##

f.0.count1.b <- cum[fertilization == 0 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.0.count2.b <- cum[fertilization == 0 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.0.NO.1.b <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.0.NO.2.b <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
f.0.N2O.1.b <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.0.N2O.2.b <- cum.event[fertilization == 0 & WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]

f.50.count1.b <- cum[fertilization == 50 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.50.count2.b <- cum[fertilization == 50 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.50.NO.1.b <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.50.NO.2.b <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
f.50.N2O.1.b <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.50.N2O.2.b <- cum.event[fertilization == 50 & WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]

f.100.count1.b <- cum[fertilization == 100 & WFPS %in% U.12.b, sum(count.event1, na.rm=T)]
f.100.count2.b <- cum[fertilization == 100 & WFPS %in% U.12.b, sum(count.event2, na.rm=T)]
f.100.NO.1.b <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==1, sum(NO, na.rm=T)]
f.100.NO.2.b <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==2, sum(NO, na.rm=T)]
f.100.N2O.1.b <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==1, sum(N2O, na.rm=T)]
f.100.N2O.2.b <- cum.event[fertilization == 100 & WFPS %in% U.12.b & event ==2, sum(N2O, na.rm=T)]

f.0.primming.NO.b <- f.0.NO.1.b/f.0.count1.b / (f.0.NO.2.b/f.0.count2.b)
f.50.primming.NO.b <- f.50.NO.1.b/f.50.count1.b / (f.50.NO.2.b/f.50.count2.b)
f.100.primming.NO.b <- f.100.NO.1.b/f.100.count1.b / (f.100.NO.2.b/f.100.count2.b)
f.0.primming.N2O.b <- f.0.N2O.1.b/f.0.count1.b / (f.0.N2O.2.b/f.0.count2.b)
f.50.primming.N2O.b <- f.50.N2O.1.b/f.50.count1.b / (f.50.N2O.2.b/f.50.count2.b)
f.100.primming.N2O.b <- f.100.N2O.1.b/f.100.count1.b / (f.100.N2O.2.b/f.100.count2.b)

################################################################################
NO <- data.table(
        species = "NO",
        situation = c("all",
                      "NT", "TT",
                      "0", "50", "100"),
        
        prim.1vs2 = c(primming.NO,
                      nt.primming.NO, tt.primming.NO,
                      f.0.primming.NO, f.50.primming.NO, f.100.primming.NO),
        
        prim.1vs2.a = c(primming.NO.a,
                      nt.primming.NO.a, tt.primming.NO.a,
                      f.0.primming.NO.a, f.50.primming.NO.a, f.100.primming.NO.a),
        
        prim.1vs2.b = c(primming.NO.b,
                        nt.primming.NO.b, tt.primming.NO.b,
                        f.0.primming.NO.b, f.50.primming.NO.b, f.100.primming.NO.b),
        
        prim.1vs23.b = c(primming.NO.23.b,
                        nt.primming.NO.23.b, tt.primming.NO.23.b,
                        f.0.primming.NO.23.b, f.50.primming.NO.23.b, f.100.primming.NO.23.b)
        
)

N2O <- data.table(
        species = "N2O",
        situation = c("all",
                      "NT", "TT",
                      "0", "50", "100"),
        
        prim.1vs2 = c(primming.N2O,
                      nt.primming.N2O, tt.primming.N2O,
                      f.0.primming.N2O, f.50.primming.N2O, f.100.primming.N2O),
        
        prim.1vs2.a = c(primming.N2O.a,
                        nt.primming.N2O.a, tt.primming.N2O.a,
                        f.0.primming.N2O.a, f.50.primming.N2O.a, f.100.primming.N2O.a),
        
        prim.1vs2.b = c(primming.N2O.b,
                        nt.primming.N2O.b, tt.primming.N2O.b,
                        f.0.primming.N2O.b, f.50.primming.N2O.b, f.100.primming.N2O.b),
        
        prim.1vs23.b = c(primming.N2O.23.b,
                         nt.primming.N2O.23.b, tt.primming.N2O.23.b,
                         f.0.primming.N2O.23.b, f.50.primming.N2O.23.b, f.100.primming.N2O.23.b)
        
)

primming <- rbindlist(list(NO, N2O))

# write file
mydata <- copy (primming)
no.format <- c("species", "situation")
to.format2 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
myfile <- paste0(folderout, "/primming_effect.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

