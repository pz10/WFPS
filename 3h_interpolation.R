folder_out <- paste0(output_path, "/presentation_plots/WFPS_vs_N/WFPS_vs_cum_N")
dir.create(folder_out)

## combine flux files
#
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
treatment.file <- paste0(input_path, "/_all_fluxes/treatments.dat")

Aflux <- fread(input = A.flux.file)
Bflux <- fread(input = B.flux.file) 
Cflux <- fread(input = C.flux.file) 
Dflux <- fread(input = D.flux.file)

Aflux[, incubation:= "A"]
Bflux[, incubation:= "B"]
Cflux[, incubation:= "C"]
Dflux[, incubation:= "D"]
treatments <- fread(input = treatment.file)

ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
ABCDfluxes <- ABCDfluxes[,.(days.adjusted = days.adj.full, incubation, treatment, fertilizer, precipitation, tillage,
                            NO, N2O, CO2, CH4, WFPS, WFPSg, WFPS_0, WFPS_f)]
setkey(ABCDfluxes, treatment, incubation, days.adjusted)
ABCDfluxes[NO < 0 , NO:=0]
ABCDfluxes[N2O < 0 , N2O:=0]
ABCDfluxes[CO2 < 0 , CO2:=0]

## fill gaps at event edges to perform a proppper interpolation
source("fill_gaps.R")

## create interpolated and gap-filled flux file including WFPS_0
s0 <- seq(from = -3, to = -1, by = 1/8)
s1 <- seq(from = 1/8, to = 14, by = 1/8)
s2 <- seq(from = 15 + 1/8, to = 15 + 14, by = 1/8)
s3 <- seq(from = 30 + 1/8, to = 30 + 14, by = 1/8)
days <- c(s0,s1, s2, s3)
treat <- unique(ABCDfluxes$treatment)

data <- data.table(
        days.adjusted = rep(days, times = 18*4),
        incubation = rep(LETTERS[1:4], each=length(days)*18 ),
        treatment = rep(treat, each=length(days), times = 4)
)
# table(data[,.(incubation, treatment)])

source("3h_interpol.R")
# source("check_3h_interpol.R")

# source("WFPS_vs_N.R")
source("WFPS_N_fluxes.R")
