require(multcomp)
require(car)
require(HH)
require(gvlma)
require(bootstrap)
require(MASS)
require(leaps)
require(coin)
require(lmPerm)
require(rmarkdown)
library(npar)
source("_ANOVA_priming_1_123/W.MW.oneway.R")

# define output directory
my.output.dir <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/_ANOVA/priming/priming_1_123"
dir.create(my.output.dir)


# load data
folder.in <- paste0(output_path, "/priming/summaries.1.123.ratio")
myfile <- paste0(folder.in, "/priming_by_CORE_1_123.dat")
RAW.data <- fread (myfile)


data <- RAW.data[,.(
        incubation = factor(incubation),
        fertilization = factor(fertilization),
        precipitation = factor(precipitation),
        tillage = factor(tillage),
        
        NO.1.123 = NO.1.123,
        N2O.1.123 = N2O.1.123,
        NO.N2O.1.123 = NO.N2O.1.123,
        NO.N2O.NH3.1.123 = NO.N2O.NH3.1.123,
        CO2.1.123 = CO2.1.123
        
)]
data[,id:= paste0(incubation, fertilization, precipitation, tillage)]

# export to html
my.files <- list.files("_ANOVA_priming_1_123", pattern = ".Rmd", full.names = TRUE)
my.files
render(input = my.files[8], output_format = "html_document", output_dir = my.output.dir)


for(i in my.files){
        render(input = i, output_format = "html_document", output_dir = my.output.dir)
}
