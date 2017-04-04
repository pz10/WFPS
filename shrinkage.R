# folder out
folderout.shrink <- paste0(output_path, "/shrink")
dir.create(folderout.shrink, recursive=T)

# file in (balanced 3h interpolated values)

myfolder <- paste0(input_path_RAW, "/_extra")
data <- paste0(myfolder, "/rain_events_time.dat")


# read files
data <- fread(input = data)
data[, shrink:= volume_dif/vol_0 *100]
setkey(data, incubation, tillage, precipitation, fertilization)

s.till <- data[,.(
        vol_0 = mean(vol_0, na.rm=T),
        sd_vol_0 = sd(vol_0, na.rm=T),
        se_vol_0 = sd(vol_0, na.rm=T)/ sqrt(sum(!is.na(vol_0))),
        
        volume_dif = mean(volume_dif, na.rm=T),
        sd_volume_dif = sd(volume_dif, na.rm=T),
        se_volume_dif = sd(volume_dif, na.rm=T)/ sqrt(sum(!is.na(volume_dif))),
        
        shrink = mean(shrink, na.rm=T),
        sd_shrink = sd(shrink, na.rm=T),
        se_shrink = sd(shrink, na.rm=T)/ sqrt(sum(!is.na(shrink)))
        
), by=.(tillage)]

s.rain <- data[,.(
        vol_0 = mean(vol_0, na.rm=T),
        sd_vol_0 = sd(vol_0, na.rm=T),
        se_vol_0 = sd(vol_0, na.rm=T)/ sqrt(sum(!is.na(vol_0))),
        
        volume_dif = mean(volume_dif, na.rm=T),
        sd_volume_dif = sd(volume_dif, na.rm=T),
        se_volume_dif = sd(volume_dif, na.rm=T)/ sqrt(sum(!is.na(volume_dif))),
        
        shrink = mean(shrink, na.rm=T),
        sd_shrink = sd(shrink, na.rm=T),
        se_shrink = sd(shrink, na.rm=T)/ sqrt(sum(!is.na(shrink)))
        
), by=.(precipitation)]

s.till.rain <- data[,.(
        vol_0 = mean(vol_0, na.rm=T),
        sd_vol_0 = sd(vol_0, na.rm=T),
        se_vol_0 = sd(vol_0, na.rm=T)/ sqrt(sum(!is.na(vol_0))),
        
        volume_dif = mean(volume_dif, na.rm=T),
        sd_volume_dif = sd(volume_dif, na.rm=T),
        se_volume_dif = sd(volume_dif, na.rm=T)/ sqrt(sum(!is.na(volume_dif))),
        
        shrink = mean(shrink, na.rm=T),
        sd_shrink = sd(shrink, na.rm=T),
        se_shrink = sd(shrink, na.rm=T)/ sqrt(sum(!is.na(shrink)))
        
), by=.(tillage, precipitation)]


s.core <- data[,.(
        vol_0 = vol_0,
        volume_dif = volume_dif,
        shrink = shrink

), by=.(incubation, fertilization, precipitation, tillage)]


summary(data$shrink)

# write summary files
# tillage
################################################################################
mydata <- copy (s.till)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.shrink, "/shrink_by_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


# rain
################################################################################
mydata <- copy (s.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.shrink, "/shrink_by_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# tillage, rain
################################################################################
mydata <- copy (s.till.rain)
no.format <- c("precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.shrink, "/shrink_by_till_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# core
################################################################################
mydata <- copy (s.core)
no.format <- c("incubation","precipitation", "tillage", "fertilization", "treatment")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folderout.shrink, "/shrink_by_core.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

