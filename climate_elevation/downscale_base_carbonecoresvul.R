############################################################################################################ 
############################################################################################################
# This is the worst code I have ever written.
# However, it works, it's reliable and easy to use.
############################################################################################################
############################################################################################################

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

###########################
###########################
###########################
# data_base <- read.table("data_base.csv", header = TRUE, dec = ".", sep = ";")

data_base <- read.table("data_base_str.csv",
  header = TRUE, dec = ".", sep = ";")

data_base <- data_base %>%
  rename(
    lon = long)

####
#define extent

extent1 = extent( min(data_base$lon)-1, max(data_base$lon)+1, min(data_base$lat)-1, max(data_base$lat)+1)

###########################
###########################
###########################

#CRU 1970-2000

nc.pre1961_1970  <- nc_open("cru_ts4.04.1961.1970.pre.dat.nc")
nc.pre1971_1980  <- nc_open("cru_ts4.04.1971.1980.pre.dat.nc")
nc.pre1981_1990  <- nc_open("cru_ts4.04.1981.1990.pre.dat.nc")
nc.pre1991_2000  <- nc_open("cru_ts4.04.1991.2000.pre.dat.nc")

pre1961_1970w <- brick("cru_ts4.04.1961.1970.pre.dat.nc", varname="pre")
pre1971_1980w<- brick("cru_ts4.04.1971.1980.pre.dat.nc", varname="pre")
pre1981_1990w  <- brick("cru_ts4.04.1981.1990.pre.dat.nc", varname="pre")
pre1991_2000w <- brick("cru_ts4.04.1991.2000.pre.dat.nc", varname="pre")

br.area <- extent1

pre1961_1970 <- crop(pre1961_1970w, br.area)
pre1971_1980 <- crop(pre1971_1980w, br.area)
pre1981_1990 <- crop(pre1981_1990w, br.area)
pre1991_2000 <- crop(pre1991_2000w, br.area)
plot(pre1961_1970$X1961.01.16)

raster_to_crop=pre1991_2000$X1991.05.16 #random
plot(raster_to_crop)

#testing
summary(values(pre1991_2000$X1991.03.16))
summary(values(pre1991_2000[[3]]))
#######

values1961_1970=as.data.frame(values(pre1961_1970))
head(values1961_1970)
summary(values1961_1970)
dim(values1961_1970)

values1971_1980=as.data.frame(values(pre1971_1980))
head(values1971_1980)
summary(values1971_1980)
dim(values1971_1980)

values1981_1990=as.data.frame(values(pre1981_1990))
head(values1981_1990)
summary(values1981_1990)
dim(values1981_1990)

values1991_2000 =as.data.frame(values(pre1991_2000 ))
head(values1991_2000 )
summary(values1991_2000)
dim(values1991_2000)

values1970=as.data.frame(values1961_1970) %>% select(contains("1970"))
head(values1970)

df_all_years_CRU=cbind.data.frame(values1970,values1971_1980,values1981_1990,values1991_2000)
head(df_all_years_CRU)
dim(df_all_years_CRU)

df_all_years_CRU1=t(df_all_years_CRU)
dim(df_all_years_CRU1)
df_all_years_CRU1[1:3,1:3]

years <- 1970:2000
monthindex <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
nyears=-(1970-2000)+1

month_col <- rep(monthindex, times=nyears)

df_all_years_CRU1df <- tibble::rownames_to_column(as.data.frame(df_all_years_CRU1), "date_raw")
dim(df_all_years_CRU1df)
df_all_years_CRU1df[1:3,1:2]
head(df_all_years_CRU1df)

df_all_years_CRU1df$month=month_col

df_all_years_CRU1df= df_all_years_CRU1df %>%
  relocate(month, .after = date_raw)

df_all_years_CRU1df[1:3,1:2]

df_all_years_CRU1df[1:3,1:4]


df_monthlymean=  df_all_years_CRU1df[,-1]%>% group_by(month) %>% summarise_all(list(mean))
df_monthlymean

#############

#valuesjan=t(as.vector(as.data.frame(df_monthlymean[df_monthlymean$month=="Jan",-1])))
#valuesjan=unname(unlist(df_monthlymean[df_monthlymean$month=="Jan",-1]))

df_monthlymean_1=apply(df_monthlymean[,-1], 1,unlist, unname)
colnames(df_monthlymean_1)=df_monthlymean$month

df_monthlymean_f=as.data.frame(df_monthlymean_1)

listraster=c()
for (i in 1:ncol(df_monthlymean_f)) {
  
  listraster[[i]] <-  setValues(raster_to_crop,df_monthlymean_f[,i])
}

listraster

plot(listraster[[6]])

names(listraster) <- colnames(df_monthlymean_f)

listraster

plot(listraster[["Jan"]])
plot(listraster[["Jul"]])

cru1970_2000_stack = stack(listraster)

writeRaster(cru1970_2000_stack, "cru1970_2000_prec_stack" , format= "GTiff",overwrite=TRUE)

fnames= paste("CRU1970_2000_prec",names(cru1970_2000_stack),sep="_")
writeRaster(cru1970_2000_stack,filename=fnames,format="GTiff",overwrite=TRUE,bylayer=T)

#########################################
# list variables
ti <- dir(pattern = ".tif")
ti

CRU70_20 <- grep("CRU", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_prec_Apr)


#######################################
#######################################
#######################################

samples=subset(data_base,select=c(lon,lat))

pre.sites_cru1970_2000_monthly <- data.frame(raster::extract(cru1970_2000_stack, samples))
pre.sites_cru1970_2000_monthly

#CRU 1970-2000

nc.tmp1961_1970  <- nc_open("cru_ts4.04.1961.1970.tmp.dat.nc")
nc.tmp1971_1980  <- nc_open("cru_ts4.04.1971.1980.tmp.dat.nc")
nc.tmp1981_1990  <- nc_open("cru_ts4.04.1981.1990.tmp.dat.nc")
nc.tmp1991_2000  <- nc_open("cru_ts4.04.1991.2000.tmp.dat.nc")

tmp1961_1970w <- brick("cru_ts4.04.1961.1970.tmp.dat.nc", varname="tmp")
tmp1971_1980w<- brick("cru_ts4.04.1971.1980.tmp.dat.nc", varname="tmp")
tmp1981_1990w  <- brick("cru_ts4.04.1981.1990.tmp.dat.nc", varname="tmp")
tmp1991_2000w <- brick("cru_ts4.04.1991.2000.tmp.dat.nc", varname="tmp")


br.area <- extent1

tmp1961_1970 <- crop(tmp1961_1970w, br.area)
tmp1971_1980 <-  crop(tmp1971_1980w, br.area)
tmp1981_1990  <-  crop(tmp1981_1990w, br.area)
tmp1991_2000 <-  crop(tmp1991_2000w, br.area)
plot(tmp1961_1970$X1961.01.16)

raster_to_crop=tmp1991_2000$X1991.05.16 #random
plot(raster_to_crop)

#testing
summary(values(tmp1991_2000$X1991.03.16))
summary(values(tmp1991_2000[[3]]))
#######

values1961_1970=as.data.frame(values(tmp1961_1970))
head(values1961_1970)
summary(values1961_1970)
dim(values1961_1970)

values1971_1980=as.data.frame(values(tmp1971_1980))
head(values1971_1980)
summary(values1971_1980)
dim(values1971_1980)

values1981_1990=as.data.frame(values(tmp1981_1990))
head(values1981_1990)
summary(values1981_1990)
dim(values1981_1990)

values1991_2000 =as.data.frame(values(tmp1991_2000 ))
head(values1991_2000 )
summary(values1991_2000)
dim(values1991_2000)

values1970=as.data.frame(values1961_1970) %>% select(contains("1970"))
head(values1970)

df_all_years_CRU=cbind.data.frame(values1970,values1971_1980,values1981_1990,values1991_2000)
head(df_all_years_CRU)
dim(df_all_years_CRU)

df_all_years_CRU1=t(df_all_years_CRU)
dim(df_all_years_CRU1)
df_all_years_CRU1[1:3,1:3]

years <- 1970:2000
monthindex <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
nyears=-(1970-2000)+1


month_col <- rep(monthindex, times=nyears)

df_all_years_CRU1df <- tibble::rownames_to_column(as.data.frame(df_all_years_CRU1), "date_raw")
dim(df_all_years_CRU1df)
df_all_years_CRU1df[1:3,1:2]
head(df_all_years_CRU1df)

df_all_years_CRU1df$month=month_col

df_all_years_CRU1df= df_all_years_CRU1df %>%
  relocate(month, .after = date_raw)

df_all_years_CRU1df[1:3,1:2]

df_all_years_CRU1df[1:3,1:4]


df_monthlymean=  df_all_years_CRU1df[,-1]%>% group_by(month) %>% summarise_all(list(mean))
df_monthlymean


#############
#valuesjan=t(as.vector(as.data.frame(df_monthlymean[df_monthlymean$month=="Jan",-1])))
#valuesjan=unname(unlist(df_monthlymean[df_monthlymean$month=="Jan",-1]))

df_monthlymean_1=apply(df_monthlymean[,-1], 1,unlist, unname)
colnames(df_monthlymean_1)=df_monthlymean$month

df_monthlymean_f=as.data.frame(df_monthlymean_1)

listraster=c()
for (i in 1:ncol(df_monthlymean_f)) {
  
  listraster[[i]] <-  setValues(raster_to_crop,df_monthlymean_f[,i])
}

listraster

plot(listraster[[6]])

names(listraster) <- colnames(df_monthlymean_f)

listraster

plot(listraster[["Jan"]])
plot(listraster[["Jul"]])

cru1970_2000_stack = stack(listraster)

writeRaster(cru1970_2000_stack, "cru1970_2000_temp_stack" , format= "GTiff",overwrite=TRUE)

fnames= paste("CRU1970_2000_temp",names(cru1970_2000_stack),sep="_")
writeRaster(cru1970_2000_stack,filename=fnames,format="GTiff",overwrite=TRUE,bylayer=T)

#########################################
# list variables
ti <- dir(pattern = ".tif")
ti

CRU70_20 <- grep("CRU", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_prec_Apr)

#######################

#CRU 1970-2000

nc.tmx1961_1970  <- nc_open("cru_ts4.04.1961.1970.tmx.dat.nc")
nc.tmx1971_1980  <- nc_open("cru_ts4.04.1971.1980.tmx.dat.nc")
nc.tmx1981_1990  <- nc_open("cru_ts4.04.1981.1990.tmp.dat.nc")
nc.tmx1991_2000  <- nc_open("cru_ts4.04.1991.2000.tmp.dat.nc")

tmx1961_1970w <- brick("cru_ts4.04.1961.1970.tmx.dat.nc", varname="tmx")
tmx1971_1980w<- brick("cru_ts4.04.1971.1980.tmx.dat.nc", varname="tmx")
tmx1981_1990w  <- brick("cru_ts4.04.1981.1990.tmx.dat.nc", varname="tmx")
tmx1991_2000w <- brick("cru_ts4.04.1991.2000.tmx.dat.nc", varname="tmx")


br.area <- extent1

tmx1961_1970 <- crop(tmx1961_1970w, br.area)
tmx1971_1980 <-  crop(tmx1971_1980w, br.area)
tmx1981_1990  <-  crop(tmx1981_1990w, br.area)
tmx1991_2000 <-  crop(tmx1991_2000w, br.area)
plot(tmx1961_1970$X1961.01.16)

raster_to_crop=tmx1991_2000$X1991.05.16 #random
plot(raster_to_crop)

#testing
summary(values(tmx1991_2000$X1991.03.16))
summary(values(tmx1991_2000[[3]]))
#######

values1961_1970=as.data.frame(values(tmx1961_1970))
head(values1961_1970)
summary(values1961_1970)
dim(values1961_1970)

values1971_1980=as.data.frame(values(tmx1971_1980))
head(values1971_1980)
summary(values1971_1980)
dim(values1971_1980)

values1981_1990=as.data.frame(values(tmx1981_1990))
head(values1981_1990)
summary(values1981_1990)
dim(values1981_1990)

values1991_2000 =as.data.frame(values(tmx1991_2000 ))
head(values1991_2000 )
summary(values1991_2000)
dim(values1991_2000)

values1970=as.data.frame(values1961_1970) %>% select(contains("1970"))
head(values1970)

df_all_years_CRU=cbind.data.frame(values1970,values1971_1980,values1981_1990,values1991_2000)
head(df_all_years_CRU)
dim(df_all_years_CRU)

df_all_years_CRU1=t(df_all_years_CRU)
dim(df_all_years_CRU1)
df_all_years_CRU1[1:3,1:3]

years <- 1970:2000
monthindex <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
nyears=-(1970-2000)+1


month_col <- rep(monthindex, times=nyears)

df_all_years_CRU1df <- tibble::rownames_to_column(as.data.frame(df_all_years_CRU1), "date_raw")
dim(df_all_years_CRU1df)
df_all_years_CRU1df[1:3,1:2]
head(df_all_years_CRU1df)

df_all_years_CRU1df$month=month_col

df_all_years_CRU1df= df_all_years_CRU1df %>%
  relocate(month, .after = date_raw)

df_all_years_CRU1df[1:3,1:2]

df_all_years_CRU1df[1:3,1:4]


df_monthlymean=  df_all_years_CRU1df[,-1]%>% group_by(month) %>% summarise_all(list(mean))
df_monthlymean


#############
#valuesjan=t(as.vector(as.data.frame(df_monthlymean[df_monthlymean$month=="Jan",-1])))
#valuesjan=unname(unlist(df_monthlymean[df_monthlymean$month=="Jan",-1]))

df_monthlymean_1=apply(df_monthlymean[,-1], 1,unlist, unname)
colnames(df_monthlymean_1)=df_monthlymean$month

df_monthlymean_f=as.data.frame(df_monthlymean_1)

listraster=c()
for (i in 1:ncol(df_monthlymean_f)) {
  
  listraster[[i]] <-  setValues(raster_to_crop,df_monthlymean_f[,i])
}

listraster

plot(listraster[[6]])

names(listraster) <- colnames(df_monthlymean_f)

listraster

plot(listraster[["Jan"]])
plot(listraster[["Jul"]])

cru1970_2000_stack = stack(listraster)

writeRaster(cru1970_2000_stack, "cru1970_2000_maxtemp_stack" , format= "GTiff",overwrite=TRUE)

fnames= paste("CRU1970_2000_maxtemp",names(cru1970_2000_stack),sep="_")
writeRaster(cru1970_2000_stack,filename=fnames,format="GTiff",overwrite=TRUE,bylayer=T)

#########################################
# list variables
ti <- dir(pattern = ".tif")
ti


CRU70_20 <- grep("CRU", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_prec_Apr)


################################################
################################################
################################################
################################################
################################################
############
############



############################################################
###################anomalies##############################
############################################################

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

#########raster CRU1970-2000
CRU70_20 <- grep("CRU1970_2000_prec", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_prec_Jul)

############################
#Climate (prec) CRU 1981-2019

# Load the CRU TS precipitation dataset into R 
pre8190 <- brick("cru_ts4.04.1981.1990.pre.dat.nc", varname="pre")
pre9100 <- brick("cru_ts4.04.1991.2000.pre.dat.nc", varname="pre")
pre0110 <- brick("cru_ts4.04.2001.2010.pre.dat.nc", varname="pre")
pre1119 <- brick("cru_ts4.04.2011.2019.pre.dat.nc", varname="pre")

plot(pre0110$X2007.01.16)#janeiro 2007

pre=stack(pre8190,pre9100,pre0110, pre1119 )
plot(pre$X2007.01.16)

#same extent
br.area <- extent1
precru1981_2019 <- crop(pre, br.area)

plot(precru1981_2019$X2007.01.16)#janeiro 2007

#anomalies 
CRU1970_2000
precru1981_2019

#group precru1981_2019 by month

###############################
###############################
###############################
month1 <- raster::subset(precru1981_2019,1) # i + 12
month1
##########################
##########################
##########################

#### JANUARY
anomalies_cru_jan=list()


for (i in seq(from=1, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_jan[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Jan
}

anomalies_cru_jan_r = anomalies_cru_jan[-which(sapply(anomalies_cru_jan, is.null))]
anomalies_cru_jan_r

names(anomalies_cru_jan_r)=c(paste("jan", 1981:2019,sep="_"))
anomalies_cru_jan_r

anomalies_cru_jan_r_stack = stack(anomalies_cru_jan_r)


#### FEBRUARY
anomalies_cru_feb=list()


for (i in seq(from=2, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_feb[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Feb
}

anomalies_cru_feb_r = anomalies_cru_feb[-which(sapply(anomalies_cru_feb, is.null))]
anomalies_cru_feb_r

names(anomalies_cru_feb_r)=c(paste("feb", 1981:2019,sep="_"))
anomalies_cru_feb_r

anomalies_cru_feb_r_stack = stack(anomalies_cru_feb_r)


#### MARCH
anomalies_cru_mar=list()


for (i in seq(from=3, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_mar[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Mar
}

anomalies_cru_mar_r = anomalies_cru_mar[-which(sapply(anomalies_cru_mar, is.null))]
anomalies_cru_mar_r

names(anomalies_cru_mar_r)=c(paste("mar", 1981:2019,sep="_"))
anomalies_cru_mar_r

anomalies_cru_mar_r_stack = stack(anomalies_cru_mar_r)

#### APRIL
anomalies_cru_apr=list()


for (i in seq(from=4, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_apr[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Apr
}

anomalies_cru_apr_r = anomalies_cru_apr[-which(sapply(anomalies_cru_apr, is.null))]
anomalies_cru_apr_r

names(anomalies_cru_apr_r)=c(paste("apr", 1981:2019,sep="_"))
anomalies_cru_apr_r

anomalies_cru_apr_r_stack = stack(anomalies_cru_apr_r)


#### MAY
anomalies_cru_may=list()


for (i in seq(from=5, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_may[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_May
}

anomalies_cru_may_r = anomalies_cru_may[-which(sapply(anomalies_cru_may, is.null))]
anomalies_cru_may_r

names(anomalies_cru_may_r)=c(paste("may", 1981:2019,sep="_"))
anomalies_cru_may_r

anomalies_cru_may_r_stack = stack(anomalies_cru_may_r)


#### JUNE
anomalies_cru_jun=list()


for (i in seq(from=6, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_jun[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Jun
}

anomalies_cru_jun_r = anomalies_cru_jun[-which(sapply(anomalies_cru_jun, is.null))]
anomalies_cru_jun_r

names(anomalies_cru_jun_r)=c(paste("jun", 1981:2019,sep="_"))
anomalies_cru_jun_r

anomalies_cru_jun_r_stack = stack(anomalies_cru_jun_r)


#### JULY
anomalies_cru_jul=list()


for (i in seq(from=7, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_jul[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Jul
}

anomalies_cru_jul_r = anomalies_cru_jul[-which(sapply(anomalies_cru_jul, is.null))]
anomalies_cru_jul_r

names(anomalies_cru_jul_r)=c(paste("jul", 1981:2019,sep="_"))
anomalies_cru_jul_r

anomalies_cru_jul_r_stack = stack(anomalies_cru_jul_r)


#### AUGUST
anomalies_cru_aug=list()


for (i in seq(from=8, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_aug[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Aug
}

anomalies_cru_aug_r = anomalies_cru_aug[-which(sapply(anomalies_cru_aug, is.null))]
anomalies_cru_aug_r

names(anomalies_cru_aug_r)=c(paste("aug", 1981:2019,sep="_"))
anomalies_cru_aug_r

anomalies_cru_aug_r_stack = stack(anomalies_cru_aug_r)


#### SEPTEMBER
anomalies_cru_sep=list()


for (i in seq(from=9, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_sep[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Sep
}

anomalies_cru_sep_r = anomalies_cru_sep[-which(sapply(anomalies_cru_sep, is.null))]
anomalies_cru_sep_r

names(anomalies_cru_sep_r)=c(paste("sep", 1981:2019,sep="_"))
anomalies_cru_sep_r

anomalies_cru_sep_r_stack = stack(anomalies_cru_sep_r)


#### OCTOBER
anomalies_cru_oct=list()


for (i in seq(from=10, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_oct[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Oct
}

anomalies_cru_oct_r = anomalies_cru_oct[-which(sapply(anomalies_cru_oct, is.null))]
anomalies_cru_oct_r

names(anomalies_cru_oct_r)=c(paste("oct", 1981:2019,sep="_"))
anomalies_cru_oct_r

anomalies_cru_oct_r_stack = stack(anomalies_cru_oct_r)


#### NOVEMBER
anomalies_cru_nov=list()


for (i in seq(from=11, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_nov[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Nov
}

anomalies_cru_nov_r = anomalies_cru_nov[-which(sapply(anomalies_cru_nov, is.null))]
anomalies_cru_nov_r

names(anomalies_cru_nov_r)=c(paste("nov", 1981:2019,sep="_"))
anomalies_cru_nov_r

anomalies_cru_nov_r_stack = stack(anomalies_cru_nov_r)


#### DECEMBER
anomalies_cru_dec=list()


for (i in seq(from=12, to=nlayers(precru1981_2019), by=12)) {
  anomalies_cru_dec[[i]]= raster::subset(precru1981_2019,i)/CRU1970_2000$CRU1970_2000_prec_Dec
}

anomalies_cru_dec_r = anomalies_cru_dec[-which(sapply(anomalies_cru_dec, is.null))]
anomalies_cru_dec_r

names(anomalies_cru_dec_r)=c(paste("dec", 1981:2019,sep="_"))
anomalies_cru_dec_r

anomalies_cru_dec_r_stack = stack(anomalies_cru_dec_r)

#################################################################
#################################################################
#################################################################

###############interpolation

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

raster_interpolation=raster("wc2.1_30s_tavg_01.tif")
raster_interpolation

br.area <-  extent1
raster_interpolation_extent <- crop(raster_interpolation, br.area)
plot(raster_interpolation_extent)



all_months_years_res05=stack(anomalies_cru_jan_r_stack,anomalies_cru_feb_r_stack,anomalies_cru_mar_r_stack,
                             anomalies_cru_apr_r_stack,anomalies_cru_may_r_stack,anomalies_cru_jun_r_stack,
                             anomalies_cru_jul_r_stack,anomalies_cru_aug_r_stack,anomalies_cru_sep_r_stack,
                             anomalies_cru_oct_r_stack,anomalies_cru_nov_r_stack,anomalies_cru_dec_r_stack)

anomalies_all_months_years_res30sec=projectRaster(from = all_months_years_res05, 
                                                  to = raster_interpolation_extent, method = "bilinear")
plot(anomalies_all_months_years_res30sec)

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

###############downscale

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

br.area <-  extent1

anomalies_all_months_years_res30sec #from interpolation

###########WC monthly data
ti <- dir(pattern = ".tif")
ti


#JANUARY

raster_wc30s_jan_full=raster("wc2.1_30s_prec_01.tif")
raster_wc30s_jan_full

raster_wc30s_jan<- crop(raster_wc30s_jan_full, br.area)
plot(raster_wc30s_jan)

#FEBRUARY

raster_wc30s_feb_full=raster("wc2.1_30s_prec_02.tif")
raster_wc30s_feb_full

raster_wc30s_feb<- crop(raster_wc30s_feb_full, br.area)
plot(raster_wc30s_feb)

#MARCH

raster_wc30s_mar_full=raster("wc2.1_30s_prec_03.tif")
raster_wc30s_mar_full

raster_wc30s_mar<- crop(raster_wc30s_mar_full, br.area)
plot(raster_wc30s_mar)

#APRIL

raster_wc30s_apr_full=raster("wc2.1_30s_prec_04.tif")
raster_wc30s_apr_full

raster_wc30s_apr<- crop(raster_wc30s_apr_full, br.area)
plot(raster_wc30s_apr)

#MAY

raster_wc30s_may_full=raster("wc2.1_30s_prec_05.tif")
raster_wc30s_may_full

raster_wc30s_may<- crop(raster_wc30s_may_full, br.area)
plot(raster_wc30s_may)

#JUNE

raster_wc30s_jun_full=raster("wc2.1_30s_prec_06.tif")
raster_wc30s_jun_full

raster_wc30s_jun<- crop(raster_wc30s_jun_full, br.area)
plot(raster_wc30s_jun)

#JULY

raster_wc30s_jul_full=raster("wc2.1_30s_prec_07.tif")
raster_wc30s_jul_full

raster_wc30s_jul<- crop(raster_wc30s_jul_full, br.area)
plot(raster_wc30s_jul)

#AUGUST

raster_wc30s_aug_full=raster("wc2.1_30s_prec_08.tif")
raster_wc30s_aug_full

raster_wc30s_aug<- crop(raster_wc30s_aug_full, br.area)
plot(raster_wc30s_aug)

#SEPTEMBER

raster_wc30s_sep_full=raster("wc2.1_30s_prec_09.tif")
raster_wc30s_sep_full

raster_wc30s_sep<- crop(raster_wc30s_sep_full, br.area)
plot(raster_wc30s_sep)

#OCTOBER

raster_wc30s_oct_full=raster("wc2.1_30s_prec_10.tif")
raster_wc30s_oct_full

raster_wc30s_oct<- crop(raster_wc30s_oct_full, br.area)
plot(raster_wc30s_oct)

#NOVEMBER

raster_wc30s_nov_full=raster("wc2.1_30s_prec_11.tif")
raster_wc30s_nov_full

raster_wc30s_nov<- crop(raster_wc30s_nov_full, br.area)
plot(raster_wc30s_nov)

#DECEMBER

raster_wc30s_dec_full=raster("wc2.1_30s_prec_12.tif")
raster_wc30s_dec_full

raster_wc30s_dec<- crop(raster_wc30s_dec_full, br.area)
plot(raster_wc30s_dec)

###################################################

raster_stack_WC30s=stack(raster_wc30s_jan,raster_wc30s_feb,raster_wc30s_mar,raster_wc30s_apr,raster_wc30s_may,
                         raster_wc30s_jun,raster_wc30s_jul,raster_wc30s_aug,raster_wc30s_sep,raster_wc30s_oct,
                         raster_wc30s_nov,raster_wc30s_dec)


raster_stack_WC30s

############################################
#each 39 is a month (jan1981:2019)
anomalies_all_months_years_res30sec
anomalies_all_months_years_res30sec[[1:39]]
anomalies_all_months_years_res30sec[[40:78]]

############################
#extract only regions of interest to ease computation

data_base=data_base %>% relocate(lon, lat, .after =  key) #.before
head(data_base)


samples=subset(data_base,select=c(lon,lat))

values_annomalies30s_aoi <- data.frame(raster::extract(anomalies_all_months_years_res30sec, samples, ncol=2))
values_annomalies30s_aoi


values_wc30s_aoi= data.frame(raster::extract(raster_stack_WC30s, samples, ncol=2))
head(values_wc30s_aoi)
###########################

#dealing with NA #NA come from anomilies of zero divided by zero, in the dry season, jun jul
for(i in 1:ncol(values_annomalies30s_aoi)){
  values_annomalies30s_aoi[is.na(values_annomalies30s_aoi[,i]), i] <- 0.0001
}


#####################################
values_annomalies30s_aoi

values_wc30s_aoi

#39 year range

#JANUARY ,1:39

df_downsc_jan= sapply(values_annomalies30s_aoi[,1:39] ,FUN= function(a){
  values_wc30s_aoi[,1] * a }) 


df_downsc_jan


#FEBRUARY ,40:78

df_downsc_feb=sapply(values_annomalies30s_aoi[,40:78] ,FUN= function(a){
  values_wc30s_aoi[,2] * a })

df_downsc_feb

#MARCH ,79:117

df_downsc_mar=sapply(values_annomalies30s_aoi[,79:117] ,FUN= function(a){
  values_wc30s_aoi[,3] * a })

df_downsc_mar

#APRIL ,118:156

df_downsc_apr=sapply(values_annomalies30s_aoi[,118:156] ,FUN= function(a){
  values_wc30s_aoi[,4] * a })

df_downsc_apr

#MAY ,157:195

df_downsc_may=sapply(values_annomalies30s_aoi[,157:195] ,FUN= function(a){
  values_wc30s_aoi[,5] * a })

df_downsc_may

#JUNE ,196:234

df_downsc_jun=sapply(values_annomalies30s_aoi[,196:234] ,FUN= function(a){
  values_wc30s_aoi[,6] * a })

df_downsc_jun

#JULY ,235:273

df_downsc_jul=sapply(values_annomalies30s_aoi[,235:273] ,FUN= function(a){
  values_wc30s_aoi[,7] * a })

df_downsc_jul

#AUGUST ,274:312

df_downsc_aug=sapply(values_annomalies30s_aoi[,274:312] ,FUN= function(a){
  values_wc30s_aoi[,8] * a })

df_downsc_aug

#SEPTEMBER ,313:351

df_downsc_sep=sapply(values_annomalies30s_aoi[,313:351] ,FUN= function(a){
  values_wc30s_aoi[,9] * a })

df_downsc_sep

#OCTOBER ,352:390

df_downsc_oct=sapply(values_annomalies30s_aoi[,352:390] ,FUN= function(a){
  values_wc30s_aoi[,10] * a })

df_downsc_oct

#NOVEMBER ,391:429

df_downsc_nov=sapply(values_annomalies30s_aoi[,391:429] ,FUN= function(a){
  values_wc30s_aoi[,11] * a })

df_downsc_nov

#DECEMBER ,430:468

df_downsc_dec=sapply(values_annomalies30s_aoi[,430:468] ,FUN= function(a){
  values_wc30s_aoi[,12] * a })
df_downsc_dec

############################
#########reorder

df_test_reord=cbind.data.frame(df_downsc_jan,df_downsc_feb,df_downsc_mar,df_downsc_apr,
                               df_downsc_may,df_downsc_jun,df_downsc_jul,df_downsc_aug,
                               df_downsc_sep,df_downsc_oct,df_downsc_nov,df_downsc_dec)
df_test_reord

colnames(df_test_reord) <- sub("jan", "01", colnames(df_test_reord))
colnames(df_test_reord) <- sub("feb", "02", colnames(df_test_reord))
colnames(df_test_reord) <- sub("mar", "03", colnames(df_test_reord))
colnames(df_test_reord) <- sub("apr", "04", colnames(df_test_reord))
colnames(df_test_reord) <- sub("may", "05", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jun", "06", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jul", "07", colnames(df_test_reord))
colnames(df_test_reord) <- sub("aug", "08", colnames(df_test_reord))
colnames(df_test_reord) <- sub("sep", "09", colnames(df_test_reord))
colnames(df_test_reord) <- sub("oct", "10", colnames(df_test_reord))
colnames(df_test_reord) <- sub("nov", "11", colnames(df_test_reord))
colnames(df_test_reord) <- sub("dec", "12", colnames(df_test_reord))

colnames(df_test_reord) <- sub("_....", "", colnames(df_test_reord))
colnames(df_test_reord) <- paste(1981:2019, colnames(df_test_reord),sep=".")

df_test_reord1=df_test_reord[ , order(names(df_test_reord))]

data_base

temporary_file=cbind.data.frame(data_base,df_test_reord1 )
temporary_file

getwd()
write.csv(temporary_file,file="temporary_file.csv")

################################################################
################################################################
################################################################
################################################################

##########################climate_aoi######################################


library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

temporary_file=read.table("temporary_file.csv",header=T,dec=".",sep=",")#location and years to downscale

temporary_file #from downscale raster
str(temporary_file)

###################

temporary_file$yr_i_loc=NA
temporary_file$yr_f_loc=NA

temporary_file=temporary_file %>% relocate(yr_i_loc,yr_f_loc, .after=year_f)

######################

serie_yrI=1981 #inicial year of the series

col_start_serie=10 #jan 1981 col 10


################## YEAR INICIAL
func_location_col_y_i= function (x) {
  y1_loc= col_start_serie + ((x - serie_yrI) * 12)
  return(y1_loc)
}

#1982 starts in 22   
func_location_col_y_i(1982)  

################## YEAR FINAL #modified to include only the complete years before the interval


func_location_col_y_f= function (x) {
  y2_loc= col_start_serie + ((x -1 - serie_yrI) * 12)+(12)-1
  return(y2_loc)
}

##1981 ends in 21 
func_location_col_y_f(1982)  

#############################################

summary(temporary_file$year_f)

temporary_file$year_f[temporary_file$year_f== 2020] <- 2020
summary(temporary_file$year_f)

###########################################
#yr_i_loc col 8
#yr_f_loc col 9

for (i in temporary_file[,8:9]) {
  temporary_file$yr_i_loc <- func_location_col_y_i(temporary_file$year_i) 
  temporary_file$yr_f_loc  <- func_location_col_y_f(temporary_file$year_f) 
  
}

############################

temporary_file

colnames(temporary_file[202])
colnames(temporary_file[309])

######################################

temporary_file1=temporary_file

###########################################


##########################################
df_dws=NULL

for (i in 1:nrow(temporary_file1)) {
  
  df_dws[i]=cbind.data.frame(t(temporary_file1[i,temporary_file1$yr_i_loc[i] :temporary_file1$yr_f_loc[i] ]))
  
}

df_dws
as.data.frame(df_dws[[4]])

df_dws1=lapply(df_dws,data.frame) 

##############################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month = names(temporary_file1)[temporary_file1$yr_i_loc[i]:temporary_file1$yr_f_loc[i]]
}



df_dws1

#####################################################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month= sub(x =df_dws1[[i]]$year_month , pattern = "...$", replacement = "")  
  
}

df_dws1

###########################################################

data_downsc=NULL

for (i in 1:length(df_dws1)) {
  
  data_downsc[i] = df_dws1[[i]] %>% group_by(year_month) %>%  summarize(sum_clim=sum(X..i..)) %>% 
    summarise(downscaled_clim=mean(sum_clim))
  
}

data_downsc

###############################
data_downsc1=lapply(data_downsc,data.frame) 
data_downsc1

data_downsc12=do.call("rbind", data_downsc1)
data_downsc12


names(data_downsc12)=c("downsc_prec")


data_downsc12

######################

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti


# Load the CRU TS evapotranspiration dataset into R 
pet8190 <- brick("cru_ts4.04.1981.1990.pet.dat.nc", varname="pet")
pet9100 <- brick("cru_ts4.04.1991.2000.pet.dat.nc", varname="pet")
pet0110 <- brick("cru_ts4.04.2001.2010.pet.dat.nc", varname="pet")
pet1119 <- brick("cru_ts4.04.2011.2019.pet.dat.nc", varname="pet")

plot(pet0110$X2007.01.16)#janeiro 2007

pet=stack(pet8190,pet9100,pet0110,pet1119 )
plot(pet$X2007.01.16)

pet


#same extent
br.area <- extent1
petcru1981_2019 <- crop(pet, br.area)

plot(petcru1981_2019$X2007.01.16)#janeiro 2007

##########################

head(data_base)
data_base=data_base %>% relocate(lon, lat, .after =  key) #.before
head(data_base)


##################

samplespet=subset(data_base,select=c(lon,lat))

values_pet_aoipet <- data.frame(raster::extract(petcru1981_2019, samples, ncol=2))
values_pet_aoipet

###############################

###################

data_base$yr_i_loc=NA
data_base$yr_f_loc=NA

temporary_filepet=data_base %>% relocate(yr_i_loc,yr_f_loc, .after=year_f)


temporary_filepet=cbind.data.frame(temporary_filepet,values_pet_aoipet)
######################

serie_yrI=1981 #inicial year of the series

col_start_serie=9 #jan 1981 col 9


################## YEAR INICIAL
func_location_col_y_i= function (x) {
  y1_loc= col_start_serie + ((x - serie_yrI) * 12)
  return(y1_loc)
}

#1982 starts in 21   
func_location_col_y_i(1981)  

################## YEAR FINAL


func_location_col_y_f= function (x) {
  y2_loc= col_start_serie + ((x -1 - serie_yrI) * 12)+(12)-1
  return(y2_loc)
}

##1981 ends in 20 
func_location_col_y_f(1981)  

#############################################

summary(temporary_filepet$year_f)

temporary_filepet$year_f[temporary_filepet$year_f== 2020] <- 2020
summary(temporary_filepet$year_f)

###########################################
#yr_i_loc col 7
#yr_f_loc col 8

for (i in temporary_filepet[,7:8]) {
  temporary_filepet$yr_i_loc <- func_location_col_y_i(temporary_filepet$year_i) 
  temporary_filepet$yr_f_loc  <- func_location_col_y_f(temporary_filepet$year_f) 
  
}

############################

temporary_filepet

colnames(temporary_filepet[262])
colnames(temporary_filepet[308])

######################################

temporary_file1pet=temporary_filepet

##########################################

##########################################
df_dwspet=NULL

for (i in 1:nrow(temporary_file1pet)) {
  
  df_dwspet[i]=cbind.data.frame(t(temporary_file1pet[i,temporary_file1pet$yr_i_loc[i] :temporary_file1pet$yr_f_loc[i] ]))
  
}

df_dwspet
as.data.frame(df_dwspet[[4]])

df_dws1pet=lapply(df_dwspet,data.frame) 


##############################################

for (i in 1:length(df_dws1pet)) { 
  
  df_dws1pet[[i]]$year_month = names(temporary_file1pet)[temporary_file1pet$yr_i_loc[i]:temporary_file1pet$yr_f_loc[i]]
}



df_dws1pet

#####################################################################

for (i in 1:length(df_dws1pet)) { 
  
  df_dws1pet[[i]]$year_month= sub(x =df_dws1pet[[i]]$year_month , pattern = ".....$", replacement = "")  
  
}

df_dws1pet

###########################################################
##add downscalaed precipitation column. (here I will use an hypothetical columns)


#change df_dws1 of the right side, to this df of precipitation

for(i in 1:length(df_dws1pet)){
  df_dws1pet[[i]]$prec = df_dws1[[i]]$X..i..
}


df_dws1pet

###########################################################
###################cwd######################
df_dws2pet=df_dws1pet

for (i in 1:length(df_dws2pet)) {
  
  df_dws2pet[[i]]$cwd = ((df_dws2pet[[i]]$X..i..)*30)- df_dws2pet[[i]]$prec
  
}

df_dws2pet

###############################################################
data_downscpet=NULL

for (i in 1:length(df_dws2pet)) {
  
  data_downscpet[i] = df_dws2pet[[i]] %>% group_by(year_month) %>%  summarize(sum_clim=sum(cwd[cwd > 0])) %>% 
    summarise(downscaled_clim=mean(sum_clim))
  
}

data_downscpet


###########################################################


###############################
data_downsc1pet=lapply(data_downscpet,data.frame) 
data_downsc1pet

data_downsc12pet=do.call("rbind", data_downsc1pet)
data_downsc12pet


names(data_downsc12pet)=c("cwd")
data_downsc12pet

df_map_cwd=cbind.data.frame(data_downsc12,data_downsc12pet)

##############################


############################################################
###################anomalies##############################
############################################################

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

#########raster CRU1970-2000
CRU70_20 <- grep("CRU1970_2000_temp", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_temp_Jul)

############################
#Climate (tmpc) CRU 1981-2019

# Load the CRU TS tmpcipitation dataset into R 
tmp8190 <- brick("cru_ts4.04.1981.1990.tmp.dat.nc", varname="tmp")
tmp9100 <- brick("cru_ts4.04.1991.2000.tmp.dat.nc", varname="tmp")
tmp0110 <- brick("cru_ts4.04.2001.2010.tmp.dat.nc", varname="tmp")
tmp1119 <- brick("cru_ts4.04.2011.2019.tmp.dat.nc", varname="tmp")

plot(tmp0110$X2007.01.16)#janeiro 2007

tmp=stack(tmp8190,tmp9100,tmp0110, tmp1119 )
plot(tmp$X2007.07.16)

#same extent
br.area <- extent1
tmpcru1981_2019 <- crop(tmp, br.area)

plot(tmpcru1981_2019$X2007.01.16)#janeiro 2007

#anomalies 
CRU1970_2000
tmpcru1981_2019

#group precru1981_2019 by month

###############################
###############################
###############################
month1 <- raster::subset(tmpcru1981_2019,1) # i + 12
month1
##########################
##########################
##########################

#### JANUARY
anomalies_cru_jan=list()


for (i in seq(from=1, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_jan[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Jan
}

anomalies_cru_jan_r = anomalies_cru_jan[-which(sapply(anomalies_cru_jan, is.null))]
anomalies_cru_jan_r

names(anomalies_cru_jan_r)=c(paste("jan", 1981:2019,sep="_"))
anomalies_cru_jan_r

anomalies_cru_jan_r_stack = stack(anomalies_cru_jan_r)


#### FEBRUARY
anomalies_cru_feb=list()


for (i in seq(from=2, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_feb[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Feb
}

anomalies_cru_feb_r = anomalies_cru_feb[-which(sapply(anomalies_cru_feb, is.null))]
anomalies_cru_feb_r

names(anomalies_cru_feb_r)=c(paste("feb", 1981:2019,sep="_"))
anomalies_cru_feb_r

anomalies_cru_feb_r_stack = stack(anomalies_cru_feb_r)



#### MARCH
anomalies_cru_mar=list()


for (i in seq(from=3, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_mar[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Mar
}

anomalies_cru_mar_r = anomalies_cru_mar[-which(sapply(anomalies_cru_mar, is.null))]
anomalies_cru_mar_r

names(anomalies_cru_mar_r)=c(paste("mar", 1981:2019,sep="_"))
anomalies_cru_mar_r

anomalies_cru_mar_r_stack = stack(anomalies_cru_mar_r)


#### APRIL
anomalies_cru_apr=list()


for (i in seq(from=4, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_apr[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Apr
}

anomalies_cru_apr_r = anomalies_cru_apr[-which(sapply(anomalies_cru_apr, is.null))]
anomalies_cru_apr_r

names(anomalies_cru_apr_r)=c(paste("apr", 1981:2019,sep="_"))
anomalies_cru_apr_r

anomalies_cru_apr_r_stack = stack(anomalies_cru_apr_r)


#### MAY
anomalies_cru_may=list()


for (i in seq(from=5, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_may[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_May
}

anomalies_cru_may_r = anomalies_cru_may[-which(sapply(anomalies_cru_may, is.null))]
anomalies_cru_may_r

names(anomalies_cru_may_r)=c(paste("may", 1981:2019,sep="_"))
anomalies_cru_may_r

anomalies_cru_may_r_stack = stack(anomalies_cru_may_r)


#### JUNE
anomalies_cru_jun=list()


for (i in seq(from=6, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_jun[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Jun
}

anomalies_cru_jun_r = anomalies_cru_jun[-which(sapply(anomalies_cru_jun, is.null))]
anomalies_cru_jun_r

names(anomalies_cru_jun_r)=c(paste("jun", 1981:2019,sep="_"))
anomalies_cru_jun_r

anomalies_cru_jun_r_stack = stack(anomalies_cru_jun_r)


#### JULY
anomalies_cru_jul=list()


for (i in seq(from=7, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_jul[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Jul
}

anomalies_cru_jul_r = anomalies_cru_jul[-which(sapply(anomalies_cru_jul, is.null))]
anomalies_cru_jul_r

names(anomalies_cru_jul_r)=c(paste("jul", 1981:2019,sep="_"))
anomalies_cru_jul_r

anomalies_cru_jul_r_stack = stack(anomalies_cru_jul_r)


#### AUGUST
anomalies_cru_aug=list()


for (i in seq(from=8, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_aug[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Aug
}

anomalies_cru_aug_r = anomalies_cru_aug[-which(sapply(anomalies_cru_aug, is.null))]
anomalies_cru_aug_r

names(anomalies_cru_aug_r)=c(paste("aug", 1981:2019,sep="_"))
anomalies_cru_aug_r

anomalies_cru_aug_r_stack = stack(anomalies_cru_aug_r)


#### SEPTEMBER
anomalies_cru_sep=list()


for (i in seq(from=9, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_sep[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Sep
}

anomalies_cru_sep_r = anomalies_cru_sep[-which(sapply(anomalies_cru_sep, is.null))]
anomalies_cru_sep_r

names(anomalies_cru_sep_r)=c(paste("sep", 1981:2019,sep="_"))
anomalies_cru_sep_r

anomalies_cru_sep_r_stack = stack(anomalies_cru_sep_r)


#### OCTOBER
anomalies_cru_oct=list()


for (i in seq(from=10, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_oct[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Oct
}

anomalies_cru_oct_r = anomalies_cru_oct[-which(sapply(anomalies_cru_oct, is.null))]
anomalies_cru_oct_r

names(anomalies_cru_oct_r)=c(paste("oct", 1981:2019,sep="_"))
anomalies_cru_oct_r

anomalies_cru_oct_r_stack = stack(anomalies_cru_oct_r)


#### NOVEMBER
anomalies_cru_nov=list()


for (i in seq(from=11, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_nov[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Nov
}

anomalies_cru_nov_r = anomalies_cru_nov[-which(sapply(anomalies_cru_nov, is.null))]
anomalies_cru_nov_r

names(anomalies_cru_nov_r)=c(paste("nov", 1981:2019,sep="_"))
anomalies_cru_nov_r

anomalies_cru_nov_r_stack = stack(anomalies_cru_nov_r)


#### DECEMBER
anomalies_cru_dec=list()


for (i in seq(from=12, to=nlayers(tmpcru1981_2019), by=12)) {
  anomalies_cru_dec[[i]]= raster::subset(tmpcru1981_2019,i)-CRU1970_2000$CRU1970_2000_temp_Dec
}

anomalies_cru_dec_r = anomalies_cru_dec[-which(sapply(anomalies_cru_dec, is.null))]
anomalies_cru_dec_r

names(anomalies_cru_dec_r)=c(paste("dec", 1981:2019,sep="_"))
anomalies_cru_dec_r

anomalies_cru_dec_r_stack = stack(anomalies_cru_dec_r)

#################################################################
#################################################################
#################################################################

###############interpolation

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

raster_interpolation=raster("wc2.1_30s_tavg_01.tif")
raster_interpolation

br.area <-  extent1
raster_interpolation_extent <- crop(raster_interpolation, br.area)
plot(raster_interpolation_extent)

all_months_years_res05=stack(anomalies_cru_jan_r_stack,anomalies_cru_feb_r_stack,anomalies_cru_mar_r_stack,
                             anomalies_cru_apr_r_stack,anomalies_cru_may_r_stack,anomalies_cru_jun_r_stack,
                             anomalies_cru_jul_r_stack,anomalies_cru_aug_r_stack,anomalies_cru_sep_r_stack,
                             anomalies_cru_oct_r_stack,anomalies_cru_nov_r_stack,anomalies_cru_dec_r_stack)

anomalies_all_months_years_res30sec=projectRaster(from = all_months_years_res05, 
                                                  to = raster_interpolation_extent, method = "bilinear")
plot(anomalies_all_months_years_res30sec)

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

###############downscale

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

br.area <- extent1

anomalies_all_months_years_res30sec #from interpolation

###########WC monthly data
ti <- dir(pattern = ".tif")
ti


#JANUARY

raster_wc30s_jan_full=raster("wc2.1_30s_tavg_01.tif")
raster_wc30s_jan_full

raster_wc30s_jan<- crop(raster_wc30s_jan_full, br.area)
plot(raster_wc30s_jan)

#FEBRUARY

raster_wc30s_feb_full=raster("wc2.1_30s_tavg_02.tif")
raster_wc30s_feb_full

raster_wc30s_feb<- crop(raster_wc30s_feb_full, br.area)
plot(raster_wc30s_feb)

#MARCH

raster_wc30s_mar_full=raster("wc2.1_30s_tavg_03.tif")
raster_wc30s_mar_full

raster_wc30s_mar<- crop(raster_wc30s_mar_full, br.area)
plot(raster_wc30s_mar)

#APRIL

raster_wc30s_apr_full=raster("wc2.1_30s_tavg_04.tif")
raster_wc30s_apr_full

raster_wc30s_apr<- crop(raster_wc30s_apr_full, br.area)
plot(raster_wc30s_apr)

#MAY

raster_wc30s_may_full=raster("wc2.1_30s_tavg_05.tif")
raster_wc30s_may_full

raster_wc30s_may<- crop(raster_wc30s_may_full, br.area)
plot(raster_wc30s_may)

#JUNE

raster_wc30s_jun_full=raster("wc2.1_30s_tavg_06.tif")
raster_wc30s_jun_full

raster_wc30s_jun<- crop(raster_wc30s_jun_full, br.area)
plot(raster_wc30s_jun)

#JULY

raster_wc30s_jul_full=raster("wc2.1_30s_tavg_07.tif")
raster_wc30s_jul_full

raster_wc30s_jul<- crop(raster_wc30s_jul_full, br.area)
plot(raster_wc30s_jul)

#AUGUST

raster_wc30s_aug_full=raster("wc2.1_30s_tavg_08.tif")
raster_wc30s_aug_full

raster_wc30s_aug<- crop(raster_wc30s_aug_full, br.area)
plot(raster_wc30s_aug)

#SEPTEMBER

raster_wc30s_sep_full=raster("wc2.1_30s_tavg_09.tif")
raster_wc30s_sep_full

raster_wc30s_sep<- crop(raster_wc30s_sep_full, br.area)
plot(raster_wc30s_sep)

#OCTOBER

raster_wc30s_oct_full=raster("wc2.1_30s_tavg_10.tif")
raster_wc30s_oct_full

raster_wc30s_oct<- crop(raster_wc30s_oct_full, br.area)
plot(raster_wc30s_oct)

#NOVEMBER

raster_wc30s_nov_full=raster("wc2.1_30s_tavg_11.tif")
raster_wc30s_nov_full

raster_wc30s_nov<- crop(raster_wc30s_nov_full, br.area)
plot(raster_wc30s_nov)

#DECEMBER

raster_wc30s_dec_full=raster("wc2.1_30s_tavg_12.tif")
raster_wc30s_dec_full

raster_wc30s_dec<- crop(raster_wc30s_dec_full, br.area)
plot(raster_wc30s_dec)

###################################################

raster_stack_WC30s=stack(raster_wc30s_jan,raster_wc30s_feb,raster_wc30s_mar,raster_wc30s_apr,raster_wc30s_may,
                         raster_wc30s_jun,raster_wc30s_jul,raster_wc30s_aug,raster_wc30s_sep,raster_wc30s_oct,
                         raster_wc30s_nov,raster_wc30s_dec)


raster_stack_WC30s

############################################
#each 39 is a month (jan1981:2019)
anomalies_all_months_years_res30sec
anomalies_all_months_years_res30sec[[1:39]]
anomalies_all_months_years_res30sec[[11:20]]

############################
#extract only regions of interest to ease computation

head(data_base)
data_base=data_base %>% relocate(lon, lat, .after =  key) #.before
head(data_base)


samples=subset(data_base,select=c(lon,lat))

values_annomalies30s_aoi <- data.frame(raster::extract(anomalies_all_months_years_res30sec, samples, ncol=2))
values_annomalies30s_aoi


values_wc30s_aoi= data.frame(raster::extract(raster_stack_WC30s, samples, ncol=2))
head(values_wc30s_aoi)
###########################


#####################################
values_annomalies30s_aoi

values_wc30s_aoi

#39 year range

#JANUARY ,1:39

df_downsc_jan= sapply(values_annomalies30s_aoi[,1:39] ,FUN= function(a){
  values_wc30s_aoi[,1] + a }) 


df_downsc_jan


#FEBRUARY ,40:78

df_downsc_feb=sapply(values_annomalies30s_aoi[,40:78] ,FUN= function(a){
  values_wc30s_aoi[,2] + a })

df_downsc_feb

#MARCH ,79:117

df_downsc_mar=sapply(values_annomalies30s_aoi[,79:117] ,FUN= function(a){
  values_wc30s_aoi[,3] + a })

df_downsc_mar

#APRIL ,118:156

df_downsc_apr=sapply(values_annomalies30s_aoi[,118:156] ,FUN= function(a){
  values_wc30s_aoi[,4] + a })

df_downsc_apr

#MAY ,157:195

df_downsc_may=sapply(values_annomalies30s_aoi[,157:195] ,FUN= function(a){
  values_wc30s_aoi[,5] + a })

df_downsc_may

#JUNE ,196:234

df_downsc_jun=sapply(values_annomalies30s_aoi[,196:234] ,FUN= function(a){
  values_wc30s_aoi[,6] + a })

df_downsc_jun

#JULY ,235:273

df_downsc_jul=sapply(values_annomalies30s_aoi[,235:273] ,FUN= function(a){
  values_wc30s_aoi[,7] + a })

df_downsc_jul

#AUGUST ,274:312

df_downsc_aug=sapply(values_annomalies30s_aoi[,274:312] ,FUN= function(a){
  values_wc30s_aoi[,8] + a })

df_downsc_aug

#SEPTEMBER ,313:351

df_downsc_sep=sapply(values_annomalies30s_aoi[,313:351] ,FUN= function(a){
  values_wc30s_aoi[,9] + a })

df_downsc_sep

#OCTOBER ,352:390

df_downsc_oct=sapply(values_annomalies30s_aoi[,352:390] ,FUN= function(a){
  values_wc30s_aoi[,10] + a })

df_downsc_oct

#NOVEMBER ,391:429

df_downsc_nov=sapply(values_annomalies30s_aoi[,391:429] ,FUN= function(a){
  values_wc30s_aoi[,11] + a })

df_downsc_nov

#DECEMBER ,430:468

df_downsc_dec=sapply(values_annomalies30s_aoi[,430:468] ,FUN= function(a){
  values_wc30s_aoi[,12] + a })
df_downsc_dec

############################
#########reorder

df_test_reord=cbind.data.frame(df_downsc_jan,df_downsc_feb,df_downsc_mar,df_downsc_apr,
                               df_downsc_may,df_downsc_jun,df_downsc_jul,df_downsc_aug,
                               df_downsc_sep,df_downsc_oct,df_downsc_nov,df_downsc_dec)
df_test_reord

colnames(df_test_reord) <- sub("jan", "01", colnames(df_test_reord))
colnames(df_test_reord) <- sub("feb", "02", colnames(df_test_reord))
colnames(df_test_reord) <- sub("mar", "03", colnames(df_test_reord))
colnames(df_test_reord) <- sub("apr", "04", colnames(df_test_reord))
colnames(df_test_reord) <- sub("may", "05", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jun", "06", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jul", "07", colnames(df_test_reord))
colnames(df_test_reord) <- sub("aug", "08", colnames(df_test_reord))
colnames(df_test_reord) <- sub("sep", "09", colnames(df_test_reord))
colnames(df_test_reord) <- sub("oct", "10", colnames(df_test_reord))
colnames(df_test_reord) <- sub("nov", "11", colnames(df_test_reord))
colnames(df_test_reord) <- sub("dec", "12", colnames(df_test_reord))

colnames(df_test_reord) <- sub("_....", "", colnames(df_test_reord))
colnames(df_test_reord) <- paste(1981:2019, colnames(df_test_reord),sep=".")

df_test_reord1=df_test_reord[ , order(names(df_test_reord))]

data_base

temporary_file=cbind.data.frame(data_base,df_test_reord1 )
temporary_file

getwd()
write.csv(temporary_file,file="temporary_file.csv")

################################################################
################################################################
################################################################
################################################################

##########################climate_aoi######################################


library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

temporary_file=read.table("temporary_file.csv",header=T,dec=".",sep=",")#location and years to downscale

temporary_file #from downscale raster
str(temporary_file)

###################

temporary_file$yr_i_loc=NA
temporary_file$yr_f_loc=NA

temporary_file=temporary_file %>% relocate(yr_i_loc,yr_f_loc, .after=year_f)

######################

serie_yrI=1981 #inicial year of the series

col_start_serie=10 #jan 1981 col 10


################## YEAR INICIAL
func_location_col_y_i= function (x) {
  y1_loc= col_start_serie + ((x - serie_yrI) * 12)
  return(y1_loc)
}

#1982 starts in 22   
func_location_col_y_i(1982)  

################## YEAR FINAL


func_location_col_y_f= function (x) {
  y2_loc= col_start_serie + ((x -1 - serie_yrI) * 12)+(12)-1
  return(y2_loc)
}

##1981 ends in 21 
func_location_col_y_f(1981) 

#############################################

summary(temporary_file$year_f)

temporary_file$year_f[temporary_file$year_f== 2020] <- 2020
summary(temporary_file$year_f)

###########################################
#yr_i_loc col 8
#yr_f_loc col 9

for (i in temporary_file[,8:9]) {
  temporary_file$yr_i_loc <- func_location_col_y_i(temporary_file$year_i) 
  temporary_file$yr_f_loc  <- func_location_col_y_f(temporary_file$year_f) 
  
}

############################

temporary_file

colnames(temporary_file[262])
colnames(temporary_file[309])

######################################

temporary_file1=temporary_file

##########################################
df_dws=NULL

for (i in 1:nrow(temporary_file1)) {
  
  df_dws[i]=cbind.data.frame(t(temporary_file1[i,temporary_file1$yr_i_loc[i] :temporary_file1$yr_f_loc[i] ]))
  
}

df_dws
as.data.frame(df_dws[[4]])

df_dws1=lapply(df_dws,data.frame) 

##############################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month = names(temporary_file1)[temporary_file1$yr_i_loc[i]:temporary_file1$yr_f_loc[i]]
}



df_dws1

#####################################################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month= sub(x =df_dws1[[i]]$year_month , pattern = "...$", replacement = "")  
  
}

df_dws1

###########################################################

data_downsc=NULL

for (i in 1:length(df_dws1)) {
  
  data_downsc[i] = df_dws1[[i]] %>% group_by(year_month) %>%  summarize(mean_clim=mean(X..i..)) %>% 
    summarise(downscaled_clim=mean(mean_clim))
  
}

data_downsc

###############################
data_downsc1=lapply(data_downsc,data.frame) 
data_downsc1

data_downsc12=do.call("rbind", data_downsc1)
data_downsc12


names(data_downsc12)=c("downsc_tmp")
data_downsc12

df_map_mat_cwd=cbind.data.frame(df_map_cwd,data_downsc12 )
df_map_mat_cwd

#write.csv(df_map_mat_cwd,"df_map_mat_cwd.csv")

#CRU 1970-2000 series are already calculated

############################################################
###################anomalies##############################
############################################################

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

#########raster CRU1970-2000
CRU70_20 <- grep("CRU1970_2000_maxtemp", ti, value=T)

# import rasters
CRU1970_2000 <- raster::stack(CRU70_20)
CRU1970_2000

plot(CRU1970_2000$CRU1970_2000_maxtemp_Jul)

############################
#Climate (tmxc) CRU 1981-2019

# Load the CRU TS tmxcipitation dataset into R 
tmx8190 <- brick("cru_ts4.04.1981.1990.tmx.dat.nc", varname="tmx")
tmx9100 <- brick("cru_ts4.04.1991.2000.tmx.dat.nc", varname="tmx")
tmx0110 <- brick("cru_ts4.04.2001.2010.tmx.dat.nc", varname="tmx")
tmx1119 <- brick("cru_ts4.04.2011.2019.tmx.dat.nc", varname="tmx")

plot(tmx0110$X2007.01.16)#janeiro 2007

tmx=stack(tmx8190,tmx9100,tmx0110, tmx1119 )
plot(tmx$X2007.07.16)

#same extent
br.area <- extent1
tmxcru1981_2019 <- crop(tmx, br.area)

plot(tmxcru1981_2019$X2007.01.16)#janeiro 2007

#anomalies 
CRU1970_2000
tmxcru1981_2019

#group precru1981_2019 by month

###############################
###############################
###############################
month1 <- raster::subset(tmxcru1981_2019,1) # i + 12
month1
##########################
##########################
##########################

#### JANUARY
anomalies_cru_jan=list()


for (i in seq(from=1, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_jan[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Jan
}

anomalies_cru_jan_r = anomalies_cru_jan[-which(sapply(anomalies_cru_jan, is.null))]
anomalies_cru_jan_r

names(anomalies_cru_jan_r)=c(paste("jan", 1981:2019,sep="_"))
anomalies_cru_jan_r

anomalies_cru_jan_r_stack = stack(anomalies_cru_jan_r)


#### FEBRUARY
anomalies_cru_feb=list()


for (i in seq(from=2, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_feb[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Feb
}

anomalies_cru_feb_r = anomalies_cru_feb[-which(sapply(anomalies_cru_feb, is.null))]
anomalies_cru_feb_r

names(anomalies_cru_feb_r)=c(paste("feb", 1981:2019,sep="_"))
anomalies_cru_feb_r

anomalies_cru_feb_r_stack = stack(anomalies_cru_feb_r)



#### MARCH
anomalies_cru_mar=list()


for (i in seq(from=3, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_mar[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Mar
}

anomalies_cru_mar_r = anomalies_cru_mar[-which(sapply(anomalies_cru_mar, is.null))]
anomalies_cru_mar_r

names(anomalies_cru_mar_r)=c(paste("mar", 1981:2019,sep="_"))
anomalies_cru_mar_r

anomalies_cru_mar_r_stack = stack(anomalies_cru_mar_r)


#### APRIL
anomalies_cru_apr=list()


for (i in seq(from=4, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_apr[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Apr
}

anomalies_cru_apr_r = anomalies_cru_apr[-which(sapply(anomalies_cru_apr, is.null))]
anomalies_cru_apr_r

names(anomalies_cru_apr_r)=c(paste("apr", 1981:2019,sep="_"))
anomalies_cru_apr_r

anomalies_cru_apr_r_stack = stack(anomalies_cru_apr_r)


#### MAY
anomalies_cru_may=list()


for (i in seq(from=5, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_may[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_May
}

anomalies_cru_may_r = anomalies_cru_may[-which(sapply(anomalies_cru_may, is.null))]
anomalies_cru_may_r

names(anomalies_cru_may_r)=c(paste("may", 1981:2019,sep="_"))
anomalies_cru_may_r

anomalies_cru_may_r_stack = stack(anomalies_cru_may_r)


#### JUNE
anomalies_cru_jun=list()


for (i in seq(from=6, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_jun[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Jun
}

anomalies_cru_jun_r = anomalies_cru_jun[-which(sapply(anomalies_cru_jun, is.null))]
anomalies_cru_jun_r

names(anomalies_cru_jun_r)=c(paste("jun", 1981:2019,sep="_"))
anomalies_cru_jun_r

anomalies_cru_jun_r_stack = stack(anomalies_cru_jun_r)


#### JULY
anomalies_cru_jul=list()


for (i in seq(from=7, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_jul[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Jul
}

anomalies_cru_jul_r = anomalies_cru_jul[-which(sapply(anomalies_cru_jul, is.null))]
anomalies_cru_jul_r

names(anomalies_cru_jul_r)=c(paste("jul", 1981:2019,sep="_"))
anomalies_cru_jul_r

anomalies_cru_jul_r_stack = stack(anomalies_cru_jul_r)


#### AUGUST
anomalies_cru_aug=list()


for (i in seq(from=8, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_aug[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Aug
}

anomalies_cru_aug_r = anomalies_cru_aug[-which(sapply(anomalies_cru_aug, is.null))]
anomalies_cru_aug_r

names(anomalies_cru_aug_r)=c(paste("aug", 1981:2019,sep="_"))
anomalies_cru_aug_r

anomalies_cru_aug_r_stack = stack(anomalies_cru_aug_r)


#### SEPTEMBER
anomalies_cru_sep=list()


for (i in seq(from=9, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_sep[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Sep
}

anomalies_cru_sep_r = anomalies_cru_sep[-which(sapply(anomalies_cru_sep, is.null))]
anomalies_cru_sep_r

names(anomalies_cru_sep_r)=c(paste("sep", 1981:2019,sep="_"))
anomalies_cru_sep_r

anomalies_cru_sep_r_stack = stack(anomalies_cru_sep_r)


#### OCTOBER
anomalies_cru_oct=list()


for (i in seq(from=10, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_oct[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Oct
}

anomalies_cru_oct_r = anomalies_cru_oct[-which(sapply(anomalies_cru_oct, is.null))]
anomalies_cru_oct_r

names(anomalies_cru_oct_r)=c(paste("oct", 1981:2019,sep="_"))
anomalies_cru_oct_r

anomalies_cru_oct_r_stack = stack(anomalies_cru_oct_r)


#### NOVEMBER
anomalies_cru_nov=list()


for (i in seq(from=11, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_nov[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Nov
}

anomalies_cru_nov_r = anomalies_cru_nov[-which(sapply(anomalies_cru_nov, is.null))]
anomalies_cru_nov_r

names(anomalies_cru_nov_r)=c(paste("nov", 1981:2019,sep="_"))
anomalies_cru_nov_r

anomalies_cru_nov_r_stack = stack(anomalies_cru_nov_r)


#### DECEMBER
anomalies_cru_dec=list()


for (i in seq(from=12, to=nlayers(tmxcru1981_2019), by=12)) {
  anomalies_cru_dec[[i]]= raster::subset(tmxcru1981_2019,i)-CRU1970_2000$CRU1970_2000_maxtemp_Dec
}

anomalies_cru_dec_r = anomalies_cru_dec[-which(sapply(anomalies_cru_dec, is.null))]
anomalies_cru_dec_r

names(anomalies_cru_dec_r)=c(paste("dec", 1981:2019,sep="_"))
anomalies_cru_dec_r

anomalies_cru_dec_r_stack = stack(anomalies_cru_dec_r)

#################################################################
#################################################################
#################################################################

###############interpolation

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

ti <- dir(pattern = ".tif")
ti

raster_interpolation=raster("wc2.1_30s_tavg_01.tif")
raster_interpolation

br.area <-  extent1
raster_interpolation_extent <- crop(raster_interpolation, br.area)
plot(raster_interpolation_extent)



all_months_years_res05=stack(anomalies_cru_jan_r_stack,anomalies_cru_feb_r_stack,anomalies_cru_mar_r_stack,
                             anomalies_cru_apr_r_stack,anomalies_cru_may_r_stack,anomalies_cru_jun_r_stack,
                             anomalies_cru_jul_r_stack,anomalies_cru_aug_r_stack,anomalies_cru_sep_r_stack,
                             anomalies_cru_oct_r_stack,anomalies_cru_nov_r_stack,anomalies_cru_dec_r_stack)

anomalies_all_months_years_res30sec=projectRaster(from = all_months_years_res05, 
                                                  to = raster_interpolation_extent, method = "bilinear")
plot(anomalies_all_months_years_res30sec)

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

###############downscale

library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

br.area <- extent1

anomalies_all_months_years_res30sec #from interpolation

###########WC monthly data
ti <- dir(pattern = ".tif")
ti


#JANUARY

raster_wc30s_jan_full=raster("wc2.1_30s_tmax_01.tif")
raster_wc30s_jan_full

raster_wc30s_jan<- crop(raster_wc30s_jan_full, br.area)
plot(raster_wc30s_jan)

#FEBRUARY

raster_wc30s_feb_full=raster("wc2.1_30s_tmax_02.tif")
raster_wc30s_feb_full

raster_wc30s_feb<- crop(raster_wc30s_feb_full, br.area)
plot(raster_wc30s_feb)

#MARCH

raster_wc30s_mar_full=raster("wc2.1_30s_tmax_03.tif")
raster_wc30s_mar_full

raster_wc30s_mar<- crop(raster_wc30s_mar_full, br.area)
plot(raster_wc30s_mar)

#APRIL

raster_wc30s_apr_full=raster("wc2.1_30s_tmax_04.tif")
raster_wc30s_apr_full

raster_wc30s_apr<- crop(raster_wc30s_apr_full, br.area)
plot(raster_wc30s_apr)

#MAY

raster_wc30s_may_full=raster("wc2.1_30s_tmax_05.tif")
raster_wc30s_may_full

raster_wc30s_may<- crop(raster_wc30s_may_full, br.area)
plot(raster_wc30s_may)

#JUNE

raster_wc30s_jun_full=raster("wc2.1_30s_tmax_06.tif")
raster_wc30s_jun_full

raster_wc30s_jun<- crop(raster_wc30s_jun_full, br.area)
plot(raster_wc30s_jun)

#JULY

raster_wc30s_jul_full=raster("wc2.1_30s_tmax_07.tif")
raster_wc30s_jul_full

raster_wc30s_jul<- crop(raster_wc30s_jul_full, br.area)
plot(raster_wc30s_jul)

#AUGUST

raster_wc30s_aug_full=raster("wc2.1_30s_tmax_08.tif")
raster_wc30s_aug_full

raster_wc30s_aug<- crop(raster_wc30s_aug_full, br.area)
plot(raster_wc30s_aug)

#SEPTEMBER

raster_wc30s_sep_full=raster("wc2.1_30s_tmax_09.tif")
raster_wc30s_sep_full

raster_wc30s_sep<- crop(raster_wc30s_sep_full, br.area)
plot(raster_wc30s_sep)

#OCTOBER

raster_wc30s_oct_full=raster("wc2.1_30s_tmax_10.tif")
raster_wc30s_oct_full

raster_wc30s_oct<- crop(raster_wc30s_oct_full, br.area)
plot(raster_wc30s_oct)

#NOVEMBER

raster_wc30s_nov_full=raster("wc2.1_30s_tmax_11.tif")
raster_wc30s_nov_full

raster_wc30s_nov<- crop(raster_wc30s_nov_full, br.area)
plot(raster_wc30s_nov)

#DECEMBER

raster_wc30s_dec_full=raster("wc2.1_30s_tmax_12.tif")
raster_wc30s_dec_full

raster_wc30s_dec<- crop(raster_wc30s_dec_full, br.area)
plot(raster_wc30s_dec)

###################################################

raster_stack_WC30s=stack(raster_wc30s_jan,raster_wc30s_feb,raster_wc30s_mar,raster_wc30s_apr,raster_wc30s_may,
                         raster_wc30s_jun,raster_wc30s_jul,raster_wc30s_aug,raster_wc30s_sep,raster_wc30s_oct,
                         raster_wc30s_nov,raster_wc30s_dec)


raster_stack_WC30s

############################################
#each 39 is a month (jan1981:2019)
anomalies_all_months_years_res30sec
anomalies_all_months_years_res30sec[[1:39]]
anomalies_all_months_years_res30sec[[11:20]]

############################
#extract only regions of interest to ease computation

head(data_base)
data_base=data_base %>% relocate(lon, lat, .after =  key) #.before
head(data_base)


samples=subset(data_base,select=c(lon,lat))

values_annomalies30s_aoi <- data.frame(raster::extract(anomalies_all_months_years_res30sec, samples, ncol=2))
values_annomalies30s_aoi


values_wc30s_aoi= data.frame(raster::extract(raster_stack_WC30s, samples, ncol=2))
head(values_wc30s_aoi)
###########################


#####################################
values_annomalies30s_aoi

values_wc30s_aoi

#39 year range

#JANUARY ,1:39

df_downsc_jan= sapply(values_annomalies30s_aoi[,1:39] ,FUN= function(a){
  values_wc30s_aoi[,1] + a }) 


df_downsc_jan


#FEBRUARY ,40:78

df_downsc_feb=sapply(values_annomalies30s_aoi[,40:78] ,FUN= function(a){
  values_wc30s_aoi[,2] + a })

df_downsc_feb

#MARCH ,79:117

df_downsc_mar=sapply(values_annomalies30s_aoi[,79:117] ,FUN= function(a){
  values_wc30s_aoi[,3] + a })

df_downsc_mar

#APRIL ,118:156

df_downsc_apr=sapply(values_annomalies30s_aoi[,118:156] ,FUN= function(a){
  values_wc30s_aoi[,4] + a })

df_downsc_apr

#MAY ,157:195

df_downsc_may=sapply(values_annomalies30s_aoi[,157:195] ,FUN= function(a){
  values_wc30s_aoi[,5] + a })

df_downsc_may

#JUNE ,196:234

df_downsc_jun=sapply(values_annomalies30s_aoi[,196:234] ,FUN= function(a){
  values_wc30s_aoi[,6] + a })

df_downsc_jun

#JULY ,235:273

df_downsc_jul=sapply(values_annomalies30s_aoi[,235:273] ,FUN= function(a){
  values_wc30s_aoi[,7] + a })

df_downsc_jul

#AUGUST ,274:312

df_downsc_aug=sapply(values_annomalies30s_aoi[,274:312] ,FUN= function(a){
  values_wc30s_aoi[,8] + a })

df_downsc_aug

#SEPTEMBER ,313:351

df_downsc_sep=sapply(values_annomalies30s_aoi[,313:351] ,FUN= function(a){
  values_wc30s_aoi[,9] + a })

df_downsc_sep

#OCTOBER ,352:390

df_downsc_oct=sapply(values_annomalies30s_aoi[,352:390] ,FUN= function(a){
  values_wc30s_aoi[,10] + a })

df_downsc_oct

#NOVEMBER ,391:429

df_downsc_nov=sapply(values_annomalies30s_aoi[,391:429] ,FUN= function(a){
  values_wc30s_aoi[,11] + a })

df_downsc_nov

#DECEMBER ,430:468

df_downsc_dec=sapply(values_annomalies30s_aoi[,430:468] ,FUN= function(a){
  values_wc30s_aoi[,12] + a })
df_downsc_dec

############################
#########reorder

df_test_reord=cbind.data.frame(df_downsc_jan,df_downsc_feb,df_downsc_mar,df_downsc_apr,
                               df_downsc_may,df_downsc_jun,df_downsc_jul,df_downsc_aug,
                               df_downsc_sep,df_downsc_oct,df_downsc_nov,df_downsc_dec)
df_test_reord

colnames(df_test_reord) <- sub("jan", "01", colnames(df_test_reord))
colnames(df_test_reord) <- sub("feb", "02", colnames(df_test_reord))
colnames(df_test_reord) <- sub("mar", "03", colnames(df_test_reord))
colnames(df_test_reord) <- sub("apr", "04", colnames(df_test_reord))
colnames(df_test_reord) <- sub("may", "05", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jun", "06", colnames(df_test_reord))
colnames(df_test_reord) <- sub("jul", "07", colnames(df_test_reord))
colnames(df_test_reord) <- sub("aug", "08", colnames(df_test_reord))
colnames(df_test_reord) <- sub("sep", "09", colnames(df_test_reord))
colnames(df_test_reord) <- sub("oct", "10", colnames(df_test_reord))
colnames(df_test_reord) <- sub("nov", "11", colnames(df_test_reord))
colnames(df_test_reord) <- sub("dec", "12", colnames(df_test_reord))

colnames(df_test_reord) <- sub("_....", "", colnames(df_test_reord))
colnames(df_test_reord) <- paste(1981:2019, colnames(df_test_reord),sep=".")

df_test_reord1=df_test_reord[ , order(names(df_test_reord))]

data_base

temporary_file=cbind.data.frame(data_base,df_test_reord1 )
temporary_file

getwd()
write.csv(temporary_file,file="temporary_file.csv")

################################################################
################################################################
################################################################
################################################################

##########################climate_aoi######################################


library(raster)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)

temporary_file=read.table("temporary_file.csv",header=T,dec=".",sep=",")#location and years to downscale

temporary_file #from downscale raster
str(temporary_file)

###################

temporary_file$yr_i_loc=NA
temporary_file$yr_f_loc=NA

temporary_file=temporary_file %>% relocate(yr_i_loc,yr_f_loc, .after=year_f)

######################

serie_yrI=1981 #inicial year of the series

col_start_serie=10 #jan 1981 col 10


################## YEAR INICIAL
func_location_col_y_i= function (x) {
  y1_loc= col_start_serie + ((x - serie_yrI) * 12)
  return(y1_loc)
}

#1982 starts in 22   
func_location_col_y_i(1982)  

################## YEAR FINAL


func_location_col_y_f= function (x) {
  y2_loc= col_start_serie + ((x -1 - serie_yrI) * 12)+(12)-1
  return(y2_loc)
}

##1981 ends in 21 
func_location_col_y_f(1982) 

#############################################

summary(temporary_file$year_f)

temporary_file$year_f[temporary_file$year_f== 2020] <- 2020
summary(temporary_file$year_f)

###########################################
#yr_i_loc col 8
#yr_f_loc col 9

for (i in temporary_file[,8:9]) {
  temporary_file$yr_i_loc <- func_location_col_y_i(temporary_file$year_i) 
  temporary_file$yr_f_loc  <- func_location_col_y_f(temporary_file$year_f) 
  
}

############################

temporary_file

colnames(temporary_file[262])
colnames(temporary_file[309])

######################################

temporary_file1=temporary_file

##########################################
df_dws=NULL

for (i in 1:nrow(temporary_file1)) {
  
  df_dws[i]=cbind.data.frame(t(temporary_file1[i,temporary_file1$yr_i_loc[i] :temporary_file1$yr_f_loc[i] ]))
  
}

df_dws
as.data.frame(df_dws[[4]])

df_dws1=lapply(df_dws,data.frame) 

##############################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month = names(temporary_file1)[temporary_file1$yr_i_loc[i]:temporary_file1$yr_f_loc[i]]
}



df_dws1

#####################################################################

for (i in 1:length(df_dws1)) { 
  
  df_dws1[[i]]$year_month= sub(x =df_dws1[[i]]$year_month , pattern = "...$", replacement = "")  
  
}

df_dws1

###########################################################

data_downsc=NULL

for (i in 1:length(df_dws1)) {
  
  data_downsc[i] = df_dws1[[i]] %>% group_by(year_month) %>%  summarize(mean_clim=mean(X..i..)) %>% 
    summarise(downscaled_clim=mean(mean_clim))
  
}

data_downsc

###############################
data_downsc1=lapply(data_downsc,data.frame) 
data_downsc1

data_downsc12=do.call("rbind", data_downsc1)
data_downsc12

names(data_downsc12)=c("downsc_tmx")
data_downsc12

df_all_climate_vars=cbind.data.frame(df_map_mat_cwd,data_downsc12 )
df_all_climate_vars

# write.csv(df_all_climate_vars,"./outputs/downscaled_climate_cru_wc.csv")
write.csv(df_all_climate_vars,"./outputs/downscaled_climate_cru_wc_str.csv")

