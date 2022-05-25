rm(list=ls())
#source("D:/Users/climate_dashboardf/Dropbox/Vulnerability_Index_Generator/R/census_api_funcdefs.R")
library(censusapi)
library("sf")
library("leaflet")
library(tidyverse)
library(Rfast)

#API reference
#https://api.census.gov/data/2010/dec/sf1/variables.html

ct2010.sex.and.age <- sf::st_read(dsn = "D:/Users/climate_dashboard/Documents/SuperPANCHO/data/input_files/Census/ACS_2018_5YR_TRACT.gdb", layer = "X01_AGE_AND_SEX")

#total pop is B01001e1
ct2010 <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/input_files/Census/ACS_2018_5YR_TRACT.gdb", layer="ACS_2018_5YR_TRACT")
#full.county.fips.list <- unique(paste(ct2010$STATEFP, ct2010$COUNTYFP, sep=""))
state.fips.list <- unique(ct2010$STATEFP)
counties.sp <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/input_files/Census/tlgdb_2019_a_us_substategeo.gdb", layer="County")

hispanic.by.origin        <- c("B03001_003E", "B03001_004E", "B03001_005E", "B03001_006E", "B03001_007E", "B03001_009E", "B03001_010E", "B03001_011E",
                               "B03001_012E", "B03001_013E", "B03001_014E", "B03001_017E", "B03001_018E", "B03001_019E", "B03001_020E", "B03001_021E",
                               "B03001_022E", "B03001_023E", "B03001_024E", "B03001_025E", "B03001_028E", "B03001_031E", "B03001_001E")
length(hispanic.by.origin)
hispanic.by.origin.labels <- c("Total Hispanic or Latino","México","Puerto Rico","Cuba","Dominican Republic","Costa Rica","Guatemala","Honduras",
                               "Nicaragua","Panamá","El Salvador","Argentina","Bolivia","Chile","Colombia","Ecuador","Paraguay",
                               "Perú", "Uruguay","Venezuela","España", "All Other Hispanic or Latino")
length(hispanic.by.origin.labels)
rounding.factor <- 3

state.fips  <- "04"
#county.fips <- "013"
#full.county.fips <- "04013"
get.latino.origin.variables.in.counties <- function(state.fips,.hispanic.by.origin=hispanic.by.origin)
{
  #state and county
  #&for=county:001&in=state:01
  #state.fips  <- substr(full.county.fips,1,2)
  #county.fips <- substr(full.county.fips,3,5)
  region      <- "county:*"
  #region      <- paste("county:", county.fips, sep="")
  regionin    <- paste("state:",  state.fips, sep="")
  #regionin    <- paste("state:", state.fips, "+county:",county.fips, "+tract:*", sep="")
  #regionin    <- paste("state:", state.fips, "+county:",county.fips, sep="")


  key <- 'dbd3d3b2b607fa114d4de35863749d042d19c8f6'
  #https://api.census.gov/data/2017/acs/acs5/groups/B03001.html
  #B03001_003E total hispanic or latino
  #B03001_004E Mexican
  #B03001_005E Puerto Rican
  #B03001_006E Cuban
  #B03001_007E Dominican
  #B03001_009E Costa Rican
  #B03001_010E Guatemalan
  #B03001_011E Honduran
  #B03001_012E Nicaraguan
  #B03001_013E Panamanian
  #B03001_014E Salvadoran
  #B03001_017E Argentinean
  #B03001_018E Bolivian
  #B03001_019E Chilean
  #B03001_020E Colombian
  #B03001_021E Ecuadorian
  #B03001_022E Paraguayan
  #B03001_023E Peruvian
  #B03001_024E Uruguayan
  #B03001_025E Venezuelan
  #B03001_028E Spaniard
  #B03001_031E All Other Hispanic or Latino
  #B03001_001E Total
  
  # hispanic.by.origin        <- c("B03001_003E", "B03001_004E", "B03001_005E", "B03001_006E", "B03001_007E", "B03001_009E", "B03001_010E", "B03001_011E",
  #                                "B03001_012E", "B03001_013E", "B03001_014E", "B03001_017E", "B03001_018E", "B03001_019E", "B03001_020E", "B03001_021E",
  #                                "B03001_022E", "B03001_023E", "B03001_024E", "B03001_025E", "B03001_028E", "B03001_031E", "B03001_001E")
  name        <- 'acs/acs5'
  vintage     <- 2017

  hispanic.by.origin.df           <- getCensus(name=name, vintage=vintage,key=key,vars=.hispanic.by.origin,region=region, regionin=regionin)
  hispanic.by.origin.df$GEOID     <- paste(hispanic.by.origin.df$state, hispanic.by.origin.df$county, hispanic.by.origin.df$tract,sep="")
  hispanic.by.origin.df$per_mex   <- round(hispanic.by.origin.df$B03001_004E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_pur   <- round(hispanic.by.origin.df$B03001_005E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_cub   <- round(hispanic.by.origin.df$B03001_006E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_dom   <- round(hispanic.by.origin.df$B03001_007E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_crc   <- round(hispanic.by.origin.df$B03001_009E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_gua   <- round(hispanic.by.origin.df$B03001_010E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_hon   <- round(hispanic.by.origin.df$B03001_011E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_nic   <- round(hispanic.by.origin.df$B03001_012E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_pan   <- round(hispanic.by.origin.df$B03001_013E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_sal   <- round(hispanic.by.origin.df$B03001_014E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_arg   <- round(hispanic.by.origin.df$B03001_017E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_bol   <- round(hispanic.by.origin.df$B03001_018E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_chi   <- round(hispanic.by.origin.df$B03001_019E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_col   <- round(hispanic.by.origin.df$B03001_020E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_ecu   <- round(hispanic.by.origin.df$B03001_021E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_par   <- round(hispanic.by.origin.df$B03001_022E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_per   <- round(hispanic.by.origin.df$B03001_023E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_uru   <- round(hispanic.by.origin.df$B03001_024E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_ven   <- round(hispanic.by.origin.df$B03001_025E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_spa   <- round(hispanic.by.origin.df$B03001_028E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  hispanic.by.origin.df$per_other <- round(hispanic.by.origin.df$B03001_031E/hispanic.by.origin.df$B03001_001E * 100,rounding.factor)
  
  v <- listCensusMetadata(name = name,
                             vintage=vintage,
                             type="variables",
                             group="B03001") 
  #View(v)
  return(hispanic.by.origin.df)
}

#all.df <- get.census.tract.ej.variables(full.2county.fips.list[1])
all.df <- data.frame()
errors.list <- list()
#for(i in seq(1,length(full.county.fips.list[c(1:10)])))
#for(i in seq(1,length(state.fips.list[c(1:5)])))
for(i in seq(1,length(state.fips.list)))
{
  print(paste(state.fips.list[i], ":", i, "/", length(state.fips.list), sep=""))
  
  tryCatch(
    {
      current <- get.latino.origin.variables.in.counties(state.fips.list[i])
    },
    error=function(cond) {
      message(paste("API error in county FIPS ",state.fips.list[i],sep=""))
      current <- data.frame()
      rlist::list.append(errors.list,state.fips.list[i])
    },
    finally={
      #print('finally')
      #print(nrow(current))
      all.df <- rbind(all.df, current)
    }
  )
}

nrow(all.df)

#extract percent and total columns

percent.columns <- c("per_mex","per_pur","per_cub","per_dom","per_crc","per_gua","per_hon","per_nic","per_pan","per_sal","per_arg","per_bol",
                     "per_chi","per_col","per_ecu","per_par","per_per","per_uru","per_ven","per_spa","per_other")

#percent.columns <- c(27:47)

df<- all.df[,percent.columns]
head(df)
head(all.df)
#calculate percent of total population that is latino
all.df$per_lat <- round(all.df$B03001_003E/all.df$B03001_001E * 100, rounding.factor)

#row<-df[1,]
#n=2
#col.names=colnames(df)
nth.largest <- function(row,n=1,col.names=colnames(df))
{
  #print(row)
  #create sort vector from max to min
  sort.vector <- order(row, decreasing=T)
  #sort the row
  row.sorted <- row[sort.vector]
  #sort the colnames
  col.names <- col.names[sort.vector]
  #get the nth value from the sorted row
  value <- dplyr::nth(row.sorted,n=n)
  #get the column name
  col <- col.names[which(row.sorted==value)]
  
  #resolve tie by taking the first one
  col <- col[1]
  #sorted <- sort(x = row,decreasing = T)
  return(col)
}

#calculate largest national origin
first.largest <- apply(df, MARGIN=1, FUN=nth.largest, n=1)
all.df$largest <- first.largest
#recode largest column values
all.df$largest.r <- recode(all.df$largest,
                           "per_mex" = hispanic.by.origin.labels[2],
                           "per_pur" = hispanic.by.origin.labels[3],
                           "per_cub" = hispanic.by.origin.labels[4],
                           "per_dom" = hispanic.by.origin.labels[5],
                           "per_crc" = hispanic.by.origin.labels[6],
                           "per_gua" = hispanic.by.origin.labels[7],
                           "per_hon" = hispanic.by.origin.labels[8],
                           "per_nic" = hispanic.by.origin.labels[9],
                           "per_pan" = hispanic.by.origin.labels[10],
                           "per_sal" = hispanic.by.origin.labels[11],
                           "per_arg" = hispanic.by.origin.labels[12],
                           "per_bol" = hispanic.by.origin.labels[13],
                           "per_chi" = hispanic.by.origin.labels[14],
                           "per_col" = hispanic.by.origin.labels[15],
                           "per_ecu" = hispanic.by.origin.labels[16],
                           "per_par" = hispanic.by.origin.labels[17],
                           "per_per" = hispanic.by.origin.labels[18],
                           "per_uru" = hispanic.by.origin.labels[19],
                           "per_ven" = hispanic.by.origin.labels[20],
                           "per_spa" = hispanic.by.origin.labels[21],
                           "per_other" = hispanic.by.origin.labels[22]
                           )
hispanic.by.origin        <- c("B03001_003E", "B03001_004E", "B03001_005E", "B03001_006E", "B03001_007E", "B03001_009E", "B03001_010E", "B03001_011E",
                               "B03001_012E", "B03001_013E", "B03001_014E", "B03001_017E", "B03001_018E", "B03001_019E", "B03001_020E", "B03001_021E",
                               "B03001_022E", "B03001_023E", "B03001_024E", "B03001_025E", "B03001_028E", "B03001_031E", "B03001_001E")

counties.join.sp       <- sp::merge(counties.sp[,-which(names(counties.sp) %in% c("FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON","COUNTYNS","CLASSFP"))],all.df, by="GEOID")
total.lat.pop.column       <- which(colnames(counties.join.sp@data)==hispanic.by.origin[1])
total.mex.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[2])
total.pur.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[3])
total.cub.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[4])
total.dom.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[5])
total.crc.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[6])
total.gua.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[7])
total.hon.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[8])
total.nic.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[9])
total.pan.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[10])
total.sal.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[11])
total.arg.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[12])
total.bol.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[13])
total.chi.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[14])
total.col.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[15])
total.ecu.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[16])
total.par.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[17])
total.per.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[18])
total.uru.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[19])
total.ven.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[20])
total.spa.pop.column   <- which(colnames(counties.join.sp@data)==hispanic.by.origin[21])
total.other.pop.column <- which(colnames(counties.join.sp@data)==hispanic.by.origin[22])
total.pop.column       <- which(colnames(counties.join.sp@data)==hispanic.by.origin[23])


colnames(counties.join.sp@data)[total.lat.pop.column]    <- "tot_lat"
colnames(counties.join.sp@data)[total.pop.column]        <- "tot_pop"
colnames(counties.join.sp@data)[total.mex.pop.column]    <- "tot_mex"
colnames(counties.join.sp@data)[total.pur.pop.column]    <- "tot_pur"
colnames(counties.join.sp@data)[total.cub.pop.column]    <- "tot_cub"
colnames(counties.join.sp@data)[total.dom.pop.column]    <- "tot_dom"
colnames(counties.join.sp@data)[total.crc.pop.column]    <- "tot_crc"
colnames(counties.join.sp@data)[total.gua.pop.column]    <- "tot_gua"
colnames(counties.join.sp@data)[total.hon.pop.column]    <- "tot_hon"
colnames(counties.join.sp@data)[total.nic.pop.column]    <- "tot_nic"
colnames(counties.join.sp@data)[total.pan.pop.column]    <- "tot_pan"
colnames(counties.join.sp@data)[total.sal.pop.column]    <- "tot_sal"
colnames(counties.join.sp@data)[total.arg.pop.column]    <- "tot_arg"
colnames(counties.join.sp@data)[total.bol.pop.column]    <- "tot_bol"
colnames(counties.join.sp@data)[total.chi.pop.column]    <- "tot_chi"
colnames(counties.join.sp@data)[total.col.pop.column]    <- "tot_col"
colnames(counties.join.sp@data)[total.ecu.pop.column]    <- "tot_ecu"
colnames(counties.join.sp@data)[total.par.pop.column]    <- "tot_par"
colnames(counties.join.sp@data)[total.per.pop.column]    <- "tot_per"
colnames(counties.join.sp@data)[total.uru.pop.column]    <- "tot_uru"
colnames(counties.join.sp@data)[total.ven.pop.column]    <- "tot_ven"
colnames(counties.join.sp@data)[total.spa.pop.column]    <- "tot_spa"
colnames(counties.join.sp@data)[total.other.pop.column]  <- "tot_other"


#write as CSV
write.csv(counties.join.sp@data,"D:/Users/climate_dashboard/Documents/SuperPANCHO/data/output/county_latino_natl_origin.csv",row.names = F)

rgdal::writeOGR(counties.join.sp,dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/output", layer="county_latino_natl_origin", driver="ESRI Shapefile",overwrite_layer = T)

#read it back
#counties.join.sp.read <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/output", layer="county_latino_natl_origin")
#View(table(counties.join.sp.read$GEOID))

head(counties.join.sp@data)
counties.join.sp@data[counties.join.sp@data$GEOID=="04013",]                
