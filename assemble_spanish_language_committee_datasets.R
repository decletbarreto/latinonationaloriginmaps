rm(list=ls())
#assemble data for Spanish Language Committee map
library("xlsx")
library(dplyr)

dropbox.output.dir <- "D:/Users/climate_dashboard/Dropbox/SuperPANCHO/latinx_population_and_climate_impacts_map" 

#killer heat
county_heat.sp <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/input_files/killer_heat", layer="county_heat",stringsAsFactors = F)
#cast into numeric
county_heat.sp$hist_90  <- as.numeric(as.character(county_heat.sp$hist_90))
county_heat.sp$hist_100 <- as.numeric(as.character(county_heat.sp$hist_100))
county_heat.sp$hist_105 <- as.numeric(as.character(county_heat.sp$hist_105))
county_heat.sp$hist_OTC <- as.numeric(as.character(county_heat.sp$hist_OTC))

county_heat.sp$mid45_90  <- as.numeric(as.character(county_heat.sp$mid45_90))
county_heat.sp$mid45_100 <- as.numeric(as.character(county_heat.sp$mid45_100))
county_heat.sp$mid45_105 <- as.numeric(as.character(county_heat.sp$mid45_105))
county_heat.sp$mid45_OTC <- as.numeric(as.character(county_heat.sp$mid45_OTC))

county_heat.sp$mid85_90  <- as.numeric(as.character(county_heat.sp$mid85_90))
county_heat.sp$mid85_100 <- as.numeric(as.character(county_heat.sp$mid85_100))
county_heat.sp$mid85_105 <- as.numeric(as.character(county_heat.sp$mid85_105))
county_heat.sp$mid85_OTC <- as.numeric(as.character(county_heat.sp$mid85_OTC))

county_heat.sp$late45_90  <- as.numeric(as.character(county_heat.sp$late45_90))
county_heat.sp$late45_100 <- as.numeric(as.character(county_heat.sp$late45_100))
county_heat.sp$late45_105 <- as.numeric(as.character(county_heat.sp$late45_105))
county_heat.sp$late45_OTC <- as.numeric(as.character(county_heat.sp$late45_OTC))

county_heat.sp$late85_90  <- as.numeric(as.character(county_heat.sp$late85_90))
county_heat.sp$late85_100 <- as.numeric(as.character(county_heat.sp$late85_100))
county_heat.sp$late85_105 <- as.numeric(as.character(county_heat.sp$late85_105))
county_heat.sp$late85_OTC <- as.numeric(as.character(county_heat.sp$late85_OTC))

county_heat.sp$paris_90  <- as.numeric(as.character(county_heat.sp$paris_90))
county_heat.sp$paris_100 <- as.numeric(as.character(county_heat.sp$paris_100))
county_heat.sp$paris_105 <- as.numeric(as.character(county_heat.sp$paris_105))
county_heat.sp$paris_OTC <- as.numeric(as.character(county_heat.sp$paris_OTC))

#latino population by national origin
latino.sp <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/output", layer="county_latino_natl_origin")
colnames(latino.sp@data)

all.sp <- sp::merge(county_heat.sp,latino.sp@data,by.x="GEOID10", by.y="GEOID")


#underwater data aggregated to counties
underwater.2045.sp <- rgdal::readOGR(dsn="D:/Users/climate_dashboard/Documents/SuperPANCHO/data/input_files/underwater", layer="county_sj_zip_code_stats_2045_NCAH_051418")
underwater.2045.sp$TotProps   <- as.numeric(as.character(underwater.2045.sp$TotProps))
underwater.2045.sp$Pop65_69   <- as.numeric(as.character(underwater.2045.sp$Pop65_69))
underwater.2045.sp$Pop70_74   <- as.numeric(as.character(underwater.2045.sp$Pop70_74))
underwater.2045.sp$Pop75_79   <- as.numeric(as.character(underwater.2045.sp$Pop75_79))
underwater.2045.sp$Pop80_84   <- as.numeric(as.character(underwater.2045.sp$Pop80_84))
underwater.2045.sp$Pop85up    <- as.numeric(as.character(underwater.2045.sp$Pop85up))
underwater.2045.sp$PopAfAm    <- as.numeric(as.character(underwater.2045.sp$PopAfAm))
underwater.2045.sp$PopNatAm   <- as.numeric(as.character(underwater.2045.sp$PopNatAm))
underwater.2045.sp$PopHisp    <- as.numeric(as.character(underwater.2045.sp$PopHisp))
underwater.2045.sp$PopWithPov <- as.numeric(as.character(underwater.2045.sp$PopWithPov))
underwater.2045.sp$PopBelowPo <- as.numeric(as.character(underwater.2045.sp$PopBelowPo))

underwater.cols <- c("GEOID","TotProps","Pop65_69","Pop70_74","Pop75_79","Pop80_84","Pop85up","PopAfAm","PopNatAm","PopHisp","PopWithPov","PopBelowPo")
all.sp <- sp::merge(all.sp,underwater.2045.sp[,underwater.cols]@data,by.x="GEOID10", by.y="GEOID")

#write final shapefile to Dropbox
rgdal::writeOGR(all.sp, dsn=dropbox.output.dir, layer="county_latino_climate_impacts", driver="ESRI Shapefile",overwrite_layer = T)
#zip up and write to Dropbox
files <- list.files(path=dropbox.output.dir, pattern="county_latino_climate_impacts*", full.names = T)
zip.filename <- paste(dropbox.output.dir, "county_latino_climate_impacts.zip", sep="/")
zip(zipfile = zip.filename, files = files, flags="-j")

colnames(all.sp@data)
getwd()
#write as an Excel spreadsheet
spreadsheet.df <- all.sp@data
#get the KH columns
spreadsheet.kh.cols <- colnames(spreadsheet.df)[c(6:29)]
#and the labels that will replace them
spreadsheet.kh.labels   <- c("Historical 90", "Historical 100", "Historical 105", "Historical OTC",
                             "Slow Action Mid-century 90", "Slow Action Mid-century 100", "Slow Action Mid-century 105", "Slow Action Mid-century OTC",
                             "No Action Mid-century 90", "No Action Mid-century 100", "No Action Mid-century 105", "No Action Mid-century OTC",
                             "Slow Action Late-century 90", "Slow Action Late-century 100", "Slow Action Late-century 105", "Slow Action Late-century OTC",
                             "No Action Late-century 90", "No Action Late-century 100", "No Action Late-century 105", "No Action Late-century OTC",
                             "Rapid Action 90", "Rapid Action 100", "Rapid Action 105", "Rapid Action OTC")
#check that they line up
cbind(spreadsheet.kh.cols,spreadsheet.kh.labels)
#concatentate national origin, KH columns
spreadsheet.latino.cols <- c("NAMELSAD10", "State_abbr","tot_lat","per_lat","largest","per_arg","per_bol","per_chi","per_col","per_crc",
                             "per_cub","per_ecu","per_spa","per_gua","per_hon","per_pur","per_mex","per_nic","per_pan","per_par", "per_per",
                             "per_sal","per_dom","per_uru","per_ven","per_thr")
# latino.total.cols   <- c("B03001_004", "B03001_005", "B03001_006", "B03001_007", "B03001_009", "B03001_010",
#                       "B03001_011", "B03001_012", "B03001_013", "B03001_014", "B03001_017", "B03001_018",
#                       "B03001_019", "B03001_020", "B03001_021", "B03001_022", "B03001_023", "B03001_024",
#                       "B03001_025", "B03001_028", "B03001_031")
latino.total.cols   <- c("tot_lat", "tot_pop", "tot_mex", "tot_pur", "tot_cub", "tot_dom",
                         "tot_crc", "tot_gua", "tot_hon", "tot_nic", "tot_pan", "tot_sal",
                         "tot_arg", "tot_bol", "tot_chi", "tot_col", "tot_ecu", "tot_par",
                         "tot_per", "tot_uru", "tot_ven", "tot_spa", "tot_thr")
spreadsheet.cols <- c(spreadsheet.latino.cols, spreadsheet.kh.cols)
#subset only desired columns
spreadsheet.df <- spreadsheet.df[,spreadsheet.cols]
colnames(spreadsheet.df)
#recode "largest" column 
spreadsheet.latino.labels <- c("Total Hispanic or Latinx","México","Puerto Rico","Cuba","Dominican Republic","Costa Rica","Guatemala","Honduras",
                               "Nicaragua","Panamá","El Salvador","Argentina","Bolivia","Chile","Colombia","Ecuador","Paraguay",
                               "Perú", "Uruguay","Venezuela","España", "All Other Hispanic or Latinx")
spreadsheet.df$largest <- recode(spreadsheet.df$largest,
                                 "per_mex" = spreadsheet.latino.labels[2],
                                 "per_pur" = spreadsheet.latino.labels[3],
                                 "per_cub" = spreadsheet.latino.labels[4],
                                 "per_dom" = spreadsheet.latino.labels[5],
                                 "per_crc" = spreadsheet.latino.labels[6],
                                 "per_gua" = spreadsheet.latino.labels[7],
                                 "per_hon" = spreadsheet.latino.labels[8],
                                 "per_nic" = spreadsheet.latino.labels[9],
                                 "per_pan" = spreadsheet.latino.labels[10],
                                 "per_sal" = spreadsheet.latino.labels[11],
                                 "per_arg" = spreadsheet.latino.labels[12],
                                 "per_bol" = spreadsheet.latino.labels[13],
                                 "per_chi" = spreadsheet.latino.labels[14],
                                 "per_col" = spreadsheet.latino.labels[15],
                                 "per_ecu" = spreadsheet.latino.labels[16],
                                 "per_par" = spreadsheet.latino.labels[17],
                                 "per_per" = spreadsheet.latino.labels[18],
                                 "per_uru" = spreadsheet.latino.labels[19],
                                 "per_ven" = spreadsheet.latino.labels[20],
                                 "per_spa" = spreadsheet.latino.labels[21],
                                 "per_other" = spreadsheet.latino.labels[22])
spreadsheet.col.labels <- c("County","State","Total Hispanic or Latinx","Percent Hispanic or Latinx","Largest Latinx Group",
                            "Percent Argentina","Percent Bolivia","Percent Chile","Percent Colombia","Percent Costa Rica","Percent Cuba", "Percent Ecuador", "Percent España",
                            "Percent Guatemala","Percent Honduras","Percent Puerto Rico","Percent México","Percent Nicaragua","Percent Panamá","Percent Paraguay","Percent Perú","Percent El Salvador","Percent República Dominicana",
                            "Percent Uruguay","Percent Venezuela","Percent All Other Hispanic or Latinx")
#check that it's right
cbind(spreadsheet.col.labels,spreadsheet.latino.cols)
colnames(spreadsheet.df) <- c(spreadsheet.col.labels, spreadsheet.kh.labels)
excel.sheet.dropbox <- paste(dropbox.output.dir, "county_latino_climate_impacts.xlsx",sep="/")

write.xlsx(spreadsheet.df, excel.sheet.dropbox, 
           sheetName="county data", 
           col.names=TRUE, row.names=F, append=FALSE)
