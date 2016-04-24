library(sp)
library(raster)

# Directory of the script
LandCoverMapAnlz.DIR <- "/PARH/TO/MELCA/"

# Load
source(paste(LandCoverMapAnlz.DIR,"lcPlotMaps.R",sep=""))
source(paste(LandCoverMapAnlz.DIR,"landcoverAnlz.R",sep=""))
source(paste(LandCoverMapAnlz.DIR,"areaStat.R",sep=""))

#UMD Land Cover Classification
Output.DIR <- "/PATH/TO/Outputs/"
lc.data <- raster("/PATH/TO/AVHRR_8km_LANDCOVER_1981_1994.GLOBAL.tif")
Results.PolyTrend.DIR <-"/PATH/TO/Results/"
Results.DBEST.DIR <-"/PATH/TO/Outputs/"

# files to map PolyTrend
Results.PolyTrend.Direction <- paste(Results.PolyTrend.DIR,"1982Jan2010Dec.Direction.PolyTrends.tif",sep="")
Results.PolyTrend.Significance <- paste(Results.PolyTrend.DIR,"1982Jan2010Dec.Significance.PolyTrends.tif",sep="")
Results.PolyTrend.TrendType <- paste(Results.PolyTrend.DIR,"1982Jan2010Dec.TrendType.PolyTrends.tif",sep="")

# files to map DBEST
Results.DBEST.Significance <- paste(Results.DBEST.DIR,"1982Jan2010Dec.Significance.tif",sep="")
Results.DBEST.ChangeType <- paste(Results.DBEST.DIR,"1982Jan2010Dec.ChangeType.tif",sep="")
Results.DBEST.StartYear <- paste(Results.DBEST.DIR,"1982Jan2010Dec.StartYear.tif",sep="")
Results.DBEST.Duration <- paste(Results.DBEST.DIR,"1982Jan2010Dec.Duration.tif",sep="")
Results.DBEST.EndYear <- paste(Results.DBEST.DIR,"1982Jan2010Dec.EndYear.tif",sep="")
Results.DBEST.Change <- paste(Results.DBEST.DIR,"1982Jan2010Dec.Change.tif",sep="")

region.countries <- list (  "BHR", ## Bahrain
                            "CYP", ## Cyprus
                            "EGY", ## Egypt
                            "IRN", ## Iran
                            "IRQ", ## Iraq
                            "ISR", ## Israel
                            "JOR", ## Jordan
                            "KWT", ## Kuwait
                            "LBN", ## Lebanon
                            "OMN", ## Oman
                            "PSE", ## Palestine
                            "QAT", ## Qatar
                            "SAU", ## Saudi Arabia
                            "SYR", ## Syria
                            "TUR", ## Turkey
                            "ARE", ## United Arab Emirates
                            "YEM") ## Yemen

# land-cover according to http://glcf.umd.edu/data/landcover/
lc.lables <- c(
        "Water", 
        "Evergreen Needleleaf Forest",
        "Deciduous Broadleaf Forest", 
        "Mixed Forest", 
        "Woodland",
        "Wooded Grassland", 
        "Closed Shrubland", 
        "Open Shrubland", 
        "Grassland", 
        "Cropland", 
        "Bare Ground", 
        "Permanent Snow and Ice"
)

# land-cover according to http://glcf.umd.edu/data/landcover/
lc.colors <- c(
        rgb(068,079,137, max = 255),
        rgb(001,100,000, max = 255),
        rgb(002,220,000, max = 255),
        rgb(000,255,000, max = 255),
        rgb(146,174,047, max = 255),
        rgb(220,206,000, max = 255),
        rgb(255,173,000, max = 255),
        rgb(255,251,195, max = 255),
        rgb(140,072,009, max = 255),
        rgb(247,165,255, max = 255),
        rgb(255,199,174, max = 255),
        rgb(000,255,255, max = 255)
)

# Load region countries data from GADM.org
region.GADM <- lapply(region.countries,
                   function(x) raster::getData('GADM', country=x, level=0))

# Change polygons IDs to unique values
region.set.IDs <- function(sp,i) spChFIDs(sp,paste(i,row.names(sp@data), sep="."))

# Combine SpatialPolygonDataFrame objects
region.SPDF  <- do.call(rbind,mapply(region.set.IDs,region.GADM, seq_along(region.GADM)))

# Mask with ME region and crop to ME
lc.local <- mask(lc.data,region.SPDF)
lc.local <- crop(lc.local,region.SPDF)

#reclass
m.rcl <- c(-1,0,1,
           0,1,2,
           1,2,NA,
           2,3,NA,
           3,4,3,
           4,5,4,
           5,6,5,
           6,7,6,
           7,8,7,
           8,9,8,
           9,10,9,
           10,11,10,
           11,12,11,
           12,13,12)

m.rcl <- matrix(m.rcl, ncol=3, byrow=TRUE)
lc.rcls <- reclassify(lc.local, m.rcl)

# land cover map
lcPlotMaps(Output.DIR, output.name = "LandCoverMap",region.SPDF, map.raster=lc.rcls,
         map.title=paste("Land Cover in the Middle East", "1981", "-", "1994"), 
         colors=lc.colors,
         legend.cols = 2, 
         legend.lables=lc.lables,
         northarrow.topoffset=4
)

# land cover statistic
areaStat(lc.rcls,lc.lables,Output.DIR, "LandCover8194")

# Generate statistics and maps from land-cover & results

# Trend Direction
Results.PolyTrend.Direction <- raster(Results.PolyTrend.Direction)

cl.values <- c(-1,1)
cl.names <- c("Negative Trends (1982-2010)","Positive Trends (1982-2010)")
cl.filenames <- c("LC_NegativeTrends","LC_PositiveTrends")

areaStat(Results.PolyTrend.Direction,cl.names,Output.DIR,
                                                     "TrendDirections8210")
for(i in 1:length(cl.values)) {

        landcoverAnlz(r.result=Results.PolyTrend.Direction,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
        
}

# Trend Significance
Results.PolyTrend.Significance <- raster(Results.PolyTrend.Significance)

cl.values <- c(-1,1)
cl.names <- c("Insignificant Trends (1982-2010)","Significant Trends (1982-2010)")
cl.filenames <- c("LC_InsignificantTrends","LC_SignificantTrends")

areaStat(Results.PolyTrend.Significance,cl.names,Output.DIR, "TrendSignificance8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.PolyTrend.Significance,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
        
}

#  Trend Type
Results.PolyTrend.TrendType <- raster(Results.PolyTrend.TrendType)

cl.values <- c(-1,0,1,2,3)
cl.names <- c("Concealed Ternds (1982-2010)",
              "No Trends (1982-2010)",
              "Linear Trends (1982-2010)",
              "Quadratic Trends (1982-2010)",
              "Cubic Trends (1982-2010)")
cl.filenames <- c("LC_ConcealedTrends",
                  "LC_NoTrends",
                  "LC_LinearTrends",
                  "LC_QuadraticTrends",
                  "LC_CubicTrends")

areaStat(Results.PolyTrend.TrendType,cl.names,Output.DIR,"TrendType8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.PolyTrend.TrendType,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
        
}

#  DBEST 
Results.DBEST.Significance <- raster(Results.DBEST.Significance)

# Classify insignificant pixels
rcl.insignificant <- Results.DBEST.Significance - 1
rcl.insignificant <- rcl.insignificant * -9999

# ChangeType
Results.DBEST.ChangeType <- raster(Results.DBEST.ChangeType)
Results.DBEST.ChangeType <- Results.DBEST.ChangeType + rcl.insignificant
classes.ChangeType <- c(0,1,1,1,2,2,2,Inf,3)
classes.ChangeType <- matrix(classes.ChangeType, ncol=3, byrow=TRUE)
Results.DBEST.ChangeType <- reclassify(Results.DBEST.ChangeType, classes.ChangeType)

cl.values <- c(0,1,3)
cl.names <- c("Non-Abrupt Changes (1982-2010)",
              "Abrupt Changes (1982-2010)", 
              "Insignificant Changes (1982-2010)")
cl.filenames <- c("LC_NonAbrupt","LC_Abrupt", "LC_InsignificantDBEST")

areaStat(Results.DBEST.ChangeType,cl.names,Output.DIR,"ChangeType8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.DBEST.ChangeType,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
        
}


# Start Year
Results.DBEST.StartYear <- raster(Results.DBEST.StartYear)
Results.DBEST.StartYear <- Results.DBEST.StartYear + rcl.insignificant
classes.StartYear <- c(1981, 1988, 1,  1988, 1995, 2, 1995, 2002, 3, 2002, 2010, 4, 2010, Inf, 5)
classes.StartYear <- matrix(classes.StartYear, ncol=3, byrow=TRUE)
Results.DBEST.StartYear <- reclassify(Results.DBEST.StartYear,
                                                         classes.StartYear)

cl.values <- c(1,2,3,4)
cl.names <- c("Change Start 1982 - 1988","Change Start 1989 - 1995", "Change Start 1996 - 2002", "Change Start 2003 - 2010")
cl.filenames <- c("LC_Start82to88","LC_Start89to95", "LC_Start96to02", "LC_Start03to10")

areaStat(Results.DBEST.StartYear,cl.names,Output.DIR, "StartYear8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.DBEST.StartYear,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
        
}

# Duration
Results.DBEST.Duration <- raster(Results.DBEST.Duration)
Results.DBEST.Duration <- Results.DBEST.Duration + rcl.insignificant
classes.Duration <- c(0, 1, 1, 1, 12, 2, 12, 24, 3, 24, 36,4, 36, 100, 4, 100, Inf, 5)
classes.Duration <- matrix(classes.Duration, ncol=3, byrow=TRUE)
Results.DBEST.Duration <- reclassify(Results.DBEST.Duration, classes.Duration)

cl.values <- c(1,2,3,4)
cl.names <- c("Change Duration 1 Month","Change Duration 2 - 12 Months",
              "Change Duration 12 - 24 Months",
              "Change Duration Above 24 Months")
cl.filenames <- c("LC_Duration1","LC_Duration2to12", "LC_Duration12to24","LC_Above24")

areaStat(Results.DBEST.Duration,cl.names,Output.DIR, "Duration8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.DBEST.Duration,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
}


# EndYear
Results.DBEST.EndYear <- raster(Results.DBEST.EndYear)
Results.DBEST.EndYear <- Results.DBEST.EndYear + rcl.insignificant
classes.EndYear <- c(1981, 1988, 1,  1988, 1995, 2, 1995, 2002, 3, 2002, 2010, 4, 2010, Inf, 5)
classes.EndYear <- matrix(classes.EndYear, ncol=3, byrow=TRUE)
Results.DBEST.EndYear <- reclassify(Results.DBEST.EndYear, classes.EndYear)

cl.values <- c(1,2,3,4)
cl.names <- c("Change End 1982 - 1988","Change End 1989 - 1995", "Change End 1996 - 2002", "Change End 2003 - 2010")
cl.filenames <- c("LC_End82to88","LC_End89to95", "LC_End96to02", "LC_End03to10")

areaStat(Results.DBEST.EndYear,cl.names,Output.DIR, "EndYear8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.DBEST.EndYear,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
}

# Change
Results.DBEST.Change <- raster(Results.DBEST.Change)
Results.DBEST.Change <- Results.DBEST.Change + rcl.insignificant
classes.Change <- c(-0.3, -0.2, 1,  -0.2, -0.1, 2, -0.1, 0, 3, 0, 0.1, 4, 
                      0.1, 0.2, 5, 0.2, 0.3, 6, 0.3, 0.4, 7, 0.4, 0.5, 8, 
                                                    0.5, 1, 9, 1, Inf, 10)
classes.Change <- matrix(classes.Change, ncol=3, byrow=TRUE)
Results.DBEST.Change <- reclassify(Results.DBEST.Change, classes.Change)

cl.values <- c(1,2,3,4,5,6,7,8,9)
cl.names <- c("Change Value -0.3 - -0.2",
                  "Change Value -0.2 - -0.1", "Change Value -0.1 - 0", 
                  "Change Value 0 - 0.1", "Change Value 0.1 - 0.2", 
                  "Change Value 0.2 - 0.3", "Change Value 0.3 - 0.4", 
                  "Change Value 0.4 - 0.5", "Change Value 0.5 - 1")

cl.filenames <- c("LC_Change-0.3to-0.2",
                  "LC_Change-0.2to-0.1", "LC_Change-0.1to0", 
                  "LC_Change0to0.1", "LC_Change0.1to0.2", 
                  "LC_Change0.2to0.3", "LC_Change0.3to0.4", 
                  "LC_Change0.4to0.5", "LC_Change0.5to1")

areaStat(Results.DBEST.Change,cl.names,Output.DIR, "Change8210")
for(i in 1:length(cl.values)) {
        
        landcoverAnlz(r.result=Results.DBEST.Change,
                      lc.rcls = lc.rcls,
                      lc.lables=lc.lables,
                      lc.colors=lc.colors,
                      region.SPDF=region.SPDF,
                      class.filename=cl.filenames[i],
                      class.name=cl.names[i],
                      class.value=cl.values[i],
                      Output.DIR=Output.DIR)
}
```
