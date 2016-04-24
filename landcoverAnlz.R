landcoverAnlz <- function(r.result,lc.rcls,lc.lables,lc.colors,
                          region.SPDF,class.filename,class.name,
                          class.value,Output.DIR) {
        
        # crop and resample by using the nearest neighbor
        lc.rcls <- resample(lc.rcls , r.result , method = "ngb" )
        lc.rcls <- crop(lc.rcls,r.result)
        
        # set other classes to NA
        r.class <- r.result
        r.class[r.class[] != class.value] <- NA
        
        # set current class to 1
        r.class[r.class[] == class.value] <- 1
        
        # land-cover x 1
        r.lc.class <- lc.rcls * r.class
        
        # get land-cover labels related to this class only
        lc.class.lables <- lc.lables[unique(r.lc.class)]
        
        # get land-cover colors related to this class only
        lc.class.colors <- lc.colors[unique(r.lc.class)]
        
        # calculate area and zonal
        r.lc.class.area <- area(r.lc.class)
        zonalarea <- zonal(r.lc.class.area, r.lc.class, 'sum')
        
        # save zonal
        rownames(zonalarea) <- lc.class.lables
        out<-capture.output(zonalarea)
        output.statfile <- paste(Output.DIR,class.filename,".txt",sep="")
        cat(out,file=output.statfile,sep="\n")
        
        # plot the map
        landcoverPlotMaps(Output.DIR, output.name = class.filename,
                          region.SPDF, map.raster=r.lc.class,
                          map.title=paste("Middle East Land Cover", " & ", class.name), 
                          colors=lc.class.colors,
                          legend.cols = 2, 
                          legend.lables=lc.class.lables,
                          northarrow.topoffset=4
        )
}
