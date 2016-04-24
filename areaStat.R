areastat <- function(r.result,cl.names,Output.DIR,filename) {
        
        # calculate area and zonal
        r.result.area <- area(r.result)
        zonalarea <- zonal(r.result.area, r.result, 'sum')
        
        # check if Insignificant class
        check <- dim(zonalarea)[1] - length(cl.names)
        if( check == 1) {
                cl.names <- c(cl.names, "Insignificant")
        }
        
        # save zonal
        rownames(zonalarea) <- cl.names
        out<-capture.output(zonalarea)
        output.statfile <- paste(Output.DIR,filename,".txt",sep="")
        cat(out,file=output.statfile,sep="\n")
        
}

