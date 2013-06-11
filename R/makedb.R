
## [[file:~/projects/hyperq/hyperq.org::*libraries][libraries:1]]

rm(list = ls())
require("mmap")
require("rindex")
require("plyr")
require(stringr)

## libraries:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*variables][variables:1]]

# stream with field header
raw.stream = "streamqh"
# where the mmap db will be located
db.path = paste("data/",raw.stream,"/",sep="")
# mmap of the entire row
main.filename = paste("data/",raw.stream,"/main.data",sep="")
# file containing the raw feed
file.csv.data = paste("data/",raw.stream,".txt",sep="")
# maximum character length of the event field
event.size = 12
# maximum character length of the id field
id.size = 12

## variables:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*slurp%20in%20raw%20data%20(mmap)][slurp\ in\ raw\ data\ \(mmap\):1]]

my.mmap.csv = function(file,
  file.mmap = NA,
  header = TRUE, 
  sep = ",", 
  quote = "\"", 
  dec = ".", 
  fill = TRUE, 
  comment.char = "", 
  row.names,
  actualColClasses = NA,
  ...)
{
    ncols <- length(gregexpr(sep, readLines(file, 1))[[1]]) + 
        1
    mcsv <- tempfile()
    tmplist <- vector("list", ncols)
    cnames <- character(ncols)
    if (!missing(row.names) && is.numeric(row.names) && length(row.names) == 
        1L) 
        ncols <- ncols - 1
    for (col in 1:ncols) {
        colclasses <- rep("NULL", ncols)
        if (!missing(actualColClasses)) {
          colclasses[col] <- actualColClasses[col]
        } else {
          colclasses[col] <- NA
        } 
        clm <- read.table(file = file, header = header, sep = sep, 
            quote = quote, dec = dec, fill = fill, comment.char = comment.char, 
            colClasses = colclasses, stringsAsFactors = FALSE, 
            row.names = row.names, ...)
        cnames[col] <- colnames(clm)
        tmplist[[col]] <- as.mmap(clm[, 1], force = TRUE)
    }
    stype <- do.call(struct, lapply(tmplist, function(X) X$storage.mode))
    totalsize <- sum(sapply(tmplist, nbytes))
    if (is.na(file.mmap)) {
      tmpstruct <- tempfile()
    } else {
      tmpstruct = file.mmap
    }
    writeBin(raw(totalsize), tmpstruct)
    tmpstruct <- mmap(tmpstruct, stype)
    for (col in 1:ncols) {
        tmpstruct[, col] <- tmplist[[col]][]
    }
    colnames(tmpstruct) <- cnames
    extractFUN(tmpstruct) <- as.data.frame
    tmpstruct
}

## slurp\ in\ raw\ data\ \(mmap\):1 ends here

## [[file:~/projects/hyperq/hyperq.org::*slurp%20in%20raw%20data%20(mmap)][slurp\ in\ raw\ data\ \(mmap\):1]]

dir.create(db.path)

colclasses = as.vector(c("character", "character", "character", "numeric", "integer", "character",
  "numeric", "integer", "character", "integer", "numeric", "integer", "character",
  "numeric", "integer", "character", "character", "integer", "character", "character","character"))

m = my.mmap.csv(file=file.csv.data, file.mmap=main.filename, header=TRUE, actualColClasses=colclasses)
head(m)
st = m$storage.mode
ticker.length =  nbytes(st$Symbol) - 1

## slurp\ in\ raw\ data\ \(mmap\):1 ends here

## [[file:~/projects/hyperq/hyperq.org::*create%20mmaps%20for%20each%20column][create\ mmaps\ for\ each\ column:1]]

stream = NULL
stream$stamp = as.mmap(as.double(strptime(m[]$Stamp, "%H:%M:%OS",tz="GMT")),file=paste(db.path,"stamp.data",sep=""), mode=double())
stream$code = as.mmap(as.character(m[]$Code),file=paste(db.path,"code.data",sep=""), mode=char(1))
stream$symbol = as.mmap(as.character(m[]$Symbol),file=paste(db.path,"symbol.data",sep=""), mode=char(ticker.length))
stream$trade = as.mmap(m[]$Most.Recent.Trade,file=paste(db.path,"trade.data",sep=""), mode=double())
stream$vol = as.mmap(m[]$Most.Recent.Trade.Size,file=paste(db.path,"vol.data",sep=""), mode=integer())
stream$tradetime = as.mmap(as.double(strptime(as.character(m[]$Most.Recent.Trade.TimeMS), "%H:%M:%OS",tz="GMT")),file=paste(db.path,"tradetime.data",sep=""), mode=double())
stream$tradeex = as.mmap(m[]$Extended.Trade,file=paste(db.path,"tradeex.data",sep=""), mode=double())
stream$volex = as.mmap(m[]$Extended.Trade.Size,file=paste(db.path,"volex.data",sep=""), mode=integer())
stream$tradetimeex = as.mmap(as.double(strptime(as.character(m[]$Extended.Trade.TimeMS), "%H:%M:%OS",tz="GMT")),file=paste(db.path,"tradetimeex.data",sep=""), mode=double())
stream$voltot = as.mmap(m[]$Total.Volume,file=paste(db.path,"voltot.data",sep=""), mode=integer())
stream$bid = as.mmap(m[]$Bid,file=paste(db.path,"bid.data",sep=""), mode=double())
stream$bidvol = as.mmap(m[]$Bid.Size,file=paste(db.path,"bidvol.data",sep=""), mode=integer())
stream$bidtime = as.mmap(as.double(strptime(as.character(m[]$Bid.TimeMS), "%H:%M:%OS",tz="GMT")),file=paste(db.path,"bidtime.data",sep=""), mode=double())
stream$ask = as.mmap(m[]$Ask,file=paste(db.path,"ask.data",sep=""), mode=double())
stream$askvol = as.mmap(m[]$Ask.Size,file=paste(db.path,"askvol.data",sep=""), mode=integer())
stream$asktime = as.mmap(as.double(strptime(as.character(m[]$Ask.TimeMS), "%H:%M:%OS",tz="GMT")),file=paste(db.path,"asktime.data",sep=""), mode=double())
stream$event = as.mmap( str_pad(as.character(m[]$Message.Contents), event.size, side = "right", pad = " "),file=paste(db.path,"event.data",sep=""), mode=char(event.size))
stream$id = as.mmap(m[]$TickID,file=paste(db.path,"id.data",sep=""), mode=integer())

## create\ mmaps\ for\ each\ column:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*create%20indices%20using%20rindex][create\ indices\ using\ rindex:1]]

require(rindex)
ind.stamp = index(as.character(stream$stamp[]))
ind.symbol = index(stream$symbol[])
ind.event = index(stream$event[])
ind.id = index(str_pad(as.character(stream$id[]), id.size, side = "left", pad = " "))

## create\ indices\ using\ rindex:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*save%20indexes][save\ indexes:1]]

fields = names(stream)
save(list = c("ind.stamp",
       "ind.symbol",
       "ind.event",
       "ind.id",
       "fields",
       "main.filename",
       "st",
       "ticker.length",
       "event.size",
       "id.size"
       ),
     file = paste(db.path,".Rdbinfo",sep=""))

## save\ indexes:1 ends here
