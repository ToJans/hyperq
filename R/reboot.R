
## [[file:~/projects/hyperq/hyperq.org::*reboot][reboot:1]]

rm(list = ls())
require("mmap")
require("rindex")
require("plyr")
require("stringr")
raw.stream = "streamqh"
# where the mmap db is located
db.path = paste("data/",raw.stream,"/",sep="")

load(paste(db.path,".Rdbinfo",sep=""))
#m = mmap(main.filename, mode=st)
stream = NULL
stream$stamp = mmap(paste(db.path,fields[1],".data",sep=""), mode=double())
stream$code = mmap(paste(db.path,fields[2],".data",sep=""), mode=char(1))
stream$symbol = mmap(paste(db.path,fields[3],".data",sep=""), mode=char(ticker.length))
stream$trade = mmap(paste(db.path,fields[4],".data",sep=""), mode=double())
stream$vol = mmap(paste(db.path,fields[5],".data",sep=""), mode=integer())
stream$tradetime = mmap(paste(db.path,fields[6],".data",sep=""), mode=double())
stream$tradeex = mmap(paste(db.path,fields[7],".data",sep=""), mode=double())
stream$volex = mmap(paste(db.path,fields[8],".data",sep=""), mode=integer())
stream$tradetimeex = mmap(paste(db.path,fields[9],".data",sep=""), mode=double())
stream$voltot = mmap(paste(db.path,fields[10],".data",sep=""), mode=integer())
stream$bid = mmap(paste(db.path,fields[11],".data",sep=""), mode=double())
stream$bidvol = mmap(paste(db.path,fields[12],".data",sep=""), mode=integer())
stream$bidtime = mmap(paste(db.path,fields[13],".data",sep=""), mode=double())
stream$ask = mmap(paste(db.path,fields[14],".data",sep=""), mode=double())
stream$askvol = mmap(paste(db.path,fields[15],".data",sep=""), mode=integer())
stream$asktime = mmap(paste(db.path,fields[16],".data",sep=""), mode=double())
stream$event = mmap(paste(db.path,fields[17],".data",sep=""), mode=char(12))
stream$id = mmap(paste(db.path,fields[18],".data",sep=""), mode=integer())
#save(list = ls(all=TRUE), file = paste(db.path,".Rsnap"))

## reboot:1 ends here
