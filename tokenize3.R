library(tau)
library(data.table)
print(paste(Sys.time(),": start"))
filename <- "final//en_US/en_US.blogs.txt"
s <- file.info(filename)$size
s <- 200000000
chars <- readChar(filename,s)
lines <- strsplit( chars,"\n",fixed=T,useBytes=T)[[1]]
groups <- split(lines, ceiling(seq_along(lines)/1))
print(paste(Sys.time(),": #lines=",length(lines),"#groups=",length(groups)))
rm(lines)
rm(chars)

groupCount <-0
for (x in groups) {
  groupCount <- groupCount + 1
   textcnt(x,2,verbose = F,method = "string",persistent = T,recursive = T)
  if(rbinom(1,1,0.1)==1)
    print(paste(Sys.time(),"#gc:",groupCount))
  
}
tc <- textcnt("",n=2,verbose = T,method = "string")
dt <- data.table(terms=names(tc),count=as.integer(tc),key="terms")
print(paste(Sys.time(),": end"))
