df <- read.csv("lutb2g.h.justLUT", header = FALSE)
dfi <- 255-df
write.csv(dfi, "inverted", row.names = FALSE,na="",col.names = FALSE)
dfi <- dfi[,-9]
dim (dfi)

initLUT <- matrix(unlist(t(dfi)),byrow=T,512,1)

# but really just make one up...

df <- data.frame(address=1:512,output=rep(0,512))
df[1,2] <- 255

dfinit <- apply(df, 1, function(n) {
    print(n)
    a <- substring(paste0(substring(rev(intToBits(n[1]-1)),2,2),collapse=""),24,32)
    print(a)
    c(n[1]-1,a,n[2])
})
dfinit <- t(dfinit)
colnames(dfinit) <- c("value","binary","output")
