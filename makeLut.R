# take an address abcdefghi bits, create a plot

val <- 16 # value from 0 - 511

bottom <- bitwAnd(val,strtoi(c("0x07")))
middle <- bitwAnd(bitwShiftR(val,3),strtoi(c("0x07")))
top <- bitwAnd(bitwShiftR(val,6),strtoi(c("0x07")))
bottom
middle
top
df <- data.frame(x=1:3,y=1:3,z=1:3)
df[1,3] <- bitwShiftR(bitwAnd(top,strtoi(c("0x04"))),2)
df[1,2] <- bitwShiftR(bitwAnd(top,strtoi(c("0x02"))),1)
df[1,1] <- bitwAnd(top,strtoi(c("0x01")))
df[2,3] <- bitwShiftR(bitwAnd(middle,strtoi(c("0x04"))),2)
df[2,2] <- bitwShiftR(bitwAnd(middle,strtoi(c("0x02"))),1)
df[2,1] <- bitwAnd(middle,strtoi(c("0x01")))
df[3,3] <- bitwShiftR(bitwAnd(bottom,strtoi(c("0x04"))),2)
df[3,2] <- bitwShiftR(bitwAnd(bottom,strtoi(c("0x02"))),1)
df[3,1] <- bitwAnd(bottom,strtoi(c("0x01")))

df <- df*255
m <- as.matrix(df)
m
x <- 1:3
y <- 1:3
par(mar=c(0, 0, 0, 0))
image(x,y,m,axes=FALSE,useRaster = TRUE,col = grey(seq(1, 0, length = 256)))

