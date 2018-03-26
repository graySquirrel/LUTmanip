# LUT utils
mkInitLut <- function(i) {
    df <- data.frame()
    if (is.null(i)) {
        df <- data.frame(address=1:512,output=rep(0,512))
        df[1,2] <- 255
    } else {
        df <- i
    }
    dfinit <- apply(df, 1, function(n) {
        #print(n)
        a <- substring(paste0(substring(rev(intToBits(n[1]-1)),2,2),collapse=""),24,32)
        c(n[1]-1,a,n[2])
    })
    dfinit <- t(dfinit)
    colnames(dfinit) <- c("value","binary","output")
    dfinit
}

readLUT13 <- function(fn) {
    mdf <- read.csv(fn, header=FALSE)
    mdf <- mdf[,-dim(mdf)[2]]
    m <- unlist(as.list(t(mdf)))
    m
}

readLUT <- function(fn) {
    mdf <- read.csv(fn, header=FALSE)
    mdf <- mdf[,-dim(mdf)[2]]
    siz <- prod(dim(mdf))
    m <- 0
    if (siz == 8192) {
        m <- mk9from13LUT(readLUT13(fn))
    } else if (siz == 512) {
        m <- unlist(as.list(t(mdf)))
    } else {
        print("GAH BAD FILE")
        stop()
    }
    mkInitLut(data.frame(address=1:512,output=m))
}

writeLUT <- function(towrite, outf="C:/Users/febner/Documents/IPCORESTUFF/LUT.txt") {
    outf13 <- paste0(outf,"13.txt")
    d <- towrite[,3]
    d <- as.integer(d)
    #m <- matrix(d,nrow=64,ncol=8,byrow = TRUE)
    d13 <- mk13from9LUT(d)
    m <- matrix(d,ncol=16,byrow = TRUE)
    m13 <- matrix(d13,ncol=16,byrow = TRUE)
    #df <- as.data.frame(m)
    #df[,17] <- NULL
    #print(head(m))
    write.table(m, file = outf, sep = ",", col.names = FALSE,
                qmethod = "double", row.names=FALSE, eol=",\n")
    write.table(m13, file = outf13, sep = ",", col.names = FALSE,
                qmethod = "double", row.names=FALSE, eol=",\n")
}

# convert between the 9 bit LUT and the 13 bit LUT
# * The 13 bit index is based on the current bit (bit #6 below) we are at while
# * dilating and the 12 bits surrounding it. The bits are numbered:
# *         0
# *     1   2   3
# * 4   5   6   7   8
# *     9  10  11
# *        12
# where 0 is LSBit
# 
# the 9 bit index is simply
# *  8  7  6
# *  5  4  3
# *  2  1  0 
# where 0 is LSBit, the numbers represent the bit position in the word

# to map bits between 13 and 9...
# 12  11  10   9   8   7   6   5   4   3   2   1   0
#  X   0   1   2   X   3   4   5   X   6   7   8   X

# So, to map from 13 bit to 9 bit, walk through the 9 bits, and find the corresponding addresses in the 13
nineTo13xlate <- function(addr9bit, justOne = FALSE) {
    # move each bit of the 9 bit address to the 13 bit addr position
    zero <- bitwShiftL(bitwAnd(addr9bit, 1), 11) 
    one <- bitwShiftL(bitwAnd(addr9bit, strtoi(c("0x02"))), 9)
    two <- bitwShiftL(bitwAnd(addr9bit, strtoi(c("0x04"))), 7)
    three <- bitwShiftL(bitwAnd(addr9bit, strtoi(c("0x08"))), 4)
    four <- bitwShiftL(bitwAnd(addr9bit, strtoi(c("0x10"))), 2)
    five <- bitwAnd(addr9bit, strtoi(c("0x20")))
    six <- bitwShiftR(bitwAnd(addr9bit, strtoi(c("0x40"))), 3)
    seven <- bitwShiftR(bitwAnd(addr9bit, strtoi(c("0x80"))), 5)
    eight <- bitwShiftR(bitwAnd(addr9bit, strtoi(c("0x100"))), 7)
    res <- bitwOr(zero,one)
    res <- bitwOr(res, two)
    res <- bitwOr(res, three)
    res <- bitwOr(res, four)
    res <- bitwOr(res, five)
    res <- bitwOr(res, six)
    res <- bitwOr(res, seven)
    res <- bitwOr(res, eight)
    # return 16, 13 bit addresses, where the missing 4 bits are blown out
    missingBits <- 0
    if(justOne == FALSE) {
        missingBits <- 0:15
    }
    loc0 <- bitwAnd(missingBits,1)
    loc4 <- bitwShiftL(bitwAnd(missingBits, 2), 3)
    loc8 <- bitwShiftL(bitwAnd(missingBits, 4), 6)
    loc12 <- bitwShiftL(bitwAnd(missingBits, 8), 9)
    mis2 <- bitwOr(loc0,loc4)
    mis2 <- bitwOr(mis2,loc8)
    mis2 <- bitwOr(mis2,loc12)
    res <- bitwOr(res, mis2)
    res
}
thirteenTo9xlate <- function(addr13bit) {
    one <- bitwShiftL(bitwAnd(addr13bit, strtoi(c("0x02"))), 7)
    two <- bitwShiftL(bitwAnd(addr13bit, strtoi(c("0x04"))), 5)
    three <- bitwShiftL(bitwAnd(addr13bit, strtoi(c("0x08"))), 3)
    five <- bitwAnd(addr13bit, strtoi(c("0x20")))
    six <- bitwShiftR(bitwAnd(addr13bit, strtoi(c("0x40"))), 2)
    seven <- bitwShiftR(bitwAnd(addr13bit, strtoi(c("0x80"))), 4)
    nine <- bitwShiftR(bitwAnd(addr13bit, strtoi(c("0x200"))), 7)
    ten <- bitwShiftR(bitwAnd(addr13bit, strtoi(c("0x400"))), 9)
    ele <- bitwShiftR(bitwAnd(addr13bit, strtoi(c("0x800"))), 11)
    res <- bitwOr(one, two)
    res <- bitwOr(res, three)
    res <- bitwOr(res, five)
    res <- bitwOr(res, six)
    res <- bitwOr(res, seven)
    res <- bitwOr(res, nine)
    res <- bitwOr(res, ten)
    res <- bitwOr(res, ele)
    res
}

# for (i in 1:bitwShiftL(1,9)-1) {
#     #print(i)
#     f <- nineTo13xlate(i)
#     l <- thirteenTo9xlate(f)
#     if (sum(i-l) != 0) {
#         print("problem at ")
#         print(i)
#         print(f)
#         print(l)
#         stop()
#     }
# }
# 
# for (i in 1:bitwShiftL(1,13)-1) {
#     #print(i)
#     l <- thirteenTo9xlate(i)
#     f <- nineTo13xlate(l)
#     if (i %in% f != TRUE) {
#         print("problem at ")
#         print(i)
#         print(f)
#         print(l)
#         stop()
#     }
# }

# to create a 13 lut, for each address in the new 13 lut, look up value by creating 9 index and looking up
# 9bitLUT 0 = black, 255=white
# 13bitLUT 0 = white, 1=black
mk13from9LUT <- function(lut9) {
    lut13 <- data.frame(1:bitwShiftL(1,13))
    lut13 <- apply(lut13,1,function(n) {
        if(lut9[thirteenTo9xlate(n-1)+1] == 255) {0} else {1}
    })
    lut13
}
# lut13 is just a vec of ints
mk9from13LUT <- function(lut13) {
    lut9 <- data.frame(1:bitwShiftL(1,9))
    lut9 <- apply(lut9,1,function(n) {
        if(lut13[nineTo13xlate(n-1, justOne = TRUE)+1] == 0) {255} else {0}
    })
    lut9
}

# LUT9  <- as.integer(readLUT("LUTtest.txt")[,3]) # just the value
# LUT13 <- mk13from9LUT(LUT9)
# LUT9_2 <- mk9from13LUT(LUT13)
# LUT13_2 <- mk13from9LUT(LUT9_2)
# all(LUT9 == LUT9_2)
# all(LUT13 == LUT13_2)
#l13 <- readLUT13("LUT13.txt")
#l9 <- mk9from13LUT(l13)
#writeLUT(mkInitLut(l9),"LUTfrom13.txt")