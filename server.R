library(png)
setwd("C:/Users/febner/Documents/IPCORESTUFF")

source("LUTutils.R")

mydata <- mkInitLut(NULL)

mkImg <- function(val) {
    bottom <- bitwAnd(val,strtoi(c("0x07")))
    middle <- bitwAnd(bitwShiftR(val,3),strtoi(c("0x07")))
    top <- bitwAnd(bitwShiftR(val,6),strtoi(c("0x07")))
    df <- data.frame(x=1:3,y=1:3,z=1:3)
    
    df[1,3] <- bitwShiftR(bitwAnd(top,strtoi(c("0x04"))),2)
    df[2,3] <- bitwShiftR(bitwAnd(top,strtoi(c("0x02"))),1)
    df[3,3] <- bitwAnd(top,strtoi(c("0x01")))
    df[1,2] <- bitwShiftR(bitwAnd(middle,strtoi(c("0x04"))),2)
    df[2,2] <- bitwShiftR(bitwAnd(middle,strtoi(c("0x02"))),1)
    df[3,2] <- bitwAnd(middle,strtoi(c("0x01")))
    df[1,1] <- bitwShiftR(bitwAnd(bottom,strtoi(c("0x04"))),2)
    df[2,1] <- bitwShiftR(bitwAnd(bottom,strtoi(c("0x02"))),1)
    df[3,1] <- bitwAnd(bottom,strtoi(c("0x01")))
    df <- df*255
    m <- as.matrix(df)

    return(m)
    }

function(input, output, session) {
    #print(str(session))
    writeOutputText <- function() {
        output$output <- renderPrint({ x<-mydata[input$slider1+1,3]; if (x == "0") {
            "0, Black"
        } else {
            "255, White"
        } })
        updateRadioButtons(session, "outRad",label = "output",
                           choices = list("0" = 1, "255" = 2),selected = {
                               if(mydata[input$slider1+1,3] == "0"){1}else{2}
                           })
    }
    
    observeEvent(input$file, {
        if (length(input$file) > 0) {
            print(input$file$name)
            mydata <<- readLUT(input$file$name)
            writeOutputText()
        }
    })
    
    observeEvent(input$outRad, {
        print(input$outRad)
        if(input$outRad == "1") {
            mydata[input$slider1+1,3] <<- "0"
        } else {
            mydata[input$slider1+1,3] <<- "255"
        }
        writeOutputText()
    })
    
    observeEvent(input$fileOut, {
        print("writing output files")
        print(head(mydata))
        writeLUT(mydata)
    })
    observeEvent(input$num, {
        updateSliderInput(session, "slider1", label = "level",
                          min = 0,
                          max = 511,
                          value = input$num)
    })
    observeEvent(input$slider1, {
        updateRadioButtons(session, "outRad",label = "output",
                            choices = list("0" = 1, "255" = 2),selected = {
                            if(mydata[input$slider1+1,3] == "0"){1}else{2}
            })
        if (input$slider1 != input$num)
            updateNumericInput(session, "num", label="", value = input$slider1)
        })
    output$value <- renderPrint({ mydata[input$slider1+1,1] })
    output$binary <- renderPrint({ paste(mydata[input$slider1+1,2],"0 is white, 1 is black") })
    
    output$image1 <- renderImage({
        width  <- 500
        height <- 500
        outfile <- tempfile(fileext = ".png")
        pic <- mkImg(input$slider1)
        #print(pic)
        #writePNG(pic, target = outfile)
        png(outfile,width=width,height=height)
        par(mar=c(0, 0, 0, 0))
        image(x,y,pic,axes=FALSE,useRaster = TRUE,col = grey(seq(.9, .3, length = 256)))
        
        dev.off()
        list(src = outfile, alt = "This is alternate text")
        
    }, deleteFile = TRUE)

}