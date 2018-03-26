fluidPage(
    fluidRow(
        column(4,fileInput("file", label = "Select LUT file")),
        column(4,br(),actionButton("fileOut", label = "write LUT out"))
    ),
    fluidRow(
        column(8,sliderInput(
        "slider1",
        label = "level",
        min = 0,
        max = 511,
        value = 16,
        width = '800px'
    )),
    column(2,numericInput("num", label="", value = 16, min=0,max=511)
    )),
    hr(),
    fluidRow(column(4,
                    textOutput("value"),
                    textOutput("binary"),
                    textOutput("output"),
                    radioButtons("outRad", label = "output",
                                 choices = list("0" = 1, "255" = 2), 
                                 selected = 1)#,
                    # textInput("update", "update output", 
                    #           value = "")
                    ),
            column(4,
                    imageOutput("image1", height = 500))
    )
)
