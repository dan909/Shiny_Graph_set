# pkgTest <- function(x)
# {
#     if (!require(x,character.only = TRUE))
#     {
#         install.packages(x,dep=TRUE)
#         if(!require(x,character.only = TRUE)) stop("Package not found")
#     }
# }
# pkgTest("shiny")
# pkgTest("ggplot2")

require("shiny")
require("ggplot2")
#options(warn=-1)
AbBkSCol <- c("springgreen4", "cornflowerblue", "mediumvioletred", "darkorange", "red4", "plum3", "wheat3", "orangered2", "blue4")
Dates<- c('08/05/2013', '18/05/2013', '28/05/2013', '08/06/2013', '18/06/2013', '28/06/2013', '08/07/2013', '18/07/2013', '28/07/2013')
Dates <- strptime(as.character(Dates),"%d/%m/%Y",tz="GMT")
WT.dat <- read.csv("MET_dater.csv", stringsAsFactors=F)
WT.dat$Rtime <- strptime(as.character(WT.dat$Rtime),"%d/%m/%Y %H:%M",tz="GMT")

shinyServer(function(input, output) {
    
    
    data.set <- reactive({
        
        
        Seed.Dat <- read.csv("all-Data + MET.csv", stringsAsFactors=FALSE)
        
        Seed.Dat$Sowing <- as.character(Seed.Dat$Sowing)        
        
        if(input$Altitz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Altitude == input$Altitz, ]
        }
        if(input$Yearz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Year == input$Yearz, ]
        } 
        if(input$Treatz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Treatment == input$Treatz, ]
        } 
        if(input$Genoz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Genotype == input$Genoz, ]
        } 
        
        Seed.Dat <- Seed.Dat
    })
    
    
    output$plot <- renderPlot({
        
        if(input$AltGraph) {
            p <- ggplot(data.set(), aes_string(x="Sowing", y=input$yaxis)) + geom_boxplot()
            p <- p + aes(color=Sowing) + scale_fill_manual(values = (AbBkSCol)) + facet_wrap(Treatment ~ Genotype)
        } else{
            p <- ggplot(data.set(), aes_string(x=input$xaxis, y=input$yaxis)) + geom_boxplot()
            
            
            p <- p + aes_string(color=input$xaxis) + facet_wrap( ~ Sowing, ncol=3) + 
                theme(strip.text.x = element_text(size=10, angle=0),
                      strip.text.y = element_text(size=12, face="bold"),
                      strip.background = element_rect(colour="black", fill="white"))
            
        }
        print(p)
        
    })
    
    output$METtemp <- renderPlot({
        
        p <-  ggplot(WT.dat, aes(Rtime, Temp)) + xlab('Date') + ylab('Temperature (C)') + xlim(Dates[1], Dates[9])
        
        p <- p + geom_point(alpha=0.1, aes(colour = Temp)) + geom_smooth() + facet_grid(Treatment ~ Altitude)  + scale_colour_gradient(low = "blue", high = "red")
        
        if(input$SowingDates) {
            p <- p + annotate("text", x = Dates[1], y = 0, label = "1", colour = AbBkSCol[1], size = 4)
            p <- p + annotate("text", x = Dates[2], y = 0, label = "2", colour = AbBkSCol[2], size = 4)
            p <- p + annotate("text", x = Dates[3], y = 0, label = "3", colour = AbBkSCol[3], size = 4)
            p <- p + annotate("text", x = Dates[4], y = 0, label = "4", colour = AbBkSCol[4], size = 4)
            p <- p + annotate("text", x = Dates[5], y = 0, label = "5", colour = AbBkSCol[5], size = 4)
            p <- p + annotate("text", x = Dates[6], y = 0, label = "6", colour = AbBkSCol[6], size = 4)
            p <- p + annotate("text", x = Dates[7], y = 0, label = "7", colour = AbBkSCol[7], size = 4)
            p <- p + annotate("text", x = Dates[8], y = 0, label = "8", colour = AbBkSCol[8], size = 4)
            p <- p + annotate("text", x = Dates[9], y = 0, label = "9", colour = AbBkSCol[9], size = 4)
            
        }
        
        print(p)
        
    })
    
    
    output$METmost <- renderPlot({
        
        q <-  ggplot(WT.dat, aes(Rtime, Moisture)) + xlab('Date') + ylab('Moisture (W/V') + xlim(Dates[1], Dates[9])
        
        q <- q + geom_point(alpha=0.1, aes(colour = Temp)) + geom_smooth() + facet_grid(Treatment ~ Altitude)  + scale_colour_gradient(low = "black", high = "blue")
        
        if(input$SowingDates2) {
            q <- q + annotate("text", x = Dates[1], y = 0, label = "1", colour = AbBkSCol[1], size = 4)
            q <- q + annotate("text", x = Dates[2], y = 0, label = "2", colour = AbBkSCol[2], size = 4)
            q <- q + annotate("text", x = Dates[3], y = 0, label = "3", colour = AbBkSCol[3], size = 4)
            q <- q + annotate("text", x = Dates[4], y = 0, label = "4", colour = AbBkSCol[4], size = 4)
            q <- q + annotate("text", x = Dates[5], y = 0, label = "5", colour = AbBkSCol[5], size = 4)
            q <- q + annotate("text", x = Dates[6], y = 0, label = "6", colour = AbBkSCol[6], size = 4)
            q <- q + annotate("text", x = Dates[7], y = 0, label = "7", colour = AbBkSCol[7], size = 4)
            q <- q + annotate("text", x = Dates[8], y = 0, label = "8", colour = AbBkSCol[8], size = 4)
            q <- q + annotate("text", x = Dates[9], y = 0, label = "9", colour = AbBkSCol[9], size = 4)
            
        }
        
        print(q)
        
    })
    #     # Generate a summary of the data
    #     output$summary <- renderPrint({
    #         summary(data())
    #     })
    #     
    #     # Generate an HTML table view of the data
    #     output$table <- renderTable({
    #         data.frame(x=data())
    #     })
})

