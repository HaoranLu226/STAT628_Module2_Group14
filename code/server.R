# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Created by Junxia Zhao.
# Revised by Xinran Miao.
# The responsibility can be found in the text when one sees
# [Junxia Zhao] or [Xinran Miao]. 
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(png)
# Please set the working directory to current directory.
cleaned = read.csv("../data/cleaned_bodyfat.csv")
# Define a server for the Shiny app
server = shinyServer(function(input, output) {
    
# [Junxia Zhao] ========================================   
    res <- reactive({
        # Read the input variables
        weight = input$weight * ifelse(input$weight_unit == "kg",2.2046226218,1)
        abdomen = input$abdomen *ifelse(input$abdo_unit=='inches',2.54,1)
        wrist = input$wrist * ifelse(input$wrist_unit == "inches",2.54,1)
        
        # Calculate the estimated body fat percentage.
        round(-25.62094-0.08318 * weight + 0.87991 *abdomen -1.20521 *wrist,2)
        
    })
    
    # Print the bodyfat percentage.
    output$value = renderPrint({
        if (res() > 0 | res() <50){
            Res = paste0("The percentage of Bodyfat:", res(), "%") 
        }
        else{
            Res = paste0("Please check you input!")
        }
    })
    
    # Calculate the quantile of user's body fat percentage.
    Bodyfat_level = function(x){
        a=paste0('Your body fat is ',res(),'%. It is lower than ',
                 round(100*sum(cleaned$BODYFAT>res())/nrow(cleaned+1),0),
                 '% of others! ',sep='')
        return(a)
    }
    
    output$res <- renderPrint({cat(res())})
    output$Bodyfat_level <-renderPrint({
        res_level = Bodyfat_level(res())
        cat(res_level)
    })
# [Xinran Miao] =========================================    
# A histogram plot   
    output$Plot <- renderPlot({
        
        user = res()
        # Determine the data for plot.
        dataset = data.frame(bodyfat = c(user,cleaned$BODYFAT),
                             group = cut(c(user,cleaned$BODYFAT),15))
        
        
        table(cut(dataset$bodyfat,15)) %>% 
            as.data.frame() %>%
            dplyr::rename('group'='Var1') %>%
            mutate(density = Freq/247,
                   color = ifelse(group==dataset$group[1],"You are here!",'Others'),
                   bodyfat = dataset %>%
                       ddply(.(group),summarise,bodyfat = mean(bodyfat),.drop=FALSE) 
                   %>%.[,2] %>% round(digits=1))  %>%
            ggplot() +
            geom_bar(aes(x = factor(bodyfat), y = density, fill=color), 
                     stat="identity")+
            
            theme(axis.text = element_text(face = "bold",size=15),      # Set the size of texts of both axes
                  axis.title = element_text(face = "bold",size=15),     # Set the size of titles of both axes
                  legend.title = element_blank(),                       # Remove the title of the legend
                  panel.background = element_rect(fill = "transparent"),# Remove the default gray background
                  panel.border=element_rect(fill='transparent',         # Remove the default border
                                            color='transparent'),
                  axis.line = element_line(color = "black"),            # Set the color of axis lines
                  legend.text =element_text(size=15),                   # Set the size of legend texts
                  title = element_text(size = 18,face = 'bold'),
                  plot.title = element_text(hjust = 0))+                  
            labs(x = "Bodyfat(%)", y = "Percentage among Males",
                 title = Bodyfat_level(res()))+          # Labels on each axis
            scale_fill_manual(values=c('gray40','gold'))                # Set the color of bins
        
        
    })
# [Junxia Zhao] ============================================
# Print a figure for reference.
    output$myImage <- renderImage({ 
        
       if (res() <= 0){ 
            return(list(src = "bodyfat_error.jpg", width = 200, height = 300, alt = "shape"))}
        else if (res() <=9){
            return(list(src = "bodyfat8.jpg", width = 88, height = 300, alt = "shape"))}
        else if (res() <=14.5){
            return(list(src = "bodyfat10_14.jpg", width = 88, height = 300, alt = "shape"))}
        else if (res() <= 19.5){
            return(list(src = "bodyfat15_18.jpg", width = 88, height = 300, alt = "shape"))}
        else if (res() <=27.5){
            return(list(src = "bodyfat20_25.jpg", width = 88, height = 300, alt = "shape"))}
        else if (res() <= 50){ 
            return(list(src = "bodyfat30.jpg", width = 88, height = 300, alt = "shape"))}
        else if (res() > 50){ 
            return(list(src = "bodyfat_error.jpg", width = 200, height = 300, alt = "shape"))}
        
    }, deleteFile = FALSE)
# [Xinran Miao] ==================================================
# Define urls as the links.
    url1 <- a('--',"14 Ways to Burn Fat Fast!", href="https://www.healthline.com/nutrition/best-ways-to-burn-fat")
    output$reduce <- renderUI({
        tagList( url1) 
    })
    
    url2 <- a("How to Take Accurate Girth Measurements?", href="http://business.fit/how-take-accurate-girth-measurements/")
    output$measure <- renderUI({
        tagList('--',url2)
    })
    
    
    url3 = a("How to Measure Abdomen?",href= 'http://www.myhealthywaist.org/fileadmin/pdf/WCMG-Healthcare_Professional.pdf')
    output$m_abdomen = renderUI({tagList('--',url3)})
    
    url4 = a("Click Here.", href= 'https://github.com/HaoranLu226/STAT628_Module2_Group14')
    output$github = renderUI({tagList("Check our github repository for codes and other details!",url4)})
    
    
    url5 = a("Click Here.", href= 'https://www.hindustantimes.com/brunch/core-training-and-the-six-pack-mania/story-Hl2ulzmS2ajb2J0mzYMMYI.html')
    output$picture= renderUI({tagList("Pictures of body shape are from following:",url5)}) 
    
})

shinyApp(ui= ui, server=server)
