#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load required packages
library(shiny)
library(rsconnect)
library(bslib)

##Import custom R modules
source("functions.R")

##Defining image variables
im <-c("flood.png","model.png","flow.png","rFrequency.png","kLulc.png","fFrequency.png","mPlan.png")
im1 <- "flood.png"
im2 <- "model.png"
im3 <- "flow.png"
im4 <- "rFrequency.png"
im5 <- "kLulc.png"
im6 <- "fFrequency.png"
im7 <- "mPlan.png"
##Defining header variables for services 
hd1 <- h6("HYDROLOGICAL SURVEY")
hd2 <- h6("FLOOD RISK ASSESSMENT")
hd3 <- h6("HYDROPOWER ASSESSMENT")
hd4 <- h6("GROUNDWATER EXPLORATION")
hd5 <- h6("WASH")
hd6 <- h6("DRAINAGE AND IRRIGATION DESIGNS")
hd7 <- h6("ENVIRONMENTAL IMPACT ASSESSMENT")
hd8 <- h6("WATER QUALITY ANALYSIS")
##Defining footer variables for services
ft1 <- p("Plan, design and conduct hydrological and hydraulic surveys", class="servPg")
ft2 <- p("Model, assess and map flood risk", class="servPg")
ft3 <- p("Assess and map location of hydropower potential", class="servPg")
ft4 <- p("Explore and map locations of groundwater potential", class="servPg")
ft5 <- p("Assess health risk of surface water and groundwater systems", class="servPg")
ft6 <- p("Plan, design and model drainage and irrigation systems", class="servPg")
ft7 <- p("Assess impacts of various projects on the environment", class="servPg")
ft8 <- p("Analyse and model water quality of surface and groundwater", class="servPg")
##Define header variables for portfolio
h_pf1 <- h6("100-year flood map")
h_pf2 <- h6("HEC-RAS 2D Flood Model")
h_pf3 <- h6("Flow Duration Curve")
h_pf4 <- h6("Rainfall Frequency Curve")
h_pf5 <- h6("Landuse Land Cover Map")
h_pf6 <- h6("Flood Frequency Curve")
##Define footer variables for portfolio
f_pf <- p(a("Read more", href=""), class="servLink")
##Define header variable for home page
h_hm <- h6("Recent projects")
##Define footer variable for home page
f_hm <- p(a("Portfolio", href=""), class="servLink")

##Define header variable for about page
h_ab <- h6("Recent projects")
##Define footer variable for home page
f_ab <- p(a("Services", href=""), class="servLink")

##Define header variable for contact page
h_cn <- h6("Recent projects")
##Define footer variable for contact page
f_cn <- p(a("Home", href=""), class="servLink")

# Define UI for navigation panel
navs <- tabsetPanel(
 type = "pills",
 id = "home",
  #Design home page 
   tabPanel( 
    title = "Home", icon = icon("home"), 
    #First row
    fluidRow(id= "dhome1",
      "Benart Hydro Consultancy Limited", class="c_h3"    
      ),
     
    #Second row
        fluidRow(id="dhome",
          column(6,
           div(class="divhome", tags$hr(h5("Hydrology, Hydraulics and General Water and Sanitation Services")),
            
             hr(p("We work to provide a top-notch hydrological and hydraulic assessments in a timely fashion."),
                
                p("Regardless of your specific project requirements, we got you covered.")),
            
             hr(actionButton("hire", "Hire Us", class="submit")),
          
             )
          ),
          column(6, uiOutput("hm"),
                 sliderInput("slide", label = p("Slides"), min=1,max=7, value =1, step = 1, animate = live, ticks = FALSE)
                 ) #card2(im7)) dhome3
      )
    ),
   #Design about us page
  tabPanel(title = "About Us", icon = icon("users"),
           class="row_about", 
           fluidRow(
             column(6,card2(im7,h_ab,f_ab), id="iabout"),
      
             div(class="abt_container", column(6, class="divhome",
                   p("We are expert in hydrology, hydraulics, sanitation and general 
                     water services with over 5 years of commercial experience.
                     Our experts are professional hydrologists, water resource engineers, 
                     civil engineers and hydrogeologists from top-notch universities."),
                   
                     p("We conduct numerous studies including hydrological surveys, hydraulic modelling, 
                     flood risk assessment, groundwater exploration, hydropower assessment, environmental
                     impact assessment, climate impact assessment etc."),
                   
                     p("We work to deliver high
                     quality works in a timely manner by employing varied tools including ArcGIS, QGIS, Terrset,
                     R, Python, C++, C#, HEC-RAS, HEC-HMS, Civil3D, AutoCAD etc. Regardless of your specific project
                     requirements, we have the tools, technologies and experts to deliver quality work on time."
                     ) 
                    ))
           )
           
           ),
#Design Services page

  tabPanel(title = "Services", icon = icon("map"),
               
           #First row three columns
           fluidRow(id="fd1_serv",
               column(4,uiOutput("serv1")),
                    
               column(4,uiOutput("serv2")),
                    
               column(4,uiOutput("serv3")),
           ),
 # Second row three columns
           hr(fluidRow(
             column(4,uiOutput("serv4")),
            
             column(4,uiOutput("serv5")),
                  
             column(4,uiOutput("serv6")),
              ))
           ),
 
#Design Portfolio page
 tabPanel(title = "Portfolio", icon = icon("water"),
          #First row three columns
          div(id="fd_port"),
        
          #Second row three columns
          fluidRow(id= "over",
            column(4, uiOutput("port1")),
            column(4, uiOutput("port2")),
            column(4, uiOutput("port3"))
          ),
          #Third row three columns
          fluidRow(id= "over",
            column(4, uiOutput("port4")),
            column(4, uiOutput("port5")),
            column(4, uiOutput("port6"))
          )
          
 ),
 
 #Design contact page
  tabPanel(title = "Contact Us", icon = icon("phone"),
           fluidRow(id="i_contact",
             column(6,
             textInput("email","Email",placeholder = "Your email"),
           textInput("subject","Subject",placeholder = "subject"),
           textAreaInput("message", "How can we help you?",placeholder = "Your message", rows = 3),
           actionButton("submit", "Submit", icon = icon("send"), class="submit")
             ),
           column(4, card2(im5,h_cn,f_cn))
           ),
           # second row
           fluidRow(
             id="contact",
                 column(4,id="footer",
                     p(span(icon(name="water", lib = "font-awesome", class = "icon", style="color:aqua;")),":Benart Hydro Consultancy Ltd"),
                     p(span(icon(name="address-card", lib = "font-awesome", class = "icon", style="color:teal;")), ":P.O.Box CC695"),
                     p(span(icon(name="location-dot", lib = "font-awesome", class = "icon", style="color:indianred;")), ":CK-0067-1259"),
                      
                 ),
                 
                 column(4,id="footer",
                        p(span(icon(name="phone", lib = "font-awesome", class = "icon", style="color:blueviolet;")), ": +233 24 1454 769/+233 20 943 0614"),
                        p(span(icon(name="envelope", lib = "font-awesome", class = "icon", style="color:gold;")),":benarthydro@gmail.com"),
                        p(span(icon(name="whatsapp", lib = "font-awesome", class = "icon", style="color:green;")), ": +233 24 1454 769/+233 20 943 0614")
                       
                 ),
                 
                 column(4,id="footer",
                    
                        p("Facebook", span(icon(name="facebook", lib = "font-awesome", class = "icon", style="color:royalblue;"))),
                        p("Twitter", span(icon(name="twitter", lib = "font-awesome", class = "icon", style="color:dodgerblue;"))),
                        p("Instagram", span(icon(name="instagram", lib = "font-awesome", class = "icon", style="color:red;")))
                 )
                 
                  
                 
  )
)
)

##Implementing the frontend/ui 
ui <- fluidPage(
#theme = bs_theme(
#bootswatch="lumen",
#bg="black",
#fg="white"
#),
  
##insert external css using includeCSS method
#tags$head(includeCSS("C:/Users/HP/Documents/shinyApps/BenartWeb/www/benart.css")),
tags$head(tags$link(
    rel="stylesheet", type="text/css",
    href="benart.css"
  )),

#title
title = "Benart Hydro Consultancy",
##navigation
navs
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#Render home card
  
output$hm <- renderUI(
  if(input$slide==1){
    card2(im1,h_hm,f_hm)
  }
  else if(input$slide==2){
    card2(im2,h_hm,f_hm)
  }
  else if(input$slide==3){
    card2(im3,h_hm,f_hm)
  }
  else if(input$slide==4){
    card2(im4,h_hm,f_hm)
  }
  else if(input$slide==5){
    card2(im5,h_hm,f_hm)
  }
  else if(input$slide==6){
    card2(im6,h_hm,f_hm)
  }
  else if(input$slide==7){
    card2(im7,h_hm,f_hm)
  }
  else{
    message("Invalid input")
  }
)
  
##Render portfolio cards

output$port1 <- renderUI(card2(im1,h_pf1,f_pf))
output$port2 <- renderUI(card2(im2,h_pf2,f_pf))  
output$port3 <- renderUI(card2(im3,h_pf3,f_pf)) 
output$port4 <- renderUI(card2(im4,h_pf4,f_pf)) 
output$port5 <- renderUI(card2(im5,h_pf5,f_pf)) 
output$port6 <- renderUI(card2(im6,h_pf6,f_pf)) 

#Render services cards
output$serv1 <- renderUI(card2(im1,hd1,ft1))
output$serv2 <- renderUI(card2(im2,hd2,ft2))  
output$serv3 <- renderUI(card2(im3,hd3,ft3)) 
output$serv4 <- renderUI(card2(im4,hd4,ft4))
output$serv5 <- renderUI(card2(im5,hd5,ft5))  
output$serv6 <- renderUI(card2(im6,hd6,ft6))
}

# Run the application 
shinyApp(ui = ui, server = server)