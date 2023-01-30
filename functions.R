##Benart web card functions implementation

card2 <- function (sc,head = "Hi",foot="Hello") {
 div(class = "card", id="dv_card",
      img(
        src=sc,
        height="auto", 
        width="auto",
        #margin= "5px",
        class ="card",
        id ="im_card"
      ),
      div(class = "container",
          h4(head),
          tags$hr(
            p(foot)
          )
      )
  )
}

##Benart web card functions implementation
slides <- function (slide) {
  div(class = "c_divslide", id="i_divslide",
      img(
        src=slide,
        height="auto", 
        width="auto",
        #margin= "5px",
        class ="c_slide",
        id ="i_slide"
      )
  )
}

##animation function
live <- animationOptions(
  interval = 2000,
  loop = TRUE,
  #playButton = tags$p("Play", span(icon("play")))
)

#Animation control structure

animate <- function(){
  if(input$slide==1){
    card2(im1)
  }
  else if(input$slide==2){
    card2(im2)
  }
  else if(input$slide==3){
    card2(im3)
  }
  else if(input$slide==4){
    card2(im4)
  }
  else if(input$slide==5){
    card2(im5)
  }
  else if(input$slide==6){
    card2(im6)
  }
  else if(input$slide==7){
    card2(im7)
  }
  else{
    message("Invalid input")
  }
}
