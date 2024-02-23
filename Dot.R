#
#ToDo:
# - (OK) implement + operator (i.e. pull from input not fixed)
#        -- this sets the stage for general set of operators
# - implement () for grouping ... this will really show of recursion vs regex
# - Figure out operator precedence -- there is some default precedence built in - so can talk about it at prezzie.  
#   (May have to do a term/factor thing to show off how () affect precedence)
# - error handling
# - - invalid characters
# - - incorrect syntax
# - evaluate using traverse function
# - token types: 
#     -- (OK) operator 
#     -- (OKish) numeric - have my lame ass Digit
#     -- literal (whut for strings)?
#     -- oh yea and parens & other syntax elements ... 
# - actual multi-digit numbers, 0 + pos integers (overflow?)
# - text panel with grammar & explanation
# - open in 'full screen' size ....
# - L8ter make a package
# - !!!!! plot display token type/value rather than node ID
# - figure out github push to facilitating working 'at home'
#
library(shiny)
library(data.tree)
library(DiagrammeR)    #graph viz HTML widget to render tree data

## Lexing and parsing functions should come in automatically from Parse.R in ./R folder 
## according to anonymous sources on the interweb.  We shall see.

ui <- fluidPage(
  titlePanel(
    h4("===   Dot   ===", style = "text-align:center")
  ),
  sidebarLayout(
    sidebarPanel(
      "Settings",
      textInput("textin","Input Text"),
      #checkboxInput("isfile","Use input as filename"),  ## currently unused
      hr(),
      actionButton("exitbutton","Quit")
    ),
    mainPanel(
      h3("Processing Sequence"),
      
      #h2("Raw Input"),
      #textOutput("echo_out"),
      
      h2("Tokens"),
      actionButton("lexit","Run Lexer"),
      tableOutput("lex_out"),
      
      h2("AST"),
      actionButton("parseit","Run Parser"),
      grVizOutput("ast")
      
    )
  )
)

server <- function(input, output) {
  
  #echo raw input
  #output$echo_out <- renderText({
  #  req(input$textin)
  #  paste(":", input$textin)
  #})
  # this is our stop button
  observeEvent(input$exitbutton, {stopApp(0)})
  
  # make a reactive element tt holding tokens table when lexer button is clicked
  tt <- eventReactive(input$lexit, {
    # lexer takes text input and generates token table tt
    lexer(input$textin)
  })
  # and render it
  output$lex_out <- renderTable({
    tt()
  })
  
  # same strategy to generate and render AST
  rn <- eventReactive(input$parseit, {
    dparse(tt())
    #as.data.frame(matrix(runif(n=100),nrow=50))
  })
  output$ast <- renderGrViz({
    #plot(rn())
    #rn()
    plot(rn())
  })
}

#run the app
shinyApp(ui = ui, server = server)