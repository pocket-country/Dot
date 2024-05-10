# Dot.R - A "toy" demo recursive descent parser for expressions hand crafted in the R programming language!
#ToDo:
# - (OK) implement + operator (i.e. pull from input not fixed)
#        -- this sets the stage for general set of operators
# - implement () for grouping ... this will really show of recursion vs regex
# - (OK) Figure out operator precedence -- did with proper grammar -  term/factor thing
# - error handling
# - - invalid characters
# - - incorrect syntax
# - (OK) evaluate (Was a simple little recursive function.  ignore complexity below easier to do than to think about)
#     - Initial thought would be to build on data.tree internal traversal functions
#     - But I'm not sure would work, convoluted, how to hold state, spend too much time on data.tree internals
#     - when just writing a set of recursive tree walk eval functions is easy.
#     - This essentially replicates the set of parse functions, in Java we avoid this using the Visitor Pattern
# - token types: 
#     -- (OK) operator 
#     -- (OKish) numeric - have my lame ass Digit
#     -- Strings?  What functionality
#     -- oh yea (OK) parenthesis & other syntax elements ... 
# - actual multi-digit numbers, 0 + pos integers (overflow?)
# - text panel with grammar & explanation
# - open in 'full screen' size .... Not an issue on Linux side!
# - L8ter make a package
# - (OK) plot display token type/value rather than node ID
# - (OK) figure out github push to facilitating working 'at home'
#
library(shiny)
library(data.tree)
library(DiagrammeR) #graph viz HTML widget to render tree data
library(igraph)     #Not sure if needed - for node plotting functions, such as SetNodeStyle   

## Lexing and parsing functions should come in automatically from Parse.R in ./R folder 
## according to anonymous sources on the interweb.  We shall see.

## Magic Function to format nodes, hard won knowledge: tast$Do(function(node) SetNodeStyle(node, label = node$sval, shape = "square"))

ui <- fluidPage(
  titlePanel(
    h4("===   Dot   ===", style = "text-align:center")
  ),
  tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  sidebarLayout(
    sidebarPanel(
      # so this "stuff" will appear in the sidebar - but because the app doesn't open in full screen mode
      # (or even close) it just squishes it up to the top, looks odd.
      "Settings",
      textInput("textin","Enter expression text here"),
      #checkboxInput("isfile","Use input as filename"),  ## currently unused
      hr(),
      p("Dot functionality - quit here or use buttons in processing tab panel!"),
      actionButton("exitbutton","Quit")
    ),
    mainPanel(
      tabsetPanel( 
        tabPanel("Processing",
          
          #h2("Raw Input"),
          #textOutput("echo_out"),
      
          actionButton("lexit","Run Lexer"),
          h2("Tokens"),
          tableOutput("lex_out"),
      
          actionButton("parseit","Run Parser"),
          h2("AST"),
          grVizOutput("ast"),
      
          actionButton("evalit","Evaluate AST"),
          h2("Value"),
          textOutput("result")
        ),
        tabPanel("Grammar",
          HTML(r"[
          <p>DOT Grammer</p>
          <h2><ul>
          <li> Expr -> Term</li>
          <li> Term -> Factor (("+" | "-") Factor)* </li>
          <li> Factor -> Primary (("*" | "/") Primary)* </li>
          <li> Primary -> DIGIT | "(" Expr ")"
          </ul></h2>
          ]")
        )
      )
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
    dot_parse(tt())
    #as.data.frame(matrix(runif(n=100),nrow=50))
  })
  output$ast <- renderGrViz({
    #rn()
    plot(rn())
  })
  
  #finally, we can evaluate the AST
  ev <- eventReactive(input$evalit, {
    dot_eval(rn())
  })
  output$result <- renderText({
    ev()
  })
}

#run the app
shinyApp(ui = ui, server = server)