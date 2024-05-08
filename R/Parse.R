# Define functions used in Dot.R

# must include library data.tree for node functions/tree data structure.

# note using a data frame to hold tokens rather than a true object means 
# that we don't have different sets of attributes for different token "types" coming out of lexer
#

lexer <- function(rawtext) {
  textaslist <- strsplit(rawtext, "")
  buffer <- unlist(textaslist)
  remove(textaslist)
  
  ptr <- 1
  tokens <-data.frame(token = character(), value = integer(), stringsAsFactors = FALSE)
  
  # no error handling - silently ignore characters not in grammar
  while (ptr <= length(buffer)) { 
  
    char <- buffer[ptr]  # [[]][] notation yr kidding me!
  
    if (grepl("[0123456789]",char)) {
      # removed strtoi() so token values will be stored as strings? Characters?  Is there a diff in R (there is in C)?
      # tokens <- rbind(tokens,list(token = "Digit", value = strtoi(char)))
      tokens <- rbind(tokens,list(token = "Digit", value = char))
    }
    # so now we have an operator token that combines the two terms on either side of it, 
    # with the value of the token determining the operation.  
    if (grepl("\\*",char)) {
      tokens <- rbind(tokens,list(token = "Operator", value = "*"))
    }
    if (grepl("\\+",char)) {
      tokens <- rbind(tokens,list(token = "Operator", value = "+"))
    }
    
    ptr = ptr + 1
  }
  # add an EOF token to mark the end of the stream
  tokens <- rbind(tokens,list(token = "EOF", value = " "))
  return(tokens)
}

## parser functions -- see notebook for reduced grammar, following Nystrom
## For a recursive descent parser, functions corresponding to production rules in grammar

## Data.tree requires ea. node at a particular level have a unique name ... 
## As these are not anonymous object ids generated automatically like in Java, 
## this clutters up the function calls with characters passed in used to distinguish child node names.
## (variable idc = index character)

#naming it dparse so as to avoid conflict with built in parse() function
#I really need to better understand scoping rules.  Where are all these tables stored?  Global? 
# how can I encapsulate them?

dparse <- function(tt) {
  # assume we have a table of tokens tt, set current pointer, call first production function/rule
  current <<- 1
  tokens <<- tt
  rootnode <- expr()
  return(rootnode)
}
expr <- function() {
  # have to make node first so can pass in to calls creating children for numbering - kinda awkward
  # but has to do with needing to assign unique names to children
  enode <- Node$new("Expression", token = "N/A", sval = "Root")  #will be root node
  
  enode$AddChildNode((binary("C")))
  return(enode)
}

binary <- function(idc) {
  # get the RHS node - we expect this to be a primary (the only primary is Digit) ... needs error checking
  right <- primary("R")
  
  # record the operator - the token value - call to primary has consumed the RHS token
  op_value = tokens[current,]$value
  
  #If we find an operator, look for the other side of the equation.  Can be another binary ...
  while(match("Operator")) {
    left <- primary("L")
    # make a node to hold the results, ... 
    tmp <- Node$new(paste("Binary",idc,sep="_"), token = "Operator", sval = op_value)
    tmp$AddChildNode(right)
    tmp$AddChildNode(left)
    expr <- tmp
  }
  return(expr)
}

primary <- function(idc) {

  # record the operator in the token - should be a digit - before match moves pointer
  op_value = tokens[current,]$value
  
  if (match("Digit")) {
    return(Node$new(paste("Unary",idc,sep="_"), token = "Digit", sval = op_value))
  }
  ## if we are here is an error of some kind - need error handling
}

## helper functions that work with the data frame holding the tokens
## As our list of tokens is a data frame, a "Token" is a row in the dataframe, and is generally
## returned as a list with the first element being the token type and the rest things like value, lexeme, etc.

# right now these are in open code, so assume DF tokens and integer current pointing to the current row/token

# the match function is our work-horse
match <- function(types) {
  ## return true if this token (current row) matches any token on the input list
  for (t in types) {
   if (check(t)) {
     advance()
     return(TRUE)
   }
  }
  return(FALSE)
}

#helper helper functions
check <- function(type) {
  if(isAtEnd()) return(FALSE)
  return(peek()[[1]] == type)
}

# functions to deal with current token pointer and "reading" token "buffer"
# "Consume" a token, returning it, and advance pointer
advance <- function() { 
  if(!isAtEnd()) {current <<- current + 1}
  return(previous())
}
# Have we reached the end of the tokens (data frame)?
isAtEnd <- function() {
  return(peek()[[1]] == "EOF")
}
# These two actually return a token (list)
# but don't affect current token pointer

peek <- function() {
  return(tokens[current, ])
}

previous <- function() {
  return(tokens[current-1, ])  
}

## End of helper functions
