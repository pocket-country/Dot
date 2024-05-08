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
      tokens <- rbind(tokens,list(token = "TIMES", value = "*"))
    }
    if (grepl("\\+",char)) {
      tokens <- rbind(tokens,list(token = "PLUS", value = "+"))
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

#naming function dot_parse (and dot_term, dot_factor ...) so as to avoid conflict with built in parse() function
#I really need to better understand scoping rules.  Where are all these tables stored?  Global? 
# how can I encapsulate them?

dot_parse <- function(tt) {
  # assume we have a table of tokens tt, set current pointer, call first production function/rule
  current <<- 1
  tokens <<- tt
  rootnode <- dot_expr()
  return(rootnode)
}
dot_expr <- function() {
  # have to make node first so can pass in to calls creating children for numbering - kinda awkward
  # but has to do with needing to assign unique names to children
  enode <- Node$new("E", production = "Expr", sval = "Root")  #will be root node
  
  enode$AddChildNode((dot_term("S")))
  return(enode)
}

dot_term <- function(idc) {
  # evaluate first part of production rule.  Either to be returned as "pass thru" or as LHS of binary
  expr <- dot_factor("L")
  # record the operator - the token value - call to primary has consumed the RHS token
  op_value = tokens[current,]$value
  #If we find a plus operator, look for the other side of the equation.  Can be another factor
  while(match("PLUS")) {
    # get RHS ... this is the */'one or more' in the production rule
    right <- dot_factor("R")
    # make a node to hold the results, add expr as LHS.  
    # am using a tmp variable as don't want to overwrite expr ... this isn't Java
    tmp <- Node$new(idc, production = "Term", sval = op_value)
    tmp$AddChildNode(expr)
    tmp$AddChildNode(right)
    expr <- tmp # with expr captured as a child at this point
  }
  return(expr)  # either as computed in first factor eval or as computed in while
}

dot_factor <- function(idc) { #same logic so leave out comments
  expr <- dot_primary("L")
  op_value = tokens[current,]$value
  while(match("TIMES")) {
    right <- dot_primary("R")
    tmp <- Node$new(idc, production = "Factor", sval = op_value)
    tmp$AddChildNode(expr)
    tmp$AddChildNode(right)
    expr <- tmp
  }
  return(expr)
}

dot_primary <- function(idc) {
  # record literal value - should be a digit - before match moves pointer
  literal_value = tokens[current,]$value
  # only digits for now, add test for paren when adding grouping
  if (match("Digit")) {
    return(Node$new(idc, production = "Primary", sval = literal_value))
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
