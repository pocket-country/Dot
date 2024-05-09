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
  
  # no real robust error handling - 
  # for multi char token error handling, restructure by splitting into outer & inner - inner
  # returns token or error, outer handles rbind token to frame, catches error
  while (ptr <= length(buffer)) { 
  
    char <- buffer[ptr]  # [[]][] notation yr kidding me!
    
    #simple error handling works only as we have single character tokens ...
    # ... allow these or space, that is it!
    if (!grepl("[0123456789+*-s/ ]",char)) {
      if (!isRunning()) {stop("Invalid character in input.")}  #not in shiny app, report error and quit
      #otherwise return an error message for Shiny to post
      # !! shiny UI element is expecting a table, what will it do with a string?
      return("Invalid character in input.")
    }
    # silently ignore spaces, so whitespace handled.  if processing file input would have to look for \n \t
    # also ... no comments.  So this is very simple!
    if (grepl("[0123456789]",char)) {
      # removed strtoi() so token values will be stored as strings? Characters?  Is there a diff in R (there is in C)?
      # tokens <- rbind(tokens,list(token = "Digit", value = strtoi(char)))
      tokens <- rbind(tokens,list(token = "Digit", value = char))
    }
    # so now we have an operator token that combines the two terms on either side of it, 
    # with the value of the token determining the operation.  
    if (grepl("\\*",char)) {
      tokens <- rbind(tokens,list(token = "STAR", value = "*"))
    }
    if (grepl("\\+",char)) {
      tokens <- rbind(tokens,list(token = "PLUS", value = "+"))
    }
    if (grepl("\\-",char)) {
      tokens <- rbind(tokens,list(token = "MINUS", value = "-"))
    }
    if (grepl("\\/",char)) {
      tokens <- rbind(tokens,list(token = "SLASH", value = "/"))
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

# also using some global variables which makes me uncomfortable.  Can I have "global to script" vars in R
# - current
# - tokens
# - GID

ggid <- function() {
  ## maintain a global ID number for nodes
  # node names have to be unique across siblings, not globaly
  # I tried to create a scheme of labeling them "L" and "R" but was tripped up by the 
  # recursive nature of the parser calls ... 'pass through' would assign same name twice
  # which means the second child added would overwrite the first ... this took a while to 
  # track down.
  # So a global ID number will (over)satisify this requirement & declutter the parser function 
  # signature (no passing idc) - at the expense of maintaining a global var and a helper function
  # and since there is no concurrency at the moment, well it will work
  # Note when we get famous and this is a web app with multiple instances running we will have to revisit
  gidstr <- toString(GID)
  GID <<- GID + 1
  return(gidstr)
  }

dot_parse <- function(tt) {
  # assume we have a table of tokens tt, set current pointer, call first production function/rule
  current <<- 1   # pointer to current row in token table being processed
  tokens <<- tt   # global token table being processed
  GID <<- 0       # next avaliable global node ID 
  rootnode <- dot_expr()
  rootnode$Do(function(node) SetNodeStyle(node, label = paste0("N",node$name,":  ",node$sval), shape = "square"))
  return(rootnode)
}
dot_expr <- function() {
  # have to make node first so can pass in to calls creating children for numbering - kinda awkward
  # but has to do with needing to assign unique names to children
  enode <- Node$new(ggid(), production = "Expr", sval = "Root")  #will be root node
  
  enode$AddChildNode(dot_term())
  return(enode)
}

dot_term <- function() {
  # evaluate first part of production rule.  Either to be returned as "pass thru" or as LHS of binary
  expr <- dot_factor()
  # record the operator - the token value - call to primary has consumed the RHS token
  op_value = tokens[current,]$value
  #If we find a plus operator, look for the other side of the equation.  Can be another factor
  while(match(c("PLUS","MINUS"))) {
    # get RHS ... this is the */'one or more' in the production rule
    right <- dot_factor()
    # make a node to hold the results, add expr as LHS.  
    # am using a tmp variable as don't want to overwrite expr ... this isn't Java
    tmp <- Node$new(ggid(), production = "Term", sval = op_value)
    tmp$AddChildNode(expr)
    tmp$AddChildNode(right)
    expr <- tmp # with expr captured as a child at this point
  }
  return(expr)  # either as computed in first factor eval or as computed in while
}

dot_factor <- function() { #same logic so leave out comments
  expr <- dot_primary()
  op_value = tokens[current,]$value
  while(match(c("STAR","SLASH"))) {
    right <- dot_primary()
    tmp <- Node$new(ggid(), production = "Factor", sval = op_value)
    tmp$AddChildNode(expr)
    tmp$AddChildNode(right)
    expr <- tmp
  }
  return(expr)
}

dot_primary <- function() {
  # record literal value - should be a digit - before match moves pointer
  literal_value = tokens[current,]$value
  # only digits for now, add test for paren when adding grouping
  if (match("Digit")) {
    return(Node$new(ggid(), production = "Primary", sval = literal_value))
  }
  ## if we are here is an error of some kind
  if (!isRunning()) {stop("Malformed expression.")}  #not in shiny app, report error and quit
  #otherwise return an error message for Shiny to post
  # shiny UI element is expecting a graph object, what will it do with a string? barfs.  
  # so make a node
  return(Node$new(ggid(), production = "ERROR", sval = "Malformed Expression"))
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

## a simple evaluate function
dot_eval <- function(ast) {
  if (isLeaf(ast) ) {
    print(ast$name)
    #just return value; need to test transform worked to catch error 'signal'
    return(strtoi(ast$sval))
  }
  else {
    print(ast$name)
    # could probably write this more efficiently, but going for clarity
    # we only have unary & binary operations and our only unary operation is 
    # expr/grouping, which simply returns a value (i.e. don't have negation, ..)
    # so, ...
    if (ast$count == 1) {
      #no unary operator so just return value of expression
      return(dot_eval(ast$children[[1]]))
    }
    if (ast$count == 2 ) {
      # get the two values for binary operator
      v1 = dot_eval(ast$children[[1]])
      v2 = dot_eval(ast$children[[2]])
      # combine as per operator - only have two at this point
      switch (ast$sval, 
        "+" = return(v1+v2),
        "*" = return(v1*v2),
        "-" = return(v1-v2),
        "/" = return(v1/v2)
      )
      # if we are here we have a problem
    }
  }
}
