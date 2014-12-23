library(RCurl)
library(XML)


# -----------------------------------------------------------------------------
# FUNCTIONS

build.url2 <- function(school) {
  #name_enc <- paste(unlist(strsplit(as.character(name), " ")), collapse="+")
  name_enc <- curlEscape(as.character(school["school.adj"]))
  query <- paste(urlbase, name_enc, "&state=", sep="", collapse="")
  out = list()
  out$unitid <- school["unitid"]
  out$name <- as.character(school["school.adj"])
  out$query <- query
  out$city <- school["city.adj"]
  out$state <- school["state"]  
  out
}
update.url <- function(url, newname) {
  url$name <- newname
  name_enc <- curlEscape(as.character(newname))
  url$query <- paste(urlbase, name_enc, "&state=", sep="", collapse="")
  url
}
update.urlcity <- function(url, newname, newcity) {
  url$name <- newname
  name_enc <- curlEscape(as.character(newname))
  url$query <- paste(urlbase, name_enc, "&state=", sep="", collapse="")
  url$city <- newcity
  url
}

#------

get.doc <- function(url) {
  print(paste("...", url$name))          
  doc <- htmlTreeParse(url$query, useInternalNodes = T)
  doc
}  
get.nodes <- function(doc, tag, selector, name) {
  #xp_expr = "//a[@class='collegename']" 
  xp_expr <- paste("//", tag, "[@", selector, "='", name, "']", collapse="", sep="")
  #xp_expr <- paste("//a[@class='", name, "']", collapse="", sep="")
  nodes = getNodeSet(doc, xp_expr)
  nodes
}
get.page.link <- function(url) {
  
  out <- list()
  out$name <- url$name;
  out$unitid <- url$unitid;
  out$link <- NA;
  subcampus = 0;
  name.adj <- url$name;  
  
  print(url$name)  
  doc <- get.doc(url)  
  nodes <- get.nodes(doc, "a", "class","collegename")  
  #xp_expr = "//a[@class='collegename']" 
  
  if (length(nodes) == 0) {
    
    # subcampus-es
    if (grepl(" at ", url$name, fixed=TRUE)) {  
      name.adj <- gsub(" at ", "--", url$name)
      doc <- get.doc(update.url(url, name.adj))
      nodes <- get.nodes(doc, "a", "class", "collegename");
      if (length(nodes) == 0) subcampus = 1;
      
    } else if (grepl(" in ", url$name)) {
      name.adj <- gsub(" in ", "--", url$name)
      doc <- get.doc(update.url(url, name.adj))
      nodes <- get.nodes(doc, "a", "class", "collegename");
      if (length(nodes) == 0) subcampus = 1;            
      
    } else if (grepl("-", url$name)) {
      name.adj <- gsub("-", "--", url$name)
      doc <- get.doc(update.url(url, name.adj))
      nodes <- get.nodes(doc, "a", "class", "collegename");
      if (length(nodes) == 0) subcampus = 1;            
    } 
    # generally, there are no 'The's
    if (grepl("^(The )", name.adj)) {
      name.adj <- gsub("^(The )", "", name.adj)
      doc <- get.doc(update.url(url, name.adj))
      nodes <- get.nodes(doc, "a", "class", "collegename");            
    }
    
    if ((length(nodes) == 0) & subcampus == 1) {
      name.adj <- substring(name.adj, 0, regexpr("--", name.adj)-1)
      doc <- get.doc(update.url(url, name.adj))
      nodes <- get.nodes(doc, "a", "class", "collegename");      
    }
  }
  
  #print(paste("final node length: ", length(nodes)))    
  #print(nodes)
  names <- sapply(nodes,xmlValue)
  links <- sapply(nodes,xmlGetAttr,"href")
  
  if (length(names) == 1) {
    out$link <- links[1]
  }
  else {     
    if (length(names) > 1) {
      if (length(which(names==name.adj)) == 1) {
        out$link <- links[which(names==name.adj)]
      } else {
        # this is where we could end up with a non-sub campus that just happens to have a dash
        # but should just fail anyway
        loc_node <- get.nodes(doc, "p", "class", "citystate"); 
        #print(loc_node)
        loc_val <- sapply(loc_node,xmlValue)
        #print(loc_val)
        vec <- as.vector(strsplit(loc_val, ", "))
        state <- sapply(vec, function(x) x[2])
        city <- sapply(vec, function(x) x[1])
        print(city)
        #print(state)
        print(url$city)
        #print("which one")
        #print(which(city==as.character(url$city)))
        cs <- which(city==as.character(url$city))
        if (length(cs) == 1) {
          out$link <- links[cs]  
        }
      }  
    }     
  }
  # if we have more than two results and can't match one exactly on name or city , then we should not do anything -- NA
  
  out$name.adj <- name.adj
  out$names <- names
  out$links <- links
  out
  #print(xpathApply(nodes[[1]],"//a"))
}

get.sections <- function(doc) {
  print(doc$name)
  out <- NA
  if (!is.na(doc$link)) {
    print("... retrieving")
    doc <- htmlTreeParse(paste(site, doc$link, sep="", collapse=""), useInternalNodes = T)
    #doc <- htmlTreeParse(paste(site, doc$link, "/student-life", sep="", collapse=""), useInternalNodes = T)
    
    xp_exprs <- sapply(sections, function(x) paste("//div[@id='", x, "']", collapse="", sep=""))
    nodes <- sapply(xp_exprs, function(x) sapply(getNodeSet(doc, x), xmlValue))
    nodes <- sapply(nodes, function(x) ifelse(length(x)==0, NA, x))
    #nodes <- sapply(xp_exprs, function(x) sapply(getNodeSet(doc, x), function(y) ifelse(!is.na(y), xmlValue(y), NA)))
    #print(nodes)
    out <- sapply(nodes, function(x) {
      ifelse(!is.na(x), regmatches(x, regexpr("([0-9]+)",x)), NA) 
    })
  }
  out
  #names <- sapply(nodes,xmlValue)
  #links <- sapply(nodes,xmlGetAttr,"href")  
  #print(xpathApply(nodes[[1]],"//a"))
}