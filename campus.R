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

# -----------------------------------------------------------------------------
# START

# -----------------------------------------------------------------------------
# IPEDS

iped <- read.csv("ipedschools_d2.csv", stringsAsFactor=FALSE)
dim(iped) #7597 - in the 50 states ONLY for 2013-2014 --- NOT Guam, Palau, Marshall Islands, etc
head(iped)
str(iped)
names(iped) <- c("unitid", "school","state", "entity", "city","zip","state.code","region",
                 "opeid", "mult.campus", "mult.campus.name", "county.code", "county","cbsa",
                 "cbsa.type", "csa", "unitid.merged", "open", "cong.dist",
                 "sector", "level","control","deg.grant","histblack","tribal","urban",
                 "psec.titleiv", "inst.cat","car.sizesetting", "carneg.ug", "carneg.enroll",
                 "carneg.basic", "carneg.uginst",
                 "size", "academic", "all.online",
                 "price.inst.oncamp","price.outst.oncamp", 
                 "ug.25older", "gr.25older","all.25older",
                 "ug.enroll","gr.enroll","pctwomen.tot.enroll",
                 "pctwomen.ug.enroll","pctwomen.gr.enroll","tot.enroll","fte.enroll","ft.enroll",
                 "pt.enroll", "pctug.finaid", "pctug.pubfinaid", "avg.finaid","fte.staff")
# difference between "school" and "entity" entry - none except for 4 rows
# those are due to capitalization.  So get rid of entity variable, no difference.
iped[which(iped$entity != iped$school),1:4]
iped <- iped[,-4]
# this just gets rid of any unicode, multibyte chars
iped$school.adj <- gsub("\211\333\322", "-", iped$school)
iped$school.adj <- gsub("\211\333\252", "'", iped$school.adj)
# this is often an issue with joining other datasets particulary greek data
iped$city.adj <- gsub("Saint", "St.", iped$city)
# will need this later for crime data
iped$city.uc <- toupper(iped$city)
# will need this later for crime data
table(nchar(iped$zip)) # these are the cases
iped$zip.base <- iped$zip
# leading zeros missed so just add on
iped$zip.base <- ifelse(nchar(iped$zip.base)==4 | nchar(iped$zip.base)==8, paste("0", iped$zip.base, sep=""), iped$zip.base)
iped$zip.base <- substring(iped$zip.base, 1, 5) # strip out whats after the dash
table(nchar(iped$zip.base))

#iped[grep("The King", iped$school.adj,fixed=TRUE),]
#iped[grep("A & M", iped$school.adj,fixed=TRUE),]
#iped[grep("Globe", iped$school.adj,fixed=TRUE),]

# degree granting isntitutions ONLY: (1)
# level: eliminate less than 2 year schools (3)
# sector: eliminate administrative units (0)
iped <- iped[which(iped$level != 3 & iped$deg.grant == 1 & iped$sector != 0),]
dim(iped) # 4899
str(iped)
head(iped)

# further eliminate
# region: eliminate military service schools (0)
# open: eliminate those not open to the public (0)
# acadmic: eliminate non-academic (0)
# all online: eliminate those with only on-line offerings (1)
iped <- iped[which(iped$region != 0 & iped$open != 0 & iped$academic != 0 & iped$all.online != 1),]
dim(iped) # 4511
iped <- iped[which(iped$ug.enroll != 0),]
dim(iped) # 4205


# -----------------------------------------------------------------------------
# GET CRIME - Campus Safety & Security

# get crime data from all parts of campus
oc.crime <- read.csv("oncampuscrime101112.csv", stringsAsFactors=FALSE, encoding="UTF-8")
oc.crime$cr.where <- 0
nc.crime <- read.csv("noncampuscrime101112.csv", stringsAsFactors=FALSE, encoding="UTF-8")
nc.crime$cr.where <- 1
pp.crime <- read.csv("publicpropertycrime101112.csv", stringsAsFactors=FALSE, encoding="UTF-8")
pp.crime$cr.where <- 2
# put it together in rows
crime <- rbind(oc.crime, nc.crime, pp.crime)
dim(crime) #11064 times 3 = 33192
str(crime)
head(crime)
length(which(table(crime$UNITID_P) == 3)) # 11064.  So each school has 1 row in each set, good.
length(which(table(crime$UNITID_P) != 3)) # 0  So each school has 1 row in each set, good.

# get totals over all three crime "locations" -- on campus, off campus, public property
add.it <- function(arg) {
  #print(arg)
  #print(ifelse(all(is.na(arg)), NA, sum(arg, na.rm=TRUE)))
  ifelse(all(is.na(arg)), NA, sum(arg, na.rm=TRUE))
}
all.campus.totals <- aggregate(crime[,c(1,13:39)], by=list(crime$UNITID_P), FUN=add.it)
which(crime$UNITID_P[1:11064] != all.campus.totals$Group.1) # should be zero -- this comes back in same row order
all.campus.totals$UNITID_P <- all.campus.totals$Group.1

crime <- merge(oc.crime[,c(1:3,5:8)], all.campus.totals[,-1], by.x="UNITID_P", by.y="UNITID_P", all.x=TRUE, all.y=TRUE)
which(table(crime$UNITID_P) != 1) # 0
length(table(crime$UNITID_P) == 1)# 11064
head(crime)

# combine both sex offenses into one variable for each year
grep("FORCIB", names(ic))
grep("NONFOR", names(ic))
grep("10", names(ic))
flag <- apply(crime[,c("FORCIB10","NONFOR10")],1, function(x) all(is.na(x)))
crime$cr.tot.sex.off.10 <- ifelse(flag, NA, rowSums(crime[,c("FORCIB10","NONFOR10")], na.rm=TRUE)) 
flag <- apply(crime[,c("FORCIB11","NONFOR11")],1, function(x) all(is.na(x)))
crime$cr.tot.sex.off.11 <- ifelse(flag, NA, rowSums(crime[,c("FORCIB11","NONFOR11")], na.rm=TRUE)) 
flag <- apply(crime[,c("FORCIB12","NONFOR12")],1, function(x) all(is.na(x)))
crime$cr.tot.sex.off.12 <- ifelse(flag, NA, rowSums(crime[,c("FORCIB12","NONFOR12")], na.rm=TRUE)) 
# get totals for all three years
grep("tot.sex.off", names(crime))
crime$cr.sex.off.101112.tot <- apply(crime[,grep("tot.sex.off", names(crime))], 1, function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE)))
crime$cr.sex.off.101112.avg <- apply(crime[,grep("tot.sex.off", names(crime))], 1, function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm=TRUE)))

crime$cr.fullid <- as.character(crime$UNITID_P)
crime$cr.unitid <- substring(crime$UNITID_P, 0, 6)
crime$cr.branchid <- substring(crime$UNITID_P, 7, 9)
crime$cr.school <- crime$INSTNM
crime$cr.branch <- crime$BRANCH
crime$cr.city.adj <- gsub("\x97n", "", crime$City)
crime$cr.city.uc <- toupper(crime$cr.city.adj)
crime <- crime[which(crime$sector > 0 & crime$sector < 7),] # 8857
crime <- crime[,-7] # get rid of sector now
nrow(crime) # 8857
str(crime)

# all of these less that are not 5 or 9 characters are campuses outside US
table(nchar(crime$ZIP))
# do not have to worry about campus-es outside US; they will not match with an IPED one
crime$zip.base <- crime$ZIP
crime$zip.base <- substring(crime$zip.base, 1, 5) # take first five



# there are NO NA's in the sex off crime data -- all have a number
cstats <- c(2,9,10,34,18,19,35, 27,28,36,37,38)
head(crime[,c(2,9,10,34,18,19,35, 27,28,36,37,38)])
summary(head(crime[,c(2,9,10,34,18,19,35, 27,28,36,37,38)]))


# -----------------------------------------------------------------------------
# MERGE IPEDS and Crime Data
# take care of duplicates and missing data

# the ones in iped w/no crime data
noc <- setdiff(iped$unitid, crime$cr.unitid)
length(noc) # 299 145
     
ic2 <- merge(iped, crime, by.x=c("unitid","zip.base"), by.y=c("cr.unitid", "zip.base"), all.x=TRUE, all.y=FALSE)
ic <- ic2
# FYI NOT using IPEDS city.adj to merge (that is for Greek data) -- use IPED's city ( is city.uc)
#ic <- merge(iped, crime, by.x=c("unitid","city.uc"), by.y=c("cr.unitid", "cr.city.uc"), all.x=TRUE, all.y=FALSE)
#ic.bak <- ic
#str(ic)
dim(ic) # 5375 4324

# -----------------------------------------------------------------------------
# handle duplicates 
# since we have one row per unit ID going in, we need to have one row going out
dupids <- names(table(ic$unitid)[table(ic$unitid) > 1]) 
length(dupids) # 261 dup IDS 88
duprows <- (ic$unitid %in% dupids)
sum(duprows) # 737 rows 207

# check that none of the missing rows are a duplicate index, though that wouldn't make sense
#sum(missingcr & duprows) # 0
#stats <- c(1:3, 33,34,37,68)
#ic[dupindexes,stats]
mainc <- ifelse(is.na(ic$cr.branchid), FALSE, (ic$cr.branchid == "001"))
sum(mainc) #4399 not sure what thismeans 3862
#ic[dupindexes & mainc,stats] # 258 of them
nrow(ic[duprows & mainc,]) # 258 of them 88
nrow(ic[duprows & !mainc,]) # 479 of them 119... 88 + 119 = 207
setdiff(ic[duprows,4], ic[duprows & mainc,4]) # these would have no "Main Campus"... now zero
# we have one row in IPED; we can't let two rows in CRIME data match to it 
# becuse the crime data would be for two different campuses, but the IPED data for the same one
# Rule: If you can tell which is Main campus (or branch 001), and city and Unitid both match, then match on that.
# If you can't tell which is Main Campus (ie if there is no main campus or 001 entry in crime) take the entry out - there is no
#   other godo way to make the decision.
# SO: take out Non-Main campus rows for the  duplicates that list a Main Campus
# AND take out all rows for the duplicates with no Main Campus listed
to.remove <- (duprows & !mainc)
sum(to.remove) # 479  119
ic[to.remove, icstats]
ic <- ic[!to.remove,]
nrow(ic) # 4896 = 5375 - 479. NOW 4324 - 119 = 4205.

icstats <- c("unitid", "zip.base","school","city", "state","cr.fullid", "cr.branchid","BRANCH")
cschools <- c("UNITID_P","cr.fullid", "cr.unitid", "cr.branchid","zip.base","City", "State","INSTNM")

# -----------------------------------------------------------------------------
# now check missing rows
# now check which rows are missing, aka "Missing crime" (missingcr)
# because some are due to error
missing.rows <- is.na(ic$cr.fullid) 
sum(missing.rows) # 473 #317
ic$zipmerge <- ifelse(missing.rows, 0, 1) #317 have zeros
head(ic[missing.rows, icstats])

missingids <- ic[missing.rows, "unitid"] # 317 unique IDS in iped with no crime data
missingipedrows <- (iped$unitid %in% missingids)
sum(missingipedrows) # 317
missingcrrows <- (crime$cr.unitid %in% missingids)  
sum(missingcrrows) # 482 rows in crime table with those 317 unit iDS
head(crime[missingcrrows, cschools])

ic.redo <- merge(iped[missingipedrows,], crime[missingcrrows,], 
              by.x=c("unitid","city.uc"), 
              by.y=c("cr.unitid", "cr.city.uc"), all.x=TRUE, all.y=FALSE)
missing.rows <- is.na(ic.redo$cr.fullid) 
sum(missing.rows) # 186. so 317-186=131 rows merged
matches <- ic.redo[!missing.rows,]
matches$zipmerge <- 0
matches$citymerge <- 1
matches$zip.base <- matches$zip.base.x
matches$cr.city.uc <- toupper(matches$cr.city.adj)
matches <- matches[,-c(57,101)]
dim(matches) # 153.. we have some duplicates
ic$citymerge <- 0
ic <- rbind(ic, matches)
dim(ic) #4358 = 4205 + 153

missingids <- ic.redo[missing.rows, "unitid"] # 186
# 145 missing unit IDs for which there is no crime UNITID - can't make a match
setdiff(missingids, crime$cr.unitid)
# 41 for which there is a mathcing UNITID in crime
# work on these
missingids <- intersect(missingids, crime$cr.unitid)
length(missingids) # 41
missingipedrows <- (iped$unitid %in% missingids)
sum(missingipedrows) # 41
missingcrrows <- (crime$cr.unitid %in% missingids)  
sum(missingcrrows) # 188 rows in crime table 


ic.redo <- merge(iped[missingipedrows,], crime[missingcrrows,], 
                   by.x=c("unitid"), 
                   by.y=c("cr.unitid"), all.x=TRUE, all.y=FALSE)
missing.rows <- is.na(ic.redo$cr.fullid) 
sum(missing.rows) # 0 - all merged
dim(ic.redo) # 188 - lots of duplicates
ic.redo$zip.base <- ic.redo$zip.base.x
ic.redo <- ic.redo[,-c(57,102)]
ic.redo$zipmerge <- 0
ic.redo$citymerge <- 0
ic.redo$unitmerge <- 1
ic$unitmerge <- 0
ic <- rbind(ic, ic.redo)
dim(ic) # 4546 = 4358 + 188 #OLD 4358 = 4205 + 153

#just check
table(ic$zipmerge) #3888  
table(ic$citymerge) #153
table(ic$unitmerge) #188. Total 4229.

# -----------------------------------------------------------------------------
# now take out any dusplicates I just introuduced
# by fixing the missing data

dupids <- names(table(ic$unitid)[table(ic$unitid) > 1]) 
length(dupids) # 172
duprows <- (ic$unitid %in% dupids)
sum(duprows) # 513

mainc <- ifelse(is.na(ic$cr.branchid), FALSE, (ic$cr.branchid == "001")) # this also gets ones that were originally unmatched
sum(mainc) #4031
#ismatched <- (ic$zipmerge==0 & ic$citymerge==0 & ic$unitmerge==0)
nrow(ic[duprows & mainc,]) # 169
nrow(ic[duprows & !mainc,]) # 344  .. + 169 = 513


# -----------------------------------------------------------------------------
# fix incorrect matches

# these matched on an ID alone (so not to a ZIP or city), but not to a main campus
# can't let these match because not confident i have the right campus
setdiff(ic[duprows,3], ic[duprows & mainc,3])
wrongmatchids <- setdiff(ic[duprows,1], ic[duprows & mainc,1])
#removerows <- (ic$unitid %in% wrongmatchids) & !is.na(ic$cr.branchid)
removerows <- ifelse(ic$unitid %in% wrongmatchids, !is.na(ic$cr.branchid), TRUE)
#to.remove <- (duprows & !mainc)
to.remove <- (duprows & !mainc & removerows)

sum(to.remove) # 344 341
ic[to.remove, icstats]
ic <- ic[!to.remove,]
nrow(ic) # 4205.  4546-341.

# -----------------------------------------------------------------------------
# for the unitids with no matches in Crime data... 
# can we do anything?
# NOT REALLY.  
length(which(ic$zipmerge==0 & ic$citymerge==0 & ic$unitmerge==0)) # this is 148
length(which(is.na(ic$cr.fullid))) # this is 148
length(setdiff(ic[which(is.na(ic$cr.fullid)),"unitid"],crime$cr.unitid)) # 145, because of three incorrrect matches we left in IC

nomatch.ids <- setdiff(ic[which(is.na(ic$cr.fullid)),"unitid"],crime$cr.unitid)
missingipedrows <- iped$unitid %in% nomatch.ids
sum(missingipedrows) # 145 here too
head(iped[missingipedrows,ipstats])
iped[missingipedrows,ipstats]



# -----------------------------------------------------------------------------
# for testing 

iped[which(iped$unitid=="482680"), ipstats]
crime[which(crime$cr.unitid=="482680"), cschools]
# case where IPED has these as different unitids but Crime has them as one unitid with sub-branchids
# branches will not match becuase unitids dont
iped[grep("Arizona State", iped$school.adj), ipstats]
crime[grep("Arizona State", crime$cr.school), cschools]




# -----------------------------------------------------------------------------
# final clean up 
ic$cr.state <- ic$State
ic$cr.zip <- ic$ZIP

grep("UNITID_P", names(ic))
names(ic)[58:63]
ic <- ic[,-c(58:63)]
str(ic)

length(which(is.na(ic$fte.enroll))) # 0
ic$cr.sex.off.101112.tot.per1000fte <- ifelse(is.na(ic$fte.enroll), NA, (1000*ic$cr.sex.off.101112.tot) / ic$fte.enroll)
ic$cr.sex.off.101112.avg.per1000fte <- ifelse(is.na(ic$fte.enroll), NA, (1000*ic$cr.sex.off.101112.avg) / ic$fte.enroll)

# -----------------------------------------------------------------------------
# make some better categories
ic$urban.adj <- ifelse(ic$urban < 0, 0, 
                       ifelse(ic$urban < 20, 1, 
                       ifelse(ic$urban < 30, 2,   
                       ifelse(ic$urban < 40, 3, 4))))


# -----------------------------------------------------------------------------
# GET GREEK DATA

# --
# create an object with the data necessary to get the urls for right USNews search for each school

site <- "http://colleges.usnews.rankingsandreviews.com"
urlbase <- "http://colleges.usnews.rankingsandreviews.com/best-colleges/search?name="

#urls <- as.list(apply(ic[1:10,], 1, function(x) build.url2(x)))
urls <- as.list(apply(ic, 1, function(x) build.url2(x)))
length(urls) # 4899
urls[[10]]
# urls.bak <- urls
# --
# use the search URL object get the URL of the USNews page for the school

pages <- NA
pages <- lapply(urls, function(x) get.page.link(x))
length(pages) # 4896
#pages.bak <- pages
#pages.bak2 <- pages
#pages.bak3 <- pages.bak2
sum(sapply(pages, function(x) !is.na(x$link))) # 2067 page hits on US Nes
cond <- sapply(pages, function(x) !is.na(x$link))
hits <- sapply(pages[cond], function(x) x$name) # 2067
length(hits)
misses <- sapply(pages[!cond], function(x) x$name) # 2829
length(misses)
as.vector(hits[1:100])
as.vector(misses[1:100])

# test cases for this
misses[grep("The University of Alabama", misses)]
hits[grep("The University of Texas at Austin", hits)]

which(ic$school=="Union College")
which(ic$school=="Kettering College") # 2409
ic[which(ic$school=="Kettering College"),]

test <- ic[2172,] # Just picked one - Union College schnectady
get.page.link(build.url2(test)) 

get.page.link(update.url(test, "All-State Career School-Allied Health Campus")) 
get.page.link(update.url(test, "The University of Alabama at Birmingham"))
get.page.link(update.url(test, "The University of Tennessee-Chattanooga"))
get.page.link(update.url(test, "The University of Alabama"))
get.page.link(update.url(test, "Antioch College")) # no results
get.page.link(update.url(test, "Antioch University-Los Angeles"))
get.page.link(update.url(test, "Antioch University"))
get.page.link(update.url(test, "The Keiser University-Ft Lauderdale"))
get.page.link(update.url(test, "Keiser University-Ft Lauderdale"))
get.page.link(update.url(test, "The University of Texas at Austin"))
get.page.link(update.url(test, "Trinity College"))
get.page.link(update.url(test, "ATA College")) # JuaniATA -- !! need to fix
get.page.link(update.urlcity(test, "Arizona State University", "Tempe"))
get.page.link(update.urlcity(test, "University of Colorado", "Boulder"))
get.page.link(update.urlcity(test, "University of Colorado", "Denver"))
get.page.link(update.url(test, "University of Colorado at Denver")) # denver no matter what the city field
get.page.link(update.urlcity(test, "Anderson University", "Anderson")) # !! both in town called anderson in two different states - match on city AND state
get.page.link(update.urlcity(test, "Aquinas College", "Nashville"))
get.page.link(update.urlcity(test, "Aquinas College", "blah"))
get.page.link(update.url(test, "Augustana College")) # 2 colleges, same name, doesn't match eother city, so no match
get.page.link(update.url(test, "Baptist Theological Seminary")) # Different prefix, names don't match exacy exactly so should be NA (city is wrong here)
get.page.link(update.urlcity(test, "Baptist Theological Seminary", "New Orleans")) # (city is RIGHT here)
get.page.link(update.url(test, "Bel-Rea Institute of Animal Technology")) # same sitch as All-State
get.page.link(update.urlcity(test, "Bethel University", "Saint Paul")) # - Saint Paul / St Paul in city
get.page.link(update.urlcity(test, "Bethel University", "St. Paul")) # - Saint Paul / St Paul in city
get.page.link(update.urlcity(test, "Concordia University", "St. Paul")) #- Saint Paul / St Paul in city
get.page.link(update.url(test, "Mid-Plains Community College")) # same as Miller-Motte TEchnical college, All-State
get.page.link(update.urlcity(test, "Washington University in St Louis", "Saint Louis")) 
get.page.link(update.urlcity(test, "Washington University in St Louis", "St Louis")) 




sort(table(as.character(iped$school)) # these will get schools that will give us multiple hits - ex Union College          
     
     
     # DONE !! figure out the [1] "Globe University\211\333\322Green Bay"  
     # DONE !! figure out the ampersand and dash when you encode it
     # !! substitute 'Agricultural and Mechanical' for A&M' 
     # DONE Colleges w/same name... multiple results (UNion College).. so need to add city match
     # !! Need to check when there is one result AND if it is the right college!! ex ATA COllege - Juaniata
     # !! Name with a dash such as "all-state" that has NO results initially -- same as Miller-Motte TEchnical college, All-State
     # DONE Fix "Saint" in city in iPED
     # !! Once college, same city name, two different states -- Anderson Unviersity -- need to mathc on City AND STATE
     
     #--
     # now get the data from each individual college page


# now get the data from each invididual school page     
sections = c("fraternity_members", "sorority_members")
data <- NA
data <- lapply(pages, function(x) get.sections(x))
data.bak <- data
#data.bak2 <- data
#data.bak3 <- data.bak2
length(data)
gdata <- sapply(data, function(x) !is.na(x[1]))
sum(gdata) # 1065 = 66six + 399
gdata.notzero <- sapply(data, function(x) !is.na(x[1]) & !as.numeric(x[1]) == 0)
sum(gdata.notzero) # 66six
gdata.zero <- sapply(data, function(x) !is.na(x[1]) & as.numeric(x[1]) == 0)
sum(gdata.zero) # 399


# -----------------------------------------------------------------------------
# MERGE SCHOOL data (iped/crime) with GREEK DATA

unitid <- as.character(unlist(lapply(pages, function(x) x["unitid"])))
length(unitid)
school.adj.greek <- as.character(sapply(pages, function(x) ifelse(!is.na(x$link), x$name.adj, NA)))
length(school.adj.greek)
pct.frat <- as.numeric(unlist(lapply(data, function(x) x["fraternity_members"])))
length(pct.frat)
pct.sor <- as.numeric(unlist(lapply(data, function(x) x["sorority_members"])))
length(pct.sor)

greeks <- data.frame(unitid, school.adj.greek, pct.frat, pct.sor, stringsAsFactors=FALSE) 
str(greeks)
head(greeks)
dim(greeks)

setdiff(ic$unitid, greeks$unitid) # 0
setdiff(greeks$unitid, ic$unitid) # quite a few
icg <- merge(ic, greeks, by.x="unitid", by.y="unitid", all.x=TRUE, all.y=FALSE)
dim(icg) #4205
str(icg)
head(icg)

length(which(!is.na(icg$pct.frat))) # 1065 1024
length(which(!is.na(icg$pct.sor))) # 1064 1023


#!! -----
# SHOULD NOT DO THIS HERE
# BUt these are schools in Top 100 greek and Sorority that I did NOT get Greek data for because of weird names
# so I am entering here for now
setdiff(frat$school, icg.doj$school.adj.greek)

icg[grep("Tulane University of Louisiana", icg$school.adj),]
get.sections(get.page.link(update.url(test, "Tulane University"))) #Tulane University of Louisiana
current <- which(icg$school.adj=="Tulane University of Louisiana") 
length(current)
icg[current,]
icg[current,]$pct.frat <- 26
icg[current,]$pct.sor <- 43

icg[grep("Hillsdale Free Will Baptist College", icg$school.adj),]
get.sections(get.page.link(update.url(test, "Hillsdale College"))) #Hillsdael Free Will Baptist College
current <- which(icg$school.adj=="Hillsdale Free Will Baptist College") 
length(current)
icg[current,]
icg[current,]$pct.frat <- 33
icg[current,]$pct.sor <- 44

icg[grep("Washington & Jefferson College", icg$school.adj),]
get.sections(get.page.link(update.url(test, "Washington and Jefferson College"))) #Hillsdael Free Will Baptist College
current <- which(icg$school.adj=="Washington & Jefferson College") 
length(current)
icg[current,]
icg[current,]$pct.frat <- 43
icg[current,]$pct.sor <- 46
# these were all OK
icg[grep("Washington University in", icg$school.adj),]
icg[grep("Birmingham Southern College", icg$school.adj),]
icg[grep("Whitewater", icg$school.adj),]
icg[grep("Wilmington", icg$school.adj),]
icg[grep("Sewanee", icg$school.adj),]
icg[grep("Pan American", icg$school.adj),]

setdiff(sor$school, icg.doj$school.adj.greek)
# this was only additional one
icg[grep("Miami University", icg$school.adj),] #ok

length(which(!is.na(icg$pct.frat))) # 1068 1027
length(which(!is.na(icg$pct.sor))) # 1067 1026



# ------------------------------------------
# Merge School/Greek/Crime Data with DOJ Investigations

doj <- read.csv("doj.csv", header=FALSE)
doj$doj.entry <- as.character(doj$V1)
doj$doj.state <- unlist(lapply(strsplit(doj$doj.entry, " "), function(x) x[1]))

doj$doj.school.adj <- gsub("^[A-Z]+ ","",doj$doj.entry)
doj$doj.school.adj <- gsub(" [0-9/]+","",doj$doj.school.adj)
doj$doj.school.adj <- gsub(" Case:(.)+", "", doj$doj.school.adj)

doj <- doj[,-1] 
doj$doj.invest <- 1 # 85
head(doj)

# make adjustments so it will merge with the iped-based names
# doj$school.adj.doj <- doj$school
# match on ic$school.adj.uc
icg$school.adj.uc <- toupper(icg$school.adj)


# these changes are made so the DOJ entry will match some name in the IPED DB
# sometimes the change is necesary becuase of punctuation, etc -- but definitely it is the same inst (ex Pace Univ)
# other does I do it to pick a sub- or "relative-" institution for which I have data -- ex Univ. of Alaska, HArvard
#!! Harvard Law no reasonable match possible just dropped it
doj[doj$doj.school.adj =="UNIVERSITY OF ALASKA SYSTEM OF HIGHER EDUCATION",]$doj.school.adj <- toupper("University of Alaska Anchorage")
doj[doj$doj.school.adj =="ARIZONA STATE UNIVERSITY",]$doj.school.adj <- toupper("Arizona State University-Tempe")
doj[doj$doj.school.adj =="BUTTE-GLENN COMMUNITY COLLEGE DISTRICT",]$doj.school.adj <- toupper("Butte College")
doj[doj$doj.school.adj =="COLORADO STATE UNIVERSITY",]$doj.school.adj <- toupper("Colorado State University-Fort Collins")
doj[doj$doj.school.adj =="UNIVERSITY OF COLORADO AT DENVER",]$doj.school.adj <- toupper("University of Colorado Denver") 
doj[doj$doj.school.adj =="UNIVERSITY OF COLORADO AT BOULDER",]$doj.school.adj <- toupper("University of Colorado Boulder") 
doj[doj$doj.school.adj =="UNIVERSITY OF SOUTH FLORIDA",]$doj.school.adj <- toupper("University of South Florida-Main Campus") 
doj[doj$doj.school.adj =="HARVARD COLLEGE",]$doj.school.adj <- toupper("Harvard University") 
doj[doj$doj.school.adj =="UNIVERSITY OF NEBRASKA-LINCOLIN",]$doj.school.adj <- toupper("UNIVERSITY OF NEBRASKA-LINCOLN") 
doj[doj$doj.school.adj =="HOBART AND WILLIAM SMITH COLLEGES",]$doj.school.adj <- toupper("Hobart William Smith Colleges") 
doj[doj$doj.school.adj =="PACE UNIVERSITY - NEW YORK",]$doj.school.adj <- toupper("Pace University-New York") 
doj[doj$doj.school.adj =="SAINT THOMAS AQUINAS COLLEGE",]$doj.school.adj <- toupper("St Thomas Aquinas College") 
doj[doj$doj.school.adj =="SUNY AT STONY BROOK",]$doj.school.adj <- toupper("Stony Brook University") 
doj[doj$doj.school.adj =="THE UNIVERSITY OF AKRON",]$doj.school.adj <- toupper("University of Akron Main Campus") 
doj[doj$doj.school.adj =="OKLAHOMA STATE UNIVERSITY",]$doj.school.adj <- toupper("Oklahoma State University-Main Campus") 
doj[doj$doj.school.adj =="PENNSYLVANIA STATE UNIVERSITY",]$doj.school.adj <- toupper("Pennsylvania State University-Main Campus") 
doj[doj$doj.school.adj =="CISCO JUNIOR COLLEGE",]$doj.school.adj <- toupper("Cisco College") 
doj[doj$doj.school.adj =="UNIVERSITY OF VIRGINIA",]$doj.school.adj <- toupper("University of Virginia-Main Campus") 
doj[doj$doj.school.adj =="DAVIS AND ELKINS COLLEGE",]$doj.school.adj <- toupper("Davis & Elkins College") 

doj$doj.school.adj2 <- doj$doj.school.adj # use this for the join so that doj.school.adj stays after
head(doj)

setdiff(doj$doj.school.adj2, icg$school.adj.uc) # two that don't match - both are grad schools

icg.doj <- merge(icg, doj, by.x=c("school.adj.uc", "state"), by.y=c("doj.school.adj2", "doj.state"), all.x=TRUE, all.y=TRUE)
nrow(icg.doj)  # 4897 4207... 2 from DOJ that didn't match, and so was added as a new row.
icg.doj$doj.invest <- ifelse(is.na(icg.doj$doj.invest), 0, icg.doj$doj.invest)
str(icg.doj)

nrow(icg.doj[which(icg.doj$doj.invest==1),]) # 85
nrow(icg.doj[which(icg.doj$doj.invest==0),]) # 4812 4122

length(icg.doj[which(icg.doj$doj.invest==1 & (is.na(icg.doj$pct.frat) | is.na(icg.doj$pct.sor))),5]) # 23 
length(icg.doj[which(icg.doj$doj.invest==0 & (is.na(icg.doj$pct.frat) | is.na(icg.doj$pct.sor))),5]) # 3812 3163
icg.doj[which(icg.doj$doj.invest==1 & (is.na(icg.doj$pct.frat) | is.na(icg.doj$pct.sor))),c(5,105,106)]
# have to try to get frat data for the 23 DOJ schools
# so check them and hand enter if find in another source
# BUTTE-GLENN: Butte College                                                  no USN                            no CDS                    stays NA  no info
# CALIFORNIA INSTITUTE OF THE ARTS                                            USN, no frat                      no CDS                    stays NA  USN 
# CISCO JUNIOR COLLEGE: Cisco College                                         no USN                            no CDS                    stays NA  no info
# CUNY HUNTER COLLEGE: CUNY--Hunter College                                   USN, no frat                      CDS                       stays NA  CDS 
# DAVIS AND ELKINS COLLEGE:Davis and Elkins                                   USN, no frat                      no CDS                    stays NA  USN
# GRAND VALLEY STATE UNIVERSITY                                               USN, no frat                      CDS                       stays NA  CDS
# HAMPSHIRE COLLEGE                                                           USN, no frat                      CDS 13-14                 0, 0      CDS ***
# HARVARD COLLEGE: Harvard University                                         USN, no frat                      CDS 11-12                 stays NA  CDS
# HOBART AND WILLIAM SMITH COLLEGES:Hobart and William Smith Colleges         USN, frat                         -                         18, 0     USN ***
# MARLBORO COLLEGE                                                            USN, no frat                      CDS 12-13                 0, 0      CDS ***
# Minot State University                                                      USN, no frat                      CDS 11-12                 stays NA  CDS
# MORGAN STATE UNIVERSITY                                                     USN, no frat                      no CDS                    stays NA  no info
# SAINT THOMAS AQUINAS COLLEGE                                                USN, no frat                      CDS - requires login      stays NA  no info
# THE UNIVERSITY OF AKRON: University of Akron                                USN, frat                         -                         4, 4      USN ***
# UNIVERSITY OF ALASKA SYSTEM OF HIGHER EDUCATION: Univ of Alaksa--Anchorage  USN, no frat                      CDS                       stays NA  CDS
# UNIVERSITY OF CALIFORNIA-BERKELEY:University of California--Berkeley        USN, no frat                      CDS 13-14                 10, 10    CDS ***
# UNIVERSITY OF CHICAGO                                                       USN, no frat                      no CDS                    stays NA  no info
# UNIVERSITY OF COLORADO AT DENVER                                            USN, no frat                      CDS 13-14                 0, 0      CDS ***
# UNIVERSITY OF VIRGINIA                                                      USN, no frat                      CDS 13-14                 25, 28    CDS ***
#http://avillage.web.virginia.edu/iaas/cds/cds1314all.shtm
# VINCENNES UNIVERSITY                                                        USN, no frat                      no CDS                    stays NA  no info
# VIRGINIA MILITARY INSTITUTE                                                 USN, no frat                      CDS 13-14                 stays NA  CDS
# WEST VIRGINIA SCHOOL OF OSTEOPATHIC MEDICINE                                no USN                            no CDS                    stays NA  no info

icg.doj[which(icg.doj$school.adj=="Hampshire College"),]$pct.frat <- 0
icg.doj[which(icg.doj$school.adj=="Hampshire College"),]$pct.sor <- 0
icg.doj[which(icg.doj$school.adj=="Hobart William Smith Colleges"),]$pct.frat <- 18
icg.doj[which(icg.doj$school.adj=="Hobart William Smith Colleges"),]$pct.sor <- 0
icg.doj[which(icg.doj$school.adj=="Marlboro College"),]$pct.frat <- 0
icg.doj[which(icg.doj$school.adj=="Marlboro College"),]$pct.sor <- 0
icg.doj[which(icg.doj$school.adj=="University of Akron Main Campus"),]$pct.frat <- 4
icg.doj[which(icg.doj$school.adj=="University of Akron Main Campus"),]$pct.sor <- 4
icg.doj[which(icg.doj$school.adj=="University of California-Berkeley"),]$pct.frat <- 10
icg.doj[which(icg.doj$school.adj=="University of California-Berkeley"),]$pct.sor <- 10
icg.doj[which(icg.doj$school.adj=="University of Colorado Denver"),]$pct.frat <- 0
icg.doj[which(icg.doj$school.adj=="University of Colorado Denver"),]$pct.sor <- 0
icg.doj[which(icg.doj$school.adj=="University of Virginia-Main Campus"),]$pct.frat <- 25
icg.doj[which(icg.doj$school.adj=="University of Virginia-Main Campus"),]$pct.sor <- 28



length(which(!is.na(icg.doj$pct.frat))) # 1034 = 1027+7.   1075 = 1068 + 7 I added by hand
length(which(!is.na(icg.doj$pct.sor))) # 1033=1026+7.    1074 = 1067 + 7 added by hand


ug.women <- icg.doj$ug.enroll * (icg.doj$pctwomen.ug.enroll/100)
ug.men <- icg.doj$ug.enroll * ((100-icg.doj$pctwomen.ug.enroll)/100)
ngreek.women <- ug.women * (icg.doj$pct.sor/100)   
ngreek.men <- ug.men * (icg.doj$pct.frat/100)   
ngreeks <- ifelse(is.na(ngreek.women), 0, ngreek.women) + ifelse(is.na(ngreek.men), 0, ngreek.men)
# NA is case where undergrad enroll is zero or (BOTH frat AND sor pct are NA).
# for all other cases, we get a number (including if frat and sor both are Zero)
icg.doj$pct.greek <- ifelse(icg.doj$ug.enroll==0 | (is.na(icg.doj$pct.frat) & is.na(icg.doj$pct.sor)), NA, ((ngreeks) / icg.doj$ug.enroll)*100)
length(which(!is.na(icg.doj$pct.greek))) # 1049 1039

icg.doj[which(is.na(icg.doj$pct.frat) & !is.na(icg.doj$pct.sor)),] #5
icg.doj[which(!is.na(icg.doj$pct.frat) & is.na(icg.doj$pct.sor)),] #6
icg.doj$pct.frat.adj <- ifelse(is.na(icg.doj$pct.frat), 0, icg.doj$pct.frat)
icg.doj$pct.sor.adj <- ifelse(is.na(icg.doj$pct.sor), 0, icg.doj$pct.sor)
icg.doj$pct.greek.adj <- ifelse(is.na(icg.doj$pct.greek), 0, icg.doj$pct.greek)
#icg.doj[1:30,]

nrow(icg.doj) # 4897 4207
str(icg.doj)


# ------------------------------------------
# Variables to make graphing easier

jitter.it <- function(a, band, onlypos){
  a + ifelse(a==0 & onlypos==1, runif(length(a),0,band), runif(length(a),-band,band))
}

icg.doj$pct.frat.jit <- jitter.it(icg.doj$pct.frat,1,1)
icg.doj$pct.sor.jit <- jitter.it(icg.doj$pct.sor,1,1)
icg.doj$pct.greek.jit <- jitter.it(icg.doj$pct.greek,1,1)
icg.doj$pct.frat.adj.jit <- jitter.it(icg.doj$pct.frat.adj,1,1)
icg.doj$pct.sor.adj.jit <- jitter.it(icg.doj$pct.sor.adj,1,1)
icg.doj$pct.greek.adj.jit <- jitter.it(icg.doj$pct.greek.adj,1,1)

display.name <- unlist(simpleCap(as.character(icg.doj$school.adj), " "))
display.name <- gsub("University", "Univ", display.name)
display.name <- gsub("College", "Col", display.name)
display.name <- gsub("Institute", "Inst", display.name)
display.name <- gsub("Technology", "Tech", display.name)
display.name <- gsub("American", "Amer", display.name)
display.name <- gsub("International", "Intl", display.name)
display.name <- gsub("Technical", "Tech", display.name)
display.name <- gsub("Academy", "Acad", display.name)
display.name <- gsub("Community", "Comm", display.name)
display.name <- gsub("Alternative", "Alt", display.name)
icg.doj$school.disp <- display.name


# ------------------------------------------
# Filter As Necessary

#icg.doj.all.bak <- icg.doj.all
icg.doj.all <- icg.doj
# this what happens to Harvard Law
#icg.doj.all[grep("HARVARD", icg.doj.all$doj.school),1:3]
#icg.doj <- icg.doj.all
length(which(icg.doj$level==1))

# THESE NUMBERS ARE FOR THE SMALL (2779) data set!!
icg.doj <- icg.doj[which(icg.doj$level==1), ] #2779 2732
nrow(icg.doj)
nrow(icg.doj[which(icg.doj$doj.invest==1),1:3]) # 81 schools under invesigation left (was 85)
nrow(icg.doj[which(icg.doj$doj.invest==1 & is.na(icg.doj$pct.frat)),1:3]) # 12
nrow(icg.doj[which(icg.doj$doj.invest==1 & is.na(icg.doj$pct.sor)),1:3]) # 12

nrow(icg.doj[which(icg.doj$pct.frat>0),1:3]) # 647 frat data points > 0  (was 66six)
nrow(icg.doj[which(icg.doj$pct.frat==0),1:3]) # 366 zero frat data points
nrow(icg.doj[which(icg.doj$pct.frat>=0),1:3]) # 1013 total frat data points
nrow(icg.doj[!is.na(icg.doj$pct.frat),1:3]) # 1013
nrow(icg.doj[which(icg.doj$pct.sor>0),1:3]) # # 651 sor data points left 
nrow(icg.doj[which(icg.doj$pct.sor==0),1:3]) # # 361 sor data points left 
nrow(icg.doj[which(icg.doj$pct.sor>=0),1:3]) # 1012 sor data points left 
nrow(icg.doj[!is.na(icg.doj$pct.sor),1:3]) # 1012

nrow(icg.doj[which(icg.doj$pct.frat>=0 | icg.doj$pct.sor>=0),1:3]) # 1018 with at least a frat or sor data point
nrow(icg.doj[which(icg.doj$pct.greek>=0),1:3]) # 1018
nrow(icg.doj[!is.na(icg.doj$pct.greek),1:3]) # 1018
icg.doj[which(is.na(icg.doj$pct.greek) & (icg.doj$pct.frat>=0 | icg.doj$pct.sor>=0)),] # 0

nrow(icg.doj[is.na(icg.doj$cr.sex.off.101112.tot),]) # 264
nrow(icg.doj[is.na(icg.doj$cr.sex.off.101112.tot.per1000fte),]) # 264
nrow(icg.doj[is.na(icg.doj$fte.enroll),]) # 1
# just double check
nrow(icg[which(is.na(icg$fte.enroll) & (icg$level==1) & icg$ug.enroll!=0),]) # 1
length(which(is.na(ic$cr.sex.off.101112.tot.per1000fte) & ic$level==1 & ic$ug.enroll!=0)) # 264
length(which(ic$cr.sex.off.101112.tot.per1000fte == 0 & ic$level==1 & ic$ug.enroll!=0)) # 1294
length(which(ic$cr.sex.off.101112.tot.per1000fte>0 & ic$level==1 & ic$ug.enroll!=0)) # 1221



library(ggplot2)
library(scales)
library(gridExtra)
library(extrafont)
font_import()
loadfonts()

# ------------------------------------------
# GRAPH
# -- BASE for all graphs

base_family = "Helvetica"
base_size = 10
base <- ggplot(icg.doj, aes(x=pct.greek)) 
base <- base + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base <- base + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                     panel.background = element_rect(fill = 'white'))

#-- graph 5 -- double histograms for pct-greek

labels5 = data.frame(x=c(35,35), y=c(.05,.035), 
                     label=c("997 degree-granting (4+ years) institutions.\nSource: NCES IPEDS database, USNews College Rankings (greek data).\nOf 2,799 institutions in IPEDS, 1,782 removed due to missing data.", 
                             "Degree-granting (4+ years) institutions with open Title IX sexual violence investigations (69).\nSource: DOJ. 12 removed due to missing data."),
                     color=c("gray", "red"))
labels5
base5 <- base + theme(axis.ticks.y = element_blank(), 
                      axis.text.y = element_blank(), 
                      axis.title.x = element_text(vjust=-0.10), 
                      axis.text.x  = element_text(vjust=0.5, size=13))
base5 <- base5 + ggtitle("Distribution of Greek Participation\nfor post-secondary 4-year+ degree-granting institutions") 
base5 <- base5 + xlab("greek participation rate (%)\npercentage of undergraduates who have joined a fraternity or a sorority") +
  ylab("") 
#base5 <- base5 + geom_density(aes(fill="all schools"))  
base5 <- base5 + geom_density(data=subset(icg.doj, doj.invest==0), aes(fill="not investigated by DOJ"), alpha=.3) 
base5 <- base5 + geom_density(data=subset(icg.doj, doj.invest==1), aes(fill="investigated by DOJ"), alpha=.3) 
base5 <- base5 + geom_point(data=subset(icg.doj, doj.invest==1), aes(x=pct.greek, y=0), size=3.5, color="red", alpha=.5) 
base5 <- base5 + scale_y_continuous(expand = c(0,.002)) + # space between tick label and x-axis, dont know why this isnt "x-cont"
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,100)) +
#  scale_fill_manual(name='', values=c('all schools'='grey83', 'investigated by DOJ'='red')) 
  scale_fill_manual(name='', values=c('all schools'='grey83', 'investigated by DOJ'='red')) 


#base5 <- base5 + geom_density(aes(fill="lightgray")) + geom_density(data=subset(icg.doj, doj.invest==1), fill="red", alpha=.3) 
#base5 <- base5 + geom_point(data=subset(icg.doj, doj.invest==1), aes(x=pct.greek.adj, y=0), size=3.5, color="red", alpha=.5) 
#base5 <- base5 + scale_y_continuous(expand = c(0,.002)) + # space between tick label and x-axis, dont know why this isnt "x-cont"
  #scale_x_continuous(breaks=c(0,10,20,30,40,50,60,100))
#base5 <- base5 + geom_text(data = labels5, aes(x = x, y = y, label = label, colour=color), size=3.5, hjust=0, vjust=0, lineheight=.7) +
#base5 <- base5 + scale_colour_manual(labels=c("no investigation", "investigated by DOJ"), values=c("gray35", "red"))# + guides(colour=FALSE) # turns off legend
base5 <- base5 + geom_vline(aes(xintercept=mean(pct.greek, na.rm=TRUE)), size=1, alpha=1, colour="grey83")
base5 <- base5 + geom_vline(data=subset(icg.doj, doj.invest==1), aes(xintercept=mean(pct.greek, na.rm=TRUE)), size=.5, alpha=.4, colour="red")
  
  base5 <- base5 + coord_fixed(ratio=1/.0045) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # get rid of fill slash
      theme(legend.position=c(.99, .99), legend.justification=c(1,1),            
            legend.key = element_rect(color = 'black')) 
   # each 1 on the x axis mean .003 on the Y + coord_flip()
base5


#-- graph 7 -- ordered chart of total sex offenses

#soff <- icg.doj[!is.na(icg.doj$cr.sex.off.101112.tot.per1000fte),]
soff <- icg.doj[(!is.na(icg.doj$cr.sex.off.101112.tot.per1000fte) & icg.doj$cr.sex.off.101112.tot.per1000fte != 0),]
soff <- icg.doj[which(icg.doj$cr.sex.off.101112.tot.per1000fte > 0),]
#nrow(soff) #2515
nrow(soff) #1221
#soff <- soff[order(soff$sex.off.per1000fte, decreasing=TRUE),]
#soff[1:20,c(5,7,80)]
ord <- order(soff$cr.sex.off.101112.tot.per1000fte, decreasing=TRUE)
soff <- soff[ord, ]
howmany <- 100
#soff <- soff[1:howmany,]
#nrow(soff)
soff$cr.fullid.ord <- factor(soff$cr.fullid, levels=soff$cr.fullid, ordered=TRUE)

base7 <- ggplot(data=soff) 
base7 <- base7 + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base7 <- base7 + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                     panel.background = element_rect(fill = 'white'),
                       legend.title = element_text(size=8, face="plain"))
base7 <- base7 + ggtitle("Total Reported Sex Offenses 2010, 2011, 2012 per 1,000 Full-Time Equivalent Students\nin Schools Reporting At Least 1 Sex Offense from 2010 to 2012") 
base7 <- base7 + xlab("schools reporting at least 1 sex offense\n(ordered by # of offenses)") +
  ylab("sex offenses 2010-12 per 1,000 fte students")
base7 <- base7 + theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank())
#base7 <- base7 + geom_bar(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte),width=.5, stat="identity")
#base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="gray", alpha=.5)
#base7 <- base7 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="red", alpha=.7)

#base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="gray", alpha=.5)
#base7 <- base7 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="red", alpha=.7)
#base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek, color=factor(doj.invest)), alpha=.3)

#base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj), color="gray83", alpha=.4)
base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj, color="no investigation"), alpha=.4)

#base7 <- base7 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj, color=factor(doj.invest)), alpha=.4)
#base7 <- base7 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj), color="red", alpha=.4)
base7 <- base7 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj, color="investigated by DOJ"), alpha=.4)

#base7 <- base7 + geom_text(data=subset(soff, cr.sex.off.101112.tot.per1000fte>10), aes(x=soff[50,]$cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, label=school.disp), 
#                           size=3, hjust=0, vjust=.5, color="black")
base7 <- base7 + scale_x_discrete(expand=c(0,12)) + 
                 #scale_colour_manual(values=c("gray75", "red"), labels=percent) +
                 scale_colour_manual(name='', values=c('no investigation'='grey83', 'investigated by DOJ'='red')) + 
                 scale_size_continuous(name="Greek Participation\nRate (%undergrads)", breaks=c(0,25,50,75), range=c(2,13)) +   
                 coord_fixed(ratio=1/.05) +
                 theme(legend.key=element_rect(fill='white')) +
                 #theme(legend.position="top") +   
                 #guides(color = guide_legend(override.aes = list(size=4))) + # just size of the "color" legend
                theme(legend.position=c(.99, .99), legend.justification=c(1,1), legend.box = "horizontal") + 
                guides(colour = guide_legend(order = 1, override.aes = list(size=4)), 
                       size = guide_legend(order = 2))
base7


#-- graph 9 -- ordered chart of who HASNT reported a sex offenses

#soff <- icg.doj[!is.na(icg.doj$cr.sex.off.101112.tot.per1000fte),]
#soff <- icg.doj[(icg.doj$cr.sex.off.101112.tot.per1000fte == 0),]
soff <- icg.doj[which(icg.doj$cr.sex.off.101112.tot.per1000fte == 0),]
nrow(soff) #1294
#soff <- soff[order(soff$sex.off.per1000fte, decreasing=TRUE),]
#soff[1:20,c(5,7,80)]
ord <- order(soff$fte.enroll, decreasing=TRUE)
soff <- soff[ord, ]
howmany <- 100
#soff <- soff[1:howmany,]
#nrow(soff)
soff$cr.fullid.ord <- factor(soff$cr.fullid, levels=soff$cr.fullid, ordered=TRUE)
toplist <- soff[1:20,c(3,27:31)]
toplist$x <- soff[50,"unitid"]
toplist$y <- 40000


base9 <- ggplot(data=soff) 
base9 <- base9 + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base9 <- base9 + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                       panel.background = element_rect(fill = 'white'),
                       legend.title = element_text(size=8, face="plain"))
base9 <- base9 + ggtitle("Full-Time Equivalent Students\nin Schools Reporting Zero Sex Offenses 2010, 2011, 2012") 
base9 <- base9 + xlab("schools that have reported zero sex offenses\n(ordered by full-time equivalent enrollment)") +
  ylab("full-time equivalent enrollment")
base9 <- base9 + theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank())
#base9 <- base9 + geom_bar(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte),width=.5, stat="identity")
#base9 <- base9 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="gray", alpha=.5)
#base9 <- base9 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="red", alpha=.7)

#base9 <- base9 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="gray", alpha=.5)
#base9 <- base9 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="red", alpha=.7)
#base9 <- base9 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek, color=factor(doj.invest)), alpha=.3)

#base9 <- base9 + geom_point(aes(x=cr.fullid.ord, y=fte.enroll, size=pct.greek.adj), color="gray", alpha=.4)
base9 <- base9 + geom_point(aes(x=cr.fullid.ord, y=fte.enroll, size=pct.greek.adj, color="no investigation"), alpha=.4)
#base9 <- base9 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=fte.enroll, size=pct.greek.adj), color="red", alpha=.4)
base9 <- base9 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=fte.enroll, size=pct.greek.adj, color="investigated by DOJ"), alpha=.4)
#base9 <- base9 + geom_text(data=toplist, aes(x=x, y=y, label=school.adj), 
#                           size=3, hjust=0, vjust=.5, color="black")
base9 <- base9 + scale_x_discrete(expand=c(0,10)) + 
  #scale_colour_manual(values=c("gray35", "red")) +
  #scale_size_continuous(range=c(3,15)) + 
  #guides(y=FALSE) + 
  #coord_fixed(ratio=1/.035) # 
  scale_y_continuous(labels=comma) + 
  scale_colour_manual(name='', values=c('no investigation'='grey83', 'investigated by DOJ'='red')) + 
  scale_size_continuous(name="Greek Participation\nRate (%undergrads)", breaks=c(0,25,50,75), range=c(2,13)) + 
  coord_fixed(ratio=1/70) +
  theme(legend.key=element_rect(fill='white')) +
  theme(legend.position=c(.99, .99), legend.justification=c(1,1), legend.box = "horizontal") +   
  #theme(legend.position="top") + 
  #guides(colour=FALSE, size=FALSE)
  guides(colour = guide_legend(order = 1, override.aes = list(size=4)), 
         size = guide_legend(order = 2)) + facet_wrap(~)

base9

nrow(soff[which(is.na(soff$fte.enroll)),]) # 264


# ---------  Scatter of pctgreek vs reported cases

fit <- lm(icg.doj$cr.sex.off.101112.tot.per1000fte ~ icg.doj$pct.greek.adj)
coef(fit)
display(fit)

base8 <- ggplot(data=icg.doj) 
base8 <- base8 + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base8 <- base8 + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                       panel.background = element_rect(fill = 'white'),
                       legend.title = element_text(size=8, face="plain"))
base8 <- base8 + ggtitle("Greek Participation vs Total Reported Sex Offenses 2010-2012\nper 1000 Full-Time Equivalent Students") 
base8 <- base8 + xlab("greek participation (% of undergrads)") +
  ylab("total reported sex offenses per 1000 full-time equivalent students")
#base8 <- base8 + geom_bar(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte),width=.5, stat="identity")
#base8 <- base8 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="gray", alpha=.5)
#base8 <- base8 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="red", alpha=.7)

#base8 <- base8 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="gray", alpha=.5)
#base8 <- base8 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="red", alpha=.7)
#base8 <- base8 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek, color=factor(doj.invest)), alpha=.3)

base8 <- base8 + geom_point(aes(x=pct.greek.adj, y=cr.sex.off.101112.tot.per1000fte, color=factor(doj.invest)), size=4, alpha=.4) #+ 
#    stat_smooth(aes(x=pct.greek.adj, y=cr.sex.off.101112.tot.per1000fte), method="lm", se=TRUE)
#base8 <- base8 + geom_point(aes(x=pct.greek, y=cr.sex.off.101112.tot.per1000fte, color=factor(doj.invest)), size=3, alpha=.3)
#base8 <- base8 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj), color="red", alpha=.4)
#base8 <- base8 + geom_text(data=subset(soff, cr.sex.off.101112.tot.per1000fte>10), aes(x=soff[50,]$cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, label=school.disp), 
#                           size=3, hjust=0, vjust=.5, color="black")
base8 <- base8 + geom_abline(intercept=.97, slope=.05, color="darkgray", alpha=.8)
base8 <- base8 + scale_x_continuous(breaks=c(0,20,40,60,80,100), expand=c(0,1)) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), expand=c(0,1)) + 
  scale_colour_manual(name="", labels=c('no investigation', 'investigated by DOJ'), values=c("gray83", "red")) +
  #scale_size_continuous(range=c(3,15)) + 
  theme(legend.key=element_rect(fill='white')) +
  theme(legend.position=c(.99, .99), legend.justification=c(1,1))   
  
base8

# ---------  Scatter of pct sor vs pct frat, doj color, size is # repotred cases

base2 <- ggplot(data=icg.doj) 
base2 <- base2 + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base2 <- base2 + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                       panel.background = element_rect(fill = 'white'),
                       legend.title = element_text(size=8, face="plain"))
base2 <- base2 + ggtitle("Fraternity Participation vs Sorority Participation")
base2 <- base2 + xlab("% of undergrad males in fraternity") +
  ylab("% of undergrad females in sorority")
#base2 <- base2 + geom_bar(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte),width=.5, stat="identity")
#base2 <- base2 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="gray", alpha=.5)
#base2 <- base2 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="red", alpha=.7)

#base2 <- base2 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="gray", alpha=.5)
#base2 <- base2 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="red", alpha=.7)
#base2 <- base2 + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek, color=factor(doj.invest)), alpha=.3)

base2 <- base2 + geom_point(aes(x=pct.frat.adj.jit, y=pct.sor.adj.jit, color=factor(doj.invest), size=cr.sex.off.101112.tot.per1000fte), alpha=.5)
#base2 <- base2 + geom_point(data=subset(icg.doj, doj.invest==1), aes(x=pct.frat.jit, y=pct.sor.jit, size=cr.sex.off.101112.tot.per1000fte), color="red", alpha=.5)

#base2 <- base2 + geom_point(aes(x=pct.frat.jit, y=pct.sor.jit, color=factor(doj.invest), alpha=cr.sex.off.101112.tot.per1000fte), size=4.5)

#base2 <- base2 + geom_point(aes(x=pct.greek, y=cr.sex.off.101112.tot.per1000fte, color=factor(doj.invest)), size=3, alpha=.3)
#base2 <- base2 + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj), color="red", alpha=.4)
#base2 <- base2 + geom_text(data=subset(soff, cr.sex.off.101112.tot.per1000fte>10), aes(x=soff[50,]$cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, label=school.disp), 
#                           size=3, hjust=0, vjust=.5, color="black")
#base2 <- base2 + geom_abline()
base2 <- base2 + geom_abline(intercept = 0, slope = 1, color="darkgray", alpha=.6)
base2 <- base2 + scale_x_continuous(expand=c(0,2)) + scale_y_continuous(breaks=c(0,20,40,60,80,100), expand=c(0,4)) + 
  scale_x_continuous(breaks=c(0,20,40,60,80,100), expand=c(0,4)) + 
  scale_colour_manual(name="", labels=c('no investigation', 'investigated by DOJ'), values=c("gray83", "red")) +
  scale_size_continuous(name="total sex offenses\n2010-2012 per\n1,000 full-time\nequivalent students", range=c(2,13)) + 
  #scale_alpha_continuous(name="Total Sex Offenses\n2010-2012\nper 1,000fte", range=c(.2,.8)) + 
    theme(legend.key=element_rect(fill='white')) +
  #theme(legend.position=c(.95, .05), legend.justification=c(1,0)) + 
  coord_equal() +
  guides(colour = guide_legend(order = 1, override.aes = list(size=4)), 
         size = guide_legend(order = 2))

base2


# ----

# ---------  Scatter of pctgreek vs reported cases

fit <- lm(icg.doj$cr.sex.off.101112.tot.per1000fte ~ icg.doj$pct.frat.adj)
coef(fit)
display(fit)

base8f <- ggplot(data=icg.doj) 
base8f <- base8f + theme(text = element_text(family = base_family, face = "plain", colour = "black", size = base_size))
base8f <- base8f + theme(plot.title = element_text(size=12, vjust=2, lineheight=.8),
                       panel.background = element_rect(fill = 'white'),
                       legend.title = element_text(size=8, face="plain"))
base8f <- base8f + ggtitle("% Undergraduate Males in Fraternities vs Total Reported Sex Offenses 2010-2012\nper 1000 Full-Time Equivalent Students") 
base8f <- base8f + xlab("% undergraduate males in fraternities") +
  ylab("total reported sex offenses per 1000 full-time equivalent students")
#base8f <- base8f + geom_bar(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte),width=.5, stat="identity")
#base8f <- base8f + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="gray", alpha=.5)
#base8f <- base8f + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte), size=2, color="red", alpha=.7)

#base8f <- base8f + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="gray", alpha=.5)
#base8f <- base8f + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek), color="red", alpha=.7)
#base8f <- base8f + geom_point(aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek, color=factor(doj.invest)), alpha=.3)

base8f <- base8f + geom_point(aes(x=pct.frat.adj, y=cr.sex.off.101112.tot.per1000fte, color=factor(doj.invest)), size=4, alpha=.4) #+ 
#    stat_smooth(aes(x=pct.greek.adj, y=cr.sex.off.101112.tot.per1000fte), method="lm", se=TRUE)
#base8f <- base8f + geom_point(aes(x=pct.greek, y=cr.sex.off.101112.tot.per1000fte, color=factor(doj.invest)), size=3, alpha=.3)
#base8f <- base8f + geom_point(data=subset(soff, doj.invest==1), aes(x=cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, size=pct.greek.adj), color="red", alpha=.4)
#base8f <- base8f + geom_text(data=subset(soff, cr.sex.off.101112.tot.per1000fte>10), aes(x=soff[50,]$cr.fullid.ord, y=cr.sex.off.101112.tot.per1000fte, label=school.disp), 
#                           size=3, hjust=0, vjust=.5, color="black")
base8f <- base8f + geom_abline(intercept=.96, slope=.05, color="darkgray", alpha=.8)
base8f <- base8f + scale_x_continuous(breaks=c(0,20,40,60,80,100), expand=c(0,1)) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), expand=c(0,1)) + 
  scale_colour_manual(name="", labels=c('no investigation', 'investigated by DOJ'), values=c("gray83", "red")) +
  #scale_size_continuous(range=c(3,15)) + 
  theme(legend.key=element_rect(fill='white')) +
  theme(legend.position=c(.99, .99), legend.justification=c(1,1))   

base8f

soff[1:15,c(1:3,70,71)])
soff[1:15,c(1:3,28:30)]


pdf("plots3.pdf", width=7.5, height=25, onefile = TRUE)
grid.arrange(base5,base2,base8, base8f,base7,base9, ncol=1)
#grid.draw(rbind(ggplotGrob(box_plotA), ggplotGrob(box_plotB), 
#                size="first"))
dev.off()

base5

# Some analysis
vars <- c("ug.enroll", "gr.enroll", "pt.enroll","ft.enroll", "fte.enroll", "pctwomen.ug.enroll", "pct.frat.adj",
          "pct.greek.adj","cr.sex.off.101112.tot", "cr.sex.off.101112.tot.per1000fte")
m <- sapply(vars, function(x) round(mean(icg.doj[,x], na.rm=TRUE), digits=2))
med <- sapply(vars, function(x) round(median(icg.doj[,x], na.rm=TRUE), digits=2)) 
sd <- sapply(vars, function(x) round(sd(icg.doj[,x], na.rm=TRUE), digits=2)) 
n <- sapply(vars, function(x) sum(!is.na(icg.doj[,x])))
stat <- data.frame(n=n, mean=m, median=med, sd)

pop.mean <- mean(icg.doj$pct.greek.adj)
pop.sd <- sd(icg.doj$pct.greek.adj)
nondoj.row <- 
doj.rows <- icg.doj[which(icg.doj$doj.invest == 1),]
n <- nrow(doj.rows)
m <- nrow()
sample.mean <- mean(doj.rows$pct.greek.adj)
sample.sd <- sd(doj.rows$pct.greek.adj)
# next two give same results for a t-test
sample.se <- sample.sd / sqrt(n)
tstat <- (sample.mean - pop.mean) / se
tstat
t.test(icg.doj$pct.greek.adj, doj.rows$pct.greek.adj, var.equal = FALSE)
# BUT we can do a z-test because I know the pop variance
z.test = function(xbar, mu, var){
  zeta = (mean(xbar, na.rm=TRUE) - mu) / (sqrt(var / length(xbar)))
  return(zeta)
}
z.test(doj.rows$pct.greek.adj, mean(icg.doj$pct.greek.adj), var(icg.doj$pct.greek.adj))  
z.test(doj.rows$pct.greek, mean(icg.doj$pct.greek,na.rm=TRUE), var(icg.doj$pct.greek, na.rm=TRUE))  

# Difference in means -- female male and pct sorority.  Run a logistics regression.  Plot gender differential versus sor/frat differential.  Control for local crime rate and pct males.  Plot local creme rate vs pct live offc ampus
  