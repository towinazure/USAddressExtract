# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

ExtractAddrressFields <- function(address.string) {

#   street_suf_keywords <- read.csv('data\\StreetSuffix.csv', header = TRUE, colClasses = rep('character', 2))
#   street_direction_keywords <- c('East','South', 'West', 'North', 'E', 'S', 'W', 'N', 'NW', 'NE', 'SW', 'SE')

  string.pool <- unlist(strsplit(address.string, c(' ', ',')))
  string.pool <- gsub('\\.', '', string.pool)
  string.pool <- toupper(string.pool)

  Zip      <- NA
  State    <- NA
  Nbr      <- NA
  St.Suf   <- NA
  St.Dr    <- NA
  St.Name  <- NA
  Unit.Suf <- NA
  Unit     <- NA

  Zip.pos   <- length(string.pool) + which(grepl('^([0-9]{5,5})$', string.pool[(length(string.pool)-1):length(string.pool)])) - 2
  State.pos <- length(string.pool) + which(grepl('^([A-Z]{2,2})$', string.pool[(length(string.pool)-1):length(string.pool)])) - 2

  if(length(Zip.pos) > 0 | length(State.pos) > 0){

    Zip   <-  string.pool[Zip.pos]
    State <-  string.pool[State.pos]

    string.pool <- string.pool[-1 * c(State.pos, Zip.pos)]

  }else{

    print('Cannot find Zip and State')
    return(NULL)

  }

  Nbr.pos <- which(grepl('^[0-9]+', string.pool[1]))

  if(length(Nbr.pos) > 0){
    if(Nbr.pos > 0){

    Nbr   <-  string.pool[Nbr.pos]
    string.pool <- string.pool[-1 * Nbr.pos]

    }
  }else{

    print('Cannot find Street Number')
    return(NULL)

  }

  Unit.Suf.pos <- which(grepl('^((Apt)|(Unit))$', string.pool, ignore.case = TRUE))

  if(length(Unit.Suf.pos) > 0){
    if(Unit.Suf.pos > 0 & Unit.Suf.pos < length(string.pool)){

      Unit.pos <- (Unit.Suf.pos + 1) : length(string.pool)

      Unit.Suf <- string.pool[Unit.Suf.pos]
      Unit     <- string.pool[Unit.pos]

      string.pool <- string.pool[-1 * c(Unit.Suf.pos, Unit.pos)]

    }
  }

  St.Suf.pos <- max(which(string.pool %in% c(street_suf_keywords$Name, street_suf_keywords$Suffix)))

  if(length(St.Suf.pos) > 0){
    if(St.Suf.pos == length(string.pool)){

      St.Suf <- string.pool[St.Suf.pos]
      string.pool <- string.pool[-St.Suf.pos]

      St.Dr.pos <- which(string.pool %in% street_direction_keywords)

      if(length(St.Dr.pos) > 0){
        if(St.Dr.pos > 0){

        St.Dr <- string.pool[St.Dr.pos]
        string.pool <- string.pool[-St.Dr.pos]

        }
      }

      St.Name <- paste(string.pool, sep = ' ', collapse = ' ')
    }

  }else{

    print('Cannot find street name properly.')
    St.Name <- paste(string.pool, sep = ' ', collapse = ' ')

  }

  address_obj <- list(Zip = Zip,
    State    = State,
    Nbr      = Nbr,
    St.Suf   = St.Suf,
    St.Dr    = St.Dr,
    St.Name  = St.Name,
    Unit.Suf = Unit.Suf,
    Unit     = Unit)



#   nbr.reg   <- gregexpr('^([0-9])+', target.addr$Address[i])
#   nbr.match <- unlist(regmatches(target.addr$Address[i], nbr.reg))
#
#   #nbr.flag <- grepl('^([0-9])+', target.addr$Address[i])
#
#   if(length(nbr.match) == 1){
#     target.addr$Nbr[i]  <- nbr.match[1]
#   }
#
#   zip.reg   <- gregexpr('([0-9]{5,5})$', target.addr$Address[i])
#   zip.match <- unlist(regmatches(target.addr$Address[i], zip.reg))
#   #zip.flag <- grepl('([0-9]{5,5})$', target.addr$Address[i])
#
#   if(length(zip.match) == 1){
#     target.addr$Zip[i]  <- zip.match
#   }
#
#   st.reg     <- gregexpr('\\s([A-Za-z0-9]+)', target.addr$Address[i])
#   st.matches <- unlist(regmatches(target.addr$Address[i], st.reg))
#   st.matches <- gsub('\\s', '', st.matches)
#   #st.name.flag <- grepl('\\s([A-Za-z]{3,99})', target.addr$Address[i])
#
#
#   if(length(st.matches) >= 1){
#     target.addr$St.Name[i]  <- st.matches[1]
#   }
#
#   if(length(st.matches) >= 2){
#
#     if(grepl('((^East$)|(^South$)|(^West$)|(^North$)|(^E\\.$)|(^S\\.$)|(^W\\.$)|(^N\\.$)|(^E$)|(^S$)|(^W$)|(^N$))|(^NW$)|(^NE$)|(^SW$)|(^SE$))', st.matches[1], ignore.case = TRUE)) {
#
#       target.addr$St.Dr[i]    <- st.matches[1]
#       target.addr$St.Name[i]  <- st.matches[2]
#
#     }
#   }
#
#   if(is.na(target.addr$St.Dr[i])){
#
#     if(grepl('^((Way)|(Ln)|(Lane)|(Street)|(St)|(Rd)|(Road)|(Ave)|(Pl)|(Ctr)|(Dr)|(Drive)|(Court)|(AVENUE))$', st.matches[2], ignore.case = TRUE)){
#       target.addr$St.Suf[i]  <- st.matches[2]
#     }else if(grepl('^((Way)|(Ln)|(Lane)|(Street)|(St)|(Rd)|(Road)|(Ave)|(Pl)|(Ctr)|(Dr)|(Drive)|(Court)|(AVENUE))$', st.matches[3], ignore.case = TRUE)){
#
#       target.addr$St.Name[i] <- paste(st.matches[1], st.matches[2])
#       target.addr$St.Suf[i]  <- st.matches[3]
#
#     }
#
#   }else{
#
#     if(grepl('^((Way)|(Ln)|(Lane)|(Street)|(St)|(Rd)|(Road)|(Ave)|(Pl)|(Ctr)|(Dr)|(Drive)|(Court)|(AVENUE))$', st.matches[3], ignore.case = TRUE)){
#       target.addr$St.Suf[i]  <- st.matches[3]
#     }else if(grepl('^((Way)|(Ln)|(Lane)|(Street)|(St)|(Rd)|(Road)|(Ave)|(Pl)|(Ctr)|(Dr)|(Drive)|(Court)|(AVENUE))$', st.matches[4], ignore.case = TRUE)){
#
#       target.addr$St.Name[i] <- paste(st.matches[2], st.matches[3])
#       target.addr$St.Suf[i]  <- st.matches[4]
#
#     }
#   }
#
#   potential.unit <- grep('^([0-9]+)$', st.matches, value = TRUE)
#   if(length(potential.unit)> 1){
#     #2nd one must be the zip code
#     target.addr$Unit[i] <- potential.unit[1]
#   }

  return(address_obj)

}
