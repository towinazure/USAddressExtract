ExtractAddrressFields <- function(address.string){

  string.pool <- unlist(strsplit(address.string, c(' ', ',')))
  string.pool <- gsub('\\.', '', string.pool)
  string.pool <- toupper(string.pool)

  address_obj <- list(Zip = NA,
    State    = NA,
    Nbr      = NA,
    St.Suf   = NA,
    St.Dr    = NA,
    St.Name  = NA,
    Unit.Suf = NA,
    Unit     = NA,
    Status   = 'Complete')


  Zip.pos   <- length(string.pool) + which(grepl('^([0-9]{5,5})$', string.pool[(length(string.pool)-1):length(string.pool)])) - 2
  State.pos <- length(string.pool) + which(grepl('^([A-Z]{2,2})$', string.pool[(length(string.pool)-1):length(string.pool)])) - 2

  if(length(Zip.pos) > 0 | length(State.pos) > 0){

    address_obj$Zip   <-  string.pool[Zip.pos]
    address_obj$State <-  string.pool[State.pos]

    string.pool <- string.pool[-1 * c(State.pos, Zip.pos)]

  }else{

    address_obj$Status  <- 'Cannot find zip code and state'
    return(address_obj)

  }

  Nbr.pos <- which(grepl('[0-9]+', string.pool[1]))

  if(length(Nbr.pos) > 0){

    ##Street number should not be in the last position because we have to have strings for street name

    if(Nbr.pos > 0 & Nbr.pos < length(string.pool)){

      address_obj$Nbr   <-  string.pool[Nbr.pos]
    string.pool <- string.pool[-1 * Nbr.pos]

    }
  }else{

    address_obj$Status <- 'Cannot find street number'
    return(address_obj)

  }

  Unit.Suf.pos <- which(grepl('^(\\#)|((Apt)|(Unit))$', string.pool, ignore.case = TRUE))

  if(length(Unit.Suf.pos) > 0){

    Unit.Suf.pos <- max(Unit.Suf.pos)

    if(Unit.Suf.pos > 0 & Unit.Suf.pos < length(string.pool)){

      address_obj$Unit.Suf <- string.pool[Unit.Suf.pos]
      Unit.search.string <- string.pool[(Unit.Suf.pos + 1) : length(string.pool)]
      string.pool <- string.pool[-1 * Unit.Suf.pos : length(string.pool)]

      Unit.pos.flag1 <- grepl('[0-9]+', Unit.search.string)
      Unit.pos.flag2 <- grepl('^[A-Z]{1,1}$', Unit.search.string)
      Unit.pos <- which(Unit.pos.flag1 | Unit.pos.flag2)

      address_obj$Unit <- paste(Unit.search.string[Unit.pos], collapse = '')

    }
  }

  St.Dr.pos  <- which(string.pool %in% street_direction_keywords)

  if(length(St.Dr.pos) > 0){

    St.Dr.pos <- min(St.Dr.pos)
    address_obj$St.Dr <- string.pool[St.Dr.pos]
    string.pool <- string.pool[-St.Dr.pos]

  }

  St.Suf.pos <- which(string.pool %in% c(street_suf_keywords$Name, street_suf_keywords$Suffix))

  if(length(St.Suf.pos) > 0){

    St.Suf.pos <- max(St.Suf.pos)

    ##We need to leave at least 1 string for street name
    if(St.Suf.pos > 1 ){

      address_obj$St.Suf <- string.pool[St.Suf.pos]
      ##If it is a full name, we convert it to suffix
      if(sum(street_suf_keywords$Name == address_obj$St.Suf) > 0){

        address_obj$St.Suf <- street_suf_keywords$Suffix[street_suf_keywords$Name == address_obj$St.Suf]

      }

      address_obj$St.Name <- paste(string.pool[1:(St.Suf.pos - 1)], sep = ' ', collapse = ' ')

      string.pool <- string.pool[-1 * (1:St.Suf.pos)]

    }
  }

  if(is.na(address_obj$St.Suf)){

    address_obj$Status <- 'Cannot find street suffix properly'
    address_obj$St.Name <- paste(string.pool, sep = ' ', collapse = ' ')

  }


  return(address_obj)

}

