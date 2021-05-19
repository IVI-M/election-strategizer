
# print(sessionInfo())
source("000-common.R")
library(gsheet)

if (T) { # __  Read BC data  -----
  
  if (T) {
    dtBC <- 
      "docs.google.com/spreadsheets/d/1XfUeRcRm-IbPYE0YNI4WGx3HfZvrvCJUhSrkV216mEE" %>% 
      gsheet2tbl() %>%  data.table() %T>%  print
  } else { # first line not read properly !
    dtBC <- 
      "2017 BC election results by district with emphasis on NDP-Green vote split - Electoral Districts by Region.tsv" %>% 
      fread(skip = 1, header=T) %T>% print(2)
    dtBC %>% names()
  }
  # cols <- dtBC %>% dt.findColsWithNA(all=F)
  dtBC[, ( dtBC %>% dt.findColsWithNA(all=F) ):=NULL][]
  dtBC[, (10:14):=NULL]
  
  # dtBC0 <- copy(dtBC)
  colKeys <- names(dtBC)[1:4]
  colParties <- names(dtBC)[5:9]
  colPartiesP <- paste0(colParties, "(%)")
  
  dtBC[, (colParties):=lapply(.SD, as.integer), .SDcols=colParties][] 
  dtBC[, (colParties):=lapply(.SD, tidyr::replace_na, 0), .SDcols=colParties][] 

  dtBC[, (colPartiesP):= .SD/rowSums(.SD)*100 ,.SDcols=colParties] 
  dtBC[, (colPartiesP):=lapply(.SD, round, 2), .SDcols=colPartiesP][] # OK
  
  dtBC[, `Total`:= apply(.SD, 1, sum) ,.SDcols=colParties] 
  dtBC[, `1st`:= apply(.SD, 1, max) ,.SDcols=colPartiesP] 
  

  dtBC[ `LIB(%)`==`1st`, ':='(winner="LIB", `2nd+3rd-1st`=`NDP(%)` + `GP(%)` - `1st` )]
  dtBC[ `NDP(%)`==`1st`, ':='(winner="NDP", `2nd+3rd-1st`=`LIB(%)` + `GP(%)` - `1st` )]
  dtBC[ `GP(%)`==`1st`, ':='(winner="GP", `2nd+3rd-1st`=`NDP(%)` + `LIB(%)` - `1st` )]
  
  
  dtBC <- dtBC [ order(winner, -`2nd+3rd-1st`) ]
  dtBC$`2nd+3rd-1st` %<>% round(2)

  

  # dt.m2 <- 
  #   melt(dtBC[, 1:9], id.vars = colKeys, variable.name="party", value.name="votes") 
  #   # setkeyv(colKeys) %>% .[order(votes)] %T>%  print
  #   dt.m2[, votes:=as.integer(votes)] %>% 
  #   .[, votes:=tidyr::replace_na(0)] %>% 
  #   .[order(`Electoral district`, -votes)] %T>%  print
  # # %>% 
  # #   setkeyv(colKeys) 
  # dt.m2 <- dt.m2 %>% 
  #   arrange(`Riding #`, desc(votes)) %T>%  print
  
  
}



# 0__ Read Ca 43 election data  -----
election.readMeltedCa <- function(method = 1) {
  if (method==9){ 
    # a) . read OTA google doc ----
    dt0 <- "docs.google.com/spreadsheets/d/1d9uSQuRk0VhCefbim0z35V8XnwiLCrcz9iXBhmerlAc" %>% 
      data.table() %T>%  print
    # .. clean it ----
    dtOTA <- dt0[!is.na(Province)] %>% select(2:11) 
    cols <- c("Bloc", "Con","Grn","Lib","NDP","Other")
    dtOTA [, (cols):= lapply(.SD, tidyr::replace_na, 0), .SDcol = cols][]
    dtOTA$`Riding #` <- dtOTA$`Riding #` %>% as.character
    fwrite(dtOTA, "43-election-results.csv")
  } 
  
  # b) . read melted official data -----
  
  dtOTA <- fread("43-election-results.csv") %>% print(2)
  dtOTA$Add <- NULL
  dtOTA$`Riding #` <- dtOTA$`Riding #` %>% as.character
  
  colKeys <- c("Province",  "Region" , "Riding #", "Riding Name") #names(dtOTA)[1:4] #names(dt.m)

  dt.m0 <- 
    melt(dtOTA[, 1:10], id.vars = colKeys, variable.name="party", value.name="votes") 
  # %>% 
  #   setkeyv(colKeys) 
  dt.m0 <- dt.m0 %>% 
    arrange(`Riding #`, desc(votes)) %T>%  print
  
  dt.m0[Province == "ON"]
  dt.m <- dt.m0
  
  if (method==2) { #  just in case - read official data
    
    dtRiding <- dtOTA[, ..colKeys]
    dt2 <- fread("https://enr.elections.ca/DownloadResults.aspx", skip=1) %T>%  names()
    #.. clean it ----
    dtAll2 <- dt2[, c(1, 9,11:14)] 
    names(dtAll2) <- c("Riding #", "party", "votes", "votes(%)", "rejected", "total0")
    
    # dtAll2$`Riding #` %>% unique
    aRidings <- dtRiding$`Riding #` %>% unique
    # dtAll2 [`Riding #` %ni% aRidings]
    dtAll2 <- dtAll2 [`Riding #` %in% aRidings]
    
    dtAll2$party %>% unique()
    dtAll2 [, party:= ifelse(party=="Conservative", "Con",
                             ifelse(party=="Green Party", "Grn",
                                    ifelse(party=="Liberal", "Lib",
                                           ifelse(party=="NDP-New Democratic Party", "NDP",
                                                  ifelse(party=="Bloc Québécois", "Bloc", "Other")
                                           )
                                    )
                             )
    )  ][, party:=as.ordered(party)] [order(`Riding #`, -votes)][]
    
    dtAll <- dtAll2[rejected == 0] # TO MAKE IT THE SAME AS OTA DATA
    
    dt.m1 <- dtAll [dtRiding, on="Riding #"] %>% 
      arrange(`Riding #`, desc(votes)) %T>%  print
    
    # . dtByRidingVotesByProvince ----
    
    dt.m1 [ , sum(votes), by = c("Province", "party")]
    # dtElectionsOfficialResultsMelted <- dt.m
    dt.m1$rejected <-  dt.m1$total0 <- dt.m1$`votes(%)` <- NULL
    
    # Collapse small parties in Other - TBD later
    dt.m <- dt.m1
  }

  return (dt.m)
}

# 1> Compute dt.m: processing melt-ed table ----

dt.m <- election.readMeltedCa(method=1) %>% print(3)



# 1b>  set colKeys colPartiesSeats ----


election.setParties <- function (dt.m) {
}
if(T) {
  
  colKeys <- c("Province",  "Region" , "Riding #", "Riding Name") 
  #names(dtOTA)[1:4] used also in read() when melting dtOTA to dt.m
  colParties <- dt.m$party %>% unique %>% as.character() %>% sort %T>% print
  
  # colPartiesSeats <- paste0(colParties, ".Seats")
  # colPartiesSeatsP <- paste0(colParties, ".Seats(%)")
  # colPartiesVotesP <- paste0(colParties, ".Votes(%)")
  colPartiesSeats <- paste0(colParties, "-seats")
  colPartiesSeatsP <- paste0(colParties, "-seat(%)")
  colPartiesVotesP <- paste0(colParties, "-votes(%)")
  colPartiesBias <- paste0(colParties, "-Bias(%)")
  
}

election.computeByRiding <- function(dt.m, colKeys) {
  # setkeyv(dt.m, colKeys)
  # setkey(dt.m, `Riding #`,) %T>%  print
  dt.m [, votes:= tidyr::replace_na (votes, 0)][]
  dt.m[ , total:=sum(votes,na.rm = T),  by=colKeys][
    ,'votes(%)':=round(100*votes/total,1)][]
  dt.m[ order(-votes), place:=1:.N, by=`Riding #`][]   
  
  dt.m$place %>% unique()
  
  # dt.m[ , 1st := max(`votes(%)`), by=`Riding Name`]
  # Or better:
  dt.m[ , `1st` := .SD[1], by=`Riding Name`, .SDcols="votes(%)"][]
  # dt.m[ , `1st-2nd` := .SD[1]-.SD[2], by=`Riding Name`, .SDcols="votes(%)"][]
  
  colRes <- c(colKeys, "total", "1st")
  
  dtByRiding <- 
    # dtRiding %>% 
    # merge( dt.m [ , ..colRes] %>% unique,  by="Riding #") %>% 
    dt.m [ , ..colRes] %>% unique %>% 
    merge( dt.m %>% dcast(`Riding #` ~ place, value.var="party"), by="Riding #" ) %>% 
    # merge( dt.m %>% dcast(`Riding #` ~ party, value.var="votes"),  by="Riding #"  ) %>%  # REDUNDANT *Total
    merge( dt.m %>% dcast(`Riding #` ~ party, value.var="votes(%)"),  by="Riding #"  ) 
  
  dtByRiding
}


dtByRiding <- dt.m %>% election.computeByRiding(colKeys)



# 2> dtByProvince ----
election.computeByProvince <- function (dt.m) {
  # .. dtVotesByProvince.woOTA ----
  
  dtVotes <- dt.m [ , sum(votes), by = c("Province", "party")] %T>% print 
  # dtVotes <- dt.m1 [ , sum(votes), by = c("Province", "party")] %T>% print 
  dtVotes [, total:=sum(V1,na.rm = T), by = Province] [ , votes:=round(V1/total*100, 1)][]
  dtVotes <- dtVotes %>% dcast(Province + total ~ party, value.var="votes") %T>% print 
  
  
  setnames(dtVotes, colParties, colPartiesVotesP)
  # dtVotes$`Votes(%):` <- "|"; setcolorder(dtVotes, "Votes(%):")
  dtVotes
  
  # .. dtSeatsByProvince.woOTA ----
  
  # dtByRiding %>% names()
  # dtByRiding [, .N, by=c("Province")]
  
  # %>% tidyr::replace_na()
  
  dtSeats.woOTA <- 
    dtByRiding [, .N, by=c("Province", "1")] %>% 
    dcast (`Province` ~ `1`, value.var="N")  %>% 
    merge(dtByRiding [, .N, by=c("Province")], by = "Province")
  
  setnames(dtSeats.woOTA, colParties, colPartiesSeats)
  # dtSeats.woOTA$`Seats:`<- "|";  setcolorder(dtSeats.woOTA, c("Seats:", "N"))
  
  # dtSeats.woOTA$`Seats(%)` <- "|"
  dtSeats.woOTA[, (colPartiesSeatsP):=(round(.SD/N*100,0)), .SDcols=colPartiesSeats][]
  
  # .. dtSeats.woOTA[dtVotes, on="Province"] ----
  
  dtByProvince <- dtSeats.woOTA[dtVotes, on="Province"]
  setcolorder(dtByProvince, "Province")
  col <- dtByProvince %>% names
  dtByProvince [, (col):= lapply(.SD, tidyr::replace_na, 0), .SDcols=col][]
  
  
  dtByProvince
}

dtByProvince <- dt.m %>% election.computeByProvince() %>% print

# 3>  Add Atlantic . Prairies ---- 

if (T) {
  combineResults <-   function (from, to) {
    dtByProvince [ Province %in% from]  [
      , Province := to ] [ 
        , (colPartiesVotesP):= .SD * total / 100, .SDcols = colPartiesVotesP ]  [   
          , lapply(.SD, sum, na.rm = T), by = Province] [
            , (colPartiesVotesP):= .SD / total * 100, .SDcols = colPartiesVotesP ] [
              , (colPartiesSeatsP):= .SD / N * 100, .SDcols = colPartiesSeats ]
  }
  
  
  dtRegionsLookup <- 
    data.table(large="National", small=list(dtByProvince$Province %>% unique)) %>% 
    rbind (
      data.table(large="Prairies", small=list(c("MB", "SK") ) ) 
    ) %>% 
    rbind (
      data.table(large="Atl+North", small=list(c("NB", "NL", "NS", "PE", "North") ) ) 
    ) 
  
  
  dtByProvince %<>% rbind ( combineResults( dtByProvince$Province %>% unique, "National" ) ) %T>% print
  dtByProvince %<>% rbind ( combineResults(c("MB", "SK"), "Prairies" ) ) %T>% print
  dtByProvince %<>% rbind ( combineResults(c("NB", "NL", "NS", "PE", "North"), "Atl+North" ) ) %T>% print
  
}

cols <- 2:ncol(dtByProvince)
dtByProvince[ , (cols):=lapply(.SD, round, 1), .SDcol=cols][]

# . Add Bias and Negated Votes by Province----

if (T) {
  dtByProvince[ , (colPartiesBias):= round( 
    ( .SD[, mget(colPartiesSeatsP)] - .SD[, mget(colPartiesVotesP)] ), 
    1) ] []

  dt1 <- dtByProvince[ , c("Province", colPartiesBias), with=F ] [
    , (colPartiesBias) := lapply(.SD, function(x) {ifelse(x>0, 0, -x)} ), .SDcols=colPartiesBias] [
      ,`Negated Votes (%)` :=  rowSums (.SD, na.rm = T), .SDcols = colPartiesBias ] 
  
  dtByProvince <- dtByProvince %>% cbind(dt1 [ , .(`Negated Votes (%)`)])
  dtByProvince [ , `Negated Votes (#)`:= as.integer(`Negated Votes (%)` * total/100)][]
}


# dtBias <- dtByProvince %>%
#   melt (id.vars=c("Province", "N", "total"),        measure.vars = c(colPartiesBias),
#         variable.name = "Party",  value.name=c("Bias") %T>% print
#   )%>% arrange(Province, Party) %>% print
# 
# # dtBias <- dtByProvince %>%
# #   melt (id.vars=c("Province"),        measure.vars = c(colPartiesBias),
# #         variable.name = "Party",  value.name=c("Bias") %T>% print
# #   )%>% arrange(Province, Party) %>% print
# 
# dtBiasVotes<- dtByProvince %>%
#   melt (id.vars=c("Province"),        measure.vars = c(colPartiesVotesP),
#         variable.name = "Party", value.name=c("Votes(%)")
#   ) %>% arrange(Province, Party) %>% select("Votes(%)") %T>% print
# dtBiasSeats<- dtByProvince %>%
#   melt (id.vars=c("Province"),        measure.vars = c(colPartiesSeatsP),
#         variable.name = "Party", value.name=c("Seats(%)")
#   ) %>% arrange(Province, Party) %T>% print


# . computeBiasAsVotesVsSeats <- function (dtByProvince) -----

dtBias <- dtByProvince %>% 
  melt (id.vars=c("Province", "N", "total"),        measure.vars = c(colPartiesBias),
        variable.name = "Party",  value.name=c("Bias")  ) %>% arrange(Province, Party) %>% 
  cbind(
    dtByProvince %>% 
      melt (id.vars=c("Province"),        measure.vars = c(colPartiesVotesP),
            variable.name = "Party", value.name=c("Votes(%)") 
      ) %>% arrange(Province, Party) %>% select("Votes(%)") ) %>% 
  cbind(
    dtByProvince %>% 
      melt (id.vars=c("Province"),        measure.vars = c(colPartiesSeatsP),
            variable.name = "Party", value.name=c("Seats(%)") 
      ) %>% arrange(Province, Party) %>% select("Seats(%)") ) %>% 
  mutate(Party = str_sub (Party, end=-9) ) %T>% print


if (T) {
  # 5> ! Finding winnable ridings ----
  

  dtBias[order(-total,Bias)]
  
  
  if(T) {
    # for (region in dtBias$Province %>% unique) {
    
    input <- list()
    region <- input$region
    region <- "National" 
    region <- "AB" 
    
    dtByRiding1 <- dtByRiding[Province ==region] 
    if (nrow(dtByRiding1)==0) # to deal with merged regions 
      dtByRiding1 <- dtByRiding[Province %in% unlist (dtRegionsLookup [large==region]) ]
    
    disadvantaged <- dtBias[Province == region ][order(Bias)][1:2]$Party; disadvantaged
    
    colTogether <- my.paste(disadvantaged,"+")
    colTogetherDiff <- my.paste(disadvantaged,"-")
    colTogetherOver1st <- paste0(colTogether, "-1st")
    
    if (T) { # no need to do it here. will do with melted later}
      # dtByRiding[, Total0 := Bloc+ Con	+ Grn	+ Lib	+ NDP	+ Other][]
      dtByRiding1[, (colTogether) := get(disadvantaged[1]) + get(disadvantaged[2])][
        , (colTogetherDiff) := get(disadvantaged[1]) - get(disadvantaged[2])][
          , (colTogetherOver1st) := get(disadvantaged[1]) + get(disadvantaged[2]) - `1st`]
      
      
      dtByRiding1[ `1` %in% disadvantaged, ((colTogetherOver1st) ) := NA ]
      
      dtByRiding1[ order(-get(colTogetherOver1st) )         ]   []
      
      # dtByRiding1[, `NDP-Grn` := NDP - Grn][]
      # dtByRiding1[, `1st-(Lib+NDP)` := `1st` - Lib - NDP][]
      # dtByRiding1[, `Lib-NDP` := Lib - NDP][]
      
    }
    
    MARGIN = 15
    dtByRiding1[get(colTogetherDiff)>0 & get(colTogetherOver1st) > -MARGIN] [order(-get(colTogetherOver1st), -get(colTogetherDiff) )      ]
    
    
    dtByRiding1[get(colTogetherDiff)<=0 & get(colTogetherOver1st) > -MARGIN] [order(-get(colTogetherOver1st), -get(colTogetherDiff) )      ]
    
    
    # readline("Press enter to proceed: ")
  }
}



if (F) {
  dtBiasByParty <-  dtBias[, .(`Averaged FTPT Bias`=sum(Bias)), by=c("Party")] %T>% print()
}

if (F) {
  # VIEW datatable ----
  
  dtByProvince %>%  d7.datatable (1,2)
  dtByRiding %>%  d7.datatable
  dtBias[order(-total,Bias)] %>% d7.datatable
  dtByRiding1[ order(-get(colTogetherOver1st) ) ]   %>% d7.datatable(1,2)
}




# End .......................................----
