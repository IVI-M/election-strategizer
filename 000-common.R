# source("000-common.R") ivi version
# D.Gorodnichy

# See also 
# source("../D7.common/dt.R") 
# source("../D7.common/00-base.R") 
# source("../D7.common/00-data.table.R") 
# source("../D7.common/00-plot.R") 

# 0. General libraries and functions ----

# packages <- c("learnr","ggplot2","plotly", "shiny", "shinythemes")
# lapply(packages, library, character.only = TRUE)


options(digits = 3); # 7
#options(max.print = 100) # 1000
options(scipen=999); #remove scientific notation
# options(DT.options = list(...))

library(magrittr)
library(readxl)
library(readr)
library(stringr)
library(tidyselect)

library(R6)

library(ggplot2)
library(lubridate,  quietly=T)
options(lubridate.week.start =  1)

library(forcats) #  fct_reorder(Metric, Metric, .desc = TRUE

#library(tidyverse) # includes:  readr ggplot2 stringr (which we use), dplyr tidyr tibble forcats purrr (may use later) and others - https://www.tidyverse.org/packages/

library(dplyr)
#library(dtplyr)

#library(timeDate)
#library(zoo) # DT[, V1:=na.locf(V1)]


library(data.table)
options(datatable.print.class=TRUE)
as.dt <- as.data.table

library(dygraphs)
library(plotly)
library(DT)

library(scales)
# scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), labels = trans_format('log10', math_format(10^.x))) +


# library(GGally)
# library(flexdashboard) #flexdashboard_0.5.1.1 -> !! is NOT compatible with current R version !!

##  source("../archives/00-base.R")
# source("../archives/00-data.table.R")
##  source("../archives/00-plot.R")



library(knitr)
# https://yihui.org/knitr/options/
knitr::opts_chunk$set(message=F, warning=F)
knitr::opts_chunk$set(echo=F)
# knitr::opts_chunk$set(include = F)
knitr::opts_chunk$set(cache = F)


# "%+%" ----

"%+%" <- function(x, y) paste0(x,y)  # "d" %+% "m" %+% "i" %+% "try"
"%wo%" <- function(x, y) x[!x %in% y] #--  x not in y 
`%ni%` = Negate(`%in%`)



percentageOf <- function(a, equalto=0, decimals=2, multiply=100) {
  ( length ( which (a == equalto) )  /  length (a) * 100 ) %>% round(decimals)
}
if (F) {
  percentageOf (a=as.ordered(0:19))
  c( rep(1,3), rep(0,7) ) %>% as.ordered() %>% percentageOf
}

#meanOrdered <- function(a) {
my.mean <- function(a, decimals=2) {
  if (is.factor(a)) {
    percentageOf(a,decimals)
  } else {
    mean(a, na.rm = T) %>%  round(decimals)
  }
}



my.paste <- function(array, sep=" ") {
  str <- ""
  for (i in 1:length(array)) 
    str <- str %+% array[i] %+% sep
  return (substr(str, 1, nchar(str)-nchar(sep)))
}


print.pre <- function(...) {
  cat("<pre>\n")
  print (...)
  cat("\n</pre>")  
} 
print.rmd <- function(...) {
  cat("\n\n")
  print (...)
  cat("\n\n")  
}

rmd.print <- function(...) {
  cat("<pre>\n")
  print (...)
  cat("\n</pre>")  
}

cat.rmd <- function(...) {
  cat("\n\n\n\n")
  cat (...)
  cat("\n\n\n\n")   
}
rmd.cat <- function(...) {
  cat("\n\n\n\n")
  cat (...)
  cat("\n\n\n\n")   
}
#my.print <- rmd.print
#my.cat <-  rmd.cat 



#  dt.Briefed <- function(dt2, width=5) {
# #   str <- names(dt)
#    # names(dt2) %<>% lapply( str_trunc, 5, ellipsis="")
#    # names(dt2) <- lapply( names(dt2), str_trunc, 5, ellipsis="")
#        names(dt2) %<>% str_trunc (5, ellipsis="")
#  #  return (str)
#  }
#  
print.short <- function(dt, width=6, kable=NULL) {
  str <- dt %>% names()
  names(dt) %<>% str_trunc (width, ellipsis="") 
  if (kable=="kable") {
    dt %>% knitr::kable() %>% print
    cat("\n\n\n")
  } else 
    dt %>% print
  names(dt) <- str
}




# my.print2 <- function(..., strCaption=NULL) { # DOES NOT WORK THIS WAy !
#   
#   if (!is.null(strCaption))
#     cat(paste0("\n\n*", strCaption, "*\n\n"))
#   cat("<pre>\n")
#   print (...)
#   cat("\n</pre>")  
# }



# source("../D7.common/dt.R") ----
#.  dt0 %>% select (input0$factors) %>% slice(1) %>% filter(`PNC3 Flight crew member` == 1 ) ----

dt.rows <- function(dt, rows) {
  dt[rows]
}

dt.cols <- function(dt, cols) {
  dt[, .SD, .SDcols = cols]   
  # OR  
  # if ( is.integer(cols) ) 
  #   cols <- names(dt)[cols]
  # dt [, cols, with=F]
}

of <- function (x, range)  { # use negative range to remove
  if (!is.data.table(x)) {
    return(x[range])
  }else {
    # dt.cols(x, range)
    return(x[, range, with=F])
  }
}


dt.autoreplace <- function (dt, A = function (x) ifelse(is.na(x), 0, x), cols=NULL) {
  if (is.null(cols)) 
    cols <- 1:ncol(dt)
  
  dt [, (cols):= lapply(.SD, eval(A)), .SDcol = cols]
}

dt.AllNAto0 <- function (dt, cols=NULL) {
  if (is.null(cols)) 
    cols <- 1:ncol(dt)
  
  dt [, (cols):= lapply(.SD, function (x) ifelse(is.na(x), 0, x)), .SDcol = cols]
}
dt.AllNAtoZero <- dt.autoreplace



dt.convert<- function(dt, cols, FUNCTION = ymd) {
  dt [, (cols):= lapply(.SD, function (x) x %>% as.character %>% FUNCTION), .SDcol = cols]
}

# readr::parse_number("10%")

dt.replace <- function (dt, FUNCTION = function (x) ifelse(is.na(x), 0L, x)) {
  dt [, (cols):= lapply(.SD, eval(FUNCTION)), .SDcol = cols]
}
dt.replaceNA <- function (dt, value) {
  dt [, (cols):= lapply(.SD, function (x) ifelse(is.na(x), value, x)), .SDcol = cols]
}



# dtUS [, (colMetrics):= lapply(.SD, tidyr::replace_na, 0), .SDcol = colMetrics]

dt.replaceNA0 <- dt.replace

dt.findColsWithNA <- function(dt0, all=T) {
  
  if (all) 
    dt0[, lapply(.SD, function(x) sum(is.na(x)) ) ] %>% 
    .[, which(.SD==nrow(dt0))]    
  else 
    dt0[, lapply(.SD, function(x) sum(is.na(x)) ) ] %>% 
    .[, which(.SD>0)]
  
  # #OR
  # dt0[, lapply(.SD, function(x) sum(is.na(x)) ) ] %>% 
  #      .[, which(.SD>0)] %>% of(names(dt0), .)
  # dt0 %>% purrr::keep(~all(is.na(.x))) %>% names
  # dt0 %>% purrr::keep(~any(is.na(.x))) %>% names
}


dd.replaceNAwithA <- function(vec, A) {
  vec[ which(is.na(vec)) ] <- A; vec
}



dt.replaceAB <- function(dt, col, a, b) {
  dt[get(col)==a, (col):=b]
}

# dt.replaceAB2 <- function(dt, cols, a, b) {
#  dt.replace [dt, cols, ifelse(get(x)==a, b, a)]


# dt.order <- function (dt, cols) { # use - cols to reverse order
#   dt[order(cols), with=F] #   dtGeoCa[order(get("state"))]
# }
# dt.arrange <- dt.order

dt.filter <- function( dt, quote=.N) {
  dt[ eval(quote) ]
}

if (F) {
  
  dt.slice <- dt.rows
  dt.topN
  
  
  dt.distinct <- data.table::unique
  dt.select <- dt.cols 
  
  dt.filter <- dtplyr::filter
  
}


DIV_WITH_BUTTON <- TRUE
DIV_SET_BUTTONS <- function(bool=TRUE) {
  DIV_WITH_BUTTON <<- bool
}


DIV_COMPILE_MODE_CONST <- c("show buttons", "show all", " show nothing", "print-ready") # for manager"
DIV_WITH_BUTTON <- DIV_COMPILE_MODE_CONST[1]
DIV_SET_COMPILE_MODE <- function(str=DIV_COMPILE_MODE_CONST[1]) {
  DIV_WITH_BUTTON <<- str
}

set.seed(22)  #btn-block


# ```{r results='asis'}
# DIV_START ("Show/Hide this table in interactive mode"")
# DIV_END ()

DIV_START <- function(strText="Details", bButton="button", bShow = "hide"){
  #DIV_START <- function(strText="Details", bShow = T){
  
  iButton <- runif(1, min=0, max=100000) %>% as.integer()
  
  # "btn btn-primary btn-block btn-lg active" 
  # "btn btn-primary btn-block" 
  # if (bShow) {
  # if (F) { #bButton == "button" | DIV_WITH_BUTTON == TRUE) {
  if (bButton == "button" | DIV_WITH_BUTTON == TRUE) {
    cat(
      glue::glue(
        paste0(
          '\n\n',
          '<button class="btn btn-primary btn-block active"  data-toggle="collapse" data-target="#BlockName',iButton, 
          # '"> SHOW/HIDE:  ', 
          '">',          
          strText, '</button>',   
          '\n',
          '<div id="BlockName', iButton,'" class="collapse', ifelse(bShow == "hide", "", " in"), '">\n\n'    
        )
      )
    )
  } else {
    
    rmd.cat(paste0("\n###", strText))
    # rmd.cat(paste0("**", strText, "**"))
    
    cat("\n<div>\n")
    
  }
}

DIV_END <- function(str="") {
  # cat(glue::glue("<br><hr><br></div>"))
  # cat("<br><hr><br></div>")
  cat("</div>\n\n")
  cat(str)
}



# source("../D7.common/00-plot.R") -----

dd.geom_label <- function(label, g = NULL, where=c(0,0), label.size = 0.2, angle=0){
  # these do nothing - label.size = 0.2, angle=0
  where=list(x=where[1],y=where[2]);   size <- list(x=c(0,1),y=c(0,1) )
  if (!is.null(g))
    size <- list(x=layer_scales(g)$x$range$range,y=layer_scales(g)$y$range$range )
  return (geom_label(mapping=aes(x,y), label.size = label.size,
                     hjust = where$x, vjust = where$y, label=label, angle=angle,
                     data=data.table(
                       x=size$x[1]+where$x*(size$x[2]-size$x[1]),
                       y=size$y[1]+where$y*(size$y[2]-size$y[1])) ) )
}

dd.geomLine <- function(x, y=NA, col=NA){
  if (is.na(col) ) {    col<- setNewColour()  }
  if ( is.na(y)) {
    y <- x; x <- 1:length(y)
  }
  
  list( #ifelse(bLine,
    geom_line(mapping=aes(x,y), col=col,  data=data.frame(x, y)),
    #       geom_step(mapping=aes(.xx,.yy), col=colour,  data=data.frame(.xx, .yy))),
    geom_point(mapping=aes(x,y), col=col, data=data.frame(x, y)),
    annotate(geom="text" , col = col, label=deparse(substitute(y)),x=mean(x),y=max(y)-(max(y)-min(y))/20)
  )
}
#myX=1:10
#ggplot() +  dd_geom_y(myX, dlnorm(myX), 1) + dd_geom_y(myX, dexp(myX), 2) + dd_geom_y(myX, dexp(myX,0.7), 3)

dd.ggLine <- function(x, y=NA, col="blue", label=NA, labelXY = c(0,0) ){
  if ( is.na(y) )     {    y <- x; x <- 1:length(y)  }
  ggplot(mapping=aes(x=x)) + geom_line(aes(y=y), col=col) +
    geom_smooth(aes(y=y), col=col) + geom_point(aes(y=y), col=col) +
    annotate(geom="text", col=col, label=label, x=mean(x)+labelXY[1], y=max(y)-labelXY[2], alpha = .8)
  #, parse = TRUE)   #label = "italic(R) ^ 2 == 0.75",   parse = TRUE)
}


# https://www.r-bloggers.com/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/
# See also https://github.com/renkun-ken/formattable
# https://rstudio.github.io/DT/plugins.html

d7.today <- function() { format(Sys.time(), '%d %B, %Y') }


d7.datatableButtons <- function(x){
  DT::datatable(
    x,
    extensions = 'Buttons',
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10,25,50,-1),
                        c(10,25,50,"All"))))
}

d7.datatable <- function(x, left=1, right=1, buttons=T){
  if (buttons) {
    x %>% DT::datatable(
      rownames = F, filter="top", 
      extensions = c('Buttons', 'FixedColumns'), # Scrolle doe work at Rmd ???
      # extensions = c('Buttons', 'FixedColumns','Scroller'),    
      options = list(
        scroller = TRUE,  
        fixedColumns = list(leftColumns = left, rightColumns = right), 
        dom = 'Blfrtip',
        lengthMenu = list(c(10,25,50,-1),
                          c(10,25,50,"All")),
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
    
  } else {
    DT::datatable(
      x, 
      filter="top",  rownames = F,  
      extensions = c('FixedColumns','Scroller'),
      options = list( scroller = TRUE,  
                      fixedColumns = list(leftColumns = left, rightColumns = right)
      )
    )
  }
}



if (F) {
  
  x <- dtBC
  
  x %>% datatable(
    caption="'Select', 'Buttons'",
    filter="top",  rownames = F, 
    extensions = c('Select', 'Buttons'), 
    options = list(
      select = list(style = 'os', items = 'row'),
      dom = 'Blfrtip',
      rowId = 0,
      buttons = c('selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells')
    ),
    selection = 'none'
  )

  x %>% datatable(
    caption="ColReorder",
    extensions = 'ColReorder', options = list(colReorder = TRUE))
  

  x %>% datatable(
    caption="FixedColumns",
    filter="top",  rownames = F, 
    extensions = 'FixedColumns',
    options = list(
      # dom = 't', # show Search and Show [] entries
      scrollX = TRUE,
      scrollY = TRUE,
      fixedColumns = list(leftColumns = 3, rightColumns = 3)
    )
  )
  
  
  x %>% datatable(
    caption="Scroller",
    filter="top",  rownames = F, 
    extensions = 'Scroller', 
    options = list(
      deferRender = TRUE,
      scrollY = 600,
      scroller = TRUE
    ))
  
  
  
  #Also:  
  # https://yihui.shinyapps.io/DT-rows/
  
  
}

