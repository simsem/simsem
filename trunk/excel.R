.excel <- "Excel.Application"

"exportDataFrame" <-
function(df, at, ...)
## export the data.frame "df" into the location "at" (top,left cell)
## output the occupying range.
## TODO: row.names, more error checking
{
   d <- dim(df)
   if(d[2]<1) stop("data.frame must have at least one column")
   r1 <- at[["Row"]]                ## 1st row in range
   c1 <- at[["Column"]]             ## 1st col in range
   c2 <- c1 + d[2] - 1              ## last col (*not* num of col)
   ws <- at[["Worksheet"]]

   ## headers

   h1 <- ws$Cells(r1, c1)
   h2 <- ws$Cells(r1, c2)
   hdrRng <- ws$Range(h1, h2)
   hdrRng[["Value"]] <- names(df)

   ## data

   rng <- ws$Cells(r1+1, c1)        ## top cell to put 1st column 
   for(j in seq(from = 1, to = d[2])){
       exportVector(df[,j], at = rng, ...)
       rng <- rng[["Next"]]         ## next cell to the right
   }
   ## return the actual used range (including headings)
   d2 <- ws$Cells(r1+d[1], c2)
   out <- ws$Range(h1, d2) 
   invisible(out)
}

"exportVector" <-
function(obj, at = NULL, byrow = FALSE, ...)
## coerce obj to a simple (no attributes) vector and export to
## the range specified at "at" (can refer to a single starting cell);
## byrow = TRUE puts obj in one row, otherwise in one column.
## How should we deal with unequal of ranges and vectors?  Currently
## we stop, modulo the special case when at refers to the starting cell.
## TODO: converters (currency, dates, etc.)
{
   n <- length(obj)
   if(n<1) return(at)
   rws <- at[["Rows"]]
   cls <- at[["Columns"]]
   d <- c(rws[["Count"]], cls[["Count"]])
   N <- prod(d)
   if(N==1 && n>1){     ## at refers to the starting cell
      r1c1 <- c(at[["Row"]], at[["Column"]])
      r2c2 <- r1c1 + if(byrow) c(0,n-1) else c(n-1, 0)
      ws <- at[["Worksheet"]]
      r1 <- ws$Cells(r1c1[1], r1c1[2])
      r2 <- ws$Cells(r2c2[1], r2c2[2])
      at <- ws$Range(r1, r2)
   } 
   else if(n != N)
      stop("range and length(obj) differ")

   ## currently we can only export primitives...

   if(class(obj) %in% c("logical", "integer", "numeric", "character"))
      obj <- as.vector(obj)     ## clobber attributes
   else
      obj <- as.character(obj)  ## give up -- coerce to chars

   ## here we create a C-level COM safearray
   d <- if(byrow) c(1, n) else c(n,1)
   at[["Value"]] <- asCOMArray(matrix(obj, nrow=d[1], ncol=d[2]))
   invisible(at)
}

"importDataFrame" <-
function(rng = NULL, wks = NULL, n.guess = 5, dateFun = as.chron.excelDate)
## Create a data.frame from the range rng or from the "Used range" in
## the worksheet wks.  The excel data is assumed to be a "database" (sic) 
## excel of primitive type (and possibly time/dates).
## We guess at the type of each "column" by looking at the first
## n.guess entries ... but it is only a very rough guess.
{
   if(is.null(rng) && is.null(wks))
      stop("need to specify either a range or a worksheet")
   if(is.null(rng))
      rng <- wks[["UsedRange"]]       ## actual region
   else
      wks <- rng[["Worksheet"]]       ## need to query rng for its class
   areas <- rng[["Areas"]]
   n.areas <- areas[["Count"]]        ## must have only one region
   if(n.areas!=1)
      stop("data must be in a contigious block of cells")

   cls <- rng[["Columns"]]            ## all cols
   c1 <- rng[["Column"]]              ## first col
   c2 <- cls[["Count"]] - c1 + 1      ## last columns
   rws <- rng[["Rows"]]               ## all rows
   r1 <- rng[["Row"]]                 ## first row
   r2 <- rws[["Count"]] - r1 + 1      ## last row

   ## headers

   n.hdrs <- rng[["ListHeaderRows"]]
   if(n.hdrs==0)
      hdr <- paste("V", seq(form=1, to=c2), sep="")
   else if(n.hdrs==1) {
      h1 <- rws$Item(r1)
      hdr <- unlist(h1[["Value2"]]) 
   }
   else {    ## collapse multi-row headers
      h <- vector("list", c2)         ## list by column
      h1 <- wks$Cells(r1, c1)
      h2 <- wks$Cells(r1+n.hdrs-1, c2)
      r <- rng$Range(h1, h2)
      jj <- 1
      hdr.cols <- r[["Columns"]]
      for(j in seq(from=c1, to=c2)){
         this.col <- hdr.cols$Item(j)
         val <- this.col[["Value2"]]
         h[[jj]] <- unlist(val[[1]])
         jj <- jj+1
      }
      hdr <- sapply(h, paste, collapse=".")
   }
   r1 <- r1 + n.hdrs

   ## Data region 

   d1 <- wks$Cells(r1, c1)
   d2 <- wks$Cells(r2, c2)
   dataRng <- wks$Range(d1, d2)
   dataCols <- dataRng[["Columns"]]
   out <- vector("list", length(hdr))
   for(j in seq(along = out)){
      f1 <- dataCols$Item(j)
      val <- f1[["Value2"]]
      f <- unlist(lapply(val[[1]], function(x) if(is.null(x)) NA else x))
      cls <- guessExcelColType(f1)
      out[[j]] <- if(cls=="logical") as.logical(f) else f
   }
   names(out) <- make.names(hdr)
   as.data.frame(out)
}

"guessExcelColType" <-
function(colRng, n.guess = 5, hint = NULL)
## colRng points to an range object corresponding to one excel column
## e.g., colRng = rng$Columns()$Item("H")
## TODO: currently we return one of "logical", "numeric", "character"
## need to add "SCOMIDispatch"
{
   app <- colRng[["Application"]]
   wf <- app[["WorksheetFunction"]]
   S.avail <- c("logical", "numeric", "integer", "character")
   ## we should get the following from the Excel type library
   fmt <- colRng[["NumberFormat"]]
   num.fmt <- c("general", "number", "currency", "accounting", 
                "percentage", "fraction", "scientific")

   fld <- colRng[["Rows"]]
   n <- fld[["Count"]]
   k <- min(n.guess, n)
   cls <- character(k)
   c1 <- colRng$Cells(1,1)
   c2 <- colRng$Cells(k,1)
   for(i in 1:k){
       x <- fld$Item(i)
       if(wf$IsText(x)) 
          cls[i] <- "character"
       else if(wf$IsNumber(x)) {
          if(tolower(fmt) %in% num.fmt)
             cls[i] <- "numeric"
          else 
             cls[i] <- "character"
          }
       else if(wf$IsLogical(x)) 
         cls[i] <- "logical"
       else if(wf$IsNA(x)) 
          cls[i] <- "NA"
       else 
          cls[i] <- "character"
    }
    ## if not all rows agree, use character type
    cls <- cls[cls %in% S.avail]
    if(length(cls)==0 || length(unique(cls))>1)
       return("character")
    else
       return(cls[1])
}

"as.chron.excelDate" <-
function(xlsDate, date1904 = FALSE)
{
   if(date1904){
      orig <- c(month=12, day=31, year=1903)
      off <- 0
   } 
   else {
      orig <- c(month=12, day=31, year=1899)
      off <- 1
   }
   chron(xlsDate - off, origin = c(month=12, day = 31, year = 1899))
}

"as.excelData.chron" <-
function(chronDate, date1904 = FALSE)
{
   if(date1904){
      orig <- c(month=12, day=31, year=1903)
      off <- 0
   } 
   else {
      orig <- c(month=12, day=31, year=1899)
      off <- 1
   }
   if(any(origin(chronDate)!=orig))
      origin(chronDate) <- orig
   as.numeric(chronDate) + off
}


library(plyr)
library(RDCOMClient)

xls <- COMCreate("Excel.Application")
xls[["Visible"]] <- TRUE

wb = xls[["Workbooks"]]$Add()

# gctorture()

quakes = quakes[,1:2]

sh = wb$Worksheets(1)
exportDataFrame(quakes, at = sh$Range("A1"))
cat("Done first export\n")

sh = wb$Worksheets(2)
exportDataFrame(quakes, at = sh$Range("A1"))

cat("Done second export\n")

sh = wb$Worksheets(2)
exportDataFrame(quakes, at = sh$Range("A1"))


cat("Done third export\n")


for(i in 1:15) { 
 print(i)
 sh = wb$Worksheets(2)
 exportDataFrame(quakes, at = sh$Range("A1"))
}

for(i in 1:15) { 
 print(i)
 sh = wb[["Worksheets"]]$Add()
 exportDataFrame(quakes, at = sh$Range("A1"))
}

wb$Close()
xls[["Visible"]] <- FALSE

rm(wb)
rm(xls)
rm(sh)

gc()



xls <- COMCreate("Excel.Application")
xls[["Visible"]] <- TRUE
wb = xls[["Workbooks"]]$Add(1)

rdcomexport <- function(x) {
     sh = wb[["Worksheets"]]$Add()
     sh[["Name"]] <- as.character(x$Species[1])
     exportDataFrame(x, at = sh$Range("A1"))
}
d_ply(iris, .(Species), rdcomexport)

xls$Sheets("Sheet1")$Delete()
filename <- paste(getwd(), "RDCOMClient.xlsx",
     sep = "/")
filename <- gsub('/', '\\\\', filename)
wb$SaveAs(filename)
wb$Close(filename)

#########################################################
# SimSem Application


library(simsem)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)

SimData <- simData(200, CFA.Model)

SimModel <- simModel(CFA.Model)

#SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
Output <- simResult(200, SimData, SimModel)
#Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)

object <- summaryParam(Output)

library(RDCOMClient)

xls <- COMCreate("Excel.Application")
xls[["Visible"]] <- FALSE
wb = xls[["Workbooks"]]$Add(1)

object2 <- data.frame(parameter = rownames(object), object)
sh = wb[["Worksheets"]]$Add()
exportDataFrame(object2, at = sh$Range("A1"))


xls$Sheets("Sheet1")$Delete()
filename <- paste(getwd(), "simsem.xlsx", sep = "/")
filename <- gsub('/', '\\\\', filename)
wb$SaveAs(filename)
wb$Close(filename)
