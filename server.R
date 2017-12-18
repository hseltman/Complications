# Purpose: Shiny server function for Medical Complications
# Author: H. Seltman
# Date: Dec. 2017
# Reference: http://shiny.rstudio.com

library(shiny, warn.conflicts=FALSE, quietly=TRUE)
library(shinyjs, warn.conflicts=FALSE, quietly=TRUE)

function(input, output, session) {
  # assume first two columns of transformed data are "year" and "complications"
  monthCols = 3:14

  monthNames = c("January", "February", "March", "April", "May",
                 "June", "July", "August", "September", "October",
                 "November", "December")
  
  initFile = "Complications.csv"
  # helper function
  count = function(x) sum(!is.na(x))
  
  ##
  
  ### Observers ###
  observe({
    info = info()
    if (is.null(info)) {
      shinyjs::disable("analysisType")
      shinyjs::disable("yearStart")
      shinyjs::disable("plotType")
      shinyjs::show("instructions")
    } else {
      minMax = max(1, (nrow(info) - 11))
      updateSliderInput(session, "yearStart", max=minMax, value=minMax)
      shinyjs::enable("analysisType")
      shinyjs::enable("yearStart")
      shinyjs::enable("plotType")
      shinyjs::hide("instructions")
    }
  })

  observeEvent(input$analysisType, {
    req(input$analysisType)
    if (input$analysisType == "One year") {
      shinyjs::show("yearStart")
    } else {
      shinyjs::hide("yearStart")
    }
  })
  
  
  ### Reactives ###
  # Get data file and convert to useful form (columns for year, complication,
  # and each month)
  csv = reactive({
    fileRef = input$fileRef
    if (is.null(fileRef)) {
      if (file.exists(initFile)) {
        filename = initFile
      } else {
        return(NULL)
      }
    } else {
      filename = fileRef$datapath
    }
    OKcsv = substring(filename, nchar(filename)-3) == ".csv"
    if (!OKcsv) return(NULL)
    csv = try(read.csv(filename, as.is=TRUE), silent=TRUE)
    if (is(csv, "try-error")) return(NULL)
    csv
  })
    
  data = reactive({
    csv = csv()
    req(csv)
    # Validate file format
    monLocs = sapply(monthNames, grep, x=csv[, ])
    if (!all(monLocs == 2:13) || 
        !all(sapply(csv[, 1:13], class) == "character")) {
      showModal(modalDialog(title="Error",
                            "File format has changed (months not on line 1)"))
      return(NULL)
    }
    
    # Transform to correct format (e.g., year is with "January" for 
    # first year, then alone after that)
    csv = csv[sapply(csv[, 2], function(x) nchar(trimws(x)) > 0), 1:13]
    names(csv) = c("complication", substring(monthNames, 1, 3))
    csv$complication = tolower(trimws(csv$complication))
    years = csv[sapply(csv[, 1], function(x) nchar(x)==0), 2]
    options(warn=2)
    years = try(as.numeric(substring(years, 1, 4)), silent=TRUE)
    options(warn=0)
    if (is(years, "try-error")) {
      showModal(modalDialog(title="Error", "Years not positioned correctly"))
      return(NULL)
    }
    if (sum(csv$complication == "cases") != length(years)) {
      showModal(modalDialog(title="Error",
                            "Need one 'cases' row per year"))
      return(NULL)
    }
    csv = csv[-1, ] # discard month names (and first year value)
    yearLocs = nchar(csv[, 1])==0
    rle = rle(yearLocs)
    rowsPerYear = rle$lengths[rle$values == FALSE]
    csv = csv[!yearLocs, ]
    csv = cbind(year=rep(years, rowsPerYear), csv)
    
    # convert to numeric (or NA)
    for (col in monthCols) csv[, col] = suppressWarnings(as.numeric(csv[, col]))
    return(csv)
  })
  
  info = reactive({
    data = data()
    req(data)
    years = unique(data$year)
    nYears = length(years)
    cases = as.vector(t(as.matrix(data[data$complication=="cases", monthCols])))
    months = 1:length(cases)
    nMonths = length(months)
    monthLabels = format(as.Date(paste(rep(years, each=12),
                                       rep(as.character(1:12), nYears),
                                       "15", sep="-")),
                         "%b %Y")
    names(cases) = monthLabels
    good = !is.na(cases)
    cases = cases[good]
    
    compNames = setdiff(unique(data$complication), "cases")
    compData = lapply(compNames,
                      function(comp) {
                        dat = as.vector(t(as.matrix(data[data$complication==comp,
                                                         monthCols])))
                        names(dat) = monthLabels
                        dat = dat[good]
                        return(dat)
                      })
    names(compData) = compNames
    compData = data.frame(compData)
    byMonth = data.frame(year=rep(years, each=12)[good])
    byMonth$monthNum = match(substring(names(cases), 1, 3),
                             substring(monthNames, 1, 3))
    byMonth = cbind(cases, byMonth, compData,
                    complications=apply(as.matrix(compData), 1, sum, na.rm=TRUE))
    
    return(byMonth)
  })
  
  
  ### Renderings ###
  output$fileMsg = renderPrint({
    data = data()
    validate(need(data, "Valid data file not yet downloaded."))
    yr = range(data$year)
    cat("Data file covers ", yr[1], " to ", yr[2], ".\n", sep="")
  })
    
  output$stats = renderPrint({
    info = info()
    validate(need(data, "no stats"))
    if (input$analysisType == "One year") {
      info = info[input$yearStart:(11+input$yearStart), ]
    } else if (input$analysisType == "Year to date") {
      info = info[info$year == max(info$year), ]
    }
    cat("Available data: ")
    wholeYears = nrow(info) %/% 12
    remMonths = nrow(info) %% 12
    if (input$analysisType %in% c("One year", "Year to date")) {
      cat(ifelse(remMonths==0, 12, remMonths), "months\n")
    } else {
      cat(wholeYears, "years ")
      if (remMonths == 0 ) {
        cat("\n")
      } else {
        cat("and", remMonths, "months\n")
      }
    }
    caseSum = sum(info$cases)
    cat("Total cases:", caseSum, "\n")
    compSum = sum(info$complications, na.rm=TRUE)
    cat("Total complications: ", compSum,
        " (", formatC(compSum/caseSum*100, digits=1, format="f") ,
        "% of cases)\n", sep="")
    compNames = setdiff(names(info), 
                        c("year", "monthNum", "cases", "complications"))
    for (comp in compNames) {
      compSum1 = sum(info[, comp], na.rm=TRUE)
      cat(gsub(".", " ", comp, fixed=TRUE), ": ", compSum1,
          " (", formatC(compSum1/compSum*100, digits=1, format="f") ,
          "% of complications)\n", sep="")
    }
    
    if (input$analysisType == "Compare years") {
      years = unique(info$year)
      cat("\nChi-square test of equal complication rates across years:\n")
      caseYr = sapply(split(info$cases, info$year), sum, na.rm=TRUE)
      complYr = sapply(split(info$complications, info$year), sum, na.rm=TRUE)
      tbl = rbind(complications=complYr, "no complications"=caseYr-complYr)
      colnames(tbl) = as.character(years)
      cat("\nCounts:\n")
      print(tbl)
      cat("\nPercents:\n")
      print(round(100*prop.table(tbl, 2), digits=1))
      cst = chisq.test(tbl)
      cat("\nX2=", round(cst$statistic, 1), "  df=", cst$parameter,
          "  p=", format.pval(cst$p.value, digits=3), "\n", sep="")
    }
  })
  
  output$plot = renderPlot({
    info = info()
    validate(need(info, "no plot"))
    if (input$analysisType %in% c("All years", "One year", "Year to date")) {
      if (input$analysisType == "One year") {
        info = info[input$yearStart:(11+input$yearStart), ]
      } else if (input$analysisType == "Year to date") {
        info = info[info$year == max(info$year), ]
      }
      if (input$analysisType == "All years") {
        monthsToLabel = c(1, 7)
      } else {
        monthsToLabel = c(1, 4, 7, 10)
      }
      # Time series on top with monthly barplots on top; overall barplot on bottom
      par(mfrow=c(2, 1))
      totCases = info$cases
      x = 1:length(totCases)
      toLabel = info$monthNum %in% monthsToLabel
      labLoc = x[toLabel]
      lab = rownames(info)[toLabel]
      ylim = c(0, max(totCases)*1.3)
      xlim = range(x) + c(-0.5, +0.5)
      
      # Plot total cases
      par(mar=par("mar") + c(0, 0, 0, 3))
      plot(x, totCases, type="b", xlab="month", ylab="count",
           ylim=ylim, xlim=xlim, pch=16, xaxt="n", xaxs="i",
           main="Counts over time")
      axis(1, at=x, labels=FALSE)
      axis(1, at=labLoc, labels=lab, lwd.ticks=2, tcl=-0.75)
      axis(4)
      abline(h=c(20, 40), col=rgb(0.2, 0.2, 0.2, 0.2))
      usr = par("usr")
      cxy = par("cxy")
      labWidth = diff(usr[1:2])/5
      lineLen = labWidth/5
      compNames = setdiff(names(info), 
                          c("year", "monthNum", "cases", "complications"))
      nComp = length(compNames)
      for (i in 1:(1+nComp)) {
        lines(usr[1] + (i-1)*labWidth + cxy[1] + c(0, lineLen),
              rep(usr[4]-cxy[2]/2, 2), col=i, lwd=3)
        text(usr[1] + (i-1)*labWidth + 2*cxy[1] + lineLen,
             usr[4], c("total", gsub(".", " ", compNames, fixed=TRUE))[i],
             col=i, adj=c(0,1))
      }
      
      # Plot complications each month as barplots on the time series
      wacb = 0.5  # width of all complication bars
      cWidth = wacb / nComp  # width of one complication bar
      xOffset = (nComp * cWidth) / 2  # offset to center bars (always wacb/2)
      for (i in 1:length(compNames)) {
        comp = compNames[i]
        comp1 = info[, comp]
        avail = (!is.na(comp1)) & (comp1 > 0)
        if (any(avail)) {
          xLeft = (1:length(comp1))[avail] -xOffset + cWidth*(i - 1)
          n = length(xLeft)
          rect(xLeft, rep(0, n), xLeft+cWidth, comp1[avail], col=i+1, border=i+1)
        }
      }
      
      # Total complications over all years as a barplot
      perComp = sapply(info[, compNames], sum, na.rm=TRUE)
      par(mar=par("mar") + c(0, 5, 0, 0))
      perComp = sapply(info[, compNames], sum, na.rm=TRUE)
      barplot(rev(perComp), names.arg=rev(gsub(".", " ", compNames, fixed=TRUE)),
              horiz=TRUE, las=1,
              col = rev(2:(1+length(compNames))),
              main="Counts of each complication", xlab="count")

    } else {  # Compare years
      par(mfrow=c(1, 2))
      years = unique(info$year)
      casesMat = do.call(cbind, lapply(split(info, info$year),
                                       function(x) sapply(1:12, 
                                                          function(n) sum(x$cases[x$monthNum==n]))))
      barplot(t(casesMat), beside=TRUE, names.arg=substring(monthNames, 1, 1),
              legend.text=as.character(years), col=1:length(years), xlab="month",
              ylab="cases", main="Total cases by month and year",
              ylim=c(0, max(casesMat, na.rm=TRUE)*1.25), 
              args.legend=list(bty="n", ncol=length(years)))

      complMat = do.call(cbind, lapply(split(info, info$year),
                                       function(x) sapply(1:12, 
                                         function(n) sum(x$complications[x$monthNum==n]))))
      complMat = complMat/casesMat
      complMat[!is.finite(complMat)] = NA
      complMat = 100 * complMat
      ylim = c(0, 1.25*ceiling(max(complMat/10, na.rm=TRUE))*10)
      barplot(t(complMat), beside=TRUE, names.arg=substring(monthNames, 1, 1),
              legend.text=as.character(years), col=1:length(years), xlab="month",
              ylab="cases", main="Complications rate by month and year",
              ylim=ylim, yaxt="n",
              args.legend=list(bty="n", ncol=length(years)))
      Seq = seq(0, ylim[2], by=10)
      axis(2, at=Seq, labels=paste(Seq, "%"))
    } 
  })
  

  output$tbl = renderDataTable({
    data = data()
    info = info()
    validate(need(data, "no data yet"))
    validate(need(info, "no data yet"))
    if (input$analysisType == "One year") {
      years = unique(info$year[input$yearStart:(11+input$yearStart)])
      data = data[data$year %in% years, ]
    } else if (input$analysisType == "Year to date") {
      info = info[info$year == max(info$year), ]
      data = data[data$year == max(data$year), ]
    }
      
    moCol = match("Jan", names(data))
    moCol = moCol:(moCol+11)
    data = cbind(data[, c("year", "complication")],
                 total=apply(as.matrix(data[, moCol]), 1, sum, na.rm=TRUE),
                 data[, moCol])
    data
  }, options=list(paging=FALSE, searching=FALSE, info=FALSE))

} # end server function
