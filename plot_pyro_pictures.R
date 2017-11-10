# File-Name:       plot_pyro_pictures.R        
# Date:            2017-11-10                                
# Author:          mahaocheng
# Email:           haocheng.ma@haohedata.com
# Purpose:         plot pyro pictures
# Data Used:       ...
# Packages Used:   ...
# Machine:         haocheng.ma's lenovo
#dir.create("E://pyropictures_test5")
require(XML)
argv <- commandArgs(TRUE)
input.path <- argv[1]
output.path <- argv[2]
#Rscript C:/Users/lenovo/workspace/precise_medicine/plot_pyro_pictures.R "E:/test/tmp4" "E:/pyropictures_test6"
#input.path <- "E://test//tmp4"
#output.path <- "E://pyropictures_test5//TEST"
if (!file.exists(output.path)){
  dir.create(output.path, showWarnings = FALSE)
}
setwd(output.path)

opar <- par(no.readonly = T)
ClassifyWellType <- function(input.path, filename){
  #filename <- "CS160718.pyrorun"
  file.parsed <- htmlParse(paste(input.path, "//", filename, sep=""))  
  wellsetuplist.xpath <- "//wellsetup[@wellnr]"
  if (class(xmlSApply(getNodeSet(file.parsed, wellsetuplist.xpath),
          xmlAttrs)) == "list"){
    wellsetuplist <- as.vector(sapply(xmlSApply(getNodeSet(file.parsed, 
                    wellsetuplist.xpath), xmlAttrs), 
            function(x) {return (x["wellnr"])}))
  } else {
    wellsetuplist <- as.vector(xmlSApply(getNodeSet(file.parsed, 
                wellsetuplist.xpath), xmlAttrs)["wellnr",])
  }
  kTotalWell <-paste(c("A","B","C"), rep(1:8, each=3), sep="")
  wellsetuplist <- intersect(wellsetuplist, kTotalWell)
  analyzed.welllist.xpath <- "//wellanalysismethodresults[@wellnr]"
  analyzed.welllist <- as.vector(xmlSApply(getNodeSet(file.parsed, 
              analyzed.welllist.xpath), xmlAttrs))
  notanalyzed.welllist <- setdiff(wellsetuplist, analyzed.welllist)
  AQnotanalyzed.welllist <- vector()
  for (notanalywell in notanalyzed.welllist) {
    #notanalywell <- notanalyzed.welllist[1]
    notanalyzed.welllist.xpath <- paste("//wellsetup[@wellnr='",
        notanalywell, "']", sep="")
    if (xmlSApply(getNodeSet(file.parsed, notanalyzed.welllist.xpath), 
        xmlAttrs)['type', 1] == "AQ") {
      AQnotanalyzed.welllist <- c(AQnotanalyzed.welllist, notanalywell)
    }
  }
  Othernotanalyzed.welllist <- setdiff(notanalyzed.welllist, 
      AQnotanalyzed.welllist)
  AQanalyzed.welllist <- vector()
  for (analywell in analyzed.welllist) {
    #analywell <- analyzed.welllist[1]
    #analywell <- "A7"
    AQanalyzed.welllist.xpath <- paste("//wellanalysismethodresults[@wellnr='", 
        analywell, "']/wellanalysismethodresult[@analysismethodname=
'AQAnalysisMethod']", sep='')
    AQanalyzed.welllist.node <- getNodeSet(file.parsed, AQanalyzed.welllist.xpath)
    if (length(AQanalyzed.welllist.node) >0) {
      AQanalyzed.welllist <- c(AQanalyzed.welllist, analywell)
    }
  }
  
  Otheranalyzed.welllist <- setdiff(analyzed.welllist, AQanalyzed.welllist)
  categorywelllist <- list (
      notanalyzed.welllist = as.vector(notanalyzed.welllist),
      AQnotanalyzed.welllist = as.vector(AQnotanalyzed.welllist),
      Othernotanalyzed.welllist = Othernotanalyzed.welllist,
      analyzed.welllist = as.vector(analyzed.welllist),
      AQanalyzed.welllist = as.vector(AQanalyzed.welllist),
      Otheranalyzed.welllist = as.vector(Otheranalyzed.welllist))
  print(categorywelllist)
  return(categorywelllist)
}

GenerateInterval <- function(intensity){
  intensity.range <- max(intensity) - min(intensity)     
  if (intensity.range < 15) {
    interval <- 2
  } else if (intensity.range < 40) {
    interval <- 5 
  } else if (intensity.range < 80) {
    interval <- 10
  } else if (intensity.range < 350) {
    interval <- 50
  } else if (intensity.range < 700) {
    interval <- 100
  } else {
    interval <- 200
  }
  return(interval)
}

GenerateAQList <- function(input.path, filename, wellname){
  #GenerateAQList("GZJ20170906.pyrorun","A1")
  #filename <- "CS160718.pyrorun";wellname <- "A1"
  file.parsed <- htmlParse(paste(input.path, "//", filename, sep=""))  
  well.name <- wellname
  wellnr.attrstring <- paste("[@wellnr='", well.name, "']", sep="")
  welldata.xpath <- paste("//welldata", wellnr.attrstring, sep="")
  moment.xpath <- paste(welldata.xpath, "//moment", sep = "")
  substance.xpath <- paste(welldata.xpath, "//substance", sep = "")
  signalvalue.xpath <- paste(welldata.xpath, "//signalvalue", sep = "")      
  baseline.xpath <- paste(welldata.xpath, "//baselineoffset", sep = "")   
  intensity.xpath <- paste(welldata.xpath, "/intensity", sep = "")
  wellanalysis.xpath <- paste("//wellanalysismethodresults",
      wellnr.attrstring, "/*[@index='0']", sep = "")
  pattern.xpath <- paste(wellanalysis.xpath, "//regions", sep = "")
  qualitypeak.xpath <- paste(wellanalysis.xpath, "//qualitypeaks", sep="")
  disorderinfo.xpath <- paste(wellanalysis.xpath, 
      "//regions/*[@dispensationorder]", sep = "")
  constantinfo.xpath <- paste(wellanalysis.xpath, 
      "//regions/constantregion", sep = "")
  variableinfo.xpath <- paste(wellanalysis.xpath, 
      "//regions/variableregion", sep = "")
  qualityinfo.xpath <- paste(wellanalysis.xpath, 
      "//regions//polymorphism", sep = "")
  alleleresult.xpath <- paste(wellanalysis.xpath, 
      "//regions//allele", sep = "")
  timestamps.xpath <- "//timestamps"
  pattern.value <- xmlGetAttr(getNodeSet(file.parsed, pattern.xpath)[[1]],
      'pattern')
  pattern.value <- paste(well.name, ":", pattern.value, sep = "")      
  qualitypeak <- strsplit(xmlValue(getNodeSet(file.parsed, 
              qualitypeak.xpath)[[1]]), ";")[[1]]
  qualitypeak.whichfalse <- which(qualitypeak == "False")
  disorderinfo.node <- getNodeSet(file.parsed, disorderinfo.xpath)
  disorderinfo <- xmlSApply(disorderinfo.node, xmlAttrs)
  constantinfo.node <- getNodeSet(file.parsed, constantinfo.xpath)
  constantinfo <- xmlSApply(constantinfo.node, xmlAttrs)
  variableinfo.node <- getNodeSet(file.parsed, variableinfo.xpath)
  variableinfo <- xmlSApply(variableinfo.node, xmlAttrs)     
  qualityinfo.node <- getNodeSet(file.parsed, qualityinfo.xpath)
  qualityinfo <- xmlSApply(qualityinfo.node, xmlAttrs)          
  alleleresult.node <- getNodeSet(file.parsed, alleleresult.xpath)
  alleleresult <- xmlSApply(alleleresult.node, xmlAttrs)
  intensity.value <- as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, 
                  intensity.xpath)[[1]]), ";")[[1]])
  timestamps.value <- as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, 
                  timestamps.xpath)[[1]]), ";")[[1]]) / 1000
  moment.value <- as.numeric(xmlSApply(getNodeSet(file.parsed, 
              moment.xpath), xmlValue)) / 1000
  substance.node <- getNodeSet(file.parsed, substance.xpath)
  substance.value <- xmlSApply(substance.node, xmlValue)
  signalvalue.node <- getNodeSet(file.parsed, signalvalue.xpath)
  signal.value <- as.numeric(xmlSApply(signalvalue.node, xmlValue))
  baseline.node <- getNodeSet(file.parsed, baseline.xpath)
  baseline.value <- as.numeric(xmlSApply(baseline.node, xmlValue))
  wellinfo.list <- list(intensity = intensity.value,
      timestamps = timestamps.value, 
      moment = moment.value, 
      substance = substance.value,
      sigvalue = signal.value,
      qualityinfo = qualityinfo,
      baseline = baseline.value, 
      disorder = disorderinfo, 
      constant = constantinfo, 
      variableinfo = variableinfo, 
      alleleresult = alleleresult, 
      pattern = pattern.value,
      qualitypeak.whichfalse = qualitypeak.whichfalse)
  return(wellinfo.list)     
}

GenerateVariableList <- function(welldata.list){
  variablestop <- vector()
  variablestart <- vector()
  variablecolor <- vector()
  allelestop <- vector()
  allelestart <- vector() 
  variable.counter <- 0
  allele.counter <- 0
  for (i in 1 : length(welldata.list$variableinfo["position", ])) {
    # i <- 1
    whichvariable <- which(welldata.list$variableinfo["position", ] == 
            as.character(sort(as.numeric(welldata.list$variableinfo[
                            "position", ]))[i]))        
    variable.counter <- variable.counter + 
        nchar(welldata.list$variableinfo["dispensationorder", whichvariable])
    variablestop[i] <- variable.counter
    variablestart[i] <- variablestop[i] + 1 - 
        nchar(welldata.list$variableinfo["dispensationorder", whichvariable])
    allele.counter <- allele.counter + length(unique(strsplit(
                welldata.list$variableinfo[
                    "dispensationorder", whichvariable], "")[[1]]))
    allelestop[i] <- allele.counter
    allelestart[i] <- allelestop[i] + 1 - length(unique(strsplit(
                welldata.list$variableinfo["dispensationorder", whichvariable],
                "")[[1]]))
    if (!is.matrix(welldata.list$qualityinfo)){
      qualityvalue <- as.matrix(welldata.list$qualityinfo[[whichvariable
                  ]])["quality", ]
    } else {
      qualityvalue <- welldata.list$qualityinfo["quality", whichvariable]
    }
    if (qualityvalue == "P") {
      variablecolor[i] <- "light blue"
    } else if (qualityvalue == "C") {
      variablecolor[i] <- "yellow"
    } else {
      variablecolor[i] <- "red"
    }
  }
  if (is.matrix(welldata.list$alleleresult)) {
    showpattern <- vector()
    for (vnum in 1 : length(welldata.list$variableinfo["position", ])){
      showpattern[vnum] <- ""
      alleleinfo <- welldata.list$alleleresult[,
          allelestart[vnum] : allelestop[vnum]]         
      allelenumber <- max(as.numeric(alleleinfo["nr", ]))
      if (!("frequency" %in% rownames(alleleinfo))) {
        showpattern[vnum] <- paste(showpattern[vnum], "N/A", sep="")
      } else {
        for (i in 0 : allelenumber) {
          whichnumber <- which(alleleinfo["nr", ] == as.character(i))
          if (nchar(alleleinfo["pattern", whichnumber]) == 0) {
            showpattern[vnum] <- paste(showpattern[vnum], "--", sep="")
            patternnumber <- max(nchar(alleleinfo["pattern", ]))
            for(j in 2 : patternnumber) {
              showpattern[vnum] <-
                  paste(showpattern[vnum], "--", sep="")}
          } else {
            showpattern[vnum] <- paste(showpattern[vnum],
                alleleinfo["pattern", whichnumber], sep="")
          }
          showpattern[vnum] <-paste(showpattern[vnum], sprintf(":%3.f", 
                  round(as.numeric(alleleinfo["frequency",
                              whichnumber])*100)), "%", sep="")
          showpattern[vnum] <- paste(showpattern[vnum], "\n", sep="")
        }
        showpattern[vnum] <- substr(showpattern[vnum], 
            1, nchar(showpattern[vnum]) - 1)
      }  
    }  
  } else {
    showpattern <- vector()
    for (vnum in 1 : length(welldata.list$variableinfo["position", ])) {
      showpattern[vnum] <- ""
      alleleinfo <- as.matrix(welldata.list$alleleresult[[
              allelestart[vnum]]])
      for (allelecount in (allelestart[vnum]+1):
          allelestop[vnum]){
        alleleinfo <- cbind(alleleinfo,
            as.matrix(welldata.list$alleleresult[[allelecount]]))
      }    
      allelenumber <- max(as.numeric(alleleinfo["nr", ]))
      if (!("frequency" %in% rownames(alleleinfo))) {
        showpattern[vnum] <- paste(showpattern[vnum], "N/A", sep="")
      } else {
        for (i in 0 : allelenumber){
          whichnumber <- which(alleleinfo["nr", ] == as.character(i))
          if (nchar(alleleinfo["pattern", whichnumber]) == 0) {
            showpattern[vnum] <- paste(showpattern[vnum], "--", sep="")
            patternnumber <- max(nchar(alleleinfo["pattern", ]))
            for (j in 2 : patternnumber){showpattern[vnum] = 
                  paste(showpattern[vnum], "--", sep="")}
          } else {
            showpattern[vnum] <- paste(showpattern[vnum],
                alleleinfo["pattern", whichnumber], sep="")
          }
          showpattern[vnum] <- paste(showpattern[vnum],
              paste(sprintf(":%3.f", round(as.numeric(alleleinfo["frequency",
                                  whichnumber]) * 100)), "%", sep=''), sep="")
          showpattern[vnum] <- paste(showpattern[vnum], "\n", sep="")
        }
        showpattern[vnum] <- substr(showpattern[vnum],
            1, nchar(showpattern[vnum]) - 1)
      }  
    }          
  }
  variableorder <- list(variablestart = variablestart,
      variablestop = variablestop,
      variablecolor = variablecolor,
      allelestart = allelestart, 
      allelestop = allelestop,
      showpattern = showpattern) 
  return(variableorder)
}

PlotAnalyedAQWell <- function(input.path, filename, wellname){
  #wellinfo.list <-  GenerateAQList("GZJ20170906.pyrorun", "A1")
  wellinfo.list <-  GenerateAQList(input.path, filename, wellname)    
  variableinfo.list <- GenerateVariableList(wellinfo.list)
  interval.y <- GenerateInterval(wellinfo.list$intensity) 
  intensityoffset <- wellinfo.list$intensity - wellinfo.list$baseline[1]
  range.ylimit <- c(floor(min(intensityoffset) / interval.y) * interval.y,
      ceiling(max(wellinfo.list$sigvalue) / interval.y) * interval.y)
# posvari <- as.numeric(wellinfo.list$variableinfo["position", 1])      
  mycolor <- rainbow(1, start = 0.6, end = 0.7, alpha = 0.2)
  filename.output <- paste(wellname, "_", filename, ".jpeg", sep="")
  jpeg(file = filename.output, width = 1200, height = 400)
  if (sum(variableinfo.list$showpattern != "N/A") == 0) {
    layoutnum <- 5
  } else {
    layoutnum <- 5 - max((variableinfo.list$allelestop -
            variableinfo.list$allelestart)[which(variableinfo.list$showpattern
                    != "N/A")])
  }
  
  layout(matrix(c(2, rep(1, layoutnum)), layoutnum + 1, 1, byrow=T))
  par(tck=0.01, 
      bty="l", 
      mar=c(6.1, 4.1, 0, 2.1)) 
  plot(wellinfo.list$timestamps,
      intensityoffset,
      yaxt = "n",
      xaxt = "n",
      ylim = c(range.ylimit[1],range.ylimit[2] + 0.1 * interval.y),
      xlim = c(0, wellinfo.list$moment[length(wellinfo.list$substance)] + 60),
      type = 'l', 
      xlab = "", 
      axes = T, 
      ylab = '',
      col = "dark blue")     
  axis(side = 1, 
      at = wellinfo.list$moment,
      labels = c(wellinfo.list$substance, "E", "S"), 
      cex.axis = 2)
  axis(side = 1, 
      at = wellinfo.list$moment[seq(5, length(wellinfo.list$substance), 5)], 
      labels = as.character(seq(5, length(wellinfo.list$substance),5)),
      line = 3,
      tick = F,
      cex.axis = 2)
  axis(side = 2,
      las = 1,
      at = seq(range.ylimit[1], (range.ylimit[2] + 0.1 * interval.y), 
          interval.y),
      cex.axis = 1.5)
  abline(h = seq(-1000, range.ylimit[2] - 0.1 * interval.y, interval.y), 
      lty = 3)
  for (i in 1 : length(variableinfo.list$variablestart)) {
    rect(wellinfo.list$moment[wellinfo.list$qualitypeak.whichfalse[
                variableinfo.list$variablestart[i]]] - 30, -10000,
        wellinfo.list$moment[wellinfo.list$qualitypeak.whichfalse[
                variableinfo.list$variablestop[i]]] + 26, 10000,
        col = mycolor, 
        border = F)   
  }
  par(tck = 0.01, 
      bty = "l", 
      mar = c(0, 4.1, 2.1, 2.1)) 
  plot(1, 1, yaxt = "n",
      xaxt = "n",
      ylim = c(0, 10),
      xlim = c(0, wellinfo.list$moment[length(wellinfo.list$substance)] + 60),
      type = 'l', 
      xlab = "", 
      axes = F, 
      ylab = '',
      col = "white")
  mtext(wellinfo.list$pattern, 
      side = 3, 
      adj = 0, 
      line = 0,
      cex = 1.7)
  for (i in 1 : length(variableinfo.list$variablestart)) {
    rect(wellinfo.list$moment[wellinfo.list$qualitypeak.whichfalse[
                variableinfo.list$variablestart[i]]] - 30, 0,
        wellinfo.list$moment[wellinfo.list$qualitypeak.whichfalse[
                    variableinfo.list$variablestart[i]]] - 30 + 
            2 * strwidth(variableinfo.list$showpattern[i]) + xinch(0.1),
        2 * strheight(variableinfo.list$showpattern[i]) + yinch(0.1),
        col = variableinfo.list$variablecolor[i],
        border = F)
    text(wellinfo.list$moment[wellinfo.list$qualitypeak.whichfalse[
                    variableinfo.list$variablestart[i]]] -
            30 + xinch(0.05), yinch(0.05), 
        variableinfo.list$showpattern[i], 
        adj = c(0, 0), 
        cex = 2) 
  }
  dev.off()      
}

PlotOtherWell <- function(filename, wellname, reason.text, color){
  filename.output <- paste(wellname, "_", filename, ".jpeg", sep="")
  jpeg(file = filename.output, width = 1200, height = 400)
  plot(1, 1, yaxt = "n",
      xaxt = "n",
      ylim = c(0, 10),
      xlim = c(0, 10),
      type = 'l', 
      xlab = "", 
      axes = F, 
      ylab = '',
      col = "white")
  text(5, 5, reason.text, cex = 4, col = color)
  dev.off()       
}
#try(, silent=TRUE) 
#PlotAnalyedAQWell("GZJ20170906.pyrorun", "A1")
if(TRUE){
  filelist <- list.files(input.path, pattern = "*[^)].pyrorun$")
  for (file.name in filelist) {
    #file.name <- filelist[2]
    print(file.name)
    category.welltype <- try(ClassifyWellType(input.path, file.name), 
        silent=TRUE)
    if('try-error' %in% class(category.welltype)){
      break
    }
    if (length(category.welltype$AQanalyzed.welllist) > 0) {
      for (wellname in category.welltype$AQanalyzed.welllist) {
        fit <- try(PlotAnalyedAQWell(input.path, file.name, wellname), 
            silent=TRUE)
        if('try-error' %in% class(fit)){
          PlotOtherWell(file.name, wellname, "出错了！请人工插入峰形图并联系管理员！", "red")
        }
      }
    }
    if (length(category.welltype$AQnotanalyzed.welllist) > 0) {
      for (wellname in category.welltype$AQnotanalyzed.welllist) {
        fit <- try(PlotOtherWell(file.name, wellname, "未分析AQ类型孔位，请人工插入峰形图！", 
                "dark red"), silent=TRUE) 
        if('try-error' %in% class(fit)){
          PlotOtherWell(file.name, wellname, "出错了！请人工插入峰形图并联系管理员！", "black")
        }
      }
    }
    if (length(category.welltype$Othernotanalyzed.welllist) > 0) {
      for (wellname in category.welltype$Othernotanalyzed.welllist) {
        fit <- try(PlotOtherWell(file.name, wellname, "未分析非AQ类型孔位，请人工插入峰形图！", 
                "yellow"), silent=TRUE)
        if('try-error' %in% class(fit)){
          PlotOtherWell(file.name, wellname, "出错了！请人工插入峰形图并联系管理员！", "black")
        }
      }
    }
    if (length(category.welltype$Otheranalyzed.welllist) > 0) {
      for (wellname in category.welltype$Otheranalyzed.welllist) {
        fit <- try(PlotOtherWell(file.name, wellname, "SQA等非AQ类型孔位，请人工插入峰形图！", 
                "blue"), silent=TRUE)
        if('try-error' %in% class(fit)){
          PlotOtherWell(file.name, wellname, "出错了！请人工插入峰形图并联系管理员！", "black")
        }
      }
    }
  }
  #ClassifyWellType("BZJ-1705025-1.pyrorun")
}
par(opar)
rm(list = ls())

