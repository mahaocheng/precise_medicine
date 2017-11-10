# TODO: Add comment
# 
# Author: mahaocheng
###############################################################################

# File-Name:       Predict_xml_matching.R        
# Date:            2017-9-21                                
# Author:          haocheng.ma
# Email:           haocheng.ma@haohedata.com
# Purpose:         test pyrorunfile
# Data Used:       ...
# Packages Used:   ...
# Machine:         haocheng.ma's lenovo

#load libraries

#library("plyr")
#library('rJava')
#library('ggplot2')
#library('e1071')
library('XML')
#library('rjson')


setwd("e://test")
file.parsed <- htmlParse("E://test//201707-878-886-002.pyrorun")
well='C2'
wellnr.attrstring<-paste(paste("[@wellnr='",well,sep=""),"']",sep="")
welldata.xpath<-paste("//welldata", wellnr.attrstring, sep="")
#dispensationpath<-paste(welldatapath, "/dispensations/dispensation", sep="")
moment.xpath<-paste(welldata.xpath, "//moment", sep="")
substance.xpath<-paste(welldata.xpath, "//substance", sep="")
signalvalue.xpath<-paste(welldata.xpath, "//signalvalue", sep="")
peakarea.xpath<-paste(welldata.xpath, "//peakarea", sep="")
peakwidth.xpath<-paste(welldata.xpath, "//peakwidth", sep="")
baseline.xpath<-paste(welldata.xpath, "//baselineoffset", sep="")
signaltonoise.xpath<-paste(welldata.xpath, "//signaltonoise", sep="")

intensity.xpath<-paste(welldata.xpath, "/intensity", sep="")

#edispenmomentpath<-paste(welldatapath, "/edispensation/moment", sep="")
#sdispensationpath<-paste(welldatapath, "/sdispensation", sep="")
wellanalysis.xpath<-paste(paste("//wellanalysismethodresults",wellnr.attrstring,sep=""),"/*[@index='0']", sep='')
#wellanalysispath<-"//wellanalysismethodresults[@wellnr='A1']/*[@index='0']"
pattern.xpath<- paste(wellanalysis.xpath,"//regions", sep="")
pattern.value<-xmlGetAttr(getNodeSet(file.parsed, pattern.xpath)[[1]], 'pattern')
pattern.value<-paste(paste(well,":",sep=""),pattern.value,sep="")
qualitypeak.xpath<-paste(wellanalysis.xpath,"//qualitypeaks", sep="")
qualitypeak=strsplit(xmlValue(getNodeSet(file.parsed, qualitypeak.xpath)[[1]]), ";")[[1]]
qualitypeak.whichfalse=which(qualitypeak=="False")
disorderinfo.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions/*[@dispensationorder]", sep=""))
disorderinfo<-xmlSApply(disorderinfo.node, xmlAttrs)
constantinfo.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions/constantregion", sep=""))
constantinfo<-xmlSApply(constantinfo.node, xmlAttrs)
variableinfo.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions/variableregion", sep=""))
variableinfo<-xmlSApply(variableinfo.node, xmlAttrs)

alleleresult.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions//allele", sep=""))
alleleinfo<-xmlSApply(alleleresult.node, xmlAttrs)
#intensity.node<-getNodeSet(resultfile, intensitypath)
#intensity.node
#intensity = xmlValue(getNodeSet(resultfile, intensitypath)[[1]])
#intensity
intensity.value<-as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, intensity.xpath)[[1]]), ";")[[1]])
timestamps.xpath<-"//timestamps"
timestamps.value<-as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, timestamps.xpath)[[1]]), ";")[[1]])/1000
#dispensation.node<-getNodeSet(resultfile, dispensationpath)
#dispensation.node
#moment.node<-getNodeSet(resultfile, momentpath)
#moment.node
moment.value<-as.numeric(xmlSApply(getNodeSet(file.parsed, moment.xpath), xmlValue))/1000
substance.value<-xmlSApply(getNodeSet(file.parsed, substance.xpath), xmlValue)
signal.value<-as.numeric(xmlSApply(getNodeSet(file.parsed, signalvalue.xpath), xmlValue))
peakarea.value<-as.numeric(xmlSApply(getNodeSet(file.parsed, peakarea.xpath), xmlValue))
peakwidth.value<-as.numeric(xmlSApply(getNodeSet(file.parsed, peakwidth.xpath), xmlValue))
baseline.value<-as.numeric(xmlSApply(getNodeSet(file.parsed, baseline.xpath), xmlValue))
signaltonoise<-as.numeric(xmlSApply(getNodeSet(file.parsed, signaltonoise.xpath), xmlValue))
wellinfo.list<-list(intensity=intensity.value,timestamps=timestamps.value, moment=moment.value, substance=substance.value,
    sigvalue=signal.value, area=peakarea.value ,width=peakwidth.value, 
    baseline=baseline.value, noise=signaltonoise, 
    disorder=disorderinfo, constant=constantinfo, 
    variableinfo=variableinfo, alleleresult=alleleinfo, pattern=pattern.value,whichfalse=qualitypeak.whichfalse)
#xmlGetAttr(dispensation.node[[2]], 'moment')
#plot(intensity, type='l')
#welldata.node<-getNodeSet(resultfile, welldatapath)
#welldata.node
wellinfo.list
intensityoffset =wellinfo.list$intensity-wellinfo.list$baseline[1]
intensity.range=max(intensityoffset)-min(intensityoffset)

if(intensity.range<15){interval.y=2}else if(intensity.range <40){interval.y=5}else if(intensity.range <80){interval.y=10}else if(intensity.range <350){interval.y=50}else if(intensity.range <700){interval.y=100}else{interval.y=200}
range.ylimit<-c(floor(min(intensityoffset)/interval.y)*interval.y,ceiling(max(wellinfo.list$sigvalue)/interval.y)*interval.y)
posvari=as.numeric(wellinfo.list$variableinfo["position",1])

#startvari=1
#for (i in 0:(posvari-1)){
#  whichnumber = which(wellinfo.list$constant["position",]==as.character(i))
#  startvari = startvari + as.numeric(nchar(wellinfo.list$constant["dispensationorder",whichnumber]))
#}
#endvari = startvari-1+as.numeric(nchar(wellinfo.list$variableinfo["dispensationorder",1]))
mycolor <- rainbow(5, alpha=0.2)
#wellinfo.list$moment[length(wellinfo.list$substance)]+60

variablestop<-vector()
variablestart<-vector()

allele.counter<-0
for (i in 1:length(variableinfo["position",])){
  whichvariable =which(variableinfo["position",]==as.character(sort(as.numeric(variableinfo["position",]))[i]))
  
  allele.counter<-allele.counter+nchar(variableinfo["dispensationorder",whichvariable])
  variablestop[i]=allele.counter
  variablestart[i]=variablestop[i]+1-nchar(variableinfo["dispensationorder",whichvariable])
}
variableorder<-list(variablestart=variablestart,variablestop=variablestop)
allelestop<-vector()
allelestart<-vector()
allele.counter<-0
for (i in 1:length(variableinfo["position",])){
  whichvariable =which(variableinfo["position",]==as.character(sort(as.numeric(variableinfo["position",]))[i]))
  allele.counter<-allele.counter+length(unique(strsplit(variableinfo["dispensationorder",whichvariable],"")[[1]]))
  allelestop[i]=allele.counter
  allelestart[i]=allelestop[i]+1-length(unique(strsplit(variableinfo["dispensationorder",whichvariable],"")[[1]]))
}
variableorder<-list(allelestart=allelestart,allelestop=allelestop)

if (is.matrix(wellinfo.list$alleleresult)){
  showpattern=vector()
  for (vnum in 1:length(variableinfo["position",])){
    showpattern[vnum]=""
    alleleinfo=wellinfo.list$alleleresult[,variableorder$allelestart[vnum]:variableorder$allelestop[vnum]]
    
    allelenumber<-max(as.numeric(alleleinfo["nr",]))
    if(!("frequency" %in% rownames(alleleinfo))){
      showpattern[vnum]=paste(showpattern[vnum],"N/A",sep="")
    }else{
      for (i in 0:allelenumber){
        whichnumber =which(alleleinfo["nr",]==as.character(i))
        if(nchar(alleleinfo["pattern",whichnumber])==0){
          showpattern[vnum]=paste(showpattern[vnum],"--",sep="")
          patternnumber=max(nchar(alleleinfo["pattern",]))
          for(j in 2:patternnumber){showpattern[vnum] =paste(showpattern[vnum],"--",sep="")}
        }else{
          showpattern[vnum]=paste(showpattern[vnum],alleleinfo["pattern",whichnumber],sep="")
        }
        showpattern[vnum]=paste(showpattern[vnum],paste(sprintf(":%3.f",round(as.numeric(alleleinfo["frequency",whichnumber])*100)),"%",sep=''),sep="")
        showpattern[vnum] =paste(showpattern[vnum],"\n",sep="")
      }
      showpattern[vnum] =substr(showpattern[vnum],1,nchar(showpattern[vnum])-1)
    }  
  }  
}else{
  showpattern=vector()
  for (vnum in 1:length(variableinfo["position",])){
    showpattern[vnum]=""
    alleleinfo=as.matrix(wellinfo.list$alleleresult[[variableorder$allelestart[vnum]]])
    for (allelecount in (variableorder$allelestart[vnum]+1):variableorder$allelestop[vnum]){
      alleleinfo=cbind(alleleinfo,as.matrix(wellinfo.list$alleleresult[[allelecount]]))
    }    
    allelenumber<-max(as.numeric(alleleinfo["nr",]))
    if(!("frequency" %in% rownames(alleleinfo))){
      showpattern[vnum]=paste(showpattern[vnum],"N/A",sep="")
    }else{
      for (i in 0:allelenumber){
        whichnumber =which(alleleinfo["nr",]==as.character(i))
        if(nchar(alleleinfo["pattern",whichnumber])==0){
          showpattern[vnum]=paste(showpattern[vnum],"--",sep="")
          patternnumber=max(nchar(alleleinfo["pattern",]))
          for(j in 2:patternnumber){showpattern[vnum] =paste(showpattern[vnum],"--",sep="")}
        }else{
          showpattern[vnum]=paste(showpattern[vnum],alleleinfo["pattern",whichnumber],sep="")
        }
        showpattern[vnum]=paste(showpattern[vnum],paste(sprintf(":%3.f",round(as.numeric(alleleinfo["frequency",whichnumber])*100)),"%",sep=''),sep="")
        showpattern[vnum] =paste(showpattern[vnum],"\n",sep="")
      }
      showpattern[vnum] =substr(showpattern[vnum],1,nchar(showpattern[vnum])-1)
    }  
  }  
  
}

#jpeg(file="myplot.jpeg",width=1000,height=350)
opar<-par(no.readonly=T)
if(sum(showpattern!="N/A")==0){
  layoutnum=5
}else{
  layoutnum<-5-max((variableorder$allelestop-variableorder$allelestart)[which(showpattern!="N/A")])
}

layout(matrix(c(2,rep(1,layoutnum)),layoutnum+1,1,byrow=T))
par(tck=0.01, bty="l",mar=c(4.1,4.1,0,2.1))

plot(wellinfo.list$timestamps,intensityoffset,yaxt="n",xaxt="n",ylim= c(range.ylimit[1],range.ylimit[2]+0.1*interval.y),
    xlim=c(0,wellinfo.list$moment[length(wellinfo.list$substance)]+60),
    type='l', xlab="", axes = T, ylab= '')

axis(side=1, at=wellinfo.list$moment, labels = c(wellinfo.list$substance,"E","S"),cex.axis=2)
axis(side=2,las =1,at=seq(range.ylimit[1],(range.ylimit[2]+0.1*interval.y) ,interval.y),cex.axis=2)
axis(side=1, at=wellinfo.list$moment[seq(5,length(wellinfo.list$substance),5)], 
    labels = as.character(seq(5,length(wellinfo.list$substance),5)),line=2,tick=F,cex.axis=2)
#mtext(wellinfo.list$pattern,side =3, adj =0, line =0)
#text(wellinfo.list$moment[startvari]-43,rangelimit[2]+0.1*interval,"mahaocheng",pos=1)  
#paste(":%",sprintf("%02.f",0.34652*100),sep='')
abline(h=seq(-1000, range.ylimit[2]-0.1*interval.y, interval.y), lty = 3)
for (i in 1:length(variableorder$variablestart)){
  rect(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30,-10000,wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestop[i]]]+26,10000,col = mycolor, border = F)   
  #text(wellinfo.list$moment[whichfalse[varialbeorder$variablestart[i]]]-30,rangelimit[2]-0.5*interval,showpattern[i],pos=4) 
}
par(tck=0.01, bty="l",mar=c(0,4.1,2.1,2.1)) 
#plot(1:10);legend(2,2,"N/A",bg ="red",xjust=0,yjust=0,x.intersp = -0.5,y.intersp=0.1,adj=c(0,0.4))
plot(1,1,yaxt="n",xaxt="n",ylim= c(0,10),
    xlim=c(0,wellinfo.list$moment[length(wellinfo.list$substance)]+60),
    type='l', xlab="", axes = F, ylab= '',col="white")
mtext(wellinfo.list$pattern,side =3, adj =0, line =0)
#rect(wellinfo.list$moment[startvari]-43,-10000,wellinfo.list$moment[endvari]+14,rangelimit[2]-0.5*interval,col = mycolor, border = F)  
for (i in 1:length(variableorder$variablestart)){
  rect(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30,0,wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30+strwidth(showpattern[i]),strheight(showpattern[i]),col="green")
  #rect(wellinfo.list$moment[whichfalse[varialbeorder$variablestart[i]]]-30,-10000,wellinfo.list$moment[whichfalse[varialbeorder$variablestop[i]]]+26,rangelimit[2]+0.4*interval,col = mycolor, border = F)   
  text(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30,0,showpattern[i],adj=c(0,0),cex=1.4) 
}

#dev.off()
#alleleresult
#png(file="e://myplot.png", bg="transparent")
#n=length(wellinfo.list[[1]]);wellinfo.list[[1]][2:n]-wellinfo.list[[1]][1:(n-1)]