
# File-Name:       test_batch.R        
# Date:            2017-9-21                                
# Author:          haocheng.ma
# Email:           haocheng.ma@haohedata.com
# Purpose:         Batch processing to pyrorunfile
# Data Used:       ...
# Packages Used:   ...
# Machine:         haocheng.ma's lenovo

setwd("E://test//tmp2")

library(XML)

if(FALSE){
  filelist=list.files("e://test", pattern="*[^)].pyrorun")
  filelist_analysis<-vector()
  for (i in 1:length(filelist)){   
    if(!length(getNodeSet(htmlParse(filelist[i]), "//runanalysis//wellanalysismethodresults"))==0){
      filelist_analysis=c(filelist_analysis,filelist[i])
      file.copy(filelist[i], "tmp2")
    }
  }
}
filelist=list.files("e://test//tmp2",pattern="*[^)].pyrorun$")
for(file.ordernumber in 1:length(filelist)){
  file.parsed <- htmlParse(filelist[file.ordernumber])
  #resultfile <- htmlParse("GZJ160122-2.pyrorun")
  well.xpath ="//runinfo//wellinfo[@wellnr]"
  welllist = as.vector(xmlSApply(getNodeSet(file.parsed, well.xpath), xmlAttrs))
  aqwelllist=vector()
  for (wellorder in 1:length(welllist)){
    wellnr.attrstring<-paste(paste("[@wellnr='",welllist[wellorder],sep=""),"']",sep="")
    welldata.xpath<-paste("//wellanalysismethodresults", wellnr.attrstring, sep="")
    typepath = paste(welldata.xpath, "//wellanalysisresult[@type='AQ']",sep='')
    if(!length(getNodeSet(file.parsed, typepath))==0){
      aqwelllist=c(aqwelllist,welllist[wellorder])
      well=welllist[wellorder]
      wellnr.attrstring<-paste(paste("[@wellnr='",well,sep=""),"']",sep="")
      welldata.xpath<-paste("//welldata", wellnr.attrstring, sep="")
      moment.xpath<-paste(welldata.xpath, "//moment", sep="")
      substance.xpath<-paste(welldata.xpath, "//substance", sep="")
      signalvalue.xpath<-paste(welldata.xpath, "//signalvalue", sep="")
      peakarea.xpath<-paste(welldata.xpath, "//peakarea", sep="")
      peakwidth.xpath<-paste(welldata.xpath, "//peakwidth", sep="")
      baseline.xpath<-paste(welldata.xpath, "//baselineoffset", sep="")
      signaltonoise.xpath<-paste(welldata.xpath, "//signaltonoise", sep="")     
      intensity.xpath<-paste(welldata.xpath, "/intensity", sep="")
      wellanalysis.xpath<-paste(paste("//wellanalysismethodresults",wellnr.attrstring,sep=""),"/*[@index='0']", sep='')
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
      qualityinfo.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions//polymorphism", sep=""))
      qualityinfo<-xmlSApply(qualityinfo.node, xmlAttrs)     
      
      alleleresult.node<-getNodeSet(file.parsed, paste(wellanalysis.xpath,"//regions//allele", sep=""))
      alleleinfo<-xmlSApply(alleleresult.node, xmlAttrs)
      intensity.value<-as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, intensity.xpath)[[1]]), ";")[[1]])
      timestamps.xpath<-"//timestamps"
      timestamps.value<-as.numeric(strsplit(xmlValue(getNodeSet(file.parsed, timestamps.xpath)[[1]]), ";")[[1]])/1000
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
      wellinfo.list
      intensityoffset =wellinfo.list$intensity-wellinfo.list$baseline[1]
      intensity.range=max(intensityoffset)-min(intensityoffset)     
      if(intensity.range<15){interval.y=2}else if(intensity.range <40){interval.y=5}else if(intensity.range <80){interval.y=10}else if(intensity.range <350){interval.y=50}else if(intensity.range <700){interval.y=100}else{interval.y=200}
      range.ylimit<-c(floor(min(intensityoffset)/interval.y)*interval.y,ceiling(max(wellinfo.list$sigvalue)/interval.y)*interval.y)
      posvari=as.numeric(wellinfo.list$variableinfo["position",1])      
      mycolor <- rainbow(1, start = 0.6, end = 0.7, alpha=0.2)
      variablestop<-vector()
      variablestart<-vector()
      variablecolor<-vector()
      allele.counter<-0
      for (i in 1:length(variableinfo["position",])){
        whichvariable =which(variableinfo["position",]==as.character(sort(as.numeric(variableinfo["position",]))[i]))        
        allele.counter<-allele.counter+nchar(variableinfo["dispensationorder",whichvariable])
        variablestop[i]=allele.counter
        variablestart[i]=variablestop[i]+1-nchar(variableinfo["dispensationorder",whichvariable])
        if(!is.matrix(qualityinfo)){qualityvalue=as.matrix(qualityinfo[[whichvariable]])["quality",]}else{
          qualityvalue=qualityinfo["quality",whichvariable]
        }
        if(qualityvalue=="P"){variablecolor[i]="light blue"}else if(qualityvalue=="C"){variablecolor[i]="yellow"}else{variablecolor[i]="red"}
      }
      variableorder<-list(variablestart=variablestart,variablestop=variablestop,variablecolor=variablecolor) 
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
      
      filename.output=paste(paste(filelist[file.ordernumber],welllist[wellorder],sep=""),".jpeg",sep="")
      jpeg(file=filename.output,width=1200,height=400)
      opar<-par(no.readonly=T)
      if(sum(showpattern!="N/A")==0){
        layoutnum=5
      }else{
        layoutnum<-5-max((variableorder$allelestop-variableorder$allelestart)[which(showpattern!="N/A")])
      }
      
      layout(matrix(c(2,rep(1,layoutnum)),layoutnum+1,1,byrow=T))
      par(tck=0.01, bty="l",mar=c(6.1,4.1,0,2.1))
      
      plot(wellinfo.list$timestamps,intensityoffset,yaxt="n",xaxt="n",ylim= c(range.ylimit[1],range.ylimit[2]+0.1*interval.y),
          xlim=c(0,wellinfo.list$moment[length(wellinfo.list$substance)]+60),
          type='l', xlab="", axes = T, ylab= '',col="dark blue")
      
      axis(side=1, at=wellinfo.list$moment, labels = c(wellinfo.list$substance,"E","S"),cex.axis=2)
      axis(side=1, at=wellinfo.list$moment[seq(5,length(wellinfo.list$substance),5)], 
          labels = as.character(seq(5,length(wellinfo.list$substance),5)),line=3,tick=F,cex.axis=2)
      axis(side=2,las =1,at=seq(range.ylimit[1],(range.ylimit[2]+0.1*interval.y) ,interval.y),cex.axis=1.5)
      abline(h=seq(-1000, range.ylimit[2]-0.1*interval.y, interval.y), lty = 3)
      for (i in 1:length(variableorder$variablestart)){
        rect(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30,-10000,wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestop[i]]]+26,10000,col = mycolor, border = F)   
        #text(wellinfo.list$moment[whichfalse[varialbeorder$variablestart[i]]]-30,rangelimit[2]-0.5*interval,showpattern[i],pos=4) 
      }
      par(tck=0.01, bty="l",mar=c(0,4.1,2.1,2.1)) 
      plot(1,1,yaxt="n",xaxt="n",ylim= c(0,10),
          xlim=c(0,wellinfo.list$moment[length(wellinfo.list$substance)]+60),
          type='l', xlab="", axes = F, ylab= '',col="white")
      mtext(wellinfo.list$pattern,side =3, adj =0, line =0,cex=1.7)
      for (i in 1:length(variableorder$variablestart)){
        rect(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30,0,
            wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30+2*strwidth(showpattern[i])+xinch(0.1),
            2*strheight(showpattern[i])+yinch(0.1),col=variableorder$variablecolor[i],border = F)
        #rect(wellinfo.list$moment[whichfalse[varialbeorder$variablestart[i]]]-30,-10000,wellinfo.list$moment[whichfalse[varialbeorder$variablestop[i]]]+26,rangelimit[2]+0.4*interval,col = mycolor, border = F)   
        text(wellinfo.list$moment[qualitypeak.whichfalse[variableorder$variablestart[i]]]-30+xinch(0.05),yinch(0.05),showpattern[i],adj=c(0,0),cex=2) 
      }
      dev.off()      
    }
  }
}
rm(list=ls())

