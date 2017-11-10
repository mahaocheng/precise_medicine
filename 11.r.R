# TODO: Add comment
# 
# Author: lenovo
###############################################################################
1 -
   2

filelist=list.files("e://test//tmp2",pattern="*[^)].pyrorun$")
dir.create("tmp3") 
for(file.ordernumber in 1:length(filelist)){
  file.parsed <- htmlParse(filelist[file.ordernumber])
  #resultfile <- htmlParse("TJY-2017-05-24-1.pyrorun")
  well.xpath ="//runinfo//wellinfo[@wellnr]"
  welllist = as.vector(xmlSApply(getNodeSet(file.parsed, well.xpath), xmlAttrs))

  for (wellorder in 1:length(welllist)){
    wellnr.attrstring<-paste(paste("[@wellnr='",welllist[wellorder],sep=""),"']",sep="")
    welldata.xpath<-paste("//wellanalysismethodresults", wellnr.attrstring, sep="")
    typepath = paste(welldata.xpath, "//wellanalysisresult[@type='AQ']",sep='')
    if(length(getNodeSet(file.parsed, typepath))==0){
      #file.copy(filelist[nameorder], "tmp3")
      print(paste(filelist[file.ordernumber],welllist[wellorder],sep=''))
    }
  }}
      
#
setwd("e://test//tmp3")
filelist<- list.files(pattern="*.jpeg")
file.remove(filelist)

setwd("e://test//tmp2")

filelist<- list.files(pattern="*C7.jpeg")
file.copy(filelist,"e://test//tmp3")


layout(matrix(c(rep(2,5),rep(1,20)),5,5,byrow=T))
par(tck=0.01, bty="l",mar=c(5,4,0,2)+0.1)  
plot(1:10)
legend(1,1,"N/A",bg ="red",xjust=0,yjust=0,x.intersp = -0.5,y.intersp=0.1,adj=c(0,0.4))

par(tck=0.01, bty="l",mar=c(2,4,4,2)+0.1) 
plot(1,1,xlim=c(0,10),axes=F,xlab="",  ylab= '',col="white")
mtext("DAFADFAFDA",side =3, adj =0, line =0)
legend(1,1,"N/A",bg ="red",xjust=0,yjust=0,x.intersp = -0.5,y.intersp=0.1)



require(grDevices)
## set up the plot region:
op <- par(bg = "thistle")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "",
    main = "2 x 11 rectangles; 'rect(100+i,300+i,  150+i,380+i)'")
i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i
## and top right (150, 380)+i
rect(100+i, 300+i, 150+i, 380+i, col = rainbow(1, start = 0.6, end = 0.7))
#rect(240-i, 320+i, 250-i, 410+i, col = heat.colors(11), lwd = i/5)
## Background alternating  ( transparent / "bg" ) :


