library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

dt<-fread("timings20210330104858.txt",sep="#",header=F)
setnames(dt,c("cc","ts","step"))
dt<-dt[order(cc,ts)]
dt[,c("tsn","mints","maxts"):=.(shift(ts,-1),min(ts),max(ts)),by=cc][,c("etime","ttime"):=.(difftime(tsn,ts),difftime(maxts,mints))][,ptime:=as.numeric(etime)/as.numeric(ttime)*100]
dt

mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(10)

ggplot(dt, aes(fill=step, y=etime, x=cc)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = mycolors)+
  ggtitle("hhi_oja timing") +
  ylab ("ellapsed time in seconds")+
  xlab("")
# scale_color_brewer(palette = "RdYlBu")
# scale_fill_viridis(discrete = T,option="inferno") +

ggplot(dt[!grepl("finishing",step)], aes(x=cc, y=ptime,fill=step,label=round(ptime,1))) + 
  geom_bar(position="stack",stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = mycolors)+
  ggtitle("hhi_oja relative timing") +
  ylab ("percentage of total time")+
  xlab("")
