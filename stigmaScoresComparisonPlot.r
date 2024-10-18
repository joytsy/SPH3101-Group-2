# run 'stigmaEDA.R' script
source('stigmaEDA.R')

library(grid)

COLS = c('lightpink','steelblue')	# changed color codes

scoresComparisonData = list(datatb1$stigma_score[!is.na(datatb2$stigma_score)],
           datatb2$stigma_score[!is.na(datatb2$stigma_score)])

# Summary Statistics for both time frames
summary(datatb1$stigma_score[!is.na(datatb2$stigma_score)]) # mean=15.37, median=16
summary(datatb2$stigma_score[!is.na(datatb2$stigma_score)]) # mean=13.21, median=14


BOXPLOT_DATA = list()  
VIOLIN_DATA = list()  

for(i in 1:length(scoresComparisonData))
{
  BOXPLOT_DATA[[i]] = list()
  BOXPLOT_DATA[[i]]$min = min((scoresComparisonData[[i]]),na.rm = TRUE)
  # BOXPLOT_DATA[[i]]$min[BOXPLOT_DATA[[i]]$min < -1] = -1 # not applicable since scores are > 0
  BOXPLOT_DATA[[i]]$q1 = as.numeric(quantile((scoresComparisonData[[i]]),probs = 0.25,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q2 = as.numeric(quantile((scoresComparisonData[[i]]),probs = 0.5,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q3 = as.numeric(quantile((scoresComparisonData[[i]]),probs = 0.75,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$max = max((scoresComparisonData[[i]]),na.rm = TRUE)
}

for(i in 1:length(scoresComparisonData))
{
  VIOLIN_DATA[[i]] = list()
  dens = density(scoresComparisonData[[i]])
  VIOLIN_DATA[[i]]$x = pmax(dens$x, 0) # ensure density does not fall below 0
  VIOLIN_DATA[[i]]$y = pmax(dens$y, 0) # ensure density does not fall below 0
}

# save in folder
# png(filename = 'stigmaScoresComparisonPlot.png',width = 20,height = 12,units = 'cm',res = 700) # save in plots folder
if(1)
{
  YRANGE = c(0,38)
  XRANGE = c(0,38)
  scale = c(40,40)
  grid.newpage()  
  pushViewport(plotViewport(c(4,4,1.5,1.5),xscale=c(XRANGE),yscale=c(YRANGE)))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize=10))
  grid.xaxis(gp=gpar(fontsize=10))
  
  grid.text('Stigma Scores of Follow Up Cohort at Baseline and Follow-Up', y=unit(1, 'npc')+unit(0.7, 'lines'), gp=gpar(fontsize=15,fontface = 'bold'))
  grid.text('Follow-Up',x=unit(-2.8,'lines'),rot=90,gp=gpar(fontsize=14,fontface = 'bold'))
  grid.text('Baseline',y=unit(-2.4,'lines'),rot=0,gp=gpar(fontsize=14,fontface = 'bold'))
  
  XAXISPOS <- c(1:4)  # Positions of the boxplots on the x-axis
  
  grid.lines(x= XRANGE,y = YRANGE,default.units = 'native',
             gp = gpar(lty='dotted'))
  
  # separate into baseline<followup, baseline==followup and baseline>followup for plotting points
  for (i in 1:length(scoresComparisonData[[1]])) {
    color <- ifelse(scoresComparisonData[[1]][i] < scoresComparisonData[[2]][i], 'steelblue',
                    ifelse(scoresComparisonData[[1]][i] > scoresComparisonData[[2]][i], 'indianred', 'gray'))
    
    grid.points(x = scoresComparisonData[[1]][i],
                y = scoresComparisonData[[2]][i], 
                default.units = 'native', 
                pch = 16, 
                gp = gpar(col = color, alpha = 0.4, cex = 0.5))
  }
  
  
  for (i in 1:length(scoresComparisonData)) 
  {

    if(i==1) 
    {
      grid.polygon(y = unit(c(VIOLIN_DATA[[i]]$y)*scale[i],'native')+unit(0,'native'),
                   x = unit(VIOLIN_DATA[[i]]$x,'native'),
                   gp = gpar(col = NA, fill = COLS[i],alpha = 0.2))
      
      grid.polygon(x = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                   y = c(0, 2, 2, 0),
                   default.units = 'native', 
                   gp = gpar(col = NA, fill = 'white', alpha=0.8))
      
      grid.lines(x = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
                 y = c(0, 2),
                 default.units = 'native', 
                 gp = gpar(col =COLS[i], lwd = 2,alpha = 0.2))
      
    }
    
    if(i==2) 
    {
      grid.polygon(x = unit(c(VIOLIN_DATA[[i]]$y)*scale[i],'native')+unit(0,'native'),
                   y = unit(VIOLIN_DATA[[i]]$x,'native'),
                   gp = gpar(col = NA, fill = COLS[i],alpha = 0.2))
      
      grid.polygon(y = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                   x = c(0, 1, 1, 0),
                   default.units = 'native', 
                   gp = gpar(col = NA, fill = 'white', alpha=0.8))
      
      grid.lines(y = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
                 x = c(0, 1),
                 default.units = 'native', 
                 gp = gpar(col =COLS[i], lwd = 2,alpha = 0.2))
      
    }
  }
  popViewport()
  
}
# dev.off()

