# Run subgroup scripts before running plotViolin function to obtain finalised subgroup data for visualisations
source('processBeforeSubgroup.R')
source('subgroupProvinceAge.R')
source('subgroupFeverWeightSweat.R')
source('subgroupCaseCoughSmokeTbtypeChestpain.R')

library(grid)


# plotViolin function for subgroups with only 2 levels
# arguments: data takes in a list of dataframes i.e. data = list(baseline_below55, followup_below55, baseline_55AndAbove, followup_55AndAbove)
# textLabels takes in the x-axis labels of the levels i.e. textLabels=c("Below 55", "55 and Above")
# title takes in text to add after "Distribution of Stigma Scores across" for the title of the overall plot i.e. title="Age Group"
# example of code usage: plotViolin(data=list(baseline_below55, followup_below55, baseline_55AndAbove, followup_55AndAbove), 
#                                   textLabels=c("Below 55", "55 and Above"), title="Age Groups")
plotViolin <- function(data, textLabels, title) {
  COLS = c('#faab5c','#bf3414', '#faab5c','#bf3414')
  
  xlevels= lapply(data, function(df) df$stigma_score)
  
  BOXPLOT_DATA = list()  
  VIOLIN_DATA = list()  
  
  for(i in 1:length(xlevels))
  {
    BOXPLOT_DATA[[i]] = list()
    BOXPLOT_DATA[[i]]$min = min((xlevels[[i]]),na.rm = TRUE)
    # BOXPLOT_DATA[[i]]$min[BOXPLOT_DATA[[i]]$min < -1] = -1 # not applicable in our data since scores are all >=0
    BOXPLOT_DATA[[i]]$q1 = as.numeric(quantile((xlevels[[i]]),probs = 0.25,na.rm = TRUE))
    BOXPLOT_DATA[[i]]$q2 = as.numeric(quantile((xlevels[[i]]),probs = 0.5,na.rm = TRUE))
    BOXPLOT_DATA[[i]]$q3 = as.numeric(quantile((xlevels[[i]]),probs = 0.75,na.rm = TRUE))
    BOXPLOT_DATA[[i]]$max = max((xlevels[[i]]),na.rm = TRUE)
  }
  
  for(i in 1:length(xlevels))
  {
    VIOLIN_DATA[[i]] = list()
    dens = density(xlevels[[i]])
    
    # Clip the density values to the min/max of the data
    dens$x = pmin(pmax(dens$x, min(xlevels[[i]])), max(xlevels[[i]]))
    
    VIOLIN_DATA[[i]]$x = dens$x
    VIOLIN_DATA[[i]]$y = dens$y
  }
  
  # png(filename = 'violin.png',width = 12,height = 12,units = 'cm',res = 700)
  if(1)
  {
    YRANGE = c(0,38)
    XRANGE = c(0,4)
    grid.newpage()  
    pushViewport(plotViewport(c(3,3.5,1.5,1.5),xscale=XRANGE+0.5,yscale=c(YRANGE)))
    grid.rect()
    grid.xaxis(label= c("Baseline", "Follow-Up", "Baseline", "Follow-Up"),  
               at = 1:4,gp=gpar(fontsize=8))
    grid.text(label= c(textLabels[1], textLabels[2]),  # text labels for each level
              x = unit(c(1.5,3.5),units = 'native'),
              y= unit(-2,units = 'lines'),
              gp=gpar(fontsize=10,fontface = 'bold'))
    
    # added grid text for displaying number of observations in each level
    grid.text(label=c(paste0("(n = ", as.character(length(xlevels[[1]])), ")"), paste0("(n = ", as.character(length(xlevels[[3]])), ")")),
              x = unit(c(1.5, 3.5), units = 'native'),
              y = unit(-3.5, units = 'lines'),
              gp=gpar(fontsize=8))
    
    grid.yaxis(gp=gpar(fontsize=8))
    
    grid.text(paste('Distribution of Stigma Scores across', title), y=unit(1, 'npc')+unit(1, 'lines'), gp=gpar(fontsize=10,fontface = 'bold'))
    grid.text('Stigma Score',x=unit(-3.4,'lines'),rot=90,gp=gpar(fontsize=10,fontface = 'bold'))
    
    XAXISPOS <- c(1:4)  # Positions of the boxplots on the x-axis
    
    grid.lines(x = unit(2.5,'native'),gp = gpar(lty = 'dashed'))
    for (i in 1:length(xlevels)) 
    {
      
      # Draw the vertical line from min to max (whiskers)
      grid.lines(x = c(XAXISPOS[i], XAXISPOS[i]), 
                 y = c(BOXPLOT_DATA[[i]]$min, BOXPLOT_DATA[[i]]$max), 
                 default.units = 'native', 
                 gp = gpar(col = COLS[i], lwd = 2))
      
      
      grid.polygon(x = i+c(-VIOLIN_DATA[[i]]$y,rev(VIOLIN_DATA[[i]]$y)),
                   y = c(VIOLIN_DATA[[i]]$x,rev(VIOLIN_DATA[[i]]$x)),
                   default.units = 'native',
                   gp = gpar(col = NA, fill = COLS[i]))
      
      offset = 0.02
      # Draw the box from Q1 to Q3
      grid.polygon(x = c(XAXISPOS[i] - offset, XAXISPOS[i] + offset, XAXISPOS[i] + offset, XAXISPOS[i] - offset), 
                   y = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                   default.units = 'native', 
                   gp = gpar(col = NA, fill = 'white'))
      
      # Draw the median line (inside the box)
      grid.lines(x = c(XAXISPOS[i] - offset, XAXISPOS[i] + offset), 
                 y = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
                 default.units = 'native', 
                 gp = gpar(col =COLS[i], lwd = 2))
    }
    popViewport()
    
  }
}
