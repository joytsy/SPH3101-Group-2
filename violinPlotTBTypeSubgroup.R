############################################################
# Subgroup Analysis Violin Plot Visualisations for TB Type # 
############################################################

# Run subgroup script to obtain finalised subgroup tb type data for visualisations
source('subgroupCaseCoughSmokeTbtypeChestpain.R')

# Load grid library
library(grid)

tbTypeData = list(datatb1_followup_TBbacPlus, datatb2_followup_TBbacPlus,
            datatb1_followup_TBbacMinus, datatb2_followup_TBbacMinus,
            datatb1_followup_RRTB, datatb2_followup_RRTB)

xlevels = lapply(tbTypeData, function(df) df$stigma_score) # obtain stigma scores from each dataset

COLS = rep(c('#faab5c','#bf3414'), length(xlevels)/2) # repeat colour scheme for each level

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
  XRANGE = c(0,6) ## change range for number of levels i.e. 3 levels -> change to c(0,6)
  grid.newpage()  
  pushViewport(plotViewport(c(3,3.5,1.5,1.5),xscale=XRANGE+0.5,yscale=c(YRANGE)))
  grid.rect()
  grid.xaxis(label= rep(c("Baseline", "Follow-Up"), length(xlevels)/2), # varying depending on number of labels
             at = 1:6,gp=gpar(fontsize=8))
  grid.text(label= c("TB Bac+", "TB Bac-", "RR TB"),  ## change depending on subgroup levels
            x = unit(c(1.5,3.5,5.5),units = 'native'),
            y= unit(-2,units = 'lines'),
            gp=gpar(fontsize=10,fontface = 'bold'))
  
  # added grid text for displaying number of observations in each level
  grid.text(label=c(paste0("(n = ", as.character(length(xlevels[[1]])), ")"), 
                    paste0("(n = ", as.character(length(xlevels[[3]])), ")"),
                    paste0("(n = ", as.character(length(xlevels[[5]])), ")")),
            x = unit(c(1.5, 3.5, 5.5), units = 'native'),
            y = unit(-3.5, units = 'lines'),
            gp=gpar(fontsize=8))
  
  grid.yaxis(gp=gpar(fontsize=8))
  
  grid.text('Distribution of Stigma Scores across TB Types', y=unit(1, 'npc')+unit(1, 'lines'), gp=gpar(fontsize=10,fontface = 'bold')) ## change title
  grid.text('Stigma Score',x=unit(-3.4,'lines'),rot=90,gp=gpar(fontsize=10,fontface = 'bold'))
  
  XAXISPOS <- c(1:6)  # Positions of the boxplots on the x-axis
  
  ## add more depending on the number of lines to segregaate between each level
  grid.lines(x = unit(2.5,'native'),gp = gpar(lty = 'dashed'))
  grid.lines(x = unit(4.5,'native'),gp = gpar(lty = 'dashed'))
  
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
# dev.off()

