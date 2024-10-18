#############################################################
# Subgroup Analysis Violin Plot Visualisations for Province # 
#############################################################

# Run subgroup script to obtain finalised subgroup province data for visualisations
source('subgroupProvinceAge.R')

# Load grid library
library(grid)

provData = list(kampCham_baseline, kampCham_followup,
            tboung_baseline, tboung_followup,
            kandal_baseline, kandal_followup,
            phnom_baseline, phnom_followup)

xlevels = lapply(provData, function(df) df$stigma_score) # obtain stigma scores from each dataset

# rural areas and urban have different colour schemes
COLS = c(rep(c('#faab5c','#bf3414'), 2), rep(c('#173f62','#5b8f99'), 2)) 

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

# saves in folder
# png(filename = 'ProvinceViolin.png',width = 20,height = 15,units = 'cm',res = 700)
if(1)
{
  YRANGE = c(0,38)
  XRANGE = c(0,8) ## change range for number of levels i.e. 3 levels -> change to c(0,6)
  # Ensure Kampong Cham plots are within axis boundaries, while making sure boxplot of Phnom Penh can be seen
  scale = c(rep(1.4, 2), rep(1.8, 4), rep(2.1, 2)) 
  grid.newpage()  
  pushViewport(plotViewport(c(3.4,3.8,1.9,1.9),xscale=XRANGE+0.5,yscale=c(YRANGE)))
  grid.rect()
  grid.xaxis(label= rep(c("Baseline", "Follow-Up"), length(xlevels)/2), # varying depending on number of labels
             at = 1:8,gp=gpar(fontsize=10)) ## change at according to number of levels
  grid.text(label= c("Kampong Cham", "Tboung Khmum", "Kandal", "Phnom Penh"),  ## change depending on subgroup levels
            x = unit(c(1.5,3.5,5.5,7.5),units = 'native'),
            y= unit(-2,units = 'lines'),
            gp=gpar(fontsize=12,fontface = 'bold'))
  
  # added grid text for displaying number of observations in each level
  grid.text(label=c(paste0("(n = ", as.character(length(xlevels[[1]])), ")"), 
                    paste0("(n = ", as.character(length(xlevels[[3]])), ")"),
                    paste0("(n = ", as.character(length(xlevels[[5]])), ")"),
                    paste0("(n = ", as.character(length(xlevels[[7]])), ")")),
            x = unit(c(1.5, 3.5, 5.5, 7.5), units = 'native'),
            y = unit(-3.5, units = 'lines'),
            gp=gpar(fontsize=10))
  
  grid.yaxis(gp=gpar(fontsize=10))
  
  grid.text('Distribution of Stigma Scores across Provinces', y=unit(1, 'npc')+unit(0.5, 'lines'), gp=gpar(fontsize=14,fontface = 'bold')) ## change title
  grid.text('Stigma Score',x=unit(-3,'lines'),rot=90,gp=gpar(fontsize=12,fontface = 'bold'))
  
  XAXISPOS <- c(1:8)  ## Change positions of the boxplots on the x-axis accordingly
  
  ## add more depending on the number of lines to segregaate between each level
  grid.lines(x = unit(2.5,'native'),gp = gpar(lty = 'dashed'))
  grid.lines(x = unit(4.5,'native'),gp = gpar(lty = 'dashed'))
  grid.lines(x = unit(6.5,'native'),gp = gpar(lty = 'dashed'))
  
  for (i in 1:length(xlevels)) 
  {
    
    # Draw the vertical line from min to max (whiskers)
    grid.lines(x = c(XAXISPOS[i], XAXISPOS[i]),
               y = c(BOXPLOT_DATA[[i]]$min, BOXPLOT_DATA[[i]]$max),
               default.units = 'native',
               gp = gpar(col = COLS[i], lwd = 2))
    
  
    grid.polygon(x = i+c(-(VIOLIN_DATA[[i]]$y)*scale[i],rev(VIOLIN_DATA[[i]]$y*scale[i])),
                 y = c(VIOLIN_DATA[[i]]$x,rev(VIOLIN_DATA[[i]]$x)),
                 default.units = 'native',
                 gp = gpar(col = NA, fill = COLS[i]))
    
    offset = 0.05
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

