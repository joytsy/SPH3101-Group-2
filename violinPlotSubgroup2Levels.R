#############################################################
# Subgroup Analysis Violin Plot Visualisations for 2 levels # 
#############################################################

# Run subgroup scripts before running plotViolin function to obtain finalised subgroup data for visualisations
source('processBeforeSubgroup.R')
source('subgroupAgeGroups.R')
source('subgroupFever.R')
source('subgroupNightSweat.R')
source('subgroupCough.R')
source('subgroupSmokingStatus.R')
source('subgroupChestPain.R')
source('subgroupTBtype.R')
source('subgroupCaseStatus.R')


########################################################
# plotViolin function for subgroups with only 2 levels #
########################################################

# Arguments: data takes in a list of dataframes i.e. data = list(baseline_below55, followup_below55, baseline_55AndAbove, followup_55AndAbove)
# textLabels takes in the x-axis labels of the levels i.e. textLabels=c("Below 55", "55 and Above")
# title takes in text to add after "Distribution of Stigma Scores across" for the title of the overall plot i.e. title="Age Group"
# example of code usage: plotViolin(data=list(baseline_below55, followup_below55, baseline_55AndAbove, followup_55AndAbove), 
#                                   textLabels=c("Below 55", "55 and Above"), title="Age Groups")
plotViolin <- function(data, textLabels, title) {
  
  xlevels= lapply(data, function(df) df$stigma_score)
  
  COLS = rep(c('#faab5c','#bf3414'), length(xlevels)/2)
  
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
  
  # save plot in folder
  # png(filename = paste0(gsub(" ", "", title), 'Violin.png'),width = 20,height = 15,units = 'cm',res = 700)
  if(1)
  {
    YRANGE = c(0,38)
    XRANGE = c(0,4)
    scale = 2.5 # added scale to adjust widths of violin plots
    grid.newpage()  
    pushViewport(plotViewport(c(2.8,3.4,1.4,1.4),xscale=XRANGE+0.5,yscale=c(YRANGE)))
    grid.rect()
    grid.xaxis(label= c("Baseline", "Follow-Up", "Baseline", "Follow-Up"),  
               at = 1:4,gp=gpar(fontsize=10))
    grid.text(label= c(textLabels[1], textLabels[2]),  # text labels for each level
              x = unit(c(1.5,3.5),units = 'native'),
              y= unit(-1.8,units = 'lines'),
              gp=gpar(fontsize=12,fontface = 'bold'))
    
    # added grid text for displaying number of observations in each level
    grid.text(label=c(paste0("(n = ", as.character(length(xlevels[[1]])), ")"), paste0("(n = ", as.character(length(xlevels[[3]])), ")")),
              x = unit(c(1.5, 3.5), units = 'native'),
              y = unit(-3, units = 'lines'),
              gp=gpar(fontsize=10))
    
    grid.yaxis(gp=gpar(fontsize=10))
    
    grid.text(paste('Distribution of Stigma Scores across', title), y=unit(1, 'npc')+unit(0.5, 'lines'), gp=gpar(fontsize=14,fontface = 'bold'))
    grid.text('Stigma Score',x=unit(-2.8,'lines'),rot=90,gp=gpar(fontsize=12,fontface = 'bold'))
    
    XAXISPOS <- c(1:4)  # Positions of the boxplots on the x-axis
    
    grid.lines(x = unit(2.5,'native'),gp = gpar(lty = 'dashed'))
    for (i in 1:length(xlevels)) 
    {
      
      # Draw the vertical line from min to max (whiskers)
      grid.lines(x = c(XAXISPOS[i], XAXISPOS[i]), 
                 y = c(BOXPLOT_DATA[[i]]$min, BOXPLOT_DATA[[i]]$max), 
                 default.units = 'native', 
                 gp = gpar(col = COLS[i], lwd = 2))
      
      
      grid.polygon(x = i+c(-(VIOLIN_DATA[[i]]$y*scale),rev(VIOLIN_DATA[[i]]$y*scale)),
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
}

#################################################
# Subgroup Analysis Visualisations for 2 levels #
#################################################
# Age group
plotViolin(data=list(baseline_below55, followup_below55,
                     baseline_55AndAbove, followup_55AndAbove),
           textLabels=c("Below 55", "55 and Above"), title="Age Groups")

# Case Status (Completed and Cured)
plotViolin(data=list(datatb1_followup_case_completed, datatb2_followup_case_completed,
                     datatb1_followup_case_cured, datatb2_followup_case_cured),
           textLabels=c("Completed", "Cured"), title="Case Statuses")

# Smoking Status
plotViolin(data=list(datatb1_followup_nonsmoker, datatb2_followup_nonsmoker,
                     datatb1_followup_smoker, datatb2_followup_smoker),
           textLabels=c("Non-Smoker", "Smoker"), title="Smoking Status")

# Cough
plotViolin(data=list(datatb1_followup_nocough, datatb2_followup_nocough,
               datatb1_followup_cough, datatb2_followup_cough),
     textLabels=c("No Cough", "Cough"), title="Presence of Cough")

# Fever
plotViolin(data=list(no_fever_baseline, no_fever_followup,
                     fever_baseline, fever_followup),
           textLabels=c("No Fever", "Fever"), title="Presence of Fever")

# Night Sweat
plotViolin(data=list(no_night_sweat_baseline, no_night_sweat_followup,
                     night_sweat_baseline, night_sweat_followup),
           textLabels=c("No Night Sweat", "Night Sweat"), title="Presence of Night Sweat")

# Chest Pain
plotViolin(data=list(datatb1_followup_nochestpain, datatb2_followup_nochestpain,
                     datatb1_followup_chestpain, datatb2_followup_chestpain),
           textLabels=c("No Chest Pain", "Chest Pain"), title="Presence of Chest Pain")

# TB Type (TB Bac+ and TB Bac-)
plotViolin(data=list(datatb1_followup_TBbacPlus, datatb2_followup_TBbacPlus,
                     datatb1_followup_TBbacMinus, datatb2_followup_TBbacMinus),
           textLabels=c("TB Bac+", "TB Bac-"), title="TB Types")

