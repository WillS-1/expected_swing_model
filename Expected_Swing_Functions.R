library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)


# Functions

#----Function that takes in PlateLocSide (x) & PlateLocHeight (y) to create new zone system
ZONECA <- function(x, y){
  ifelse(x > -.83 & x < -.277 & y > 1.52 & y < 2.26, '1',
  ifelse(x > -.277 & x < .277 & y > 1.52 & y < 2.26, '2',
  ifelse(x > .277 & x < .83 & y > 1.52 & y < 2.26, '3',
  ifelse(x > -.83 & x < -.277 & y > 2.26 & y < 3, '4',
  ifelse(x > -.277 & x < .277 & y > 2.26 & y < 3, '5',
  ifelse(x > .277 & x < .83 & y > 2.26 & y < 3, '6',
  ifelse(x > -.83 & x < -.277 & y > 3 & y < 3.73, '7',
  ifelse(x > -.277 & x < .277 & y > 3 & y < 3.73, '8',
  ifelse(x > .277 & x < .83 & y > 3 & y < 3.73, '9',
  ifelse(x > -10 & x < 0 & y > 3.73 & y < 10, '11',
  ifelse(x > -10 & x < -.83 & y > 2.625 & y < 3.73, '11',
  ifelse(x > 0 & x < 10 & y > 3.73 & y < 10, '12',
  ifelse(x > .83 & x < 10 & y > 2.625 & y < 3.73, '12',
  ifelse(x > -10 & x < 0 & y > -10 & y < 1.52, '13',
  ifelse(x > -10 & x < -.83 & y > 1.52 & y < 2.625, '13',
  ifelse(x > 0 & x < 10 & y > -10 & y < 1.52, '14',
  ifelse(x > .83 & x < 10 & y > 1.52 & y < 2.625, '14','0')))))))))))))))))
}

#----Function that creates a swing rate heat map
###----There are also variables created that go into the function

##---Creates the mapping system that gives each zone a coordinate for the heat map
ZONE_XY <- data.frame(Zone = c('1', '2', '3', '4', '5', '6','7', '8', '9', '13', '11', '14', '12'),
                     Side = c(-.375, 0, .375, -.375, 0, .375, -.375, 0, .375, -.375, -.375, .375, .375),
                     Height = c(2.1, 2.1, 2.1, 2.8, 2.8, 2.8, 3.5, 3.5, 3.5, 2.1, 3.5, 2.1, 3.5))

##---Creates the outline for the heatmap in ggplot
heatmap_frame <- data.frame(x = c(rep(-.56,5),.56,-.19,.19,0,0, rep(-.75,4),.75,.75,rep(-.56,3),0,.56),
xend = c(rep(.56,4), -.56,.56,-.19,.19,0,0,-.56,.75,-.75,.75,.75,.56,.56,-.56,0,.56,.56),
y = c(1.75,3.85,2.45,3.15,rep(1.75,5),3.85,2.8,1.4,1.4,4.2,4.2,2.8,.5,.35,.35,.15,.35),
yend = c(1.75,3.85,2.45,3.15,rep(3.85,4),1.4,4.2,2.8,1.4,4.2,4.2,1.4,2.8,.5,.5,.15,.35,.5))

#----Function that creates the heat map
Swing_HeatMap <- function(DF){
  
  HMWH <- DF %>% 
    mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging','FoulBall','InPlay','BuntFoul', 'BuntSwingingStrike'))) %>%
    mutate(Zone = factor(Zone, levels = c('1', '2', '3', '4', '5', '6', '7', '8','9', '11', '12', '13', '14'))) %>% 
    group_by(PitchCall, Zone) %>% count(PitchCall, .drop = F) %>% ungroup(PitchCall) %>% 
    mutate(Total = sum(n, na.rm = T)) %>% filter(., PitchCall != 'NA') %>% mutate(Whiffs = sum(n, na.rm = T)) %>% 
    mutate(Perc = Whiffs/Total) %>% mutate(Perc = ifelse(is.na(Perc) == TRUE | Perc %in% c('NA', 'NaN', Inf), 0, as.numeric(Perc))) %>% 
    distinct(Zone,Perc) %>% left_join(., ZONE_XY, by = 'Zone')
  
  RBCLAB <- data.frame(Zone = c('13', '11', '14', '12'), LAB = c(-.55, -.55, .55, .55), LAH = c(1.55, 4, 1.55, 4))
  RCB <- HMWH %>% filter(., Zone %in% c('11', '12', '13','14')) %>% left_join(., RBCLAB, by = 'Zone')
  RCP <- HMWH %>% filter(., !(Zone %in% c('11', '12', '13','14')))
  
  L1 <- ifelse(sum(RCP$Perc, na.rm = T) + sum(RCB$Perc, na.rm = T) == 0, 0, 'CH')
  if(L1 == 0) L2 <- 1 else L2 <- NULL
  if(L1 == 0) L1 <- 0 else L1 <- NULL
  
  
  G1 <- ggplot(RCB, aes(x=Side, y = Height))+
    geom_tile(aes(fill = Perc))+
    geom_tile(data = RCP, aes(x=Side, y = Height, fill = Perc))+
    geom_text(aes(x=LAB, y = LAH, label= percent(Perc,1)), fontface = 'bold', size = 8.5)+
    geom_text(data = RCP, aes(x = Side, y = Height, label = percent(Perc,1)), fontface = 'bold', size = 8.5)+
    scale_fill_gradientn(colors = c(low = 'dodgerblue3', mid = 'white', high = 'red3'), values = NULL, limits = c(L1, L2), breaks =(0.25*0:4), labels = percent(0.25*0:4), oob = squish)+
    geom_segment(data = heatmap_frame, aes(x=x, xend = xend, y = y, yend = yend))+
    coord_cartesian(ylim = c(0,4.35), xlim = c(-1.1,1.1))+
    theme_void()+
    theme(panel.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
    axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = 'none', 
    plot.title = element_text(size = 32, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 26, hjust = 0.5, face = 'italic'))+
    ggtitle('Swing Rates By Zone')
}

#----Function that calculates the f1 score utilized so we can get the most optimal f1 score
###--tp is true predictions, fp is false predictions, fn is false negatives
calculate_f1_score <- function(tp, fp, fn) {
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    f1_score <- 2 * precision * recall / (precision + recall)
    return(f1_score)
}

#----Function that calculates the model performance scores and puts it into one dataframe
##---DF1 is the test data, DF2 is the binary predictions
model_performance <- function(DF1, DF2){
  
confusion_matrix_1 <- table(DF1$Swing, DF2)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
auc <- roc(test_data$Swing, as.numeric(predictions))$auc

df1 <- data.frame(variable = c('Accuracy', 'Precision', 'Recall', 'F1-Score', 'AUC'),
  value = c(accuracy, precision, recall, f1_score, auc)) %>% mutate(value = sprintf('%.3f', value))

}

#-----Function that creates the table to display the model performance scores

#----Themes for each of the tables
tt <- ttheme_default(core = list(bg_params=list(fill='#ffffff', col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='#b6cff0', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
#----Creating the table DF is the output from the funciton model_preformance and TITLE is the title of the table (Model Number)
eval_table <- function(DF, TITLE){
tab3 <- tableGrob(DF[1:dim(DF)[1], 1:dim(DF)[2]], rows = NULL, cols = c('Metric', 'Value'), theme = tt)
header2 <- tableGrob(DF[1,c(1:1)], rows=NULL, cols=c(TITLE), theme = tt2) 
jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(dim(DF)[2]))
g <- jn2
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = dim(DF)[2])
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = dim(DF)[2])
g$widths <- unit(c(rep(1/ncol(g), ncol(g))), 'npc')
g$heights <- unit(c(rep(1/nrow(g), nrow(g))), 'npc')
TABLE1 <- g
}


#----Function that splits up plate location to create heat map tiles for Strike Zone function
#----DF is Dataframe, NAME is Pitcher Name
Swing_Tile <- function(DF, NAME){
    
  DFS <- DF %>% filter(., PitcherName == NAME)
  
  side_breaks <- seq(min(DFS$PlateLocSide), max(DFS$PlateLocSide), by = 0.1)  # Adjust the bin width as needed
  height_breaks <- seq(min(DFS$PlateLocHeight), max(DFS$PlateLocHeight), by = 0.1)  # Adjust the bin width as needed

  
  averages <- DFS %>%
  group_by(
    tile_side = cut(PlateLocSide, breaks = side_breaks, labels = FALSE),
    tile_height = cut(PlateLocHeight, breaks = height_breaks, labels = FALSE)
  ) %>%
  summarise(
    expected_swing_avg = mean(swing_predictions),
    side_value = (max(side_breaks[tile_side]) + min(side_breaks[tile_side])) / 2,
    height_value = (max(height_breaks[tile_height]) + min(height_breaks[tile_height])) / 2
  ) %>% filter(., side_value > -1.15 & side_value < 1.15 & height_value < 4.5 & height_value > .25)
  
}

#----Second heat map to display swing probabilities
Strike_Zone <- function(DF, TITLE){
  
  SK1 <- ggplot(data = DF, aes(x = side_value, y = height_value, fill = expected_swing_avg)) +
    geom_tile() +
    scale_fill_gradientn(colors = c(low = 'dodgerblue3', mid = 'white', high = 'red3'), limits = c(.2,.8),oob = squish)+
    geom_segment(aes(x=-.95, xend = -.71, y = 3.5, yend = 3.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.71, xend = -.71, y = 3.5, yend = 3.74), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.71, xend = -.71, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.95, xend = -.71, y = 1.5, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.95, xend = -.71, y = 2.16, yend = 2.16), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.95, xend = -.71, y = 2.82, yend = 2.82), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.24, xend = -.24, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.24, xend = .24, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.71, xend = .71, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.71, xend = .71, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.71, xend = .71, y = 1.26, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.71, xend = .95, y = 1.5, yend = 1.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.95, xend = .71, y = 2.16, yend = 2.16), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.95, xend = .71, y = 2.82, yend = 2.82), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.95, xend = .71, y = 3.5, yend = 3.5), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.24, xend = -.24, y = 3.5, yend = 3.74), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.24, xend = .24, y = 3.5, yend = 3.74), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=.71, xend = .71, y = 3.5, yend = 3.74), lwd = .5, lty = 'dashed')+
    geom_segment(aes(x=-.95, xend = -.95, y = 1.26, yend = 3.74), lwd = .5)+
    geom_segment(aes(x=-.95, xend = .95, y = 3.74, yend = 3.74), lwd = .5)+
    geom_segment(aes(x=.95, xend = .95, y = 1.26, yend = 3.74), lwd = .5)+
    geom_segment(aes(x=-.95, xend = .95, y = 1.26, yend = 1.26), lwd = .5)+
    geom_segment(aes(x=-.71, xend = -.71, y = 1.5, yend = 3.5))+
    geom_segment(aes(x=.71, xend = .71, y = 1.5, yend = 3.5))+
    geom_segment(aes(x=-.71, xend = .71, y = 1.5, yend = 1.5))+
    geom_segment(aes(x=-.71, xend = .71, y = 3.5, yend = 3.5))+
    geom_segment(aes(x=-.71, xend = .71, y = 0, yend = 0))+
    geom_segment(aes(x=-.71, xend = -.71, y = 0, yend = -.15))+
    geom_segment(aes(x=.71, xend = .71, y = 0, yend = -.15))+
    geom_segment(aes(x=-.71, xend = 0, y = -.15, yend = -.35))+
    geom_segment(aes(x=.71, xend = 0, y = -.15, yend = -.35))+
    coord_cartesian(xlim = c(-1.6, 1.6), ylim = c(-.65,4.5))+
    theme_void()+
    theme(plot.title = element_text(size = 34, face = 'bold', hjust = 0.5), legend.position = 'none')+
    ggtitle(TITLE)
  
}
