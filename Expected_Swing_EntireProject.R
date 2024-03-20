library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)

#---------------------------------------------------------------------Step 1 Create Functions

#---Functions & Global Variables from Expected_Swing_Functions
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

#---------------------------------------------------------------------Step 2 Load Data

# Scrape Data using baseballr
data1 <- scrape_statcast_savant(start_date = '2023-04-01', end_date = '2023-04-07', player_type = 'pitcher')
data2 <- scrape_statcast_savant(start_date = '2023-05-01', end_date = '2023-05-04', player_type = 'pitcher')
data3 <- scrape_statcast_savant(start_date = '2023-06-01', end_date = '2023-06-04', player_type = 'pitcher')
data4 <- scrape_statcast_savant(start_date = '2023-08-01', end_date = '2023-08-04', player_type = 'pitcher')
data5 <- scrape_statcast_savant(start_date = '2023-08-10', end_date = '2023-08-14', player_type = 'pitcher')
data6 <- scrape_statcast_savant(start_date = '2023-05-10', end_date = '2023-05-14', player_type = 'pitcher')

# Combine dataframes using dplyr
data <- data1 %>% bind_rows(., data2,data3,data4,data5,data6)

# New Column Names
new_names <- c('Date' = 'game_date','RelSpeed' = 'release_speed','RelSide' = 'release_pos_x', 'RelHeight' = 'release_pos_z', 'PitcherName' = 'player_name',
               'Zone' = 'zone','BatterSide' = 'stand', 'PitcherThrows' = 'p_throws','Balls' = 'balls', 'Strikes' = 'strikes','HorzBreak' = 'pfx_x',
               'InducedVertBreak' = 'pfx_z','PlateLocSide' = 'plate_x', 'PlateLocHeight' = 'plate_z', 'On3B' = 'on_3b', 'On2B' = 'on_2b', 'On1B' = 'on_1b',
               'Outs' = 'outs_when_up', 'Inning' = 'inning', 'Top.Bottom' = 'inning_topbot','ExitSpeed' = 'launch_speed','Angle' = 'launch_angle',
               'SpinRate' = 'release_spin_rate','Extension' = 'release_extension','PitchofPA' = 'pitch_number','TaggedPitchType' = 'pitch_name',
               'SpinAxis' = 'spin_axis')

# New Pitch Call Names
Pitch_Calls <- c('swinging_strike' = 'StrikeSwinging', 'foul' = 'FoulBall', 'ball' = 'BallCalled', 'called_strike' = 'StrikeCalled',
  'hit_into_play' = 'InPlay', 'blocked_ball' = 'BallCalled', 'foul_tip' = 'FoulBall', 'swinging_strike_blocked' = 'StrikeSwinging',
  'hit_by_pitch' = 'HitByPitch', 'foul_bunt' = 'BuntFoul', 'missed_bunt' = 'BuntSwingingStrike', 'bunt_foul_tip' = 'BuntFoul')

# New Pitch Type Names
Pitch_Types <- c('4-Seam Fastball' = 'Fastball', 'Split-Finger' = 'Splitter')

# New Play Result Names
Play_Results <- c('Field_out' = 'Out', 'Grounded_into_double_play' = 'Out', 'Force_out' = 'Out','Home_run' = 'Homerun',
  'Hit_by_pitch' = 'HitByPitch', 'Strikeout_double_play' = 'Strikeout','Field_error' = 'Error', 'Sac_fly' = 'Sacrifice',
  'Sac_bunt' = 'Sacrifice', 'Fielders_choice' = 'FieldersChoice', 'Sac_fly_double_play' = 'Sacrifice',
  'Double_play' = 'Out', 'Fielders_choice_out' = 'FieldersChoice', 'Other_out' = 'Out')


DF <- data %>% rename(any_of(new_names)) %>% mutate(Count = paste0(Balls, '-', Strikes)) %>% 
  mutate(OutsOnPlay = case_when(events %in% c('strikeout', 'field_out', 'force_out', 'caught_stealing_home', 'sac_fly', 'caught_stealing_2b', 'sac_bunt', 'fielders_choice',
  'pickoff_1b', 'fielders_choice_out', 'other_out', 'pickoff_caught_stealing_2b', 'pickoff_3b', 'caught_stealing_3b')~1,
  events %in% c('double_play', 'sac_fly_double_play', 'strikeout_double_play', 'grounded_into_double_play')~2, T~0)) %>% 
  mutate(PitcherThrows = recode(PitcherThrows, 'L' = 'Left', 'R' = 'Right'), BatterSide = recode(BatterSide, 'L' = 'Left', 'R' = 'Right'),
  TaggedPitchType = recode(TaggedPitchType, !!!Pitch_Types),PitchCall = recode(description, !!!Pitch_Calls),PlayResult = str_to_title(events), 
  PlayResult = recode(PlayResult, !!!Play_Results)) %>% 
  filter(., !(TaggedPitchType %in% c('', 'Other', 'Screwball', 'Pitch Out')) & PitchCall != 'pitchout' & RelSpeed > 0) %>% 
  mutate(ScoreDiff = case_when(Top.Bottom == 'Top'~away_score-home_score, T~home_score-away_score)) %>% 
  mutate(Swing = case_when(PitchCall %in% c('BuntFoul', 'BuntSwingingStrike', 'StrikeSwinging', 'FoulBall', 'InPlay')~'Swing', T~'Take')) %>% 
  mutate(ManOn1 = case_when(is.na(On1B) == TRUE~'No', T~'Yes'), ManOn2 = case_when(is.na(On2B) == TRUE~'No', T~'Yes'),
  ManOn3 = case_when(is.na(On3B) == TRUE~'No', T~'Yes')) %>% 
  mutate(Zone = ZONECA(PlateLocSide, PlateLocHeight))

#----For the Individual Pitchers I went to BaseballSavant and manually download the data on 
#----Cole, Cease and Kremer then saved the file to my computer and read it into R
individual_data <- read.csv('IndividualPitchersData.csv')

Individ <- individual_data %>% rename(any_of(new_names)) %>% mutate(Count = paste0(Balls, '-', Strikes)) %>% 
  mutate(OutsOnPlay = case_when(events %in% c('strikeout', 'field_out', 'force_out', 'caught_stealing_home', 'sac_fly', 'caught_stealing_2b', 'sac_bunt', 'fielders_choice',
  'pickoff_1b', 'fielders_choice_out', 'other_out', 'pickoff_caught_stealing_2b', 'pickoff_3b', 'caught_stealing_3b')~1,
  events %in% c('double_play', 'sac_fly_double_play', 'strikeout_double_play', 'grounded_into_double_play')~2, T~0)) %>% 
  mutate(PitcherThrows = recode(PitcherThrows, 'L' = 'Left', 'R' = 'Right'), BatterSide = recode(BatterSide, 'L' = 'Left', 'R' = 'Right'),
  TaggedPitchType = recode(TaggedPitchType, !!!Pitch_Types),PitchCall = recode(description, !!!Pitch_Calls),PlayResult = str_to_title(events), 
  PlayResult = recode(PlayResult, !!!Play_Results)) %>% 
  filter(., !(TaggedPitchType %in% c('', 'Other', 'Screwball', 'Pitch Out')) & PitchCall != 'pitchout' & RelSpeed > 0) %>% 
  mutate(ScoreDiff = case_when(Top.Bottom == 'Top'~away_score-home_score, T~home_score-away_score)) %>% 
  mutate(Swing = case_when(PitchCall %in% c('BuntFoul', 'BuntSwingingStrike', 'StrikeSwinging', 'FoulBall', 'InPlay')~'Swing', T~'Take')) %>% 
  mutate(ManOn1 = case_when(is.na(On1B) == TRUE~'No', T~'Yes'), ManOn2 = case_when(is.na(On2B) == TRUE~'No', T~'Yes'),
  ManOn3 = case_when(is.na(On3B) == TRUE~'No', T~'Yes')) %>% 
  mutate(Zone = ZONECA(PlateLocSide, PlateLocHeight))

#---------------------------------------------------------------------Step 3 Visualize Data with Bar Plots
#---Count of pitcher handedness
A1 <- DF %>% group_by(PitcherThrows) %>% count(PitcherThrows) %>% ungroup() %>% mutate(Percent = (n/sum(n, na.rm = T)))
#---Count of batter handedness
A2 <- DF %>% group_by(BatterSide) %>% count(BatterSide) %>% ungroup() %>% mutate(Percent = (n/sum(n, na.rm = T)))
#---Count of pitcher-batter matchups based on handedness
A3 <- DF %>% mutate(Matchup = paste0(PitcherThrows, '-', BatterSide)) %>% group_by(Matchup) %>% count(Matchup) %>% 
   ungroup() %>% mutate(Percent = (n/sum(n, na.rm = T)))
#---Count of pitch calls
A4 <- DF %>% group_by(PitchCall) %>% count(PitchCall) %>% ungroup() %>% mutate(Percent = (n/sum(n, na.rm = T)))
#---Calculation of swing rates by PITCHER HANDEDNESS
A5 <- DF %>% group_by(PitcherThrows, Swing) %>% count(Swing) %>% reshape2::dcast(., PitcherThrows~Swing, value.var = 'n') %>% ungroup() %>% 
  add_row(., PitcherThrows = 'Both', Swing = sum(.$Swing), Take = sum(.$Take)) %>% mutate(SwingRate = Swing/(Swing+Take))
#---Calculation of swing rates by OUTS
A6 <- DF %>% group_by(Swing, Outs) %>% count(Swing) %>% ungroup(Swing) %>% mutate(Percent = (n/sum(n, na.rm = T))) %>% filter(., Swing == 'Swing')
#---Calculation of swings rates by COUNT
A7 <- DF %>% group_by(Count, Swing) %>% count(Swing) %>% ungroup(Swing) %>% mutate(SwingRate = n/sum(n)) %>% filter(., Swing == 'Swing')

#---Creation of theme used in graphs 1-7
theme_1 <- theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),axis.title = element_text(size = 28), axis.text = element_text(size = 26),
           legend.position = 'none')

#---Count of PITCHER HANDEDNESS Bar Plot
G1 <- ggplot(A1, aes(x = PitcherThrows, y = Percent, fill = PitcherThrows))+
  geom_bar(stat = 'identity', width = .75, color= 'black')+
  scale_fill_manual(values = c('Left' = '#c26d6d', 'Right' = '#6d78c2'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  xlab('\nPitcher Handedness')+
  ylab('Frequency\n')+
  theme_bw()+
  theme_1+
  ggtitle('Pitcher Handedness in Data\n')

#---Count of BATTER HANDEDNESS Bar Plot
G2 <- ggplot(A2, aes(x = BatterSide, y = Percent, fill = BatterSide))+
  geom_bar(stat = 'identity', width = .75, color= 'black')+
  scale_fill_manual(values = c('Left' = '#c26d6d', 'Right' = '#6d78c2'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  xlab('\nBatter Handedness')+
  ylab('Frequency\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Batter Handedness in Data\n')

#---Count of PITCHER-BATTER HANDEDNESS MATCHUP Bar Plot
G3 <- ggplot(A3, aes(x = Matchup, y = Percent, fill = Matchup))+
  geom_bar(stat = 'identity', width = .75, color= 'black')+
  scale_fill_manual(values = c('Left-Left' = '#c26d6d', 'Right-Right' = '#6d78c2', 'Left-Right' = '#c29d6d', 'Right-Left' = '#a66dc2'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  scale_x_discrete(labels = c('Left-Right' = 'LHP-RHH', 'Right-Right' = 'RHP-RHH', 'Left-Left' = 'LHP-LHH', 'Right-Left' = 'RHP-LHH'))+
  xlab('\nMatchup')+
  ylab('Frequency\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Pitcher-Batter Matchups in Data\n')

#---Count of PITCH CALLS Bar Plot
G4 <- ggplot(A4, aes(x = PitchCall, y = Percent))+
  geom_bar(stat = 'identity', width = .75, color= 'black', fill = '#dae9f2')+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  scale_x_discrete(labels = c('BallCalled' = 'Ball', 'BuntFoul' = 'Bunt Foul', 'BuntSwingingStrike' = 'Bunt Whiff', 'FoulBall' = 'Foul',
                              'HitByPitch' = 'Hit By Pitch', 'InPlay' = 'Ball in Play', 'StrikeCalled' = 'Call Strike', 'StrikeSwinging' = 'Whiff'))+
  xlab('\nPitch Call')+
  ylab('Frequency\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Pitch Calls in Data\n')

#---SWING RATES BY PITCHER HANDEDNESS Bar Plot
G5 <- ggplot(A5, aes(x = PitcherThrows, y = SwingRate))+
  geom_bar(stat = 'identity', width = .75, color= 'black', fill = '#dae9f2')+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  scale_x_discrete(labels = c('Left' = 'LHP', 'Right' = 'RHP', 'Both' = 'ALL'))+
  xlab('\nPitcher Handedness')+
  ylab('Swing %\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Swing Rates in Data\n')

#---SWING RATES BY OUTS Bar Plot
G6 <- ggplot(A6, aes(x = as.factor(Outs), y = Percent))+
  geom_bar(stat = 'identity', width = .75, color= 'black', fill = '#dae9f2')+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  xlab('\nNumber of Outs')+
  ylab('Swing %\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Swing Rates by Outs in Data\n')

#---SWING RATES BY COUNT Bar Plot
G7 <- ggplot(A7, aes(x = Count, y = SwingRate))+
  geom_bar(stat = 'identity', width = .75, color= 'black', fill = '#dae9f2')+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.25), labels = scales::percent)+
  xlab('\nPitch Count')+
  ylab('Swing %\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 28), axis.text = element_text(size = 26),
        legend.position = 'none')+
  ggtitle('Swing Rates by Count in Data\n')

#---Swing Rate Heat Map utilizing function from Expected_Swing_Functions
G8 <- Swing_HeatMap(DF)

#---------------------------------------------------------------------Step 4 Model Creation
#---Model 1-----------
model_data <- DF %>% select(PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,Balls,Strikes,Outs,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))

set.seed(123)

train_index_1 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_1 <- model_data[train_index_1, ]
test_data_1 <- model_data[-train_index_1, ]

model_1 <- glm(Swing ~ ., data = train_data_1, family = 'binomial')

#---Model 1 Predictions
predictions_1 <- predict(model_1, newdata = test_data_1, type = "response")
binary_predictions_1 <- ifelse(predictions_1 > 0.5, 1, 0)

#---Optimal Threshold Calculation for Model  
f1_scores <- data.frame(threshold = numeric(length = 101), f1 = numeric(length = 101))
  
# Calculate F1 scores for each threshold
for (i in 1:101) {
    threshold <- (i - 1) / 100
    binary_predictions <- ifelse(predictions > threshold, 1, 0)
    confusion_matrix <- table(test_data$Swing, binary_predictions)
    
    # Ensure that confusion matrix has all needed values
    if (ncol(confusion_matrix) < 2 || nrow(confusion_matrix) < 2) {
      # Not enough values in the confusion matrix, skip
      next
    }
    
    tp <- confusion_matrix[2, 2]
    fp <- confusion_matrix[1, 2]
    fn <- confusion_matrix[2, 1]
    
    f1_scores[i, "threshold"] <- threshold
    f1_scores[i, "f1"] <- calculate_f1_score(tp, fp, fn)
  }
  
# Find the threshold that maximizes the F1 score
optimal_threshold <- f1_scores$threshold[which.max(f1_scores$f1)]

# Threshold Determination Attempt 2
thresholds <- seq(0, 1, by = 0.01)
metrics <- data.frame(threshold = thresholds, sensitivity = numeric(length(thresholds)), specificity = numeric(length(thresholds)), youden_j = numeric(length(thresholds)))
  
# Calculate sensitivity, specificity, and Youden's J for each threshold
for (i in 1:length(thresholds)) {
    threshold <- thresholds[i]
    binary_predictions <- ifelse(predictions > threshold, 1, 0)
    confusion_matrix <- table(test_data$Swing, binary_predictions)
    # Ensure that confusion matrix has all needed values
    if (ncol(confusion_matrix) < 2 || nrow(confusion_matrix) < 2) {
      # Not enough values in the confusion matrix, skip
      next
    }
    # Calculate sensitivity and specificity
    sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
    
    # Calculate Youden's J statistic
    youden_j <- sensitivity + specificity - 1
    
    # Store metrics in the data frame
    metrics[i, c("sensitivity", "specificity", "youden_j")] <- c(sensitivity, specificity, youden_j)
  }
  
# Find the threshold that maximizes Youden's J statistic
optimal_threshold <- metrics$threshold[which.max(metrics$youden_j)]

#---Model 1 Predictions with new threshold of .44
predictions_1 <- predict(model_1, newdata = test_data_1, type = "response")
binary_predictions_1 <- ifelse(predictions_1 > 0.44, 1, 0)

#----Calculation the performance for model 1
###---test_data_1 & binary_predictions_1 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_1 <- model_performance(test_data_1,binary_predictions_1)

#----Creation of the model 1 evaluation table from Expected_Swing_Functions
Table_1 <- eval_table(Eval_1, 'Model 1 Eval.')

# Correlation Testing
cor_matrix <- cor(model_data)
vif_model <- lm(Swing~., data = model_data)
vif_results <- vif(vif_model)
eigen_values <- eigen(cor_matrix)$values
tolerance <- 1 / vif_results
# Correlation Plot - Used in Correlation Heat Map in Expected_Swing_Graphs_Heatmaps
cor_melted <- melt(cor_matrix)

#---Correlation Plot of Variables in Model
#---cor_melted comes from Expected_Swing_Models file
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c(low = 'dodgerblue3', mid = 'white', high = 'red3')) +
  labs(x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---Model 2-----------(Takes out Induced Vertical Break and Outs as Predictors)
model_data <- DF %>% select(PlateLocHeight,PlateLocSide,RelSpeed,HorzBreak,Balls,Strikes,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))

train_index_2 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_2 <- model_data[train_index_2, ]
test_data_2 <- model_data[-train_index_2, ]

model_2 <- glm(Swing ~ ., data = train_data_2, family = 'binomial')
#---Model 2 Predictions
predictions_2 <- predict(model_2, newdata = test_data_2, type = "response")
binary_predictions_2 <- ifelse(predictions_2 > 0.44, 1, 0)

#----Calculation the performance for model 2
###---test_data_2 & binary_predictions_2 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_2 <- model_performance(test_data_2,binary_predictions_2)

#----Creation of the model 2 evaluation table from Expected_Swing_Functions
Table_2 <- eval_table(Eval_2, 'Model 2 Eval.')

#---Model 3-----------
###-REMATRIX is the run expectancy matrix used in the 3rd model
REMATRIX <- data.frame(Runners = c('No_No_No','Yes_No_No','No_Yes_No','Yes_Yes_No','No_No_Yes','Yes_No_Yes','No_Yes_Yes','Yes_Yes_Yes' ),
  Zero = c(.461,.831,1.068,1.373,1.426,1.798,1.920,2.282), 
  One = c(.243,.489,.644,.908,.865,1.140,1.352,1.520), 
  Two = c(.095,.214,.305,.343,.413,.471,.570,.736)) %>% reshape2::melt(., id.vars = c('Runners')) %>% 
  mutate(variable = recode(variable, 'Zero' = 0, 'One' = 1, 'Two' = 2)) %>% 
  mutate(Situation = paste0(Runners,'_', variable)) %>% select(Situation, RE = value)

###-Creates the situations so we can combine the REMATRIX with the main dataframe
model_data <- DF %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0)) %>%
  mutate(Situation = paste0(ManOn1, '_', ManOn2, '_', ManOn3, '_', Outs)) %>% left_join(., REMATRIX, by = c('Situation')) %>% 
  select(PlateLocHeight,PlateLocSide,RelSpeed,HorzBreak,Balls,Strikes,RE,Swing)

train_index_3 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_3 <- model_data[train_index_3, ]
test_data_3 <- model_data[-train_index_3, ]

model_3 <- glm(Swing ~ ., data = train_data_3, family = 'binomial')
#---Model 3 Predictions
predictions_3 <- predict(model_3, newdata = test_data_3, type = "response")
binary_predictions_3 <- ifelse(predictions_3 > 0.44, 1, 0)

#----Calculation the performance for model 3
###---test_data_3 & binary_predictions_3 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_3 <- model_performance(test_data_3,binary_predictions_3)

#----Creation of the model 3 evaluation table from Expected_Swing_Functions
Table_3 <- eval_table(Eval_3, 'Model 3 Eval.')

#---Individual Pitchers Swing Predictions
###--Individ is the individual pitchers data (Gerrit Cole, Dylan Cease and Dean Kremer)
Individ_Data <- Individ %>% select(PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,Balls,Strikes,Outs,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))
predictions_I <- predict(model_1, newdata = Individ_Data, type = "response")
binary_predictions_I <- ifelse(predictions_I > 0.44, 1, 0)

#---Individual Data 
###-predictions_I & binary_predictions_I are from Expected Swing Models (Predictions for Individual Pitchers)
Individ <- Individ %>% mutate(swing_predictions = predictions_I, swing_binary = binary_predictions_I)

#---Calculations of the individual pitchers predictions (Overall)
B1 <- Individ %>% group_by(PitcherName, swing_binary) %>% count(swing_binary) %>% ungroup(swing_binary) %>% 
  mutate(Total = sum(n,na.rm = T)) %>% filter(., swing_binary == 1) %>% mutate(ExpectedSwingRate_Binary = n/Total) %>% 
  select(PitcherName, ExpectedSwingRate_Binary)
B2 <- Individ %>% group_by(PitcherName) %>% summarize(SwingProbability = mean(swing_predictions, na.rm = T))
B3 <- Individ %>% group_by(PitcherName, PitchCall) %>% count(PitchCall) %>% ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% 
  filter(., PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBall')) %>% mutate(Swings = sum(n,na.rm = T)) %>% 
  mutate(ActualSwingRate = Swings/Total) %>% distinct(PitcherName, ActualSwingRate)

B4 <- Reduce(function(x,y) merge(x,y, by.all = c('PitcherName')), list(B1,B2,B3)) %>% 
  mutate_at(vars(ExpectedSwingRate_Binary:ActualSwingRate), ~paste0(sprintf('%.1f', (.*100)), '%'))

#---Calculations of the individual pitchers predictions (By Pitch Type)
C1 <- Individ %>% group_by(PitcherName, TaggedPitchType, swing_binary) %>% count(swing_binary) %>% ungroup(swing_binary) %>% 
  mutate(Total = sum(n,na.rm = T)) %>% filter(., swing_binary == 1) %>% mutate(ExpectedSwingRate_Binary = n/Total) %>% 
  distinct(PitcherName, TaggedPitchType, ExpectedSwingRate_Binary)
C2 <- Individ %>% group_by(PitcherName, TaggedPitchType) %>% summarize(SwingProbability = mean(swing_predictions, na.rm = T)) %>% 
  distinct(PitcherName, TaggedPitchType, SwingProbability)
C3 <- Individ %>% group_by(PitcherName, TaggedPitchType, PitchCall) %>% count(PitchCall) %>% ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% 
  filter(., PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBall')) %>% mutate(Swings = sum(n,na.rm = T)) %>% 
  distinct(PitcherName, Total, Swings) %>% mutate(ActualSwingRate = Swings/Total) %>% distinct(PitcherName, TaggedPitchType, ActualSwingRate)

C4 <- Reduce(function(x,y) merge(x,y, by.all = c('PitcherName', 'TaggedPitchType')), list(C1,C2,C3)) %>% 
  mutate_at(vars(ExpectedSwingRate_Binary:ActualSwingRate), ~paste0(sprintf('%.1f', (.*100)), '%'))

#----Creation of Individual Pitcher Predictions Table
df2 <- C4 %>% distinct(PitcherName)
df1 <- C4
colours <- df1 %>% mutate(ID = row_number()) %>% mutate_at(vars(PitcherName:ActualSwingRate), ~case_when(ID%%2 == 0~'#e4ebf5', T~'#ffffff')) %>% 
  select(-c(ID)) %>% as.matrix()
pcol <- df2 %>% mutate(ID = row_number()) %>% mutate_at(vars(PitcherName), ~case_when(ID%%2 == 0~'#e4ebf5', T~'#ffffff')) %>%
  as.matrix()

merge_X <- df1 %>% mutate(ID = row_number()) %>% group_by(PitcherName) %>% filter(., ID == max(ID)) %>% ungroup() %>% 
  mutate(ID = ID+1) %>% pull(ID)
merge_Y <- df1 %>% mutate(ID = row_number()) %>% group_by(PitcherName) %>% filter(., ID == min(ID)) %>% ungroup() %>% 
  mutate(ID = ID+1) %>% pull(ID)

P1n <- dim(df1)[1]
P2n <- dim(df2)[1]
tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 42, col = 'black')))
tt3 <- ttheme_default(core = list(bg_params=list(fill=pcol, col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 'bold')), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tab2 <- tableGrob(df2[1:P2n, 1:1], rows = NULL,cols = c('Pitcher'), theme = tt3)
tab3 <- tableGrob(df1[1:P1n, 1:5], rows = NULL, cols = c('no', 'Pitch', 'xSwing Binary', 'xSwing Prob.', 'Actual Swing'), theme = tt)
halign_alt <- gtable_combine(tab2,tab3, along =1)
halign_alt$layout[halign_alt$layout$t != 1 & halign_alt$layout$l == 1, c("t")] <- c(merge_X)
halign_alt$layout[halign_alt$layout$b != 1  & halign_alt$layout$l == 1, c("b")] <- c(merge_Y)
header2 <- tableGrob(df2[1,], rows=NULL, cols=c('Expected Swing Rates'), theme = tt2) 
jn2 <- gtable_combine(header2[1,], halign_alt, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(5))
g <- jn2
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd = 7)),t = 1, b = nrow(g), l = 1, r = 5)
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = 5)
g$widths <- unit(c(rep(.2,5)), 'npc')
g$heights <- unit(c(.07, .07, rep(.86/(nrow(g)-2), nrow(g)-2)), 'npc')
TABLE4 <- g

#---Expected Swing Rates Heat Maps utilizing function from Expected_Swing_Functions
###-Individ IS INDIVIDUAL PLAYER DATA with the predictions from the Expected_Swing_Models
Individ <- Individ %>% mutate(swing_predictions = predictions_I, swing_binary = binary_predictions_I)
TILE_GC <- Swing_Tile(Individ, 'Cole, Gerrit')
SK1 <- Strike_Zone(TILE_GC, 'Gerrit Cole Swing Probability')

TILE_DC <- Swing_Tile(Individ, 'Cease, Dylan')
SK2 <- Strike_Zone(TILE_DC, 'Dylan Cease Swing Probability')

TILE_DK <- Swing_Tile(Individ, 'Kremer, Dean')
SK3 <- Strike_Zone(TILE_DK, 'Dean Kremer Swing Probability')

