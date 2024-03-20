library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)



# Scrape Data using baseballr
data1 <- scrape_statcast_savant(start_date = '2023-04-01', end_date = '2023-04-07', player_type = 'pitcher')
data2 <- scrape_statcast_savant(start_date = '2023-05-01', end_date = '2023-05-04', player_type = 'pitcher')
data3 <- scrape_statcast_savant(start_date = '2023-06-01', end_date = '2023-06-04', player_type = 'pitcher')
data4 <- scrape_statcast_savant(start_date = '2023-08-01', end_date = '2023-08-04', player_type = 'pitcher')
data5 <- scrape_statcast_savant(start_date = '2023-08-10', end_date = '2023-08-14', player_type = 'pitcher')
data6 <- scrape_statcast_savant(start_date = '2023-05-10', end_date = '2023-05-14', player_type = 'pitcher')

# Combine dataframes using dplyr
data <- data1 %>% bind_rows(., data2,data3,data4,data5,data6)



# 2. Data Preprocessing:
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