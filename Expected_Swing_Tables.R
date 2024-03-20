library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)


#----Calculation the performance for model 1
###---test_data_1 & binary_predictions_1 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_1 <- model_performance(test_data_1,binary_predictions_1)

#----Creation of the model 1 evaluation table from Expected_Swing_Functions
Table_1 <- eval_table(Eval_1, 'Model 1 Eval.')

#----Calculation the performance for model 2
###---test_data_2 & binary_predictions_2 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_2 <- model_performance(test_data_2,binary_predictions_2)

#----Creation of the model 2 evaluation table from Expected_Swing_Functions
Table_2 <- eval_table(Eval_2, 'Model 2 Eval.')

#----Calculation the performance for model 3
###---test_data_3 & binary_predictions_3 from Expected_Swing_Models 
###---model_performance from Expected_Swing_Functions
Eval_3 <- model_performance(test_data_3,binary_predictions_3)

#----Creation of the model 3 evaluation table from Expected_Swing_Functions
Table_3 <- eval_table(Eval_3, 'Model 3 Eval.')

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



