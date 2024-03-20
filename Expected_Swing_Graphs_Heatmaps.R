library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)



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

#---Correlation Plot of Variables in Model
#---cor_melted comes from Expected_Swing_Models file
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c(low = 'dodgerblue3', mid = 'white', high = 'red3')) +
  labs(x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---Expected Swing Rates Heat Maps utilizing function from Expected_Swing_Functions
###-Individ IS INDIVIDUAL PLAYER DATA with the predictions from the Expected_Swing_Models
Individ <- Individ %>% mutate(swing_predictions = predictions_I, swing_binary = binary_predictions_I)
TILE_GC <- Swing_Tile(Individ, 'Cole, Gerrit')
SK1 <- Strike_Zone(TILE_GC, 'Gerrit Cole Swing Probability')

TILE_DC <- Swing_Tile(Individ, 'Cease, Dylan')
SK2 <- Strike_Zone(TILE_DC, 'Dylan Cease Swing Probability')

TILE_DK <- Swing_Tile(Individ, 'Kremer, Dean')
SK3 <- Strike_Zone(TILE_DK, 'Dean Kremer Swing Probability')


