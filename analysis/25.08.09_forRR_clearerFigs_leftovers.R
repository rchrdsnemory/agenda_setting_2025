#################################################################################################
########################           Original Figures, Pre- R & R            ######################
#################################################################################################



######## 
### What do people think the most common patterns are?
ASP_Exp1_PlotA<-ASP_Exp1%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.  
  mutate(all_or_none = ifelse(Values == "YYYYY" | Values == "YBBBB", as.character(Values),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")),
         # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
         Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
         Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count),
                # label=paste0(after_stat(count),"\n(", scales::percent(after_stat(count)/120, accuracy=1),")"),
                y=after_stat(3.0)),
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=120", "Pop\n n=118"), x = c(3, 3), y = c(90,90),
                         PowerLevel   = c("Anonymous", "Popular"),
                         Measure = c("PublicVote", "PrivateBelief")),
             aes(x = x, y = y, label = label), fill="white")+
  scale_y_continuous("count", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  # scale_y_continuous("count ", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  labs(x="inferred judgment for each speaker")+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  theme_cleanPub+
  scale_x_discrete(drop=F)+
  facet_grid(PowerLevel~"Measure")+
  labs(tag = "2a")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none")+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")

## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotB<-ASP_Exp1_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")))%>%
  ggplot(aes(x=as.numeric(SpkrID)+1, y=Values, color=Measure))+
  geom_beeswarm(size=3, cex=.75, alpha=.5)+ # dodge.width=1, 
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=interaction(Measure,  SpkrID)), 
               position=position_dodge(width=1), size=1.15, width=.30, color="black")+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(..y.., 2),  y=Values, group=interaction(Measure, SpkrID), fill=Measure), 
               position=position_dodge(width=1), color="black", size=4.5)+
  scale_y_continuous("Inferred Propensity to Endorse Yellow", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_x_continuous("Speaker Number", breaks = c(2,3,4,5), labels = c('Spkr2', 'Spkr3', 'Spkr4', 'Spkr5'))+
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~all_or_none, scales = "free_x",
             labeller = labeller(all_or_none = as_labeller(c('YBBBB' = "Public Votes: YBBBB",
                                                             'Mixed' = "Public Votes: Mixed",
                                                             'YYYYY' = "Public Votes: YYYYY"))))+
  theme_cleanPub+
  labs(tag = "2b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")



######### team choice & accuracy as a function of predicted flips
ASP_Exp1_PlotC<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("best design according to participant",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue\nBetter (1)",  "Uncertain\n(10.5)", "Yellow\nBetter (20)")
  )+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"trust majority")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=90, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        # legend.position = "bottom",
        legend.position = "inside",
        legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "vote-belief gaps versus participant judgment,
          <br>depending on predicted final team choice")



#### Patchwork Plot
(ASP_Exp1_PlotA+ASP_Exp1_PlotB+ASP_Exp1_PlotC)+
  plot_layout(ncol=3, widths = c(1, 3, 1),
              guides = 'collect')+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Target Speaks First')


#################################################################################################
####################        Trying to Clarify Figures Post- R & R          ######################
#################################################################################################


str(ASP_Exp1_counts)

######### COMBINE PLOT C WITH NEW PLOT: team choice & accuracy as a function of predicted flips
ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("best design according to participant",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue\nBetter (1)",  "Uncertain\n(10.5)", "Yellow\nBetter (20)"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"trust majority")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=90, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        # legend.position = "bottom",
        legend.position = "inside",
        legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "vote-belief gaps versus participant judgment,
          <br>depending on predicted final team choice")







# 1st Attempt at figure to pair with Fig 2c 
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, c("rating_avgVote", "rating_avgBelief"))%>%
  pivot_longer(cols = c("rating_avgVote", "rating_avgBelief"), names_to = "spkr_ratingType", values_to = "spkr_rating")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  ggplot(aes(x=spkr_rating, y=participant_rating, color=spkr_ratingType))+
  geom_jitter(size=4, alpha=.5, width=.15, height=.15)+ # aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5)
  # scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("mean rating across Spkrs2-5 (3rd person inference)",breaks = c(1, 10.5, 20), limits = c(0,21),
                     labels =  c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  scale_y_continuous("participant rating (1st person inference)", breaks = c(1,10.5, 20),limits = c(0,21),
                     labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~participant_ratingType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: relationship between 3rd ~ 1st person inferences in raw data",
          subtitle = "Right facet - participants expect team to follow majority vote (especially if they expect private beliefs to shift too), BUT (Left facet) participants themselves think Blue is better
          <br> NOTE: probably not the right plot for us (?) we want to highlight public vs. private divergence, but also its relationship to 1st person accuracy judgments
          <br> and this plot hides the 'no participants trust the team's votes, even though they think the team will follow majority rule' finding")
  
  
# 2nd Attempt at figure to pair with Fig 2c 
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, c("rating_avgVote", "rating_avgBelief"))%>%
  pivot_longer(cols = c("rating_avgVote", "rating_avgBelief"), names_to = "spkr_ratingType", values_to = "spkr_rating")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  ggplot(aes(x=spkr_rating, y=participant_rating, color=participant_ratingType))+
  geom_jitter(size=4, alpha=.5, width=.25, height=.25)+ # aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5)
  # scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("mean rating across Spkrs2-5 (3rd person inference)",breaks = c(1, 10.5, 20), limits = c(0,21),
                     labels =  c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  scale_y_continuous("participant rating (1st person inference)", breaks = c(1,10.5, 20),limits = c(0,21),
                     labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~spkr_ratingType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: relationship between 3rd ~ 1st person inferences in raw data",
          subtitle = "this plot highlights that TeamChoice vs Accurate Choice are **different** (team choice follows votes & beliefs)
          <br> BUT, that seems to overshadow accuracy always being 'blue', and you can't see the vote~belief divergence at all")

str(ASP_Exp1_counts)


# 3rd Attempt at figure to pair with Fig 2c 
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  # filter(if_any(everything(), is.na))%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+  
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("3rd person: total flips to Yellow, Spkr2-5", # breaks = c(0:4), limits = c(-.5,4.5)
                     )+
  scale_y_continuous("1st person: ppropensity to endorse Yellow ", # breaks = c(1,10.5, 20),limits = c(-1,21),
                     # labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)")
                     )+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~participant_ratingType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: relationship between 3rd ~ 1st person inferences in raw data",
          subtitle = "more 3rd person flips to yellow ~ higher 1st person propensity to endorse Yellow
          <br> BUT, only for TeamChoice, not for participants themselves")


# 4th Attempt at figure to pair with Fig 2c 
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  # filter(if_any(everything(), is.na))%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=participant_ratingType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+ 
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("3rd person: total flips to Yellow, Spkr2-5", # breaks = c(0:4), limits = c(-.5,4.5)
  )+
  scale_y_continuous("1st person: propensity to endorse Yellow ", # breaks = c(1,10.5, 20),limits = c(-1,21),
                     # labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)")
  )+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("red3", "cyan3"))+
  scale_color_manual(values = c("red3", "cyan3"))+
  facet_grid(PowerLevel~spkr_JudgeType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: relationship between 3rd ~ 1st person inferences in raw data",
        subtitle = "this plot feels close: it shows that team choice follows judgments everywhere, but accurate choice doesn't,
        <br> AND it shows that private beliefs don't flip - only votes, and only for the popular condition.
        <br> BUT, plot still not right")



# 5th Attempt at figure to pair with Fig 2c 
str(ASP_Exp1_counts)
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
  pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, rating_avgVote, rating_avgBelief),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  ggplot(aes(x=rating_avgBelief, y=rating_avgVote))+
  geom_jitter(size=4, alpha=.5, width=.25, height=.25, aes(color=participant_rating))+ # aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5)
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_smooth(method="lm", aes(group=participant_ratingType))+ # aes(color=decision_BvY, group=decision_BvY)
  scale_x_continuous("speaker beliefs (average of Spkrs2-5)",breaks = c(1, 10.5, 20), limits = c(0,21),
                     labels =  c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  scale_y_continuous("speaker votes (average of Spkrs2-5)", breaks = c(1,10.5, 20),limits = c(0,21),
                     labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  paletteer::scale_color_paletteer_c("pals::kovesi.diverging_linear_bjy_30_90_c45")+
  facet_grid(PowerLevel~participant_ratingType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: speaker beliefs ~ votes versus participant beliefs",
          subtitle = "this plot's probably not what we want(?) - nor does plotting YvB counts in place of averages help
          <br> we want to highlight that participants predict a *divergence* between speakers' votes ~ beliefs,
          <br> in order to show how each relates to accuracy vs team choice, but this highlights the vote ~ belief correlation")



# 6th Attempt at figure to pair with Fig 2c 
ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, c("rating_avgVote", "rating_avgBelief"))%>%
  pivot_longer(cols = c("rating_avgVote", "rating_avgBelief"), names_to = "spkr_ratingType", values_to = "spkr_rating")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice")),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  ggplot(aes(x=rating_TeamChoice, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.5, height=.5, aes(color=spkr_rating))+ # aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5)
  # scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("What will the team choose? (3rd person inference)",breaks = c(1, 10.5, 20), limits = c(0,21),
                     labels =  c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  scale_y_continuous("What's actually best? (3rd person inference)", breaks = c(1,10.5, 20),limits = c(0,21),
                     labels = c("Blue<br>Better (1)",  "Uncertain<br>(10.5)", "Yellow<br>Better (20)"))+
  geom_smooth(method="lm", aes(group=spkr_ratingType))+ # aes(color=decision_BvY, group=decision_BvY)
  paletteer::scale_color_paletteer_c("pals::kovesi.diverging_linear_bjy_30_90_c45")+
  facet_grid(PowerLevel~spkr_ratingType)+
  theme_cleanPub+
  theme(axis.text.x = element_markdown(size=10.5),
        axis.text.y = element_markdown(size=10.5))+
  ggtitle("Exp 1: relationship between 3rd ~ 1st person inferences in raw data",
          subtitle = "")



# ASP_Exp1%>%
#   mutate(avg_PrivateBelief = mean(c_across(Think_Spkr2:Think_Spkr5)),
#          avg_PublicVote = mean(c_across(Say_Spkr2:Say_Spkr5)))%>%
#   pivot_longer(cols = c("avg_PrivateBelief","avg_PublicVote"), names_to = "spkr_JudgeType", values_to = "spkr_Judgement")%>%str()

# View(ASP_Exp1)
# ASP_Exp1_long%>%
#   filter(Measure != "Decision")%>%
#   mutate(all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
#          all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")))%>%str()



#################################################################################################
####################        Updated Figures for Clarity Post- R & R        ######################
#################################################################################################



######### COMBINE PLOT C WITH NEW PLOT: team choice & accuracy as a function of predicted flips
ASP_Exp1_PlotC2<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        # legend.position = "inside",
        # legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "the greater the gap participants expect between votes & beliefs, <br> the less they trust the team's final choice,
           but only if <br>they expect it to contradict initial beliefs - no such pattern <br> for participants who don't expect consensus to flip")
ASP_Exp1_PlotC2

ASP_Exp1_PlotD2<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  filter(participant_ratingType == "rating_AccurateChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2d")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "nearly all participants trust pre-sequence consensus<br> regardless of how many speakers they expect to flip, <br>either publicly or privately")
ASP_Exp1_PlotD2

ASP_Exp1_PlotE2<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  filter(participant_ratingType == "rating_TeamChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+  
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the participant think the *team* will choose?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("darkorchid1", "green3"))+
  scale_color_manual(values = c("darkorchid1", "green3"))+
  facet_grid(PowerLevel~"participant judgment: team choice")+
  theme_cleanPub+
  labs(tag = "2e")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "the more speakers participants expect to flip<br>(especially privately) - the more they expect the <br>team's final decision to flip")
ASP_Exp1_PlotE2



#### Patchwork Plot - 2 then 1
Patchwork1a<-(ASP_Exp1_PlotE2+ASP_Exp1_PlotD2)+
  plot_layout(ncol=2, widths = c(1, 1),
              guides = "collect", axis_titles="collect_x")+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Public & Private Divergence')

#### Patchwork Part 2
Patchwork1b<-(Patchwork1a+ASP_Exp1_PlotC2)+
  plot_layout(ncol=3, widths = c(1,1,1))+
  plot_annotation(theme = theme(
    # legend.position = "bottom",
    # legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Public & Private Divergence')
Patchwork1b



# #### Patchwork Plot - All 3
# (ASP_Exp1_PlotE2+ASP_Exp1_PlotD2+ASP_Exp1_PlotC2)+
#   plot_layout(ncol=3, widths = c(1, 1, 1),
#               guides = 'collect')+
#   plot_annotation(theme = theme(
#     legend.position = "bottom",
#     legend.box.spacing = unit(0, "pt"),
#     plot.title = element_text(hjust = 0.5, family="Palatino", 
#                               size=23, face="bold", colour="black"),
#     plot.subtitle=element_text( hjust=0.5, family="Palatino", 
#                                 size=14, face="italic", color="black")),
#     title = 'Experiment 1: Public & Private Divergence')
# 



ASP_Exp1_PlotC2b<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue\nBetter",  "Uncertain", "Yellow\nBetter"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=1),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        # legend.position = "inside",
        # legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "inferred gap in votes~beliefs <br>vs inferences about best decision")
ASP_Exp1_PlotC2b



ASP_Exp1_PlotD2b<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  filter(participant_ratingType == "rating_AccurateChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("red3", "cyan3"))+
  scale_color_manual(values = c("red3", "cyan3"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2d")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "participant inferences about <br>number of flipped votes vs best decision")
ASP_Exp1_PlotD2b

ASP_Exp1_PlotE2b<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  filter(participant_ratingType == "rating_TeamChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+  
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the participant think the *team* will choose?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("red3", "cyan3"))+
  scale_color_manual(values = c("red3", "cyan3"))+
  facet_grid(PowerLevel~"participant judgment: team choice")+
  theme_cleanPub+
  labs(tag = "2e")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "participant inferences about <br>number of flipped votes vs team's final choice") # more speakers flip ~ higher ratings of team flip
ASP_Exp1_PlotE2b



#### Patchwork Plot - 2 then 1
Patchwork1a<-(ASP_Exp1_PlotE2b+ASP_Exp1_PlotD2b)+
  plot_layout(ncol=2, widths = c(1, 1),
              guides = "collect", axis_titles="collect_x")+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Public & Private Divergence')

#### Patchwork Part 2
Patchwork1b<-(Patchwork1a+ASP_Exp1_PlotC2b)+
  plot_layout(ncol=3, widths = c(1,1,1))+
  plot_annotation(theme = theme(
    # legend.position = "bottom",
    # legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Public & Private Divergence')
Patchwork1b



#################################################################################################
####################        Updated Figures for Clarity Post- R & R        ######################
#################################################################################################

### What do people think the most common patterns are?
ASP_Exp1_PlotA2<-ASP_Exp1%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.  
  mutate(all_or_none = ifelse(Values == "YYYYY" | Values == "YBBBB", as.character(Values),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")),
         # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
         Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
         Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count), y=after_stat(3.0)), color="white",
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=120", "Pop\n n=118"), x = c(3, 3), y = c(90,90),
                         PowerLevel   = c("Anonymous", "Popular"),
                         Measure = c("PublicVote", "PrivateBelief")),
             aes(x = x, y = y, label = label), fill="white")+
  scale_y_continuous("count", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  # scale_y_continuous("count ", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  labs(x="inferred judgment for each speaker")+
  scale_fill_manual(values = c("red", "gray40"))+
  theme_cleanPub+
  scale_x_discrete(drop=F)+
  facet_grid(PowerLevel~"Measure")+
  labs(tag = "2a")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none")+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")
ASP_Exp1_PlotA2

## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotB2<-ASP_Exp1_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")))%>%
  ggplot(aes(x=as.numeric(SpkrID)+1, y=Values, color=Measure))+
  geom_beeswarm(size=3, cex=.75, alpha=.5)+ # dodge.width=1, 
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=interaction(Measure,  SpkrID)), 
               position=position_dodge(width=1), size=1.15, width=.30, color="black")+
  stat_summary(fun ="mean", geom="label", label.size = 0,
               aes(label=round(..y.., 2),  y=Values, group=interaction(Measure, SpkrID), fill=Measure),
               position=position_dodge(width=1), color="white", size=4.5)+
  scale_y_continuous("Inferred Propensity to Endorse Yellow", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_x_continuous("Speaker Number", breaks = c(2,3,4,5), labels = c('Spkr2', 'Spkr3', 'Spkr4', 'Spkr5'))+
  scale_fill_manual(values = c("red", "gray40"))+
  scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~all_or_none, scales = "free_x",
             labeller = labeller(all_or_none = as_labeller(c('YBBBB' = "Public Votes: YBBBB",
                                                             'Mixed' = "Public Votes: Mixed",
                                                             'YYYYY' = "Public Votes: YYYYY"))))+
  theme_cleanPub+
  labs(tag = "2b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")
ASP_Exp1_PlotB2

#### Patchwork Plot - 2 then 1
Patchwork1A<-(ASP_Exp1_PlotA2+ASP_Exp1_PlotB2)+
  plot_layout(ncol=2, widths = c(1, 3),
              guides = "collect")+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Target Speaks First')
Patchwork1A


ASP_Exp1_PlotC2<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  mutate(spkr_JudgeType = ifelse(spkr_JudgeType == "PublicVoteCount", "PublicVote", "PrivateBelief"),
         spkr_JudgeType = factor(spkr_JudgeType, levels = c("PublicVote", "PrivateBelief")))%>%
  filter(participant_ratingType == "rating_TeamChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+  
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the participant think the *team* will choose?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("red", "gray40"))+
  scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~"participant judgment: team choice")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "participant inferences about <br>number of flipped votes vs team's final choice") # more speakers flip ~ higher ratings of team flip
ASP_Exp1_PlotC2

ASP_Exp1_PlotD2<-ASP_Exp1_counts%>%
  select(subID, PowerLevel, Spkr2_voteFlip, PublicVoteCount, PrivateVoteCount)%>%
  pivot_longer(cols = c("PublicVoteCount", "PrivateVoteCount"), names_to = "spkr_JudgeType", values_to = "spkr_Judgment")%>%
  left_join(ASP_Exp1_counts%>%
              select(subID, PowerLevel, Spkr2_voteFlip, c("rating_TeamChoice", "rating_AccurateChoice"))%>%
              pivot_longer(cols = c("rating_TeamChoice", "rating_AccurateChoice"), names_to = "participant_ratingType", values_to = "participant_rating"),
            by = c("subID", "PowerLevel", "Spkr2_voteFlip"))%>%
  mutate(spkr_JudgeType = ifelse(spkr_JudgeType == "PublicVoteCount", "PublicVote", "PrivateBelief"),
         spkr_JudgeType = factor(spkr_JudgeType, levels = c("PublicVote", "PrivateBelief")))%>%
  filter(participant_ratingType == "rating_AccurateChoice")%>%
  ggplot(aes(x=spkr_Judgment, y=participant_rating, color=spkr_JudgeType))+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=1)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_hline(yintercept=10.5, linetype="dashed")+
  scale_x_continuous("how many speakers did the participant think flipped to Yellow?")+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue",  "Uncertain", "Yellow"))+
  geom_smooth(method="lm")+ # aes(color=decision_BvY, group=decision_BvY)
  scale_fill_manual(values = c("red", "gray40"))+
  scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2d")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=.5),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "participant inferences about <br>number of flipped votes vs best decision")
ASP_Exp1_PlotD2


ASP_Exp1_PlotE2<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("which option does the *participant* think is better?",
                     breaks = c(1,10, 20), 
                     limits = c(0,21),
                     labels = c("Blue\nBetter",  "Uncertain", "Yellow\nBetter"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2e")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=8, angle=45, hjust=1),
        axis.text.x = element_markdown(size=10.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        # legend.position = "inside",
        # legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "inferred gap in votes~beliefs <br>vs inferences about best decision")
ASP_Exp1_PlotE2



#### Patchwork Plot - 2 then 1
Patchwork1B<-(ASP_Exp1_PlotC2+ASP_Exp1_PlotD2)+
  plot_layout(ncol=2, widths = c(1, 1),
              guides = "collect", axis_titles="collect_x")+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=16, face="italic", color="black")),
    title = 'Exp 1: Public vs Private Divergence & Observer Trust') # public versus private divergence & observer trust

#### Patchwork Part 2
Patchwork1C<-(Patchwork1B+ASP_Exp1_PlotE2)+
  plot_layout(ncol=3, widths = c(1,1,1))+
  plot_annotation(theme = theme(
    # legend.position = "bottom",
    # legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=16, face="italic", color="black")),
    title = 'Exp 1: Public vs Private Divergence & Observer Trust') # public versus private divergence & observer trust
Patchwork1C


# Final Patchwork

Patchwork1A
Patchwork1C

(Patchwork1A/Patchwork1C)









#################################################################################################
####################           Working on forRR_clearerFigs v0.2           ######################
#################################################################################################
## Load packages
library(tidyverse)
library(glue)
library(ggeffects)
library(ggh4x)
library(ggtext)
library(broom.mixed)
library(scales)
library(ordinal)
library(viridis)
library(ggbeeswarm)
library(patchwork)
library(janitor)

############
# ########### Create clean theme
# theme_cleanPub<-theme_minimal()+theme(axis.text.x = element_text(size=12, lineheight=.9, face="bold",
#                                                                  color="black", family="Palatino",
#                                                                  angle=0, hjust=.5, vjust=0),
#                                       axis.title.x=element_text(size=14, face="bold", color="black", family="Palatino",),
#                                       axis.title.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       axis.text.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       legend.text = element_text(size=13, face="bold", color="black", family="Palatino",),
#                                       legend.position="bottom",
#                                       # legend.title = element_blank(),
#                                       legend.title = element_text(size = 12, face = "bold", family="Palatino"),
#                                       plot.title = element_markdown(hjust = 0.5, family="Palatino",
#                                                                 size=23, face="bold", colour="black"),
#                                       plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",
#                                                                  face="italic", color="black"),
#                                       strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
#                                       strip.background = element_rect(fill="gray82", color="black")
#                                       )


### What do people think the most common patterns are?
ASP_Exp1_PlotA3<-ASP_Exp1%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.  
  mutate(all_or_none = ifelse(Values == "YYYYY" | Values == "YBBBB", as.character(Values),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")),
         # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
         Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
         Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count), y=after_stat(3.0)), color="white",
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=120", "Pop\n n=118"), x = c(3, 3), y = c(90,90),
                         PowerLevel   = c("Anonymous", "Popular"),
                         Measure = c("PublicVote", "PrivateBelief")),
             aes(x = x, y = y, label = label), fill="white")+
  scale_y_continuous("count", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  # scale_y_continuous("count ", limits=c(0,121), breaks = c(0, 30, 60, 90, 120))+
  labs(x="inferred sequence: public vs private judgments")+
  scale_fill_manual(values = c("red", "gray40"))+
  theme_cleanPub+
  scale_x_discrete(drop=F)+
  facet_grid(PowerLevel~"Measure")+
  labs(tag = "2a")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")
ASP_Exp1_PlotA3

## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotB3<-ASP_Exp1_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "YYYYY" | PublicPattern == "YBBBB", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("YBBBB", "Mixed", "YYYYY")))%>%
  ggplot(aes(x=as.numeric(SpkrID)+1, y=Values, color=Measure))+
  geom_beeswarm(size=3, cex=.75, alpha=.5)+ # dodge.width=1, 
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=interaction(Measure,  SpkrID)), 
               position=position_dodge(width=1), size=1.15, width=.30, color="black")+
  stat_summary(fun ="mean", geom="label", label.size = 0,
               aes(label=round(..y.., 2),  y=Values, group=interaction(Measure, SpkrID), fill=Measure),
               position=position_dodge(width=1), color="white", size=4.5)+
  scale_y_continuous("predicted *informant* judgment: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_x_continuous("Speaker Number", breaks = c(2,3,4,5), labels = c('Spkr2', 'Spkr3', 'Spkr4', 'Spkr5'))+
  scale_fill_manual(values = c("red", "gray40"))+
  scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~all_or_none, scales = "free_x",
             labeller = labeller(all_or_none = as_labeller(c('YBBBB' = "Public Votes: YBBBB",
                                                             'Mixed' = "Public Votes: Mixed",
                                                             'YYYYY' = "Public Votes: YYYYY"))))+
  theme_cleanPub+
  labs(tag = "2b")+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        legend.title = element_blank(),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")
ASP_Exp1_PlotB3




#### Patchwork Plot - 2 then 1
Patchwork1A<-(ASP_Exp1_PlotA3+ASP_Exp1_PlotB3)+
  plot_layout(ncol=2, widths = c(1, 3))+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: Target Speaks First')
Patchwork1A






## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotC3<-ASP_Exp1_long%>%
  filter(Measure == "Decision")%>%
  select(subID, PowerLevel, Spkr2_voteFlip, SpkrID, Values)%>%
  mutate(Measure = ifelse(SpkrID == "TeamChoice", "Team\nChoice", "Accurate\nChoice"))%>%
  ggplot(aes(x=Measure, y=Values))+
  geom_violin(alpha=.5)+
  geom_beeswarm(size=3, cex=.75, alpha=.5, dodge.width=.75, aes(shape=Spkr2_voteFlip))+
  scale_shape_manual(values = c(4, 17))+
  geom_line(aes(group=interaction(subID, Measure)), alpha=.35)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(y=Values, group=Measure),
               position=position_dodge(width=1), size=1.15, width=.05, color="black")+
  stat_summary(fun ="mean", geom="label", label.size=0,
               aes(label=round(..y.., 2),  y=Values, group=Measure), fill='black',
               position=position_dodge(width=1), color="white", size=4.5)+
  scale_y_continuous("*participant* judgment: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_fill_manual(values = c("red", "gray40"))+
  # scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~'observer trust', scales = "free_x")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "pluralistic ignorance & observer trust")
ASP_Exp1_PlotC3




ASP_Exp1_PlotD3<-ASP_Exp1_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, shape=Spkr2_voteFlip, stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("*participant* judgment: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_y_continuous("option *participant* thinks is better",
  #                    breaks = c(1,10, 20), 
  #                    limits = c(0,21),
  #                    labels = c("Blue\nBetter",  "Uncertain", "Yellow\nBetter"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "2d")+
  theme(axis.title.x=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=1),
        axis.text.x = element_markdown(size=10.5),
        # alternate
        # axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        # axis.text.x = element_text(size=12, angle=0, hjust=.5),
        # axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        # axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        # legend.position = "inside",
        # legend.position.inside = c(.15, .5),
        legend.title = element_blank())+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "inferred gap in votes~beliefs <br>vs inferences about best decision")
ASP_Exp1_PlotD3


#### Patchwork Plot - 2 then 1
Patchwork1B<-(ASP_Exp1_PlotC3+ASP_Exp1_PlotD3)+
  plot_layout(ncol=2, widths = c(1.5, 1.5))+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    subtitle = 'public and private divergence in Exp 1')
Patchwork1B



#####
areas <- c(area(1, 1, 2, 1),
           area(2, 3, 3, 3))
areas <-"A##
         A#B
         ##B"

layout <- c(
  area(1,1,1,4),
  area(2,2,2,3)
)


ASP_Exp1_PlotA3
ASP_Exp1_PlotB3
ASP_Exp1_PlotC3
ASP_Exp1_PlotD3


(Patchwork1A/Patchwork1B)+
  plot_layout(design=layout)+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")))


#####
layout_Fig2 <- c(
  area(1,1,1,3),
  area(2,1,2,1),
  area(2,2,2,2),
  area(2,3,2,3)
)



#####
layout_Fig2b <- c(
  area(2,1,2,1),
  area(2,2,2,2),
  area(2,3,2,3)
)

ASP_Exp1_PlotA3
ASP_Exp1_PlotB3
ASP_Exp1_PlotC3
ASP_Exp1_PlotD3


PatchworkFig2_top<-(ASP_Exp1_PlotB3)+
  # plot_layout(width=3)+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title="Experiment 1: Target Speaks First")
PatchworkFig2_top

PatchworkFig2_bottom<-(ASP_Exp1_PlotA3+ASP_Exp1_PlotC3+ASP_Exp1_PlotD3)+
  # plot_layout(ncol=3, width=c(1,1,1))+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_markdown(hjust=0.5, family="Palatino", 
                                   size=14, face="bold", color="black")),
    subtitle = '*public and private divergence in Exp 1*')
PatchworkFig2_bottom


layout_Fig2 <- c(
  area(1,1,6,3),
  area(7,1,7,3),
  area(8,1,12,3)
)

###1
###2
###3 Bottom
###4 Text grob
###5Top
###6
###7

###1
###2
###3 
###4 
###5 Bottom
###6 Text
###7 Top
###8
###9
###10

library(grid)
PatchworkFig2_mid<-textGrob("public and private divergence in Exp 1", x = 0.5, y = 0.5, hjust = 0.5,
                            gp = gpar(
                              fontfamily = "Palatino",
                              fontsize  = 15,
                              fontface  = "bold.italic",
                              col       = "black"))




PatchworkFig2<-(PatchworkFig2_top/PatchworkFig2_mid/(PatchworkFig2_bottom))+
  plot_layout(design=layout_Fig2)+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = "Experiment 1: Target Speaks First")
PatchworkFig2
ggsave("PatchworkFig2.svg", PatchworkFig2, width = 12, height = 16)



























