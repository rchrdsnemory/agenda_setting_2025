#################################################################################################
####################        Updated Figures for Clarity Post- R & R        ######################
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
        legend.title = element_blank(),
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
        legend.title = element_blank(),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")
ASP_Exp1_PlotB2

## Display by-subject and averages for votes and beliefs
ASP_Exp1_Plot_extra<-ASP_Exp1_long%>%
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
  scale_y_continuous("Inferred Propensity to Endorse Yellow", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_fill_manual(values = c("red", "gray40"))+
  # scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~'observer trust', scales = "free_x")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        # axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        # axis.text.y = element_text(size=8, angle=45, hjust=.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "pluralistic ignorance & observer trust")
ASP_Exp1_Plot_extra

#### Patchwork Plot - all 3
Patchwork1A<-(ASP_Exp1_PlotA2+ASP_Exp1_PlotB2+ASP_Exp1_Plot_extra)+
  plot_layout(ncol=3, widths = c(1, 3, 1))+
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

ggsave("Patchwork1A.svg", Patchwork1A, width = 16, height = 8)

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
    title = 'Exp 1: Public-Private Divergence & Observer Trust') # public versus private divergence & observer trust

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
    title = 'Exp 1: Public-Private Divergence & Observer Trust') # public versus private divergence & observer trust
Patchwork1C


ggsave("Patchwork1C.svg", Patchwork1C, width = 16, height = 8)

# # Final Patchwork
# 
# Patchwork1A
# Patchwork1C
# 
# (Patchwork1A/Patchwork1C)




#################################################################################################
##########################          Figures for Model Fit              ##########################
#################################################################################################


##############################################
############################################## Fig 3a: model fits
##############################################
paramFig3A<-ASP_Exp1_augmentedLong%>%
  drop_na(model_values)%>%
  # filter(modelSource == "varMod")%>%
  mutate(SpkrID = factor(SpkrID, levels = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")),
         wAcc_Bins2 = ifelse(param_weightAccFav > .5, "wAcc more (>.5)", "socFav more (<.5)"),
         modelSource = case_when(modelSource == "fixMod" ~ "fixPower", 
                                 modelSource == "varMod" ~ "varPower"))%>%
  select(subID, modelSource, PowerLevel, Measure, SpkrID, wAcc_Bins2, model_values, human_values)%>%
  ggplot(aes(x=model_values, y=human_values, color=SpkrID))+
  # geom_line(aes(group=subID), color="black", alpha=.3)+
  geom_point(size=1.5, alpha=.5, position=position_jitter(width=.01, height=.01))+
  geom_abline(intercept = 0, slope=1, linetype="dashed")+
  scale_x_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  scale_y_continuous(breaks = c(0,.5,1.0), limits=c(-0.1,1.1), labels = c(0.0, .5, 1.0)) +
  geom_hline(yintercept=.5, linetype="dashed")+
  geom_vline(xintercept=.5, linetype="dashed")+
  geom_smooth(method="lm")+
  # scale_color_manual(values = c("#00FDFF", "#00A4FF","#0C00FF", "#9600FF"))+
  # paletteer::scale_color_paletteer_d("ggprism::plasma", direction = -1)+
  # scale_color_manual(values = c("#FCA636FF", "#E16462FF", "#B12A90FF", "#6A00A8FF"))+
  # scale_color_manual(values = c("#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF"))+
  # scale_color_manual(values = c("#00FDFF", "#00A4FF", "#0C00FF", "#000080"))+
  scale_color_manual(values = c("#000080","#0C00FF", "#00A4FF", "#00FDFF"))+
  labs(tag = "3a")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"))+
  facet_grid2(Measure~modelSource+PowerLevel, strip = strip_nested(size = "variable"))+
  ggtitle("",
          subtitle = "model fits")

pal<-colorRampPalette(c("#00FDFF", "#0C00FF"))
pal(4)

##############################################
############################################## Fig 3b: model parameter distributions
##############################################
paramFig3B<-ASP_Exp1_augmented%>%
  pivot_longer(cols = starts_with("fittedParam"),
               names_pattern = "(.*)_(.*)_(.*)", names_to = c("discard", "modType", "paramName"),
               values_to = "param_values")%>%
  mutate(paramName = factor(paramName, levels = c("power","weightAccFav", "selfWeight","infoCon", "socCon")),
         modType = case_when(modType == "fixMod" ~ "fixPower", 
                             modType == "varMod" ~ "varPower"))%>%
  select(-discard)%>%
  ggplot(aes(x=modType, y=param_values, fill=paramName,alpha=modType))+
  geom_boxplot()+
  scale_alpha_manual(values=c(.2, 1))+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", position=position_dodge(width=1), size=2, width=.10, color="black",
               aes(y=param_values, group=interaction(modType,  paramName, PowerLevel)))+
  stat_summary(fun ="mean", geom="label", position=position_dodge(width=1), color="black", fill="gray", size=6,
               aes(label=round(..y.., 2),  y=param_values, group=interaction(modType, paramName, PowerLevel)))+
  paletteer::scale_fill_paletteer_d("MetBrewer::Lakota")+
  labs(tag = "3b")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        # axis.text.x = element_text(size=8),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
        legend.position = "none",
        strip.background = element_rect(fill="gray82", color="black"))+
  facet_grid2(~paramName+PowerLevel, scales="free",independent = "y",
              strip = strip_nested(size = "variable"), 
              labeller = labeller(paramName = as_labeller(c('infoCon' = "infoConformity: \n*higher = consensus implies accuracy*",
                                                            'power' = "PowerLevel: \n*number of 'votes' given to 1st Speaker*",
                                                            'selfWeight' = "selfWeight: \n*higher = trust self more than others*",
                                                            'socCon' = "socConformity: \n*higher = will vote conformist despite beliefs*",
                                                            'weightAccFav' = "socFav_vsAcc: \n*>.5 = values acc more than social favor*"))))+
  facetted_pos_scales(
    y = list(
      paramName == "power" ~ scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)),
      paramName == "weightAccFav" ~ scale_y_continuous(breaks = c(0,.25, .5, .75, 1)),
      paramName == "selfWeight" ~ scale_y_continuous(breaks = c(1,2,3,4,5)),
      paramName == "infoCon" ~ scale_y_continuous(breaks = c(1,2,3,4,5)),
      paramName == "socCon" ~ scale_y_continuous(breaks = c(1,2,3,4,5))))+
  ggtitle("",
          subtitle = "parameter distributions")


##############################################
############################################## Fig 3c: parameter space in 4 regions
##############################################

# we want to shade the lower diagonal of each plot to separate vote_Y > belief_Y & vice versa  
triangle_data <- data.frame(
  x = c(0, 1, 1),
  y = c(0, 0, 1))
facet_data<-expand.grid(
  wSelf_vPower_Bins = c("self <= Spkr1", "self > Spkr1"),
  wAcc_Bins = c("wAcc <= .5", "wAcc > .5"),
  votingRound = c("Spkr2", "Spkr3", "Spkr4", "Spkr5")
)
triangle_data <- merge(facet_data, triangle_data)
triangle_data

# now make the figure 
paramFig3C.alt<-ASP_Exp1_varFits%>%
  drop_na(model_values)%>%
  select(-humanPattern, -modelPattern)%>%
  pivot_wider(id_cols = c(subID, Spkr2_voteFlip, votingRound, contains(c("param", "PowerLevel", "_false"))), 
              names_from = Measure, values_from = model_values)%>%
  mutate(wSelf_vPower = param_selfWeight - param_power)%>%
  rename(model_beliefs = PrivateBelief, model_votes = PublicVote)%>%
  mutate(wAcc_Bins = case_when(param_weightAccFav <= .5 ~ "wAcc <= .5", 
                               # param_weightAccFav == .5 ~ "wAcc = .5", 
                               param_weightAccFav > .5 ~ "wAcc > .5"),
         wSelf_vPower_Bins = case_when(wSelf_vPower <= 0 ~ "self <= Spkr1", 
                                       # wSelf_vPower == 0 ~ "self = Spkr1",
                                       wSelf_vPower > 0 ~ "self > Spkr1"))%>%
  ggplot(aes(x=model_beliefs, y=model_votes))+
  geom_polygon(data = triangle_data, aes(x = x, y = y, group = interaction(wSelf_vPower_Bins, wAcc_Bins, votingRound)), 
               fill = "lightgrey", alpha = 0.5)+
  geom_abline(intercept = 0, slope=1, color="black")+
  geom_hline(yintercept=.5, linetype = "dashed")+
  geom_vline(xintercept=.5, linetype = "dashed")+
  geom_point(stroke=1.5,size=1, alpha=.5, position=position_jitter(width=0.01, height=0.01), 
             # could substitute shape=PowerLevel, but it's visually busy & Spkr2_voteFlip is the more general pattern
             aes(color=Spkr2_voteFlip, shape=Spkr2_voteFlip))+ 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), limits=c(-0.05,1.05), labels = c(0.0, .5, 1.0))+
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), limits=c(-0.05,1.05), labels = c(0.0, .5, 1.0))+
  scale_shape_manual(values=c(4,19))+ 
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"),
        strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        legend.title = element_blank(),
        # legend.title = element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        legend.title.position = "top")+
  facet_grid2(wSelf_vPower_Bins~wAcc_Bins+votingRound, strip = strip_nested(size = "variable"))+
  labs(tag = "3c")+
  ggtitle("",
          subtitle = "4 regions of parameter space")

paramFig3C.alt

##############################################
############################################## Patchwork Fig 3
##############################################

paramFig<-(paramFig3A|paramFig3C.alt)/paramFig3B+
  # plot_layout(ncol=5, widths = c(2, 3, 5),
  #             guides = 'collect')
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = 'Experiment 1: model fits')
paramFig


ggsave("paramFig.svg", paramFig, width = 22, height = 11)
