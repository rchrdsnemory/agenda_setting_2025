#################################################################################################
####################        Updated Figures for Clarity Post- R & R        ######################
#################################################################################################
## Load packages
library(tidyverse)
library(glue)
library(ggh4x)
library(ggtext)
library(scales)
library(viridis)
library(ggbeeswarm)
library(patchwork)
library(grid)

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

######################### Create figures for Exp 1
#########################
#########################

######################### FIGURE 2a: Upper plot
## Display by-subject and averages for votes and beliefs
ASP_Exp1_PlotA3<-ASP_Exp1_long%>%
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
  labs(tag = "2a")+
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
ASP_Exp1_PlotA3

######################### FIGURE 2b: Panel 1 of lower plot
### What do people think the most common patterns are?
ASP_Exp1_PlotB3<-ASP_Exp1%>%
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
  labs(tag = "2b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom")+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")
ASP_Exp1_PlotB3

######################### FIGURE 2c: Panel 2 of lower plot
### Inferred team choice versus inferred best choice
ASP_Exp1_PlotC3<-ASP_Exp1_long%>%
  filter(Measure == "Decision")%>%
  select(subID, PowerLevel, Spkr2_voteFlip, SpkrID, Values)%>%
  mutate(Measure = ifelse(SpkrID == "TeamChoice", "predicted<br>*team* choice", "*participant*<br>judgment"))%>%
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
  scale_y_continuous("better design: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_fill_manual(values = c("red", "gray40"))+
  # scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~'observer trust', scales = "free_x")+
  theme_cleanPub+
  labs(tag = "2c")+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_markdown(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        legend.direction = "vertical",
        legend.position = "non",
        legend.title = element_blank(),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 1: Target Speaks First",
          subtitle = "pluralistic ignorance & observer trust")
ASP_Exp1_PlotC3

######################### FIGURE 2d: Panel 3 of lower plot
### relationship between inferred pluralistic ignorance (informant's vote-belief gap) and participant trust
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
  # swap the order of the legends so that shape (Spkr2_VotesBlue vs _VotesYellow) can be shared with panel c
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1))+
  ggtitle("Experiment 1: Public & Private Divergence",
          subtitle = "inferred gap in votes~beliefs <br>vs inferences about best decision")
ASP_Exp1_PlotD3

############################
############################ Assembling Patchwork plot for Fig 2 of Exp 1
############################

# these are our figures
ASP_Exp1_PlotA3
ASP_Exp1_PlotB3
ASP_Exp1_PlotC3
ASP_Exp1_PlotD3

############################ Patchwork: upper panel of Fig 2
######## 
PatchworkFig2_top<-(ASP_Exp1_PlotA3)+
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

############################ Patchwork: lower panel of Fig 2
######## 
PatchworkFig2_bottom<-(ASP_Exp1_PlotB3+ASP_Exp1_PlotC3+ASP_Exp1_PlotD3)+
  plot_layout(guides="collect")+
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


############################ Patchwork: middle panel for Fig - grob for extra text acting as subtitle for lower panel
########  
layout_Fig2 <- c(
  area(1,1,6,3),
  area(7,1,7,3),
  area(8,1,12,3)
)
# 
###1 Top of upper fig starts at top of 1st row (used for overall title)
###2 fig (aside of from title) starts approximately here
###3 
###4 
###5 
###6 Bottom of upper fig ends at bottom of 6thth
###7 Text goes here (acts as subtitle for lower fig)
###8 Top of lower fig starts here (no title)
###9
###10
###11  
###12 Bottom of lower fig ends at bottom of 12th row



PatchworkFig2_mid<-grid::textGrob("public and private divergence in Exp 1", x = 0.5, y = 0.5, hjust = 0.5,
  gp = gpar(
    fontfamily = "Palatino",
    fontsize  = 15,
    fontface  = "bold.italic",
    col       = "black"))


Patchwork_Fig2<-(PatchworkFig2_top/PatchworkFig2_mid/(PatchworkFig2_bottom))+
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
Patchwork_Fig2
ggsave("Patchwork_Fig2.svg", Patchwork_Fig2, width = 13, height = 16)



#################################################################################################
##########################          Figures for Model Fit              ##########################
#################################################################################################


##############################################
############################################## Fig 3a: model fits
##############################################
# create a palette of equally spaced color
pal<-grDevices::colorRampPalette(c("#00FDFF", "#0C00FF"))
pal(4)

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
  geom_point(size=2, alpha=.5, position=position_jitter(width=.01, height=.01))+
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
  # scale_color_manual(values = c("#000080","#0C00FF", "#00A4FF", "#00FDFF"))+
  # scale_color_manual(values = c("#000080","#00FDFF", "#0C00FF", "#00A4FF"))+
  scale_color_manual(values = c("#000080", "#0C00FF", "#00A4FF", "#00FDFF"))+
  labs(tag = "3a")+
  theme_cleanPub+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-1),
        strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
        plot.tag = element_text(hjust = 0.5, size=14, face="bold", colour="black"),
        plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino",face="italic", color="black"))+
  facet_grid2(Measure~modelSource+PowerLevel, strip = strip_nested(size = "variable"))+
  ggtitle("",
          subtitle = "model fits")
paramFig3A


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

Patchwork_Fig3_mod<-(paramFig3A|paramFig3C.alt)/paramFig3B+
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
Patchwork_Fig3_mod

# this plot should be rotated 90 degrees in the paper
ggsave("Patchwork_Fig3_mod.svg", Patchwork_Fig3_mod, width = 22, height = 11)



##########################################################################################################
######################            Experiment 2: Figure 4             ##################################### 
##########################################################################################################


redo_Fig4_Exp2<-ASP_Exp2_long%>%
  mutate(Measure = ifelse(Measure == "Rate_teamChoice", "predicted<br>*team* choice", "*participant*<br>judgment"))%>%
  ggplot(aes(x=Measure, y=Values))+
  geom_violin(alpha=.75, aes(fill=Measure))+
  geom_beeswarm(size=4, cex=.5, alpha=.35, aes(fill=Measure))+
  geom_line(aes(group=subID), alpha=.15)+
  geom_hline(yintercept=10.5, linetype="dashed")+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(y=Values, group=Measure),
               position=position_dodge(width=1), size=1.15, width=.05, color="white")+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(..y.., 2),  y=Values, group=Measure), fill='white',alpha=.75,
               position=position_dodge(width=1), color="black", size=4.5)+
  scale_y_continuous("better design: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_fill_manual(values = c("#F82AB7", "seagreen2"))+
  # scale_color_manual(values = c("#F82AB7", "seagreen2"))+
  scale_fill_manual(values = c("gray80", "gray10"))+
  scale_color_manual(values = c("gray80", "gray10"))+
  facet_grid(~PowerLevel, scales = "free_x")+
  theme_cleanPub+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_markdown(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_markdown(size=12, angle=0, hjust=.5),
        legend.position = "none")+
  ggtitle("Experiment 2: Predicting team choice vs trusting it",
          subtitle = "Unlike Exp 1, private beliefs are not revealed, and participant doesn't predict votes or beliefs.
          <br> Instead, participant is shown target speak first followed a unanimous sequence of endorsements")
redo_Fig4_Exp2
# this plot should be rotated 90 degrees in the paper
ggsave("redo_Fig4_Exp2.svg", redo_Fig4_Exp2, width = 12, height = 6)


##########################################################################################################
######################            Experiment 3: Figure 5             ##################################### 
##########################################################################################################


######################### FIGURE 5a: Upper plot
## Display by-subject and averages for votes and beliefs
ASP_Exp3_PlotA5<-ASP_Exp3_long%>%
  filter(Measure != "Decision")%>%
  # here, we want to stick with all_or_none being the PublicPattern
  mutate(all_or_none = ifelse(PublicPattern == "BBBBB" | PublicPattern == "BBBBY", as.character(PublicPattern),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("BBBBB", "Mixed", "BBBBY")))%>%
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
             labeller = labeller(all_or_none = as_labeller(c('BBBBB' = "Public Votes: BBBBB",
                                                             'Mixed' = "Public Votes: Mixed",
                                                             'BBBBY' = "Public Votes: BBBBY"))))+
  theme_cleanPub+
  labs(tag = "5a")+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        legend.title = element_blank(),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 3: Target Speaks Last",
          subtitle = "Participant ratings of each speaker's Public Vote & Private Belief <br>")
ASP_Exp3_PlotA5

######################### FIGURE 5b: Panel 1 of lower plot
### What do people think the most common patterns are?

ASP_Exp3_PlotB5<-ASP_Exp3%>%
  pivot_longer(cols = PublicPattern:PrivatePattern,
               names_to = "Measure", values_to = "Values")%>%
  # we want all_or_none for both PublicPattern AND PrivatePattern.
  mutate(all_or_none = ifelse(Values == "BBBBB" | Values == "BBBBY", as.character(Values),  "Mixed"),
         all_or_none = factor(all_or_none, levels = c("BBBBB", "Mixed", "BBBBY")),
         # for clarity, rename PublicPattern & PrivatePattern to PublicVote & PrivateBelief
         Measure = ifelse(Measure == "PublicPattern", "PublicVote", "PrivateBelief"),
         Measure = factor(Measure, levels = c("PublicVote", "PrivateBelief")))%>%
  # uncomment chunk below to confirm counts in histogram
  # group_by(PowerLevel, Measure, all_or_none)%>%
  # add_tally()%>%
  # tally()
  # pivot_wider(names_from=c("PowerLevel", "Measure"), values_from = "n")%>%
  # adorn_totals()
  ggplot(aes(x=all_or_none, fill=Measure))+
  geom_bar(position = "dodge")+
  geom_text(aes(label = after_stat(count), y=after_stat(3.0)), color="white",
            stat = "count", position = position_dodge(width=.95))+
  geom_label(data=tibble(label = c("Anon\n n=121", "Pop\n n=120"), x = c(2.5, 2.5), y = c(90,90),
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
  labs(tag = "5b")+
  theme(axis.title.x=element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_text(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank(),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom")+
  ggtitle("Experiment 3: Target Speaks Last",
          subtitle = "Histogram of most commonly <br> inferred order of Yellow vs. Blue judgments")
ASP_Exp3_PlotB5


######################### FIGURE 5c: Panel 2 of lower plot
### Inferred team choice versus inferred best choice
ASP_Exp3_PlotC5<-ASP_Exp3_long%>%
  filter(Measure == "Decision")%>%
  select(subID, PowerLevel, Spkr2_voteFlip, SpkrID, Values)%>%
  mutate(Measure = ifelse(SpkrID == "TeamChoice", "predicted<br>*team* choice", "*participant*<br>judgment"))%>%
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
  scale_y_continuous("better design: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  # scale_fill_manual(values = c("red", "gray40"))+
  # scale_color_manual(values = c("red", "gray40"))+
  facet_grid(PowerLevel~'observer trust', scales = "free_x")+
  theme_cleanPub+
  labs(tag = "5c")+
  theme(axis.title.x=element_blank(),# element_text(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.x = element_markdown(size=12, angle=0, hjust=.5),
        axis.title.y=element_markdown(size=12, face="bold", color="black", family="Palatino", vjust=-.25),
        axis.text.y = element_text(size=12, angle=0, hjust=.5),
        legend.direction = "vertical",
        legend.position= "none",
        legend.title = element_blank(),
        plot.tag.position = c(.02, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"),
        plot.title = element_blank())+
  ggtitle("Experiment 3: Target Speaks Last",
          subtitle = "pluralistic ignorance & observer trust")
ASP_Exp3_PlotC5

######################### FIGURE 5d: Panel 3 of lower plot
### relationship between inferred pluralistic ignorance (informant's vote-belief gap) and participant trust
ASP_Exp3_PlotD5<-ASP_Exp3_counts%>%
  # human_avgDiff_voteHigh, human_avgVote, human_avgBelief, scale01_AccurateChoice
  # rating_avgDiff_voteHigh, rating_avgVote, rating_avgBelief, rating_AccurateChoice
  ggplot(aes(x=rating_avgDiff_voteHigh, y=rating_AccurateChoice))+
  geom_jitter(size=4, alpha=.5, width=.01, height=.01,
              aes(color=decision_BvY, 
                  shape=Spkr2_voteFlip, 
                  stroke=1.5))+
  scale_shape_manual(values = c(4, 17))+
  geom_hline(yintercept=10.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY),
              se = T, alpha=.15, linetype=0)+
  stat_smooth (geom="line", method="lm", alpha=0.5, size=1, span=0.5, aes(color=decision_BvY, group=decision_BvY)) +
  geom_smooth(method="lm", color="black", linetype="dashed")+
  # geom_smooth(method="lm", aes(color=decision_BvY, group=decision_BvY))+
  scale_x_continuous("avg difference in votes & beliefs of Spkr2-5",
                     breaks = c(-20, 0, 20), limits = c(-20,20),
                     labels = c("vote_Y < think_Y", "vote = belief", "vote_Y > think_Y"))+
  scale_y_continuous("*participant* judgment: blue (1) vs yellow (20)", limits = c(0,21), breaks = c(1,5,10,15,20))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod1"))+
  scale_color_manual(values = c("dodgerblue", "goldenrod1"))+
  facet_grid(PowerLevel~"participant judgment: accuracy")+
  theme_cleanPub+
  labs(tag = "5d")+
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
  # swap the order of the legends so that shape (Spkr2_VotesBlue vs _VotesYellow) can be shared with panel c
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1))+
  ggtitle("Experiment 3: Public & Private Divergence",
          subtitle = "inferred gap in votes~beliefs <br>vs inferences about best decision")
ASP_Exp3_PlotD5

############################
############################ Assembling Patchwork plot for Fig 2 of Exp 1
############################

# these are our figures
ASP_Exp3_PlotA5
ASP_Exp3_PlotB5
ASP_Exp3_PlotC5
ASP_Exp3_PlotD5

############################ Patchwork: upper panel of Fig 2
######## 
PatchworkFig5_top<-(ASP_Exp3_PlotA5)+
  # plot_layout(width=3)+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title="Experiment 3: Target Speaks Last")
PatchworkFig5_top

############################ Patchwork: lower panel of Fig 2
######## 
PatchworkFig5_bottom<-(ASP_Exp3_PlotB5+ASP_Exp3_PlotC5+ASP_Exp3_PlotD5)+
  plot_layout(guides="collect")+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_markdown(hjust=0.5, family="Palatino", 
                                   size=14, face="bold", color="black")),
    subtitle = '*public and private (non-) divergence in Exp 3*')
PatchworkFig5_bottom


############################ Patchwork: middle panel for Fig - grob for extra text acting as subtitle for lower panel
########  
layout_Fig5 <- c(
  area(1,1,6,3),
  area(7,1,7,3),
  area(8,1,12,3)
)
# 
###1 Top of upper fig starts at top of 1st row (used for overall title)
###2 fig (aside of from title) starts approximately here
###3 
###4 
###5 
###6 Bottom of upper fig ends at bottom of 6thth
###7 Text goes here (acts as subtitle for lower fig)
###8 Top of lower fig starts here (no title)
###9
###10
###11  
###12 Bottom of lower fig ends at bottom of 12th row



PatchworkFig5_mid<-grid::textGrob("public and private (non-) divergence in Exp 3", x = 0.5, y = 0.5, hjust = 0.5,
                                  gp = gpar(
                                    fontfamily = "Palatino",
                                    fontsize  = 15,
                                    fontface  = "bold.italic",
                                    col       = "black"))


Patchwork_Fig5<-(PatchworkFig5_top/PatchworkFig5_mid/(PatchworkFig5_bottom))+
  plot_layout(design=layout_Fig5)+
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(hjust = 0.5, family="Palatino", 
                              size=23, face="bold", colour="black"),
    plot.subtitle=element_text( hjust=0.5, family="Palatino", 
                                size=14, face="italic", color="black")),
    title = "Experiment 3: Target Speaks Last")
Patchwork_Fig5
ggsave("Patchwork_Fig5.svg", Patchwork_Fig5, width = 12, height = 16)







