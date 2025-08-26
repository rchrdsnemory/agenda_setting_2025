###########################################################################################
#############################        ASP  Experiment 2           ##########################
###########################################################################################
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
############ Create clean theme
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
#                                       plot.title = element_text(hjust = 0.5, family="Palatino",
#                                                                 size=23, face="bold", colour="black"),
#                                       plot.subtitle=element_text(size=14, hjust=0.5, family="Palatino",
#                                                                  face="italic", color="black"),
#                                       strip.text = element_markdown(size=10, face="bold", color="black", family="Palatino"),
#                                       strip.background = element_rect(fill="gray82", color="black")
#                                       )

#########################################################################################################################
#######################################              IMPORT DATA          ###############################################
#########################################################################################################################

################################################### Import response data
###################################################
library(readxl)
ASP_Exp2 <- read_excel("data/24.02.13_ASP_Exp2_data.xlsx", 
                       sheet = "RData")


# Check exclusions
# 19 ppl failed basic comprehension and weren't allowed to do experiment
# 2 more failed only the post-check screen, but their explanations passed the bot exclusions
ASP_Exp2%>%
  group_by(exclude_preScreen, exclude_postScreen, exclude_forAnalysis)%>%
  summarise(Totals=n())%>%
  adorn_totals()

# filter out exclusions
ASP_Exp2<-ASP_Exp2%>%
  filter(exclude_forAnalysis == 0)%>%
  select(-starts_with("exclude"))

# condition counts: 123 each
ASP_Exp2%>%
  group_by(PowerLevel)%>%
  tally()

# pivot_long 
ASP_Exp2_long<-ASP_Exp2%>%
  pivot_longer(cols = Rate_teamChoice:Rate_actuallyBest,
               names_to = "Measure",
               values_to = "Values")%>%
  mutate(Measure = factor(Measure))

##########################################################################################################
######################       Figure             ###############################################
##########################################################################################################

## Display vote confidence 
ASP_Exp2_long%>%
  mutate(Measure = ifelse(Measure == "Rate_actuallyBest", "AccurateChoice", 
                   ifelse(Measure == "Rate_teamChoice", "TeamChoice", Measure)),
         Measure = factor(Measure, levels = c("TeamChoice", "AccurateChoice")))%>%
  ggplot(aes(x=Measure, y=Values, fill = Measure))+
  geom_beeswarm(size=4, cex=1, alpha=.7)+ 
  geom_line(aes(group=subID), alpha=.35)+
  geom_violin(alpha=.7)+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", 
               aes(y=Values, group=Measure), 
               position=position_dodge(width=.5), linewidth=2, width=.10, color="black")+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(..y.., 2),  y=Values, group=Measure, fill=Measure), 
               position=position_dodge(width=.5), color="black", size=6)+
  geom_hline(yintercept = 10.5, linetype = "dashed", color = "black")+
  scale_y_continuous("Propensity to Endorse Yellow", limits = c(1,20), breaks = c(1,5,10,15,20))+
  scale_fill_manual(values = c("#F82AB7", "seagreen2"))+
  scale_color_manual(values = c("#F82AB7", "seagreen2"))+
  theme_cleanPub+theme(plot.subtitle=element_markdown(size=14, hjust=0.5, family="Palatino", 
                                                      face="italic", color="black"))+
  facet_grid2(~PowerLevel, scales="free_x", strip = strip_nested(size = "variable"))+
  ggtitle("Experiment 2: predicting team choice versus endorsing it",
          subtitle = "Target speaks first, followed by unanimous sequence of public votes (no private beliefs);
          <br> after voting, team makes final choice by discussion")


###########################################################################################
#############################        ASP  Exp 2   STATS          ##########################
###########################################################################################

#### 
ASP_Exp2_long%>%
  mutate(Ct_Values = Values - 10.5,
         # create a variable reordering the factor so that we can get the intercept
         # estimates for each condition
         PowerLevel_flip = factor(PowerLevel, levels = c("Popular", "Anonymous")))%>%
  group_by(Measure)%>%
  nest()%>%
  mutate(mod_anonVpop = map(data, ~tidy(lm(Ct_Values ~ PowerLevel, data=.), 
                                        conf.int=TRUE)),
         mod_popVanon = map(data, ~tidy(lm(Ct_Values ~ PowerLevel_flip, data=.), 
                                        conf.int=TRUE)))%>%
  unnest(mod_anonVpop)%>%
  select(-data)%>%
  mutate(p_round = scales::pvalue(p.value, accuracy = 0.001, decimal.mark = "."))






