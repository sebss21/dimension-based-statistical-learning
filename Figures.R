### figures for ### ### ###  Examining variability and generalization in dimension based statistical learning for speech: The case of place of articulation  ### ### ###

# last updated by Jeremy Steffman 4th March 2025

# load required packages and set wd
library(brms);library(tidyverse);library(ggthemes);library(viridis);library(cowplot)
theme_set(theme_bw(8))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

same.all <- read.csv("data/same_place_all_frames.csv")
switch.all <- read.csv("data/switch_place_all_frames.csv")


# stimulus figure

same.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  mutate(target_place = ifelse(subjectGroup =="1"|subjectGroup =="2","/b/-/p/",
                        ifelse(subjectGroup =="3"|subjectGroup =="4","/d/-/t/",
                        ifelse(subjectGroup =="5"|subjectGroup =="6","/g/-/k/","xxx")))) %>%
  group_by(subjectGroup,dist,F0_step,place,VOT_step) %>% slice(1) %>%
  ggplot(aes(x=as.numeric(VOT_step),y=F0_step,color=place,shape=place,size= trial_type))+
  scale_color_manual(values = c("#F05039","#1F449C","#7CA1CC"))+
 # geom_hline(yintercept = 0.95)+
  
  scale_size_manual(values = c(1.8,0.9))+
  guides(size = "none")+
  scale_x_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+scale_y_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+
  scale_shape_manual(values = c(3,1,4))+
  xlab("VOT step")+ylab("F0 step")+
  geom_point(alpha=1,stroke =0.8,
             position = position_dodge(0.0))+facet_wrap(~paste(target_place,dist),nrow=1) -> same_stim

switch.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                  ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  mutate(target_place = ifelse(subjectGroup =="1"|subjectGroup =="2","/b/-/p/",
                               ifelse(subjectGroup =="3"|subjectGroup =="4","/d/-/t/",
                                      ifelse(subjectGroup =="5"|subjectGroup =="6","/g/-/k/","xxx")))) %>%
  group_by(subjectGroup,dist,F0_step,place,VOT_step) %>% slice(1) %>%
  ggplot(aes(x=as.numeric(VOT_step),y=F0_step,color=place,shape=place,size=trial_type))+
  scale_color_manual(values = c("#F05039","#1F449C","#7CA1CC"))+
  #geom_hline(yintercept = 0.95)+
  scale_x_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+scale_y_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+
  scale_shape_manual(values = c(3,1,4))+
  xlab("VOT step")+ylab("F0 step")+
  scale_size_manual(values = c(1.8,0.9))+
  guides(size = "none")+
  geom_point(alpha=1,stroke =0.8,
             position = position_dodge(0.0))+facet_wrap(~paste(target_place,dist),nrow=1) -> switch_stim



same.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  ggplot(aes(x=as.numeric(VOT_step), alpha = trial_type, shape = place,color=as.factor(F0_step),y=response_num))+
  ylab("prop. voiceless response")+  xlab("VOT step")+
  scale_x_continuous(breaks = c(1,3,5,7,9))+
  scale_color_viridis(discrete=T,end=0.8, name = "F0 step")+
  ggtitle("same place")+
  scale_alpha_manual(values = c(1,0.4),name = "trial type")+
  scale_shape_manual(values=c(3,1,4))+
  stat_summary(fun.data = mean_cl_boot,
               position = position_dodge(1))+facet_wrap(~dist) -> same_all.plot



switch.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  ggplot(aes(x=as.numeric(VOT_step), alpha = trial_type, shape = place,color=as.factor(F0_step),y=response_num))+
  ylab("prop. voiceless response")+  xlab("VOT step")+
  scale_x_continuous(breaks = c(1,3,5,7,9))+
  scale_color_viridis(discrete=T,end=0.8, name = "F0 step")+
  ggtitle("switched place")+
  scale_alpha_manual(values = c(1,0.4),name = "trial type")+
  scale_shape_manual(values=c(3,1,4))+
  stat_summary(fun.data = mean_cl_boot,
               position = position_dodge(1))+facet_wrap(~dist) -> switch_all.plot


same.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(trial_type=="test") %>%
  ggplot(aes(x=paste(place,dist,sep="\n"), shape = place,color=as.factor(F0_step),y=response_num))+
  ylab("prop. voiceless response")+ xlab("place and distribution")+
  scale_color_viridis(discrete=T,end=0.8, name = "F0 step")+
  ggtitle("same place")+
  scale_alpha_manual(values = c(1,0.4),name = "trial type")+
  scale_shape_manual(values=c(3,1,4))+
  stat_summary(fun.data = mean_cl_boot,geom = "line", aes(group = paste(place,F0_step)),alpha=0.5,
               position = position_dodge(1))+
  stat_summary(fun.data = mean_cl_boot,geom = "errorbar", width = 0.3,alpha=0.65,
               position = position_dodge(1))+
  coord_cartesian(ylim = c(0,0.9))+
  stat_summary(fun.data = mean_cl_boot,geom = "point", size = 2, stroke = 1.5,
               position = position_dodge(1)) -> same_test.plot;same_test.plot



switch.all %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(trial_type=="test") %>%
  ggplot(aes(x=paste(place,dist,sep="\n"), shape = place,color=as.factor(F0_step),y=response_num))+
  ylab("prop. voiceless response")+ xlab("place and distribution")+
  scale_color_viridis(discrete=T,end=0.8, name = "F0 step")+
  ggtitle("switched place")+
  coord_cartesian(ylim = c(0,0.9))+
  scale_alpha_manual(values = c(1,0.4),name = "trial type")+
  scale_shape_manual(values=c(3,1,4))+
  stat_summary(fun.data = mean_cl_boot,geom = "line", aes(group = paste(place,F0_step)),alpha=0.5,
               position = position_dodge(1))+
  stat_summary(fun.data = mean_cl_boot,geom = "errorbar", width = 0.3,alpha=0.65,
               position = position_dodge(1))+
  stat_summary(fun.data = mean_cl_boot,geom = "point", size = 2, stroke = 1.5,
               position = position_dodge(1)) -> switch_test.plot;switch_test.plot




read_rds("models/same.all.place.rds") -> same_model
read_rds("models/switch.all.place.rds") -> switch_model
library(bayestestR)
library(ggthemes)

pd(same_model) -> same_pd

same_pd %>%  filter(Parameter=="b_F0_step1"|
                   Parameter=="b_dist1"|Parameter=="b_F0_step1:dist1") %>%
plot()+theme(legend.position = "none")+ggtitle("same place")+scale_fill_economist()->pd.same.plot;pd.same.plot

pd(switch_model) -> switch_pd
switch_pd %>%
filter(Parameter=="b_F0_step1"|
         Parameter=="b_dist1"|Parameter=="b_F0_step1:dist1") %>%
plot()+theme(legend.position = "none")+ggtitle("switched place")+scale_fill_economist()->pd.switch.plot;pd.switch.plot

library(emmeans)
emmeans(same_model, pairwise ~ F0_step |dist)  ->emm.same
emmeans(switch_model, pairwise ~ F0_step |dist)  -> emm.switch

emmeans(same_model, pairwise ~ F0_step |dist*place)  ->emm.same.place
emmeans(switch_model, pairwise ~ F0_step |dist*place)  -> emm.switch.place


rbind(
  as.data.frame(emm.same.place$contrasts),
  as.data.frame(emm.switch.place$contrasts)) ->emm.combined.place

emm.combined.place$same_switch <-c("same place","same place","same place","same place","same place","same place","switched place","switched place","switched place","switched place","switched place","switched place")


rbind(
as.data.frame(emm.same$contrasts),
as.data.frame(emm.switch$contrasts)) ->emm.combined

emm.combined$same_switch <-c("same place","same place","switched place","switched place")

emm.combined %>%

  ggplot(aes(x= dist, y = estimate,ymin = lower.HPD,ymax = upper.HPD))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
          geom_point(size = 2)+geom_errorbar()+ xlab("distribution")+facet_wrap(~same_switch)+
  geom_hline(yintercept  = 0,lty = 2,color = "gray50") -> emm.plot



emm.combined.place %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  ggplot(aes(x= paste(place,dist,sep="\n"), color =place, shape = place, y = estimate,ymin = lower.HPD,ymax = upper.HPD))+
  scale_color_manual(values = c("#F05039","#1F449C","#7CA1CC"))+
  scale_shape_manual(values = c(3,1,4))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  geom_errorbar(color = "black")+
  xlab("place and distribution")+
  geom_point(size= 1.5,stroke =1.5)+facet_wrap(~same_switch)+
  geom_hline(yintercept  = 0,lty = 2,color = "gray50") -> emm.plot.place


### save figures
library(cowplot)
legend1 = cowplot::get_plot_component(same_stim+theme(legend.position = "top"), 'guide-box-top', return_all = TRUE)

legend2 = cowplot::get_plot_component(switch_test.plot, 'guide-box-right', return_all = TRUE)



jpeg("figures/fig1.jpeg",units = "in",width = 6.5,height = 3.5,res =400)
plot_grid(legend1,
          same_stim+ggtitle("same place")+theme(legend.position = "none"),
          switch_stim+ggtitle("switched place")+theme(legend.position = "none"),
          nrow = 3,rel_heights = c(0.1,1,1))
dev.off()



jpeg("figures/alltrials.jpeg",units = "in",width = 6.5,height = 3,res =400)

  plot_grid(same_all.plot+theme(legend.position = "none"),
            switch_all.plot+theme(legend.position = "none"))

  dev.off()


jpeg("figures/fig2.jpeg",units = "in",width = 6.5,height = 7.5,res =400)
plot_grid(
plot_grid(same_test.plot+theme(legend.position = "none"),
          switch_test.plot+theme(legend.position = "none"),legend2,nrow = 1,
          rel_widths = c(1,1,0.3),labels = c("A","B")
          ),
plot_grid(pd.same.plot,pd.switch.plot,labels = c("C","D")),
plot_grid(emm.plot,emm.plot.place,rel_widths = c(1.5,4),labels = c("E","F")),
nrow=3,rel_heights = c(1,0.7,1))
dev.off()

