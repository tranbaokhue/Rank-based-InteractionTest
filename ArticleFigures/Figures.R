library(nimble)
library(tidyverse)
library(httr)
library(signs)
library(extrafont)
theme_set(theme_minimal())

# Figure 1: 3x4x2 Symmetric Main Effects with Product Interaction ----
power3x4x2_norm<-ggplot(Power3x4x2_norm, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", lambda)),
       y = "Empirical Power") +
  scale_x_continuous(breaks = seq(-2.5, 2.5, by = 0.5), expand = c(0, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power3x4x2_norm
ggsave(file= "Figure1.png", plot=power3x4x2_norm, width = 14, height = 7)
ggsave(file="Figure1.eps", width = 10, height = 5)

# Figure 2: 4x6x2 Mixed Main Effects with Specific Interaction ----
power4x6x2_unif<-ggplot(Power4x6x2_unif, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", italic(c))),
       y = "Empirical Power") +
  scale_x_continuous(breaks = seq(-2.5, 2.5, by = 0.5), expand = c(0, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power4x6x2_unif
ggsave(file = "Figure2.png", plot = power4x6x2_unif, width = 14, height = 7)
ggsave(file = "Figure2.eps", width = 10, height = 5)

# Figure 3: 3x2x3 Mixed Main Effects with Product Interaction ----
power3x2x3_exp<-ggplot(Power3x2x3_exp, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", lambda)),
       y = "Empirical Power")+
  scale_x_continuous(breaks = seq(-2.5, 2.5, by = 0.5), expand = c(0, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power3x2x3_exp
ggsave(file = "Figure3.png", plot = power3x2x3_exp, width = 14, height = 7)
ggsave(file = "Figure3.eps", width = 10, height = 5)

# Figure 4: 3x4x2 Symmetric Main Effects with Product Interaction ----
power3x4x2_doubleexp<-ggplot(Power3x4x2_doubleexp, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", lambda)),
       y = "Empirical Power") +
  scale_x_continuous(breaks = seq(-2.5, 2.5, by = 0.5), expand = c(0, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power3x4x2_doubleexp
ggsave(file = "Figure4.png", plot = power3x4x2_doubleexp, width= 14, height = 7)
ggsave(file = "Figure4.eps", width = 10, height = 5)

# Figure 5: 3x3x3 Symmetric Main Effects with Product Interaction ----
power3x3x3_cauchy<-ggplot(Power3x3x3_cauchy, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", lambda)),
       y = "Empirical Power") +
  scale_x_continuous(breaks = seq(-10, 10, by = 2), expand = c(0, 0.4), labels = signs_format(accuracy=0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power3x3x3_cauchy
ggsave(file="Figure5.png", plot=power3x3x3_cauchy, width = 14, height = 7)
ggsave(file="Figure5.eps", width = 10, height = 5)

# Figure 6: 4x6x2 Mixed Main Effects with Specific Interaction in Two columns ----
power4x6x2_cauchy<-ggplot(Power4x6x2_cauchy, aes(lambda)) +
  geom_line(aes(y = DeKroon, color = "DeKroon")) + 
  geom_line(aes(y = ClassicF, color = "F")) + 
  geom_line(aes(y = RankTransform, color = "RT")) +
  geom_line(aes(y = APCSSA, color = "APCSSA")) +
  geom_line(aes(y = APCSSM, color = "APCSSM")) +
  geom_line(aes(y = ART, color = "ART")) +
  geom_line(aes(y = R_ANOVA, color = "raov")) +
  geom_point(aes(y = DeKroon, color = "DeKroon", shape = "DeKroon")) + 
  geom_point(aes(y = ClassicF, color = "F", shape = "F")) + 
  geom_point(aes(y = RankTransform, color = "RT", shape = "RT")) +
  geom_point(aes(y = APCSSA, color = "APCSSA", shape = "APCSSA")) +
  geom_point(aes(y = APCSSM, color = "APCSSM", shape = "APCSSM")) +
  geom_point(aes(y = ART, color = "ART", shape = "ART")) +
  geom_point(aes(y = R_ANOVA, color = "raov", shape = "raov")) +
  labs(x = expression(paste(Value~of, " ", italic(c))),
       y = "Empirical Power")+
  scale_x_continuous(breaks = seq(-10, 10, by = 2), expand = c(0, 0.4), labels = signs_format(accuracy = .1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0), limits = c(-0.025, 1.025)) +
  scale_shape_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM","ART","raov"),
                     values = c("DeKroon" = 0,"F"=1,"RT" = 2, "APCSSA" = 3, "APCSSM" = 4, "ART" = 5, "raov" = 6)) +
  scale_color_manual(name = "Test", breaks = c("DeKroon","F", "RT", "APCSSA", "APCSSM", "ART", "raov"),
                     values = c("DeKroon"="#44a999","F" = "darkred",
                                "RT" = "#FFB90F" , "APCSSA" = "mediumpurple2",
                                "APCSSM" = "darkslategray2",
                                "ART"="#002642","raov"="#EE6C4D"))
power4x6x2_cauchy
ggsave(file="Figure6.png", plot=power4x6x2_cauchy, width=14, height=7)
ggsave(file="Figure6.eps", width = 10, height = 5)


