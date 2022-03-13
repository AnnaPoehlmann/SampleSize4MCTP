# AP Nov 2021
# Prepare the simulation results

rm(list = ls())

# load the packages
pacman::p_load(ggplot2, gridExtra, grid, dplyr)

#setwd("./Simulation")

####################### t fixed, find optimal N ###########################

# Homoscedastic case 1 -------------------------------------------------------
# 70% power
res_hom70 <- readRDS("01_HomoscedasticVars/fixed_t/case1/res.rds") # 
dat_hom70 <- do.call("rbind", res_hom70$res)
dat_hom70 <- dat_hom70 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_hom70$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=60), 3) ))
g_hom70 <- ggplot(data = dat_hom70, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.6,0.8)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.7, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.73, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.67, size = 0.6, linetype = "dashed") + 
  xlab("Number of groups") + 
  ylab("Power") +
  facet_wrap( ~ design, nrow = 1) +
  theme(text = element_text(size=22))  

ggsave("res_homoscedastic70.pdf", plot = g_hom70, path = "01_HomoscedasticVars/fixed_t", height=6, width=9)

# Homoscedastic case 2 ------------------------------------------------------
# 80 % power
res_hom80 <- readRDS("01_HomoscedasticVars/fixed_t/case2/res.rds") # 
dat_hom80 <- do.call("rbind", res_hom80$res)
dat_hom80 <- dat_hom80 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_hom80$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=60), 3) ))
g_hom80 <- ggplot(data = dat_hom80, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.7,0.9)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.8, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.83, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.77, size = 0.6, linetype = "dashed") + 
  xlab("Number of groups") + 
  ylab("Power") +
  facet_wrap( ~ design, nrow = 1) +
  theme(text = element_text(size=22))  

ggsave("res_homoscedastic80.pdf", plot = g_hom80, path = "01_HomoscedasticVars/fixed_t", height=6, width=9)

# Homoscedastic case 3 ------------------------------------------------------
# 90 % power
res_hom90 <- readRDS("01_HomoscedasticVars/fixed_t/case3/res.rds") # 
dat_hom90 <- do.call("rbind", res_hom90$res)
dat_hom90 <- dat_hom90 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_hom90$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=60), 3) ))
g_hom90 <- ggplot(data = dat_hom90, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.8,1)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.9, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.93, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.87, size = 0.6, linetype = "dashed") + 
  xlab("Number of groups") + 
  ylab("Power") +
  facet_wrap( ~ design, nrow = 1) +
  theme(text = element_text(size=22))  

ggsave("res_homoscedastic90.pdf", plot = g_hom90, path = "01_HomoscedasticVars/fixed_t", height=6, width=9)

# Heteroscedastic case 1 ----------------------------------------------------
# 70% power
res_het70 <- readRDS("02_HeteroscedasticVars/fixed_t/case1/res.rds")  
dat_het70 <- do.call("rbind", res_het70$res)
dat_het70 <- dat_het70 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_het70$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=40), 3) ), 
         pairing= as.factor(rep(c(rep("balanced", 40),
                                rep(rep(c("unbalanced, \npositive pairing", "unbalanced, \nnegative pairing"), each=10),2)),3)))
g_het70 <- ggplot(data = dat_het70, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.625,.775)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.7, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.73, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.67, size = 0.6, linetype = "dashed") + 
  facet_wrap( ~ pairing, nrow = 1) +
  theme(text = element_text(size=22), axis.title.x=element_blank(), axis.title.y=element_blank())

ggsave("res_heteroscedastic70.pdf", plot = g_het70, path = "02_HeteroscedasticVars/fixed_t", height=6, width=9)

# Heteroscedastic case 2  --------------------------------------------------
# 80% power
res_het80 <- readRDS("02_HeteroscedasticVars/fixed_t/case2/res.rds")  
dat_het80 <- do.call("rbind", res_het80$res)
dat_het80 <- dat_het80 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_het80$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=40), 3) ), 
         pairing= as.factor(rep(c(rep("balanced", 40),
                                  rep(rep(c("unbalanced, \npositive pairing", "unbalanced, \nnegative pairing"), each=10),2)),3)))
g_het80 <- ggplot(data = dat_het80, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.725,.875), breaks = c(0.75,0.8,0.85)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.8, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.83, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.77, size = 0.6, linetype = "dashed") + 
  facet_wrap( ~ pairing, nrow = 1)  +
  theme(text = element_text(size=22), axis.title.x=element_blank(), axis.title.y=element_blank())

ggsave("res_heteroscedastic80.pdf", plot = g_het80, path = "02_HeteroscedasticVars/fixed_t", height=6, width=9)

# Heteroscedastic case 3 -----------------------------------------------------
# 90% power
res_het90 <- readRDS("02_HeteroscedasticVars/fixed_t/case3/res.rds")  
dat_het90 <- do.call("rbind", res_het90$res)
dat_het90 <- dat_het90 %>% 
  mutate(a = as.factor(rep(3:5, each = nrow(res_het90$res$ngroups3))),
         design = as.factor(rep(rep(c("balanced", "unbalanced"), each=40), 3) ), 
         pairing= as.factor(rep(c(rep("balanced", 40),
                                  rep(rep(c("unbalanced, \npositive pairing", "unbalanced, \nnegative pairing"), each=10),2)),3)))
g_het90 <- ggplot(data = dat_het90, aes(y = power, x = a)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.825,0.975)) +
  theme(text = element_text(size=16),  plot.title = element_text(size = 17)) +
  geom_hline(yintercept = 0.9, color = "red", size = 0.6) + 
  geom_hline(yintercept = 0.93, size = 0.6, linetype = "dashed") + 
  geom_hline(yintercept = 0.87, size = 0.6, linetype = "dashed") + 
  # xlab("Number of groups") +
  # ylab("Power") +
  facet_wrap( ~ pairing, nrow = 1)  +
  theme(text = element_text(size=22), axis.title.x=element_blank(), axis.title.y=element_blank())

ggsave("res_heteroscedastic90.pdf", plot = g_het90, path = "02_HeteroscedasticVars/fixed_t", height=6, width=9)

# 70%, 80%, 90% power by the number of groups --------------------------------------
# boxplots for the estimated power for balanced and unbalanced designs with negative and 
# positive pairing in case of heteroscedastic variances and a power of 0.7/0.8/0.9 for 3/4/5 groups
g_het <- grid.arrange(g_het70, g_het80, g_het90, ncol = 1,
                      bottom = textGrob("Number of groups", gp = gpar(fontsize=21)),
                      left = textGrob("Power", gp = gpar(fontsize=21), rot= 90))

ggsave("res_parametric_heteroscedastic_by_a.pdf", plot = g_het,
       height=15, width=10)






