# Schematic figures to illustrate the creation of the cost curves
# Replace crappy ones drawn in PPT with actual figs

library(ggplot2)

fakedat <- data.frame(cost = c(0, 0, 20, 20), waste = c(1, 0.75, 0.75, 0))

p1 <- ggplot(fakedat, aes(x=cost, y=waste)) +
  geom_hline(yintercept = c(1, 0.4), linetype = 'dashed', size = 1.5, color = 'orange') + 
  geom_segment(x = 0, xend = 20, y = 0.75, yend = 0.75, linetype = 'dotted', size = 1) +
  geom_segment(x = 20, xend = 20, y = 0, yend = 0.75, linetype = 'dotted', size = 1) +
  geom_segment(x = 4.3, xend = 1, y = 0.793, yend = 0.76, color = 'gray50', arrow = arrow(length=unit(0.1, 'inches'))) +
  geom_segment(x = 26, xend = 21, y = 0.07, yend = 0.01, color = 'gray50', arrow = arrow(length=unit(0.1, 'inches'))) +
  geom_point(size = 3) +
  geom_text(data = data.frame(cost = c(50, 50, 37, 46, 46), waste = c(1.05, 0.45, 0.8, 0.1, 0.05), lab = c('baseline~waste~rate~(W[0])', 'unavoidable~waste~rate~(W[u])', 'diversion~potential~waste~rate~(W[1])', 'cost~to~reduce~to~diversion', 'potential~rate~(C[1])')), aes(label = lab), parse = TRUE) +
  
  scale_x_continuous(name = 'cost', limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(name = 'waste rate', limits = c(0, 1.1), expand = c(0, 0)) +
  
  theme_classic() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())

ggsave('~/google_drive/SESYNC Food Waste/Results_Figures/makingcostcurve1.png', p1, height=4, width=4, dpi=300)

b <- (1/20)*log(2*(.6/.35)-1)
w <- function(cost, W0=1, Wu=0.4, B=b) {
  2*(W0-Wu)/(exp(B*cost) + 1) + Wu
}

library(latex2exp)
e1 <- TeX(string = '$(0, W_0)$')
e2 <- TeX(string = '$(C_1, W_1)$')
e3 <- TeX(string = '$\\lim_{C \\rightarrow \\infty} W = W_u$')

p2 <- ggplot(fakedat[c(1,3),], aes(x=cost, y=waste)) +
  stat_function(fun = w) +
  geom_hline(yintercept = c(1, 0.4), linetype = 'dashed', size = 1.5, color = 'orange') + 
  geom_point(size = 3) +
  geom_text(x = 2, y = 1, label = e1, hjust = 0, vjust = 0) +
  geom_text(x = 22, y = 0.75, label = e2, hjust = 0, vjust = 0) +
  geom_text(x = 75, y = 0.45, label = e3, hjust = 0, vjust = 0) +
  #geom_text(data = data.frame(cost = c(0, 20, 60), waste = c(1, 0.75, 0.5), lab = c()), aes(label = lab), parse = TRUE) +
  
  scale_x_continuous(name = 'cost', limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(name = 'waste rate', limits = c(0, 1.1), expand = c(0, 0)) +
  
  theme_classic() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())

ggsave('~/google_drive/SESYNC Food Waste/Results_Figures/makingcostcurve2.png', p2, height=4, width=4, dpi=300)
