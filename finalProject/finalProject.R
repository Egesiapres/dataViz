# Final project
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("hrbrthemes")
install.packages("ggpubr")

library("dplyr")
library("ggplot2")
library("RColorBrewer")
library("hrbrthemes")
library("ggpubr")
import_roboto_condensed() # fix errors related to the font

# DATA MANIPULATION ----------------------------------------------------------------------------------
df15 <- read.csv("2015.csv")
df15.1 <- df15
df15.1$Standard.Error <- NULL
df15.1$Dystopia.Residual <- NULL
colnames(df15.1) <- c("Country", "Region", "Rank Position", "Score", "GDP per Capita", "Family", "Healthy Life Expectancy", "Freedom", "Corruption Perception", "Generosity")

eu <- filter(df15.1, Region == "Central and Eastern Europe" | Region == "Western Europe")
eu <- cbind(eu, "Continent" = "Europe")
am <- filter(df15.1, Region == "North America" | Region == "Latin America and Caribbean")
am <- cbind(am, "Continent" = "America")
as <- filter(df15.1, Region == "Eastern Asia" | Region == "Southeastern Asia" | Region == "Southern Asia")
as <- cbind(as, "Continent" = "Asia")
af <- filter(df15.1, Region == "Middle East and Northern Africa" | Region == "Sub-Saharan Africa")
af <- cbind(af, "Continent" = "Africa")
oc <- filter(df15.1, Region == "Australia and New Zealand")
oc <- cbind(oc, "Continent" = "Oceania")

df15.2 <- rbind(eu, am, as, af, oc)

# FinalPlot15 ---------------------------------------------------------------------------

# Which are variables that affect the most the score of a country?
# Scatter plots of each one of the variables

# GDP per capita
p1 <- ggplot(df15.2, aes(x = `GDP per Capita`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

# Family
p2 <- ggplot(df15.2, aes(x = `Family`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

# Healthy Life Expectancy
p3 <- ggplot(df15.2, aes(x = `Healthy Life Expectancy`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

# Freedom
p4 <- ggplot(df15.2, aes(x = `Freedom`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

# Corruption Perception
p5 <- ggplot(df15.2, aes(x = `Corruption Perception`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

# Generosity
p6 <- ggplot(df15.2, aes(x = `Generosity`, y = Score, color = factor(Continent))) + 
  geom_point(size = 4, alpha = 0.7) + 
  ylab(NULL) +
  scale_color_brewer(palette = "RdYlGn") +
  guides(color = guide_legend(title = "Continent")) +
  theme_set(theme_ft_rc(base_size = 9)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 2, size = 0.3),
        axis.title.x = element_text(size = 10, face = "bold", hjust = 0, margin = margin(t = 15, unit = "pt")),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))

fp1 <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2, common.legend = TRUE, legend = "right")

fp1 <- annotate_figure(fp1, top = text_grob("Factors affecting Continents Happiness Level", face = "bold", size = 18, color = "DarkGrey")) + 
  theme_ft_rc()

ggsave("finalPlot.pdf", width = 20, height = 8, units = "in", device = cairo_pdf, dpi = 1000)