#####################################################################################################################################
##########                                    Boccaletti, S., Maranzano & Viegas, M. (2024+)                               ##########
##########     "Inequality and Concentration in Farmland Production and Size: Regional Analysis for the European Union"    ##########
#####################################################################################################################################

########## Packages
library(tidyverse)
library(ggplot2)
library(sf)
library(spdep)
library(sp)
library(ggpmisc)
library(ggpubr)
library(openxlsx)
library(eurostat)
'%notin%' <- Negate('%in%')

##### Set working directory
setwd("H:/Il mio Drive/Agriculture/PaperSpringer_Joao_May2024")

##### Settings
DownloadData <- FALSE
source("~/GitHub/SB_PM_MV_AgroMarketConc/Codes/FN - MoranIStat.R", encoding = 'UTF-8')

###########################################################
########## Import agricultural data NUTS-2 level ##########
###########################################################
if (DownloadData == TRUE) {
  source("H:/Il mio Drive/Agriculture/PaperSpringer_Joao_May2024/SB_PM_MV_AgroMarketConc - Download Eurostat farm.R", encoding = 'UTF-8')
} else {
  load("Eurostat_Agro_NUTS2_2010.RData")
}

g10 <- g10 %>%
  mutate(geo_NUTS0 = substr(geo,start=1,stop=2),
         geo_NUTS1 = substr(geo,start=1,stop=3))

Countries <- g10 %>% 
  filter(geo %in% unique(Agro_NUTS2_sf$geo)) %>%
  select(geo_NUTS0,geometry) %>%
  group_by(geo_NUTS0) %>% 
  summarise()

Agro_NUTS2_sf <- Agro_NUTS2_sf %>%
  mutate(geo_NUTS0 = substr(geo,start=1,stop=2),
         geo_NUTS1 = substr(geo,start=1,stop=3))


###########################################################
########## Import agricultural data NUTS-2 level ##########
###########################################################
table(Agro_NUTS2_sf$so_eur_lab,Agro_NUTS2_sf$so_eur)
table(Agro_NUTS2_sf$unit)

##### Check computation
Agro_NUTS2_sf %>%
  filter(farmtype %in% c("FT53_SO"), uaarea_lab %in% c("Total"), organic_lab %in% c("Total"),
         geo %in% c("ES13"), time %in% c(2013)) %>%
  View()

Agro_NUTS2_sf %>%
  filter(farmtype %in% c("FT21_SO"), uaarea_lab %in% c("Total"), organic_lab %in% c("Total"),
         geo %in% c("FR42"), so_eur %in% c("KE_GT0_LT2")) %>%
  View()

# Agro_NUTS2_sf %>%
#   filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
#          organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
#   group_by(farmtype_lab,farmtype,uaarea_lab,uaarea,organic_lab,organic,geo_lab,geo,geometry) %>%
#   mutate(Gini2_2010 = Gini2[time == 2010],
#          Delta_Gini2_2010 = Gini2 - Gini2_2010) %>%
#   filter(time %notin% c(2010)) %>%
#   ggplot() + 
#   geom_sf(mapping = aes(fill = Delta_Gini2_2010)) +
#   scale_fill_gradient2(mid = 0) + 
#   facet_wrap(~ time)

##### Mapping
Agro_NUTS2_sf %>%
  mutate(Gini_ratio = Gini_SO/Gini_HA) %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Gini_ratio)) + 
  facet_wrap(~ time) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 1)



##### Figure 1
# 1990-2007: Key variables by legal status of holding, size of farm (UAA) and NUTS 2 regions
Data1 <- get_eurostat("ef_ov_kvaa",
                      time_format = "num",
                      filters = list(
                        snglhldr = c("TOTAL"),
                        agrarea = c("TOTAL"),
                        variable = c("AGRAREA_HA","HOLD_HOLD")
                      ),
                      cache = FALSE
)
Data1 <- Data1 %>%
  select(variable,geo,time,values) %>%
  mutate(variable = case_when(variable %in% c("AGRAREA_HA") ~ "HA",
                              variable %in% c("HOLD_HOLD") ~ "HLD",
                              TRUE ~ variable)) %>%
  filter(nchar(geo) == 2) %>%
  pivot_wider(names_from = variable, values_from = values)

# 2010-2020: Farm indicators by legal status of the holding, utilised agricultural area, type and economic size of the farm and NUTS 2 region
Data2 <- get_eurostat("ef_m_farmleg",
                      time_format = "num",
                      filters = list(
                        uaarea = c("TOTAL"),
                        farmtype = c("TOTAL"),
                        leg_form = c("TOTAL"),
                        so_eur = c("TOTAL"),
                        unit = c("HA","HLD")
                      ),
                      cache = FALSE
)
Data2 <- Data2 %>%
  select(unit,geo,time,values) %>%
  filter(nchar(geo) == 2) %>%
  pivot_wider(names_from = unit, values_from = values)

# Merge/stacking
Data <- bind_rows(Data1,Data2)

# Plot
Data_plot <- Data %>%
  mutate(HA_HLD = HA/HLD) %>%
  pivot_longer(cols = c(HA,HLD,HA_HLD),names_to = "vars",values_to = "values") %>%
  filter(time %in% c(1990,2020)) %>%
  pivot_wider(names_from = time,values_from = values) %>%
  filter(!is.na(`1990`) & !is.na(`2020`)) %>%
  pivot_longer(cols = c(`1990`,`2020`),names_to = "time",values_to = "values") %>%
  pivot_wider(names_from = vars,values_from = values)

p1 <- Data_plot %>%
  ggplot(mapping = aes(x = geo, y = HA_HLD, fill = as.factor(time))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_grey("") + 
  theme_bw() + 
  theme(legend.position = "bottom",plot.title = element_text(size = 16, face = "bold")) + 
  labs(x = "", y = "Hectares per farm", title = "Average hectares per farm in selected European countries")
p2 <- Data_plot %>%
  ggplot(mapping = aes(x = geo, y = HLD, fill = as.factor(time))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_grey("") + 
  theme_bw() + 
  theme(legend.position = "bottom",plot.title = element_text(size = 16, face = "bold")) + 
  labs(x = "", y = "Number of farms", title = "Number of farms in selected European countries")
p_farms <- ggpubr::ggarrange(p1,p2,ncol = 1,  nrow = 2,common.legend = TRUE,  legend="bottom")
p_farms <- ggpubr::annotate_figure(p = p_farms,top = text_grob("Figure 1: Land concentration of farms within the European Union",
                                                               face = "bold",size = 25))
ggexport(p_farms,width = 1800, height = 1200, res = 150, filename = "FarmStructure.png")





##### Figure 2
p3a <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total")) %>%
  group_by(geo_NUTS0,time) %>%
  summarise(Min = min(Gini_SO,na.rm = T),
            Mean = mean(Gini_SO,na.rm = T),
            Max = max(Gini_SO,na.rm = T)) %>%
  pivot_longer(cols = 3:5, names_to = "Stat", values_to = "Value") %>%
  mutate(Stat = factor(Stat,levels = c("Min","Mean","Max"),labels = c("Min","Mean","Max"),
                       ordered = T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  # geom_sf(data = Countries, linewidth = 1.05, show.legend=FALSE, alpha=0,color="#000000") + 
  facet_grid(rows = vars(Stat), cols = vars(time)) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 50) + 
  labs(x = "", y = "",
       title = "Production (standard output)") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14)) + 
  theme_bw()
p3b <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total")) %>%
  group_by(geo_NUTS0,time) %>%
  summarise(Min = min(Gini_HA,na.rm = T),
            Mean = mean(Gini_HA,na.rm = T),
            Max = max(Gini_HA,na.rm = T)) %>%
  pivot_longer(cols = 3:5, names_to = "Stat", values_to = "Value") %>%
  mutate(Stat = factor(Stat,levels = c("Min","Mean","Max"),labels = c("Min","Mean","Max"),
                       ordered = T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  # geom_sf(data = Countries, linewidth = 1.05, show.legend=FALSE, alpha=0,color="#000000") + 
  facet_grid(rows = vars(Stat), cols = vars(time)) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 50) + 
  labs(x = "", y = "",
       title = "Farmland (hectares)") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14)) + 
  theme_bw()
p3 <- ggpubr::ggarrange(p3a,p3b,ncol = 2,common.legend = T,legend = "bottom")
p3 <- annotate_figure(p = p3, top = text_grob("Figure 2: Maps of descriptive stats for Gini index from 2010 to 2020 in Europe",
                                              face = "bold", size = 20))
ggexport(p3,width = 2400, height = 1200, res = 150, filename = "Maps_Descr_Gini.png")


##### Figure 3
p1 <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
  select(time,'Gini for production' = Gini_SO,'Gini for farmland' = Gini_HA) %>%
  pivot_longer(cols = c('Gini for production','Gini for farmland'), names_to = "Gini", values_to = "Value") %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  geom_sf(data = Countries, linewidth = 1.05, show.legend=FALSE, alpha=0,color="#000000") + 
  facet_grid(rows = vars(Gini), cols = vars(time)) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 50)  + 
  theme_bw() + 
  labs(x = "", y = "",
       title = "Figure 3: Maps of Gini index for production and farmland from 2010 to 2020 in Europe") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14))
ggexport(p1,width = 1800, height = 1200, res = 150, filename = "Maps_Gini.png")







##### Figure 4
AbsChange <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total"),
         time %in% c(2010,2020)) %>%
  select(geo,geo_NUTS0,time,Gini_SO,Gini_HA) %>%
  group_by(geo,geo_NUTS0) %>%
  summarise('Production' = Gini_SO[time == 2020] - Gini_SO[time == 2010],
            'Farmland' = Gini_HA[time == 2020] - Gini_HA[time == 2010]) %>%
  pivot_longer(cols = c("Production","Farmland"), names_to = "Stat", values_to = "Value")

p4 <- AbsChange %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  geom_sf(data = Countries, linewidth = 1.05, show.legend=FALSE, alpha=0,color="#000000") + 
  facet_wrap(~ Stat, ncol = 2) + 
  scale_fill_gradient2("Abs. change",mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 0)  + 
  theme_bw() + 
  labs(x = "", y = "",
       title = "Figure 4: Maps of raw change in Gini index for production and farmland from 2010 to 2020 in Europe") + 
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14),legend.position = "right")
ggexport(p4,width = 2400, height = 1200, res = 150, filename = "Maps_AbsChange_Gini.png")

p5 <- AbsChange %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  ggplot(mapping = aes(x = `Production`, y = `Farmland`)) + 
  geom_point(size = 3,
             # mapping = aes(col = geo_NUTS0)
             ) + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlim(-20,+20) + 
  ylim(-20,+20) +
  # scale_color_viridis_d(name = "Year") + 
  labs(x = "Raw change in Gini index for production", y = "Raw change in Gini index for farmland",
       title = "Correlation between raw changes in Gini index for production and farmland") + 
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggexport(p5,width = 2400, height = 1200, res = 150, filename = "Corr_AbsChange_Gini.png")


##### Figure 5
RelChange <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total"),
         time %in% c(2010,2020)) %>%
  select(geo,geo_NUTS0,time,Gini_SO,Gini_HA) %>%
  group_by(geo,geo_NUTS0) %>%
  summarise(
    'Production' = (Gini_SO[time == 2020] - Gini_SO[time == 2010])/Gini_SO[time == 2010]*100,
    'Farmland' = (Gini_HA[time == 2020] - Gini_HA[time == 2010])/Gini_HA[time == 2010]*100
  ) %>%
  pivot_longer(cols = c("Production","Farmland"), names_to = "Stat", values_to = "Value")

p6 <- RelChange %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  geom_sf(data = Countries, linewidth = 1.05, show.legend=FALSE, alpha=0,color="#000000") + 
  facet_wrap(~ Stat, ncol = 2) + 
  scale_fill_gradient2("Rel change",mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 0)  + 
  theme_bw() + 
  labs(x = "", y = "",
       title = "Figure 5: Maps of relative change in Gini index for production and farmland from 2010 to 2020 in Europe") + 
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14),legend.position = "right")
ggexport(p6,width = 2400, height = 1200, res = 150, filename = "Maps_RelChange_Gini.png")



##### Figure 6
p2 <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total")) %>%
  ggplot(mapping = aes(x = Gini_SO, y = Gini_HA)) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlim(20,100) + 
  ylim(20,100) +
  # scale_color_viridis_d(name = "Year") + 
  facet_wrap(~time, ncol = 4)  + 
  theme_bw() + 
  labs(x = "Gini index for production", y = "Gini index for farmland",
       title = "Figure 6: Correlation between Gini index for production and farmland") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),strip.text = element_text(size = 16),
        axis.title = element_text(size = 14))
ggexport(p2,width = 1800, height = 1200, res = 150, filename = "Correlation_Gini.png")


##### Figure 7
p7 <- RelChange %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  ggplot(mapping = aes(x = `Production`, y = `Farmland`)) + 
  geom_point(size = 3,
             # mapping = aes(col = geo_NUTS0)
  ) + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlim(-30,+70) + 
  ylim(-30,+70)  + 
  theme_bw() +
  # scale_color_viridis_d(name = "Year") + 
  labs(x = "Rel. change in Gini index for production", y = "Rel. change in Gini index for farmland",
       title = "Figure 7: Correlation between relative changes in Gini index for production and farmland") + 
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggexport(p7,width = 2400, height = 1200, res = 150, filename = "Corr_RelChange_Gini.png")




















Agro_NUTS2_sf %>%
  mutate(Gini_ratio = Gini_SO/Gini_HA,
         Gini_diff = Gini_SO - Gini_HA) %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
  select(time,Gini_diff) %>%
  pivot_longer(cols = c(Gini_diff), names_to = "Gini", values_to = "Value") %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Value)) + 
  facet_grid(rows = vars(Gini), cols = vars(time)) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 0)
  

Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),
         organic_lab %in% c("Total")) %>%
  ggplot(mapping = aes(x = Gini_SO, y = Gini_HA, col = as.factor(time))) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_viridis_d(name = "Year")

c(sqrt(0.53),sqrt(0.56),sqrt(0.64),sqrt(0.64))



Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28")) %>%
  group_by(so_eur_lab,time) %>%
  summarise(n_comp = sum(HLD)) %>%
  pivot_wider(names_from = time, values_from = n_comp)

Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),
         grepl("IT",geo)) %>%
  group_by(so_eur_lab,time) %>%
  summarise(n_comp = sum(HA,na.rm=T)) %>%
  pivot_wider(names_from = time, values_from = n_comp)

Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),
         # grepl("From 100 000 to 249 999 euros",so_eur_lab)
         ) %>%
  mutate(NUTS0 = substr(geo,start=1,stop=2)) %>%
  group_by(NUTS0,time) %>%
  summarise(n_comp = sum(HLD,na.rm=T)) %>%
  pivot_wider(names_from = time, values_from = n_comp) %>%
  View()

Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),
         grepl("IT",geo)) %>%
  group_by(so_eur_lab,time) %>%
  summarise(n_comp = sum(HA,na.rm=T)/sum(HLD,na.rm = T)) %>%
  pivot_wider(names_from = time, values_from = n_comp)

Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28")) %>%
  group_by(geo,time) %>%
  summarise(n_comp = sum(HA,na.rm=T)/sum(HLD,na.rm = T)) %>%
  pivot_wider(names_from = time, values_from = n_comp) %>%
  View()

Agro_NUTS2_sf %>%
  # as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28")) %>%
  group_by(geo,time) %>%
  summarise(n_comp = sum(HA,na.rm=T)/sum(HLD,na.rm = T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = n_comp)) + 
  facet_wrap(~ time) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 10)

Agro_NUTS2_sf %>%
  # as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28")) %>%
  group_by(geo,time) %>%
  summarise(n_comp = sum(EUR,na.rm=T)/sum(HLD,na.rm = T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = n_comp)) + 
  facet_wrap(~ time) + 
  scale_fill_gradient2(mid = "#FFFFFF",low = "#00FF00",high = "#FF0000",
                       midpoint = 10)

Agro_NUTS2_sf %>%
  as_tibble() %>%
  filter(farmtype_lab %in% c("Total"), organic_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),
         grepl("IT",geo)
         ) %>%
  group_by(so_eur_lab,time) %>%
  summarise(n_comp = sum(EUR,na.rm=T)/sum(HA,na.rm = T)) %>%
  pivot_wider(names_from = time, values_from = n_comp)






TabStats_SO <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total")) %>%
  select(geo_NUTS0,time,Gini = Gini_SO) %>%
  group_by(geo_NUTS0,time) %>%
  summarise(
    Min = min(Gini,na.rm = T),
    Mean = mean(Gini,na.rm = T),
    Max = max(Gini,na.rm = T),
    SD = sd(Gini,na.rm = T)
  ) %>%
  pivot_longer(cols = c(Min,Mean,Max,SD), names_to = "Stat",values_to = "Value") %>%
  pivot_wider(names_from = c(Stat,time), values_from = Value)
st_geometry(TabStats_SO) <- NULL

TabStats_HA <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total")) %>%
  select(geo_NUTS0,time,Gini = Gini_HA) %>%
  group_by(geo_NUTS0,time) %>%
  summarise(
    Min = min(Gini,na.rm = T),
    Mean = mean(Gini,na.rm = T),
    Max = max(Gini,na.rm = T),
    SD = sd(Gini,na.rm = T)
  ) %>%
  pivot_longer(cols = c(Min,Mean,Max,SD), names_to = "Stat",values_to = "Value") %>%
  pivot_wider(names_from = c(Stat,time), values_from = Value)
st_geometry(TabStats_HA) <- NULL

########## Export Excel
wb <- createWorkbook("Descrittive per paese e anno.xlsx")
addWorksheet(wb,"Standard Output")
writeData(wb, sheet = "Standard Output", TabStats_SO, colNames = T)
addWorksheet(wb,"Hectares")
writeData(wb, sheet = "Hectares", TabStats_HA, colNames = T)
saveWorkbook(wb,"Descrittive per paese e anno.xlsx",overwrite = T)




##### Spatial autocorrelation
# https://cran.r-project.org/web/packages/adespatial/vignettes/tutorial.html
# https://rpubs.com/erikaaldisa/spatialweights or https://spatialanalysis.github.io/lab_tutorials/Contiguity_Spatial_Weights.html#r-packages-used

Data_t <- Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         geo %notin% c("EU28"),so_eur %in% c("KE0"),
         organic_lab %in% c("Total"))
Data_t_10 <- Data_t %>%
  filter(
    time == 2010,
    !is.na(Gini_HA),!is.na(Gini_SO),
    !sf::st_is_empty(Data_t)
  )
coords_10 <- st_coordinates(st_centroid(Data_t_10))
colnames(coords_10) <- c("Longitude","Latitude")
contnb_q_10 <- spdep::poly2nb(Data_t_10, queen = TRUE, row.names = Data_t_10$geo)
contnb_q_listw_10 <- spdep::nb2listw(neighbours = contnb_q_10, style = "W", zero.policy = TRUE)
contmat_q_10 <- nb2mat(contnb_q_10, zero.policy = TRUE)
# # Mapping
# plot(x = sf:::as_Spatial(Data_t_10), border = 'black', col="white", lwd=2)
# plot(contnb_q, coords = coords, pch = 19, cex = 0.8, add = TRUE, col = 'red')

Data_t_13 <- Data_t %>%
  filter(
    time == 2013,
    !is.na(Gini_HA),!is.na(Gini_SO),
    !sf::st_is_empty(Data_t)
  )
coords_13 <- st_coordinates(st_centroid(Data_t_13))
colnames(coords_13) <- c("Longitude","Latitude")
contnb_q_13 <- spdep::poly2nb(Data_t_13, queen = TRUE, row.names = Data_t_13$geo)
contnb_q_listw_13 <- spdep::nb2listw(neighbours = contnb_q_13, style = "W", zero.policy = TRUE)
contmat_q_13 <- nb2mat(contnb_q_13, zero.policy = TRUE)
# # Mapping
# plot(x = sf:::as_Spatial(Data_t_13), border = 'black', col="white", lwd=2)
# plot(contnb_q, coords = coords, pch = 19, cex = 0.8, add = TRUE, col = 'red')

Data_t_16 <- Data_t %>%
  filter(
    time == 2016,
    !is.na(Gini_HA),!is.na(Gini_SO),
    !sf::st_is_empty(Data_t)
  )
coords_16 <- st_coordinates(st_centroid(Data_t_16))
colnames(coords_16) <- c("Longitude","Latitude")
contnb_q_16 <- spdep::poly2nb(Data_t_16, queen = TRUE, row.names = Data_t_16$geo)
contnb_q_listw_16 <- spdep::nb2listw(neighbours = contnb_q_16, style = "W", zero.policy = TRUE)
contmat_q_16 <- nb2mat(contnb_q_16, zero.policy = TRUE)
# # Mapping
# plot(x = sf:::as_Spatial(Data_t_16), border = 'black', col="white", lwd=2)
# plot(contnb_q, coords = coords, pch = 19, cex = 0.8, add = TRUE, col = 'red')

Data_t_20 <- Data_t %>%
  filter(
    time == 2020,
    !is.na(Gini_HA),!is.na(Gini_SO),
    !sf::st_is_empty(Data_t)
  )
coords_20 <- st_coordinates(st_centroid(Data_t_20))
colnames(coords_20) <- c("Longitude","Latitude")
contnb_q_20 <- spdep::poly2nb(Data_t_20, queen = TRUE, row.names = Data_t_20$geo)
contnb_q_listw_20 <- spdep::nb2listw(neighbours = contnb_q_20, style = "W", zero.policy = TRUE)
contmat_q_20 <- nb2mat(contnb_q_20, zero.policy = TRUE)
# # Mapping
# plot(x = sf:::as_Spatial(Data_t_20), border = 'black', col="white", lwd=2)
# plot(contnb_q, coords = coords, pch = 19, cex = 0.8, add = TRUE, col = 'red')



# Moran's I
VarName <- "Gini_SO"
Moran_SO_10 <- MoranIStat(Data = Data_t_10, VarName = VarName, ContMat_listw = contnb_q_listw_10, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2010",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_SO_13 <- MoranIStat(Data = Data_t_13, VarName = VarName, ContMat_listw = contnb_q_listw_13, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2013",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_SO_16 <- MoranIStat(Data = Data_t_16, VarName = VarName, ContMat_listw = contnb_q_listw_16, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2016",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_SO_20 <- MoranIStat(Data = Data_t_20, VarName = VarName, ContMat_listw = contnb_q_listw_20, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2020",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_SO <- ggpubr::ggarrange(plotlist = list(Moran_SO_10$P4,Moran_SO_13$P4,Moran_SO_16$P4,Moran_SO_20$P4),
                              common.legend = T, legend = "bottom")
Moran_SO <- ggpubr::annotate_figure(p = Moran_SO,top = text_grob("Spatial autocorrelation: Gini of production",
                                                                 face = "bold",size = 25))
Moran_SO <- ggpubr::ggarrange(
  Moran_SO_10$P4 + theme(plot.margin=unit(c(0,0,0,+8),"cm")),
  Moran_SO_13$P4 + theme(plot.margin=unit(c(0,+8,0,0),"cm")), 
  Moran_SO_16$P4 + theme(plot.margin=unit(c(0,0,0,+8),"cm")),
  Moran_SO_20$P4 + theme(plot.margin=unit(c(0,+8,0,0),"cm")), 
  ncol = 2,  nrow = 2,
  common.legend = TRUE,  legend="bottom" 
)
Moran_SO <- ggpubr::annotate_figure(p = Moran_SO,top = text_grob("Spatial autocorrelation: Gini index for production",
                                                                 face = "bold",size = 25)) + 
  theme(plot.margin=grid::unit(c(0,-1,0,-1), "cm"))
ggexport(Moran_SO,width = 1800, height = 1200, res = 150, filename = "SpatAutocorr_SO.png")


VarName <- "Gini_HA"
Moran_HA_10 <- MoranIStat(Data = Data_t_10, VarName = VarName, ContMat_listw = contnb_q_listw_10, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2010",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_HA_13 <- MoranIStat(Data = Data_t_13, VarName = VarName, ContMat_listw = contnb_q_listw_13, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2013",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_HA_16 <- MoranIStat(Data = Data_t_16, VarName = VarName, ContMat_listw = contnb_q_listw_16, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2016",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_HA_20 <- MoranIStat(Data = Data_t_20, VarName = VarName, ContMat_listw = contnb_q_listw_20, Labels = "geo",
                          Plot_titles = c(NA,NA,NA,"",NA), Plot_subtitles = c(NA,NA,NA,"2020",NA),
                          cols = c("red","orange","darkgreen","green"))
Moran_HA <- ggpubr::ggarrange(
  Moran_HA_10$P4 + theme(plot.margin=unit(c(0,0,0,+8),"cm")),
  Moran_HA_13$P4 + theme(plot.margin=unit(c(0,+8,0,0),"cm")), 
  Moran_HA_16$P4 + theme(plot.margin=unit(c(0,0,0,+8),"cm")),
  Moran_HA_20$P4 + theme(plot.margin=unit(c(0,+8,0,0),"cm")), 
  ncol = 2,  nrow = 2,
  common.legend = TRUE,  legend="bottom" 
)
Moran_HA <- ggpubr::annotate_figure(p = Moran_HA,top = text_grob("Spatial autocorrelation: Gini index for farmland",
                                                                 face = "bold",size = 25)) + 
  theme(plot.margin=grid::unit(c(0,-1,0,-1), "cm"))
ggexport(Moran_HA,width = 1800, height = 1200, res = 150, filename = "SpatAutocorr_HA.png")

##### Unified plot
Moran_SO <- ggpubr::ggarrange(
  Moran_SO_10$P4,
  Moran_SO_13$P4, 
  Moran_SO_16$P4,
  Moran_SO_20$P4, 
  ncol = 2,  nrow = 2,
  common.legend = TRUE,  legend="bottom" 
)
Moran_SO <- ggpubr::annotate_figure(p = Moran_SO,top = text_grob("Gini index for production",size = 20)) + 
  theme(plot.margin=grid::unit(c(0,-1,0,-1), "cm"))
Moran_HA <- ggpubr::ggarrange(
  Moran_HA_10$P4,
  Moran_HA_13$P4, 
  Moran_HA_16$P4,
  Moran_HA_20$P4, 
  ncol = 2,  nrow = 2,
  common.legend = TRUE,  legend="bottom" 
)
Moran_HA <- ggpubr::annotate_figure(p = Moran_HA,top = text_grob("Gini index for farmland",size = 20))
Morans <- ggpubr::ggarrange(Moran_SO,Moran_HA,ncol = 2,nrow = 1,common.legend = TRUE, legend = "bottom")
Morans <- ggpubr::annotate_figure(p = Morans,top = text_grob("Figure 8: Spatial autocorrelation using local Moran's index",
                                                             face = "bold",size = 25))
ggexport(Morans,width = 1800, height = 1200, res = 150, filename = "SpatAutocorr.png")


##### Global Moran's I 
MoranI <- rbind(
  # Production
  c(Moran_SO_10$MoranI_test$estimate[1], Moran_SO_13$MoranI_test$estimate[1],
    Moran_SO_16$MoranI_test$estimate[1], Moran_SO_20$MoranI_test$estimate[1]),
  c(Moran_SO_10$MoranI_test$p.value, Moran_SO_13$MoranI_test$p.value,
    Moran_SO_16$MoranI_test$p.value, Moran_SO_20$MoranI_test$p.value),
  c(Moran_SO_10$MoranI_MC$p.value, Moran_SO_13$MoranI_MC$p.value,
    Moran_SO_16$MoranI_MC$p.value, Moran_SO_20$MoranI_MC$p.value),
  # Farmland
  c(Moran_HA_10$MoranI_test$estimate[1], Moran_HA_13$MoranI_test$estimate[1],
    Moran_HA_16$MoranI_test$estimate[1], Moran_HA_20$MoranI_test$estimate[1]),
  c(Moran_HA_10$MoranI_test$p.value, Moran_HA_13$MoranI_test$p.value,
    Moran_HA_16$MoranI_test$p.value, Moran_HA_20$MoranI_test$p.value),
  c(Moran_HA_10$MoranI_MC$p.value, Moran_HA_13$MoranI_MC$p.value,
    Moran_HA_16$MoranI_MC$p.value, Moran_HA_20$MoranI_MC$p.value)
)
MoranI <- round(MoranI,4)
colnames(MoranI) <- c("2010","2013","2016","2020")
MoranI <- data.frame(Variable = c(rep("Production",3),rep("Farmland",3)),
                     Stat = c("Moran's I estimate","PV parametric","PV Monte Carlo",
                              "Moran's I estimate","PV parametric","PV Monte Carlo"),
                     MoranI)
MoranI

##### Global Moran's I spatial autocorrelogram 
Moran_SpCorr <- ggpubr::ggarrange(
  Moran_SO_10$P6 + labs(title = "2010", y = "Gini of production", x = ""),
  Moran_SO_13$P6 + labs(title = "2013", y = "", x = ""), 
  Moran_SO_16$P6 + labs(title = "2016", y = "", x = ""),
  Moran_SO_20$P6 + labs(title = "2020", y = "", x = ""), 
  Moran_HA_10$P6 + labs(title = "2010", y = "Gini of farmland"),
  Moran_HA_13$P6 + labs(title = "2013", y = ""), 
  Moran_HA_16$P6 + labs(title = "2016", y = ""),
  Moran_HA_20$P6 + labs(title = "2020", y = ""), 
  ncol = 4,  nrow = 2,
  common.legend = TRUE,  legend="bottom" 
)
Moran_SpCorr <- ggpubr::annotate_figure(p = Moran_SpCorr,top = text_grob("Figure 9: Global Moran's spatial autocorrelogram",
                                                                         face = "bold",size = 25))
ggexport(Moran_SpCorr,width = 1800, height = 1200, res = 150, filename = "MoranSpatAutocorr.png")





