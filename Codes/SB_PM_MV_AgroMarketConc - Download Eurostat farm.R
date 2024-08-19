#####################################################################################
########## Retrieving data on European farms at NUTS-2 level from Eurostat ##########
#####################################################################################

########## Packages
library(eurostat)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggpmisc)
'%notin%' <- Negate('%in%')

##### Setting working directory
setwd("H:/Il mio Drive/Agriculture/PaperSpringer_Joao_May2024")

##### Download setting
Input_Organic <- "TOTAL"
Input_Unit <- c("EUR","HLD","HA")
Input_UAA <- "TOTAL"
Grid <- expand.grid(Input_Organic,Input_Unit,Input_UAA)
d <- vector(mode = "list", length = dim(Grid)[1])
for (r in 1:dim(Grid)[1]) {
  cat(paste0("Combination: ",r," of ",dim(Grid)[1],"\n"))
  d[[r]] <- get_eurostat("ef_m_org",
                         time_format = "num",
                            filters = list(
                              uaarea = Grid$Var3[r],
                              unit = Grid$Var2[r],
                              organic = Grid$Var1[r]
                            ),
                            cache = FALSE)
  
}
Agro_NUTS2 <- bind_rows(d)

##### Management at NUTS 2010 codify
Agro_NUTS2_red <- Agro_NUTS2 %>%
  filter(!grepl("LT01|LT02",geo),
         !grepl("UK|IS|NO|CH|ME|MK|RS",geo)) %>%
  mutate(
    geo = case_when(
      # Slovenia
      geo %in% c("SI03") ~ "SI01",
      geo %in% c("SI04") ~ "SI02",
      # Ungheria
      geo %in% c("HU11","HU12") ~ "HU10",
      # Polonia
      geo %in% c("PL71") ~ "PL11",
      geo %in% c("PL72") ~ "PL33",
      geo %in% c("PL81") ~ "PL31",
      geo %in% c("PL82") ~ "PL32",
      geo %in% c("PL84") ~ "PL34",
      geo %in% c("PL91") ~ "PL12",
      geo %in% c("PL92") ~ "PL12",
      # Croazia
      geo %in% c("HR05","HR06","HR02") ~ "HR04",
      # Grecia
      geo %in% c("EL51") ~ "EL11",
      geo %in% c("EL52") ~ "EL12",
      geo %in% c("EL53") ~ "EL13",
      geo %in% c("EL54") ~ "EL21",
      geo %in% c("EL61") ~ "EL14",
      geo %in% c("EL62") ~ "EL22",
      geo %in% c("EL63") ~ "EL23",
      geo %in% c("EL64") ~ "EL24",
      geo %in% c("EL65") ~ "EL25",
      # Francia
      geo %in% c("FRB0") ~ "FR24",
      geo %in% c("FRC1") ~ "FR26",
      geo %in% c("FRC2") ~ "FR43",
      geo %in% c("FRD1") ~ "FR25",
      geo %in% c("FRD2") ~ "FR23",
      geo %in% c("FRE1") ~ "FR30",
      geo %in% c("FRE2") ~ "FR22",
      geo %in% c("FRF1") ~ "FR42",
      geo %in% c("FRF2") ~ "FR21",
      geo %in% c("FRF3") ~ "FR41",
      geo %in% c("FRG0") ~ "FR51",
      geo %in% c("FRH0") ~ "FR52",
      geo %in% c("FRI1") ~ "FR61",
      geo %in% c("FRI2") ~ "FR63",
      geo %in% c("FRI3") ~ "FR53",
      geo %in% c("FRJ1") ~ "FR81",
      geo %in% c("FRJ2") ~ "FR62",
      geo %in% c("FRK1") ~ "FR72",
      geo %in% c("FRK2") ~ "FR71",
      geo %in% c("FRL0") ~ "FR82",
      geo %in% c("FRM0") ~ "FR83",
      geo %in% c("FRY1") ~ "FR91",
      geo %in% c("FRY2") ~ "FR92",
      geo %in% c("FRY3") ~ "FR93",
      geo %in% c("FRY4") ~ "FR94",
      geo %in% c("FRY5") ~ "FRA5",
      # Altri
      TRUE ~ geo
    )
  ) %>%
  group_by(freq,farmtype,so_eur,uaarea,organic,unit,geo,time) %>%
  summarise(values = sum(values, na.rm=T)) %>%
  ungroup() %>%
  group_by(freq,farmtype,so_eur,uaarea,organic,unit) %>%
  mutate(values = case_when(time == 2020 & geo == "IE01" ~ values[time == 2020 & geo == "IE04"] + 0.50*values[time == 2020 & geo == "IE06"],
                            time == 2020 & geo == "IE02" ~ values[time == 2020 & geo == "IE05"] + 0.50*values[time == 2020 & geo == "IE06"],
                            TRUE ~ values)) %>%
  ungroup()

##### Add labels
Agro_NUTS2 <- Agro_NUTS2_red %>%
  select(-freq)
Agro_NUTS2_lab <- eurostat::label_eurostat(x = Agro_NUTS2, fix_duplicated = TRUE)
Agro_NUTS2_lab <- Agro_NUTS2_lab %>%
  select(farmtype_lab = farmtype, so_eur_lab = so_eur, uaarea_lab = uaarea,
         organic_lab = organic, unit_lab = unit, geo_lab = geo)

Agro_NUTS2 <- bind_cols(Agro_NUTS2,Agro_NUTS2_lab)

Agro_NUTS2 <- Agro_NUTS2 %>%
  mutate(n = nchar(geo)) %>%
  filter(n == 4,
         ## Exclude non-continental areas
         geo %notin% c("FR91","FR92","FR93","FR94","FRA5","PT20","PT30","ES63","ES70")) %>%
  select(farmtype_lab,farmtype,so_eur_lab,so_eur,uaarea_lab,uaarea,organic_lab,organic,
         unit_lab,unit,geo_lab,geo,time,values)

##### Computing Gini index cumulated w.r.t. standard output classes
Agro_NUTS2_wide <- Agro_NUTS2 %>%
  filter(so_eur_lab %notin% c("Total")) %>%
  mutate(so_eur = factor(so_eur,
                         levels = c("KE0","KE_GT0_LT2","KE2-3","KE4-7","KE8-14","KE15-24","KE25-49",
                                    "KE50-99","KE100-249","KE250-499","KE_GE500"),
                         ordered = TRUE)) %>%
  select(-c(unit_lab)) %>%
  pivot_wider(names_from = unit, values_from = values) %>%
  group_by(farmtype_lab,farmtype,uaarea_lab,uaarea,organic_lab,organic,geo_lab,geo,time) %>%
  arrange(so_eur) %>%
  mutate(
    Gini_SO = Gini::gini(x = HLD, y = EUR)*100,
    Gini_HA = Gini::gini(x = HLD, y = HA)*100,
  ) %>%
  ungroup()

##### Retrieve geographical information NUTS 2010
g10 <- get_eurostat_geospatial(year = "2010", nuts_level = "2")
g10 <- g10 %>%
  select(geo,geometry)
Agro_NUTS2_wide <- left_join(x = Agro_NUTS2_wide, y = g10, by = "geo")

##### Convert to sf object
Agro_NUTS2_sf <- Agro_NUTS2_wide %>%
  st_as_sf()

##### Export NUTS-2 data.frame
save(Agro_NUTS2_sf,g10,file = "Eurostat_Agro_NUTS2_2010.RData")

##### Mapping
Agro_NUTS2_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Gini_HA)) + 
  facet_wrap(~ time)

Agro_NUTS2_sf %>%
  ggplot(mapping = aes(x = Gini_SO, y = Gini_HA, col = as.factor(time))) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_viridis_d(name = "Year")



#######################################
########## NUTS1 Aggregation ##########
#######################################
g1_10 <- get_eurostat_geospatial(year = "2010", nuts_level = "1")
g1_10 <- g1_10 %>%
  select(geo,geo_lab = NUTS_NAME,geometry) %>%
  filter(geo %notin% c("EU2"))

##### Aggregating to NUTS-1 level
Agro_NUTS1 <- Agro_NUTS2 %>%
  mutate(geo = str_sub(geo, start = 1, end = 3)) %>%
  group_by(farmtype_lab,farmtype,so_eur_lab,so_eur,uaarea_lab,uaarea,organic_lab,organic,unit,geo,time) %>%
  summarise(values = sum(values,na.rm=T)) %>%
  ungroup()

##### Computing Gini index cumulated w.r.t. standard output classes
Agro_NUTS1 <- Agro_NUTS1 %>%
  filter(so_eur_lab %notin% c("Total"),
         geo %in% unique(g1_10$geo)) %>%
  mutate(so_eur = factor(so_eur,
                         levels = c("KE0","KE_GT0_LT2","KE2-3","KE4-7","KE8-14","KE15-24","KE25-49",
                                    "KE50-99","KE100-249","KE250-499","KE_GE500"),
                         ordered = TRUE)) %>%
  pivot_wider(names_from = unit, values_from = values) %>%
  group_by(farmtype_lab,farmtype,uaarea_lab,uaarea,organic_lab,organic,geo,time) %>%
  arrange(so_eur) %>%
  mutate(
    Gini_SO = Gini::gini(x = HLD, y = EUR)*100,
    Gini_HA = Gini::gini(x = HLD, y = HA)*100,
  ) %>%
  ungroup()

Agro_NUTS1 <- left_join(x = Agro_NUTS1, y = g1_10, by = "geo")

Agro_NUTS1_sf <- Agro_NUTS1 %>%
  st_as_sf() %>%
  select(farmtype_lab,farmtype,so_eur_lab,so_eur,uaarea_lab,uaarea,organic_lab,organic,
         geo_lab,geo,time,EUR,HA,HLD,Gini_SO,Gini_HA,geometry)

##### Export NUTS-1 data.frame
save(Agro_NUTS1_sf,g1_10,file = "Eurostat_Agro_NUTS1_2010.RData")

##### Mapping
Agro_NUTS1_sf %>%
  filter(farmtype_lab %in% c("Total"), uaarea_lab %in% c("Total"),
         organic_lab %in% c("Total"),so_eur %in% c("KE0")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = Gini_SO)) + 
  facet_wrap(~ time)

Agro_NUTS1_sf %>%
  ggplot(mapping = aes(x = Gini_SO, y = Gini_HA, col = as.factor(time))) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_viridis_d(name = "Year")




