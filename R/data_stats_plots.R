library(ggalluvial)
library(tidyverse)
library(googlesheets4)
library(cowplot)
library(MetBrewer)
library(magrittr)
library(here)

gs = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1TO63MRmWBPxyoLQzCj28R-qDO1Lz3zIzlsgi0ZN9_UU/edit?usp=sharing")

write_csv(gs, here("assets/global_data_review_sheet.csv"))

##
## Exploratory summarizing
##
nrow(gs)

gs |> group_by(ES_system) |> 
  summarise(freq = unique(Dataset) |> length())

gs |> group_by(Specificity) |> 
  summarise(freq = unique(Dataset) |> length())

gs |> group_by(ES_system, Specificity) |> 
  summarise(freq = unique(Dataset) |> length())


##
## stacked barplot of ES_type X Data_type
##

gs_plot = gs |> group_by(ES_system, Data_type) |> 
  summarise(freq = unique(Dataset) |> length())

ggplot(gs_plot, aes(fill=Data_type, y=freq, x=ES_system)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = met.brewer(name = "Derain")[4:7]) +
  theme_minimal()

ggsave(plot = last_plot(),
       filename = "C:/Users/xande/Desktop/data_rev_figures/stacked_data_type.png",
       width = 15, height = 10, dpi = 300)

##
## Alluvial plot
##
plot_gs = gs |> 
  dplyr::group_by(Specificity, Data_type) |> 
  summarise(
    freq = unique(Dataset) |> length()
  )

unique_ES = gs |> pull(ES_system) |> unique()

# gs_iter = gs |> filter(ES_system == unique_ES[8])
# gs_iter
ggplot(data = plot_gs,
       aes(axis1 = Specificity, axis2 = Data_type,
           y = freq)) +
  # scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = Specificity), alpha = 0.9) + 
  scale_fill_manual(values = c("#DF4126","#FFB847","#4FA0AB")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  geom_stratum(colour = "black", aes(fill = as.factor(Specificity))) +
  theme_void()

ggsave(plot = last_plot(),
       filename = "C:/Users/xande/Desktop/data_rev_figures/alluvial_specificity.png",
       width = 10, height = 10, dpi = 300)

gs |> group_by(Data_type) |> 
  summarise(freq = unique(Dataset) |> length())

gs |> group_by(Specificity) |> 
  summarise(freq = unique(Dataset) |> length())

##
## Plot of datasets per year
##

gs = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1TO63MRmWBPxyoLQzCj28R-qDO1Lz3zIzlsgi0ZN9_UU/edit?usp=sharing")
gs_ts = gs |> filter(Data_type == "2_time_series")
gs_ts$End_date[gs_ts$End_date == "-"] = 2024 
gs_ts$Static_or_begin_date = unlist(gs_ts$Static_or_begin_date) |> as.numeric()
gs_ts$End_date = unlist(gs_ts$End_date) |> as.numeric()


year_vect_dir = rep(NA)
year_vect_exp = rep(NA)
year_vect_imp = rep(NA)

for (ii in 1:nrow(gs_ts)) {
  # ii = 1
  print(ii)
  
  temp_seq = seq(gs_ts$Static_or_begin_date[ii], (gs_ts$End_date[ii]-1))
  
  if (gs_ts$Specificity[ii] == "0_Groundwater") { year_vect_dir = c(year_vect_dir, temp_seq) }
  if (gs_ts$Specificity[ii] == "1_Explicit") {    year_vect_exp = c(year_vect_exp, temp_seq) }
  if (gs_ts$Specificity[ii] == "2_Implicit") {    year_vect_imp = c(year_vect_imp, temp_seq) }
  
}

year_count_d = year_vect_dir |> table() |> as.data.frame() |> set_colnames(c("year", "0_dir_count")) |> 
  mutate(year = year |> as.character() |> as.numeric())

year_count_e = year_vect_exp |> table() |> as.data.frame() |> set_colnames(c("year", "1_exp_count")) |> 
  mutate(year = year |> as.character() |> as.numeric())

year_count_i = year_vect_imp |> table() |> as.data.frame() |> set_colnames(c("year", "2_imp_count")) |> 
  mutate(year = year |> as.character() |> as.numeric())

year_count = merge(x = year_count_i, y = year_count_e, by.x = "year", by.y = "year", all.x = T)
year_count = merge(x = year_count, y = year_count_d, by.x = "year", by.y = "year", all.x = T)
year_count[is.na(year_count)] = 0

library(reshape2)
year_count_m = melt(year_count, id.vars = "year")

ggplot(year_count_m, aes(y=value, fill = variable, x=year)) + 
  geom_bar(stat="identity", #fill = "grey30", 
           width = 1, just = 0) +
  scale_fill_manual(values = c("#DF4126","#FFB847","#4FA0AB") |> rev()) +
  coord_cartesian(xlim = c(1970, 2030), clip = "off") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(plot = last_plot(),
       filename = "C:/Users/xande/Desktop/data_rev_figures/time_series_count.png",
       width = 15, height = 5, dpi = 300)

##
## Map of authorship
##
library(rnaturalearthdata)
library(rnaturalearth)
library(terra)
library(magrittr)
library(MetBrewer)
library(tmap)
library(sf)

natearth = rnaturalearth::ne_countries(scale = 110) |> vect()

nat_count = c(gs$Loc_first, gs$Loc_corr) |> table() |> 
  as.data.frame() |> set_colnames(c("ISO", "count"))

natearth = merge(x = natearth, 
                 y = nat_count,
                 by.x = "adm0_iso",
                 by.y = "ISO",
                 all.x = TRUE)

map =  
  tm_shape(natearth |> st_as_sf(), projection = "+proj=robin") +
  tm_fill(col = "count", 
          palette = met.brewer(name = "Tam", n = 5, "continuous", direction = 1),
          # breaks = c(1, 30),
          n = 5, 
          style = "jenks",
          colorNA = "grey90") +
  # tm_borders(col = "black", lwd = 1) +
  tm_shape(natearth |> st_as_sf()) + tm_borders(col = "black", lwd = 0.2) +
  tm_layout(legend.show = T, legend.frame = F, frame = F, bg.color = "transparent")
map

tmap_save(map, "C:/Users/xande/Desktop/data_rev_figures/country_count.png", 
          dpi = 400, units = "in",
          device = png, bg = "transparent", type = 'cairo')
