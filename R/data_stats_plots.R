### -----------\
# import necessary libraries
### -----------\
library(ggalluvial)
library(tidyverse)
library(googlesheets4)
library(cowplot)
library(MetBrewer)
library(magrittr)
library(here)
### -----------------\

# import data sheet 
gs = googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1GBaYiy6crk7h6oebUCoiXRuu0wvXJmgAaJ_5DB5HPrQ/edit?usp=sharing", sheet = 1) |> 
  dplyr::filter(!is.na(dataset))

# write_csv(gs, here("assets/global_groundwater_data_sheet.csv"))


### -----------\
# basic summaries
### -----------\

# how many datasets?
gs |> pull(how_counted) |> sum()
#> 144

# how many datasets per system type?
gs |> dplyr::filter(how_counted >= 1) |> pull(system) |> table()


# how many datasets per system type AND order?
gs |> dplyr::filter(how_counted >= 1) |> 
  group_by(system, order) |> 
  summarize(
    count = unique(dataset, na.rm = T) |> length()
  )

### -----------\
# pie charts of metadata distributions per system class
### -----------\

    ############
    ### FORM ###
    ############
for (vii in unique(gs$system)) {
  # vii = unique(gs$system)[6]
  
  temp_gs = gs |> filter(system == vii)
  summary_df = temp_gs$form |> table() |> as.data.frame() |> set_colnames(c("ID", "count")) |> 
    mutate(prop = count/sum(count, na.rm = T))
  
  col_table = data.frame(ID = c("Point", "Polygon", "Polyline", "Raster", "Tabular"),
                         cols = met.brewer(name = "Hokusai2", n = 5, type = "continuous"))
  
  plot_table = merge(col_table, summary_df, by = "ID", all = T)
  plot_table$prop[is.na(plot_table$prop)] = 0
  plot_table
  
  ggplot(summary_df, aes(x="", y=prop, fill=ID)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    scale_fill_manual(values = setNames(plot_table$cols, plot_table$ID))

  ggsave(plot = last_plot(),
         filename = paste0(here("plots/pie_chart_FORM_"), vii, ".png"),
         width = 5,
         height = 5,
         bg = "transparent")
  
}

    ############
    ### TYPE ###
    ############
for (vii in unique(gs$system)) {
  # vii = unique(gs$system)[4]
  
  temp_gs = gs |> filter(system == vii)
  
  summary_df = temp_gs$type |> table() |> as.data.frame() |> set_colnames(c("ID", "count")) |> 
    mutate(prop = count/sum(count, na.rm = T))
  
  col_table = data.frame(ID = c("Zonal", "Static", "Series", "Record"),
                         cols = met.brewer(name = "VanGogh3", n = 4, type = "continuous"))
  
  plot_table = merge(col_table, summary_df, by = "ID", all = T)
  plot_table$prop[is.na(plot_table$prop)] = 0
  plot_table
  
  ggplot(summary_df, aes(x="", y=prop, fill=ID)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    scale_fill_manual(values = setNames(plot_table$cols, plot_table$ID))
  
  ggsave(plot = last_plot(),
         filename = paste0(here("plots/pie_chart_TYPE_"), vii, ".png"),
         width = 5,
         height = 5,
         bg = "transparent")
  
}


    ##############
    ### METHOD ###
    ##############
for (vii in unique(gs$system)) {
  # vii = unique(gs$system)[7]

  temp_gs = gs |> filter(system == vii)
  
  temp_gs$method[temp_gs$method == "In situ, Model"] = "Model"
  temp_gs$method[temp_gs$method == "In situ, Remote sensing, Model"] = "Model"
  temp_gs$method[temp_gs$method == "Remote sensing, Model"] = "Model"
  
  summary_df = temp_gs$method |> table() |> as.data.frame() |> set_colnames(c("ID", "count")) |> 
    mutate(prop = count/sum(count, na.rm = T))
  
  col_table = data.frame(ID = c("In situ", "Remote sensing", "Model", "Other"),
                         cols = met.brewer(name = "Morgenstern", n = 8, type = "continuous")[5:8])
  
  plot_table = merge(col_table, summary_df, by = "ID", all = T)
  plot_table$prop[is.na(plot_table$prop)] = 0
  plot_table
  
  ggplot(summary_df, aes(x="", y=prop, fill=ID)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    scale_fill_manual(values = setNames(plot_table$cols, plot_table$ID))
  
  ggsave(plot = last_plot(),
         filename = paste0(here("plots/pie_chart_METHOD_"), vii, ".png"),
         width = 5,
         height = 5,
         bg = "transparent")
  
}


### -----------\
# bar chart of spatial resolution, coloured by system type
### -----------\
scale_lookup = googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1GBaYiy6crk7h6oebUCoiXRuu0wvXJmgAaJ_5DB5HPrQ/edit?usp=sharing", sheet = 2)

gs_scale = base::merge(x= gs, y= scale_lookup, by.x= "spatial", by.y = "spatial")

table(gs_scale$scale_class)

scale_class_order = data.frame(plot_order = seq(1:7),
                               scale_class = c("Very_fine", "Fine", "Moderate", "Coarse",
                                               "Subnational", "Nation", "NA"))

gs_scale = merge(x = gs_scale, y = scale_class_order, by= "scale_class")

scale_plot_df = gs_scale |> group_by(plot_order, system) |> 
  summarize(
    count = unique(dataset, na.rm = T) |> length()
  )
scale_plot_df

ggplot(scale_plot_df, aes(fill=system, y=count, x=plot_order)) + 
  geom_bar(position="stack", stat="identity") +
  # cowplot::theme_nothing() +
  cowplot::theme_minimal_grid() +
  theme(legend.position="none") +
  coord_cartesian(expand = 0) +
  scale_fill_met_d(name = "Manet", direction = 1, n = 8, override.order = FALSE)


ggsave(plot = last_plot(),
       # filename = here("plots/scale_distribution.png"),
       filename = here("plots/scale_distribution_wgrid.png"),
       width = 8,
       height = 5,
       # height = 5,
       bg = "transparent")



### -----------\
# map of authorship
### -----------\
library(rnaturalearthdata)
library(rnaturalearth)
library(terra)
library(magrittr)
library(MetBrewer)
library(tmap)
library(sf)
library(scico)

# import country outlines
natearth = rnaturalearth::ne_countries(scale = 110) |> vect()

# determine frequency of authorship per country
gs_authorship = gs |> filter(order != "2_Implicit")

nat_count = c(gs_authorship$iso_1, gs_authorship$iso_2) |> table() |> 
  as.data.frame() |> set_colnames(c("ISO", "count"))

natearth = merge(x = natearth, 
                 y = nat_count,
                 by.x = "adm0_iso",
                 by.y = "ISO",
                 all.x = TRUE)

# import a raster of groundwater depletion
gwd = terra::rast("D:/Geodatabase/Groundwater/Wada_depletion/GWDepletion2000_0d5harm.tif")
gwd = terra::project(x = gwd, y = "+proj=robin", method = "near")
gwd[gwd < 30] = NA
gwd[gwd > 30] = 1

map =  
  tm_shape(natearth |> st_as_sf(), projection = "+proj=robin") +
  tm_fill(col = "count", 
          palette = met.brewer(name = "Hokusai2", n = 5, "continuous", direction = 1),
          # breaks = c(1, 30),
          n = 5, 
          style = "jenks",
          colorNA = "grey90") +
  tm_borders(col = "black", lwd = 1) +
  tm_shape(gwd) +
  tm_raster(palette = "red") +
  tm_shape(natearth |> st_as_sf()) + tm_borders(col = "black", lwd = 0.2) +
  tm_layout(legend.show = T, legend.frame = F, frame = F, bg.color = "transparent")
map

tmap_save(map, here("plots/country_representation_map.png"), 
          dpi = 400, units = "in",
          device = png, bg = "transparent", type = 'cairo')



### -----------\
# time series analysis
### -----------\

# import gs sheet again and format to replace strings with values
gs = googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1GBaYiy6crk7h6oebUCoiXRuu0wvXJmgAaJ_5DB5HPrQ/edit?usp=sharing", sheet = 1) |> 
  dplyr::filter(!is.na(author))
gs_ts = gs |> dplyr::filter(!is.na(begin)) |> dplyr::filter(!is.null(begin))
gs_ts$end[gs_ts$end == "present"] = 2024
end_vals = lapply(gs_ts$end, function(x) if (is.null(x)) NA else x)
gs_ts$end = unlist(end_vals)
gs_ts = gs_ts |> dplyr::filter(!is.na(end))

# re-merge the gs_ts with the scale legend
gs_ts = merge(x= gs_ts, y= scale_lookup, by.x= "spatial", by.y = "spatial")
gs_ts = merge(x = gs_ts, y = scale_class_order, by= "scale_class")

# count years within bounds of each series dataset
ts_avail_system = tibble(year = rep(NA), system = rep(NA))
ts_avail_method = tibble(year = rep(NA), method = rep(NA))
ts_avail_type = tibble(year = rep(NA), type = rep(NA))
ts_avail_scale = tibble(year = rep(NA), scale = rep(NA))
ts_avail_order = tibble(year = rep(NA), order = rep(NA))


for (ii in 1:nrow(gs_ts)) {
  # ii = 1
  print(ii)
  
  temp_system = tibble(year = seq(gs_ts$begin[ii], (gs_ts$end[ii]-1)), system = rep(gs_ts$system[[ii]]))
  ts_avail_system = rbind(ts_avail_system, temp_system)
  
  temp_method = tibble(year = seq(gs_ts$begin[ii], (gs_ts$end[ii]-1)), method = rep(gs_ts$method[[ii]]))
  ts_avail_method = rbind(ts_avail_method, temp_method)
  
  temp_type = tibble(year = seq(gs_ts$begin[ii], (gs_ts$end[ii]-1)), type = rep(gs_ts$type[[ii]]))
  ts_avail_type = rbind(ts_avail_type, temp_type)
  
  temp_scale = tibble(year = seq(gs_ts$begin[ii], (gs_ts$end[ii]-1)), scale = rep(gs_ts$plot_order[[ii]]))
  ts_avail_scale = rbind(ts_avail_scale, temp_scale)
  
  temp_order = tibble(year = seq(gs_ts$begin[ii], (gs_ts$end[ii]-1)), order = rep(gs_ts$order[[ii]]))
  ts_avail_order = rbind(ts_avail_order, temp_order)
  
}

ts_avail_syst_plot = ts_avail_system |> group_by(year, system) |> summarise(count = n())
ts_avail_mthd_plot = ts_avail_method |> group_by(year, method) |> summarise(count = n())
    ts_avail_mthd_plot$method[ts_avail_mthd_plot$method == "In situ, Model"] = "Model"
    ts_avail_mthd_plot$method[ts_avail_mthd_plot$method == "In situ, Remote sensing, Model"] = "Model"
    ts_avail_mthd_plot$method[ts_avail_mthd_plot$method == "Remote sensing, Model"] = "Model"
ts_avail_type_plot = ts_avail_type |> group_by(year, type) |> summarise(count = n())
ts_avail_scale_plot = ts_avail_scale |> group_by(year, scale) |> summarise(count = n())
ts_avail_order_plot = ts_avail_order |> group_by(year, order) |> summarise(count = n())


# plot by system ----------------------------------------
ggplot(ts_avail_syst_plot, aes(fill=system, y=count, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 1) +
  cowplot::theme_minimal_grid() +
  # cowplot::theme_nothing() +
  # theme(legend.position="none") +
  coord_cartesian(expand = 0, xlim = c(1970, 2030)) +
  scale_fill_met_d(name = "Manet", direction = 1, n = 8, override.order = FALSE)

ggsave(plot = last_plot(),
       filename = here("plots/time_series_per_system_wgrid.png"),
       # filename = here("plots/time_series_per_system.png"),
       width = 8,
       height = 5,
       # height = 5,
       bg = "transparent")

# plot by scale ----------------------------------------
ggplot(ts_avail_scale_plot, aes(fill=as.factor(scale), y=count, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 1) +
  cowplot::theme_nothing() +
  # cowplot::theme_minimal_grid() +
  # theme(legend.position="none") +
  coord_cartesian(expand = 0, xlim = c(1970, 2030)) +
  scale_fill_met_d(name = "Tam", direction = 1, n = 7, override.order = FALSE)

ggsave(plot = last_plot(),
       filename = here("plots/time_series_per_scale.png"),
       width = 8,
       height = 5,
       # height = 5,
       bg = "transparent")

# plot by order ----------------------------------------
ts_avail_order_plot = ts_avail_order |> group_by(year, order) |> summarise(count = n())

# ts_avail_order_plot$order = factor(ts_avail_order_plot$order, levels = rev(levels(ts_avail_order_plot$order)))

ggplot(ts_avail_order_plot, aes(fill=forcats::fct_rev(order), y=count, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 1) +
  cowplot::theme_nothing() + 
  # theme(legend.position="none") +
  coord_cartesian(expand = 0, xlim = c(1970, 2030)) +
  scale_fill_manual(values = c("#4C9DA8", "#F7AF3B", "#D3391F"))

ggsave(plot = last_plot(),
       filename = here("plots/time_series_per_order.png"),
       width = 8,
       height = 5,
       # height = 5,
       bg = "transparent")


# plot by method ----------------------------------------
ggplot(ts_avail_mthd_plot, aes(fill=as.factor(method), y=count, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 1) +
  cowplot::theme_minimal_grid() + 
  # theme(legend.position="none") +
  coord_cartesian(expand = 0, xlim = c(1970, 2030)) +
  scale_fill_manual(values = met.brewer(name = "Morgenstern", n = 8, type = "continuous")[5:8])
  # scale_fill_met_d(name = "Tam", direction = 1, n = 7, override.order = FALSE)

# plot by form ----------------------------------------
ggplot(ts_avail_mthd_plot, aes(fill=as.factor(method), y=count, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 1) +
  cowplot::theme_minimal_grid() + 
  # theme(legend.position="none") +
  coord_cartesian(expand = 0, xlim = c(1970, 2030)) +
  scale_fill_manual(values = met.brewer(name = "Morgenstern", n = 8, type = "continuous")[5:8])
# scale_fill_met_d(name = "Tam", direction = 1, n = 7, override.order = FALSE)


view(gs_tw)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
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

