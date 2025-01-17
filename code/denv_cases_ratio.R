
pacman::p_load(tidyverse, readxl, geobr)

# Dados do SINAN
denvbr <- read_csv("data/denvbr20152024.csv.gz")

# Populacao CENSO
pop22 <- readxl::read_xlsx(path = "data/POP2022 CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF.xlsx", 
                           sheet = "Municípios",
                           range = "B3:H5573"
)

BR0 <- read_municipality()
BR_states <- read_state()

data <- pop22 |> transmute(
  UF,
  coduf = `COD. UF`,
  codmun7 = paste0(`COD. UF`,`COD. MUNIC`),
  codmun6 = substr(codmun7,1,6),
  pop = `POP. TOTAL`
)

aux2 <- denvbr |> 
  mutate(SOROTIPO = ifelse(is.na(SOROTIPO),SOROTIPO, paste0("DENV",SOROTIPO))) |> 
  group_by(codmun6 = as.character(ID_MN_RESI)) |> 
  mutate(casos = n()) |> 
  group_by(codmun6, SOROTIPO) |> 
  summarise(
    n = n(),
    casos = casos[1]
  ) |> 
  pivot_wider(values_from = n, names_from = SOROTIPO)

data <- data |> left_join(aux2) 

data |> 
  mutate(
    ratio = casos / pop,
    ratio = replace_na(ratio, 0),
    popsize = case_when(
      pop > 5e5 ~ "Pop >500k",
      TRUE ~ "Pop <= 500k"
    )
  ) |> 
  ggplot(aes(x = ratio, color = popsize)) + 
  geom_density() +
  scale_x_continuous(breaks = seq(0,0.8, 0.1),
                     minor_breaks = seq(0,0.8, 0.05))+
  theme_bw()

# BR_ratio_density <- 
# BR0 |> 
#   left_join(data |> 
#               mutate(
#                 ratio = casos / pop,
#                 ratio = replace_na(ratio, 0),
#                 popsize = case_when(
#                   pop > 1e5 ~ "Pop >100k",
#                   TRUE ~ "Pop <= 100k"
#                 ),
#                 code_muni = as.numeric(codmun7)) |> 
#               select(code_muni, ratio, popsize)) |> 
#   filter(!is.na(popsize)) |> 
#   ggplot(aes(fill = ratio)) + 
#   geom_sf(color = NA) + 
#   colorspace::scale_fill_continuous_sequential(palette = "inferno") +
#   theme_void()+
#   facet_wrap(.~popsize)
  
data <- data |> 
  mutate(
    DENV1 = replace_na(DENV1,0),
    DENV2 = replace_na(DENV2,0),
    DENV3 = replace_na(DENV3,0),
    DENV4 = replace_na(DENV4,0),
    DENVT = DENV1 + DENV2 + DENV3 + DENV4
  ) |> 
  group_by(UF) |> 
  mutate(
    DENV1p = sum(DENV1) / sum(DENVT), 
    DENV2p = sum(DENV2) / sum(DENVT), 
    DENV3p = sum(DENV3) / sum(DENVT), 
    DENV4p = sum(DENV4) / sum(DENVT) 
  ) |> ungroup() |> 
  mutate(
    ratio = casos / pop,
    # ratio = replace_na(ratio, 0),
    DENV1p = if_else(pop >= 200000, DENV1/ DENVT, DENV1p), 
    DENV2p = if_else(pop >= 200000, DENV2/ DENVT, DENV2p), 
    DENV3p = if_else(pop >= 200000, DENV3/ DENVT, DENV3p), 
    DENV4p = if_else(pop >= 200000, DENV4/ DENVT, DENV4p)
  ) 

# data |> 
#   mutate(
#     ratio = casos / pop,
#     ratio = replace_na(ratio, 0),
#     popsize = case_when(
#       pop > 5e5 ~ "Pop >500k",
#       TRUE ~ "Pop <= 500k"
#     )
#   ) |> 
#   ggplot(aes(x = ratio, color = popsize)) + 
#   geom_density() +
#   scale_x_continuous(breaks = seq(0,0.8, 0.1),
#                      minor_breaks = seq(0,0.8, 0.05))+
#   theme_bw()

BR <- BR0 |> 
  left_join( data |> 
               mutate(code_muni = as.numeric(codmun7)) |> 
               select(code_muni, ratio, DENV1p:DENV4p)
  )

BR |> 
  mutate(ratio = replace_na(ratio, 0)) |>
  ggplot(aes(fill = ratio)) + 
  geom_sf(color = NA) + 
  colorspace::scale_fill_binned_sequential(palette = "inferno",
                                           breaks = seq(0,0.8,0.1)) +
  theme_void()

BR |>
  mutate(ratio.cat = cut(ratio,
                         breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                         labels = c("<10%", 
                                    "10-20%", 
                                    "20-30%", 
                                    "30-40%", 
                                    "40-50%", 
                                    ">50%")
                         )
         ) |>
  # mutate(ratio.cat = replace_na(ratio.cat, 0)) |>
  ggplot(aes(fill = ratio.cat)) +
  geom_sf(color = NA) +
  colorspace::scale_fill_discrete_sequential(palette = "Viridis")+
  theme_void()

denv1 <- BR |> 
  mutate(ratio = replace_na(ratio, 0) * DENV1p,
         outbreak_risk = 1 - ratio) |> 
  ggplot(aes(fill = outbreak_risk)) + 
  geom_sf(color = NA) + 
  colorspace::scale_fill_binned_sequential(palette = "inferno",
                                           breaks = seq(0,1,0.1)) +
  theme_void()
denv1

denv1.cat <- BR |> 
  mutate(ratio.cat = cut(ratio * DENV1p,
                         breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                         labels = c("<10%", 
                                    "10-20%", 
                                    "20-30%", 
                                    "30-40%", 
                                    "40-50%", 
                                    ">50%")
  )
  ) |>
  ggplot(aes(fill = ratio.cat)) + 
  geom_sf(color = NA) + 
  geom_sf(data = BR_states,
          fill = "transparent")+
  colorspace::scale_fill_discrete_sequential(name = "Ratio \n (Cases/Population)",
                                             na.value = "grey50",
                                             palette = "Viridis",
                                             breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                                             labels = c("<10%", 
                                                        "10-20%", 
                                                        "20-30%", 
                                                        "30-40%", 
                                                        "40-50%", 
                                                        ">50%"))+
  theme_void()+
  theme(legend.title.position = "left",
        legend.title = element_text(angle = 90,
                                    hjust = 0.5))+
  labs(title = "DENV1")
denv1.cat

denv2 <- BR |> 
  mutate(ratio = replace_na(ratio, 0) * DENV2p,
         outbreak_risk = 1 - ratio) |> 
  ggplot(aes(fill = outbreak_risk)) + 
  geom_sf(color = NA) + 
  colorspace::scale_fill_binned_sequential(palette = "inferno",
                                           breaks = seq(0,1,0.1)) +
  theme_void()
denv2

denv2.cat <- BR |> 
  mutate(ratio.cat = cut(ratio * DENV2p,
                         breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                         labels = c("<10%", 
                                    "10-20%", 
                                    "20-30%", 
                                    "30-40%", 
                                    "40-50%", 
                                    ">50%")
  )
  ) |>
  ggplot(aes(fill = ratio.cat)) + 
  geom_sf(color = NA) + 
  geom_sf(data = BR_states,
          fill = "transparent")+
  colorspace::scale_fill_discrete_sequential(name = "Ratio \n (Cases/Population)",
                                             na.value = "grey50",
                                             palette = "Viridis")+
  theme_void()+
  theme(legend.title.position = "left",
        legend.title = element_text(angle = 90,
                                    hjust = 0.5))+
  labs(title = "DENV2")
denv2.cat

denv3 <- BR |> 
  mutate(ratio = replace_na(ratio, 0) * DENV3p,
         outbreak_risk = 1 - ratio) |> 
  ggplot(aes(fill = outbreak_risk)) + 
  geom_sf(color = NA) + 
  colorspace::scale_fill_continuous_sequential(palette = "inferno") +
  theme_void()
denv3

denv3.cat <- BR |> 
  mutate(ratio.cat = cut(ratio * DENV3p,
                         breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                         labels = c("<10%", 
                                    "10-20%", 
                                    "20-30%", 
                                    "30-40%", 
                                    "40-50%", 
                                    ">50%")
  )
  ) |>
  ggplot(aes(fill = ratio.cat)) + 
  geom_sf(color = NA) + 
  geom_sf(data = BR_states,
          fill = "transparent")+
  colorspace::scale_fill_discrete_sequential(name = "Ratio \n (Cases/Population)",
                                             na.value = "grey50",
                                             palette = "Viridis")+
  theme_void()+
  theme(legend.title.position = "left",
        legend.title = element_text(angle = 90,
                                    hjust = 0.5))+
  labs(title = "DENV3")
denv3.cat

denv4 <- BR |> 
  mutate(ratio = replace_na(ratio, 0) * DENV4p,
         outbreak_risk = 1 - ratio) |> 
  ggplot(aes(fill = outbreak_risk)) + 
  geom_sf(color = NA) + 
  colorspace::scale_fill_continuous_sequential(palette = "inferno") +
  theme_void()
denv4

denv4.cat <- BR |> 
  mutate(ratio.cat = cut(ratio * DENV4p,
                         breaks = c(0,0.1,0.2,0.3,0.4,0.5,1),
                         labels = c("<10%", 
                                    "10-20%", 
                                    "20-30%", 
                                    "30-40%", 
                                    "40-50%", 
                                    ">50%")
  )
  ) |>
  ggplot(aes(fill = ratio.cat)) + 
  geom_sf(color = NA) + 
  geom_sf(data = BR_states,
          fill = "transparent")+
  colorspace::scale_fill_discrete_sequential(name = "Ratio \n (Cases/Population)",
                                             na.value = "grey50",
                                             breaks = c("<10%", 
                                                        "10-20%", 
                                                        "20-30%", 
                                                        "30-40%", 
                                                        "40-50%", 
                                                        ">50%"),
                                             labels = c("<10%", 
                                                        "10-20%", 
                                                        "20-30%", 
                                                        "30-40%", 
                                                        "40-50%", 
                                                        ">50%"),
                                             palette = "Viridis")+
  theme_void()+
  theme(legend.title.position = "left",
        legend.title = element_text(angle = 90,
                                    hjust = 0.5))+
  labs(title = "DENV4")
denv4.cat

library(patchwork)

patchwork <- (denv1 | denv2 | denv3 | denv4)+
  plot_layout(guides = "collect")
patchwork

patchwork.cat <- (denv1.cat | denv2.cat | denv3.cat | denv4.cat)+
  plot_layout(guides = "collect")
patchwork.cat
