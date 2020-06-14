library(pacman)

pacman::p_load(
  gapminder,
  tidyverse,
  ggthemes,
  ggrepel
)

blue <- '#1f78b4'

myTheme <- function () {
  theme_fivethirtyeight() +
    theme(axis.title = element_text())
}

migr <- read_csv('./data/migr_resfirst.csv')
demo_pjan <- read_csv('./data/demo_pjan.csv')

migr
demo_pjan
gapminder

onlyDeTotAsDe <- demo_pjan %>% 
  filter(!is.na(value), geo == 'DE_TOT') %>% 
  mutate(geo = 'DE')

cleanPjan <- demo_pjan %>% 
  filter(!is.na(value), str_length(geo) == 2, geo != 'DE', year >= 2008) %>% 
  rbind(onlyDeTotAsDe) %>% 
  group_by(year, geo) %>%  
  summarise(res  = sum(value))

fullMigr <- migr %>% inner_join(cleanPjan)

migr %>% 
  group_by(year, geo) %>% 
  summarise(migr = sum(value))

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(prop))

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(prop)) %>% 
  view()

migr %>% 
  filter(year == 2018) %>% 
  group_by(geo) %>% 
  summarise(total = sum(value)) %>% 
  arrange(desc(total)) %>% 
  mutate(geo = geo %>% fct_inorder() %>% fct_lump(20, w = total) %>%  fct_rev()) %>% 
  ggplot(aes(geo, total * 1e-3)) +
  geom_col(fill = blue) +
  coord_flip() +
  labs(
    title = 'Migration von Drittstaaten pro Land',
    subtitle = '2018',
    x = '',
    y = 'Tsd.'
  ) +
  myTheme()

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(res)) %>% 
  mutate(geo = geo %>% fct_inorder() %>% fct_rev()) %>% 
  ggplot(aes(geo, res * 1e-6)) + 
  geom_col() +
  myTheme() +
  labs(title = 'Vergleich der Bevölkerung von Ländern in Europa', x = 'Länder', y = 'Bevölkerung in mio')

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(res)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  ggplot(aes(reorder(geo, res), res * 1e-6, color = size)) + 
  scale_y_log10() +
  geom_point(size = 5) +
  myTheme() +
  labs(title = 'Vergleich der Bevölkerung von Ländern in Europa', x = 'Länder', y = 'Bevölkerung in mio')

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(res)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  ggplot(aes(x = reorder(geo, prop), y = prop, fill = size)) + 
  geom_col() +
  myTheme() +
  labs(
    title = 'Vergleich des Prozentualen Anteils an Migraten in der Bevölkerung',
    x = 'Länder',
    y = 'Prozentualer Anteil an Migraten in der Bevölkerung'
  )

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 100) %>% 
  arrange(desc(prop)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>%  
  ggplot(aes(x = reorder(geo, prop), y = prop, color = size)) + 
  geom_point(size = 5) +
  scale_y_log10() +
  myTheme() +
  labs(
    title = 'Vergleich des Prozentualen Anteils an Migraten in der Bevölkerung',
    x = 'Länder',
    y = 'Prozentualer Anteil an Migraten in der Bevölkerung'
  )

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 1e2) %>% 
  arrange(desc(prop)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr <= 10 | nr > max(nr) - 10 ) %>% 
  ggplot(aes(x = reorder(geo, prop), y = prop, color = size)) + 
  geom_point(size = 5) +
  scale_y_log10() +
  coord_flip() +
  myTheme() +
  labs(
    title = 'Vergleich der Neuvisumstellenden mit der Bevölkerung',
    subtitle = '2018',
    x = 'Länder',
    y = 'Prozentualer Anteil an Neuvisumstellenden'
  )

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year == 2018) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 1e3) %>% 
  arrange(desc(prop)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr <= 10 | nr > max(nr) - 10 ) %>% 
  ggplot(aes(x = reorder(geo, prop), y = prop, color = size)) + 
  geom_point(size = 5) +
  scale_y_log10() +
  coord_flip() +
  myTheme() +
  labs(
    title = 'Vergleich von Neuvisumstellenden mit der Bevölkerung',
    subtitle = '2018',
    x = 'Länder',
    y = 'Anteil an Neuvisumstellenden pro 1000'
  )

migr %>% 
  inner_join(cleanPjan) %>% 
  filter(!is.na(res), year %in% c(2008, 2018)) %>% 
  group_by(year, geo, res) %>% 
  summarise(migr = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = migr / res * 1e3) %>% 
  arrange(desc(prop)) %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr <= 10 | nr > max(nr) - 10 ) %>% 
  ggplot(aes(x = reorder(geo, prop), y = prop, color = size)) + 
  geom_point(size = 5) +
  scale_y_log10() +
  coord_flip() +
  facet_wrap(~year) +
  myTheme() +
  labs(
    title = 'Vergleich von Neuvisumstellenden mit der Bevölkerung',
    subtitle = 'zwischen 2008 und 2018',
    x = 'Länder',
    y = 'Anteil an Neuvisumstellenden pro 1000'
  )


fullMigr %>% 
  group_by(year, reason) %>% 
  summarise(total = sum(value)) %>% 
  ggplot(aes(year, total / 1e6, fill = reason)) + 
  geom_col() + 
  myTheme() + 
  scale_x_continuous(
    breaks = seq(2008, 2018, by = 1),
  ) + 
  theme(legend.position = 'right', legend.direction = 'vertical') + 
  scale_fill_discrete(name = 'Grund') + 
  labs(
    title = 'Neuvisumstellende in Europäischen Ländern',
    subtitle = 'in Mio.',
    x = 'Jahr',
    y = 'Anzahl'
  )

fullMigr %>% 
  mutate(size = case_when(
    res < 3e6 ~ 'small',
    res < 30e6 ~ 'medium',
    TRUE ~ 'large'
  )) %>% 
  group_by(geo, size) %>% 
  summarise(res = sum(res) / 10)

fullMigr %>% 
  group_by(geo) %>% 
  summarise(migr = sum(value) /  10) %>% 
  inner_join(
    fullMigr %>% 
      mutate(size = case_when(
        res < 3e6 ~ 'small',
        res < 30e6 ~ 'medium',
        TRUE ~ 'large'
      )) %>% 
      group_by(geo, size) %>% 
      summarise(res = sum(res) / length(res))
  ) %>% 
  mutate(prop = migr / res * 1e3) %>% 
  arrange(desc(prop)) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr <= 10 | nr > max(nr) - 10) %>% 
  ggplot(aes(x = reorder(geo, prop), y = prop, color = size)) + 
  geom_point(size = 5) +
  scale_y_log10() +
  coord_flip() +
  myTheme() +
  labs(
    title = 'Durchschnittliche Neuvisumstellende bezogen auf Bevölkerung',
    subtitle = 'von 2008 - 2018',
    x = 'Länder',
    y = 'Anteil an Neuvisumstellenden pro 1000'
  )

fullMigr %>% 
  filter(geo %in% c('CY', 'MT', 'LI', 'SE')) %>% 
  group_by(year, res, geo) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 1e3) %>% 
  ggplot(aes(year, prop)) +
  geom_col(fill = blue) +
  facet_wrap(~geo) +
  myTheme() +
  labs(
    title = 'Proportionale Neuvisumstellende zu Bevölkerung in den Big 4 Ländern',
    subtitle = 'von 2008 - 2018',
    x = 'Jahr',
    y = 'Verhältnis zwischen Bevölkerung und Neuvisumstellende pro 1000'
  )


fullMigr %>% 
  #filter(geo == 'UK') %>% 
  group_by(year, res, geo) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 1e3) %>% 
  ggplot(aes(year, prop, color = geo)) +
  geom_point() +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2)) + 
  geom_line() +
  myTheme()

fullMigr %>% 
  filter(geo %in% c('CY', 'LI', 'MT', 'SE', 'DE', 'PL', 'IE', 'LU')) %>% 
  group_by(year, res, geo) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 1e3) %>% 
  ggplot(aes(year, prop, color = geo)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  myTheme() +
  scale_x_continuous(
    breaks = seq(2008, 2018, by = 1),
  ) + 
  theme(
    legend.position = 'right', 
    legend.direction = 'vertical'
  ) +
  scale_color_discrete(
    name = 'Länder',
    limits = c('MT', 'LI', 'CY', 'PL', 'LU', 'SE', 'IE', 'DE')
  ) + 
  labs(
    title = 'Zeitreihe der "interessantesten Länder"',
    subtitle = 'Verhältnis von Neuvisumstellenden zu Bevölkerungszahl',
    x = 'Jahr',
    y = 'Anzahl an Neuvisumstellenden pro 1000 Einwohner'
  )


mtFilter <- fullMigr %>% 
  filter(geo == 'MT', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)


fullMigr %>% 
  filter(geo == 'MT', year == 2018, !is.na(citizen), citizen %in% mtFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(mtFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Malta 2018'
  )

fullMigr %>% 
  filter(geo == 'MT', year == 2018) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>% 
  arrange(desc(value)) %>%
  head(10) %>% 
  ggplot(aes(reorder(citizen, -value), value)) +
  geom_col(fill = blue) +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Malta 2018'
  )


liFilter <- fullMigr %>% 
  filter(geo == 'LI', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'LI', year == 2018, !is.na(citizen), citizen %in% liFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(liFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Lichtenstein 2018'
  )

fullMigr %>% 
  filter(geo == 'LI', year == 2018) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>% 
  arrange(desc(value)) %>%
  head(10) %>% 
  ggplot(aes(reorder(citizen, -value), value)) +
  geom_col(fill = blue) +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Lichtenstein 2018'
  )

fullMigr %>% 
  filter(geo == 'CY', year == 2018) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  head(10) %>% 
  ggplot(aes(reorder(citizen, -value), value)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Zypern 2018'
  )

zyFilter <- fullMigr %>% 
  filter(geo == 'CY', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'CY', year == 2018, !is.na(citizen), citizen %in% zyFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(zyFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Zypern 2018'
  )


mtFilter <- fullMigr %>% 
  filter(geo == 'MT', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'MT', year == 2018, !is.na(citizen), citizen %in% mtFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(mtFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Malta',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

liFilter <- fullMigr %>% 
  filter(geo == 'LI', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'LI', year == 2018, !is.na(citizen), citizen %in% liFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(liFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Lichtenstein',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

plFilter <- fullMigr %>% 
  filter(geo == 'PL', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'PL', year == 2018, !is.na(citizen), citizen %in% plFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(plFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Polen 2018',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

ukFilter <- fullMigr %>% 
  filter(geo == 'UK', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'UK', year == 2018, !is.na(citizen), citizen %in% ukFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(ukFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in England 2018',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

deFilter <- fullMigr %>% 
  filter(geo == 'DE', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(geo == 'DE', year == 2018, !is.na(citizen), citizen %in% deFilter) %>%
  group_by(citizen, reason) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(deFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Deutschland',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

fullMigr %>% 
  filter(geo == 'PL', year == 2018) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>% 
  arrange(desc(value)) %>%
  head(10) %>% 
  ggplot(aes(reorder(citizen, -value), value)) +
  geom_col(fill = blue) +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Polen 2018'
  )

seFilter <- fullMigr %>% 
  filter(geo == 'SE', year == 2018, !is.na(citizen)) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>% 
  head(10) %>% 
  pull(citizen)

fullMigr %>% 
  filter(
    geo == 'SE',
    year == 2018,
    !is.na(citizen),
    citizen %in% seFilter
  ) %>%
  group_by(citizen, reason, year) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(year, value)) %>% 
  ungroup() %>% 
  mutate(citizen = citizen %>% fct_relevel(seFilter)) %>% 
  ggplot(aes(citizen, value, fill = reason)) +
  geom_col() +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Schweden',
    subtitle = '2018',
    x = 'Herkunftsland',
    y = 'Anzahl'
  )

fullMigr %>% 
  filter(geo == 'SE', year == 2018) %>%
  group_by(citizen) %>% 
  summarise(value = sum(value)) %>% 
  arrange(desc(value)) %>%
  head(10) %>% 
  ggplot(aes(reorder(citizen, -value), value)) +
  geom_col(fill = blue) +
  myTheme() +
  labs(
    title = 'Herkunft Neuvisumsteellender in Schweden 2018'
  )

fullMigr %>% 
  filter(geo == 'EL') %>% 
  group_by(year) %>% 
  summarise(migr = sum(value)) %>% 
  ggplot(aes(year, migr)) +
  geom_col(fill = blue) +
  myTheme() +
  labs(
    title = 'Anzahl an Neuvisumstellenden in Griechenland',
    subtitle = 'von 2008 - 2018',
    x = 'Jahr',
    y = 'Anzahl an Neuvisumstellenden'
  )


fullMigr %>% 
  filter(geo %in% c('CY', 'MT', 'LI', 'SE')) %>% 
  group_by(year, res, geo) %>% 
  summarise(migr = sum(value)) %>% 
  mutate(prop = migr / res * 1e3) %>% 
  ggplot(aes(year, prop)) +
  geom_col(fill = blue) +
  facet_wrap(~geo) +
  myTheme() +
  labs(
    title = 'Proportionale Neuvisumstellende zu Bevölkerung in den Big 4 Ländern',
    subtitle = 'von 2008 - 2018',
    x = 'Jahr',
    y = 'Verhältnis zwischen Bevölkerung und Neuvisumstellende pro 1000'
  )
