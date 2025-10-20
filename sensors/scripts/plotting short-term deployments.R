do <- read_csv("sensors/data/DO/DO_Summer2021_988987.csv")

do <- read_csv("sensors/data/DO/Palmas_DO_Winter2024.csv")

do <- read_csv("sensors/data/DO/Tortuguero_DO_Winter2024.csv")


library(plotly)

ggplotly(ggplot(do, aes(x=`Eastern Standard Time`, y = `Dissolved Oxygen Saturation`)) +
  geom_point())

ggplotly(ggplot(do, aes(x=`Eastern Standard Time`, y = Temperature)) +
           geom_point())


palmas_2021 <- do %>%
  filter(`Eastern Standard Time` > "2021-06-11 14:30:00" & `Eastern Standard Time` < "2021-06-13 15:00:00")

g <- ggplot(palmas_2021, aes(x=`Eastern Standard Time`, y = `Dissolved Oxygen Saturation`)) +
  geom_point()

ggplotly(g)
