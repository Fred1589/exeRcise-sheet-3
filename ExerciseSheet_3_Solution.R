library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Studium/Fortgeschrittene_Statistische_Software/Exercise_03")

## 3

### a
pixar_films = read_csv("pixar_films.csv")
head(pixar_films, n = 5)

pixar_films_cleaned <- pixar_films %>% 
  filter(!is.na(film))

print(unique(pixar_films_cleaned$film_rating))

sort(unique(pixar_films_cleaned$film))


### b

pixar_film_series <- data.frame(
  series = c("Cars", "Finding Nemo", "Incredibles", "Monsters, Inc", "Toy Story"),
  films = I(list(
    c("Cars", "Cars 2", "Cars 3"),
    c("Finding Nemo", "Finding Dory"),
    c("The Incredibles", "Incredibles 2"),
    c("Monsters, Inc.", "Monsters University"),
    c("Toy Story", "Toy Story 2", "Toy Story 3", "Toy Story 4")
  )),
  amount = c(3, 2, 2, 2, 4)
)

head(pixar_film_series)

### c

public_response = read_csv("public_response.csv")
head(public_response, n = 5)

public_response$cinema_score <- factor(public_response$cinema_score, levels = sort(unique(public_response$cinema_score)), ordered = TRUE)

film_df_joined <- left_join(pixar_films_cleaned, 
                            public_response, 
                            by = c("film" = "film"))
                            
print(film_df_joined)


### d

series_lookup <- pixar_film_series %>%
  unnest(films) %>%
  rename(film = films)

joined_df <- pixar_films %>%
  inner_join(public_response, by = "film") %>%
  inner_join(series_lookup, by = "film") %>%
  arrange(series, release_date) 

joined_df$film <- factor(joined_df$film, levels = joined_df$film[order(joined_df$series, joined_df$release_date)])


ggplot(joined_df, aes(x = series, y = rotten_tomatoes, fill = film)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(
    title = "Rotten Tomatoes Scores of Pixar Film Series",
    x = "Film Series",
    y = "Rotten Tomatoes Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))