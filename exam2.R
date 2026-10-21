#There are two objects "ratings" and "movies". 

# Each row of ratings represents one user’s rating of a movie, with userId, movieId, and rating.
# Each row of movie represents a movie with movieId, movie title, and binary indictor 
#for movie genres: "Adventure",   "Animation"   "Children",   "Comedy" ,     "Fantasy" ,  
#"Romance"  ,   "Drama"   ,    "Action", "Crime" ,      "Thriller"  ,  "Horror"    , 
#"Mystery" ,    "Sci-Fi", "IMAX"  ,      "Documentary", "War"     ,    "Musical"  ,
# "Western"  , "Film-Noir"  

dim(movies)
dim(ratings)

movies |> summarize(n = n_distinct(movieId))
ratings |> summarize(n = n_distinct(rating))

#Which userId has rated the greatest number of movies

ratings |> slice(which.max(rating))

movies_ratings <- ratings |> inner_join(movies, by="movieId")

avg_movie <- movies_ratings |> group_by(movieId, title)|>
  summarize(avg_rating = mean(rating), rating_count = n(), .groups = "drop")

#Among users who have rated more than 2,000 movies, which userId has the lowest average rating?

user <- ratings |> 
  group_by(userId)|>
  summarize(
    rating = max(rating)
  )

minavg <- avg_movie |>  left_join(movies_ratings, by="userId") |> filter(rating_count>= 2000)|>
   slice(which.min(rating_count)) 


avg_movie |> filter(356 <= movieId  & movieId <= 2571)|> summarize(n = sum(avg_rating))

#What is the average rating across all movies in the Drama genre?

movies_ratings |> filter(Drama==1) |> summarize(avg = mean(rating))

# Which movieId in the Comedy genre has the highest average user rating?

movies_ratings |> group_by(movieId, Comedy) |> 
  filter(Comedy == 1) |> 
  summarize(high = mean(rating))

 com <- movies_ratings |> group_by(movieId, Comedy) |>
  filter(Comedy == 1 )
 
 
 com |> left_join(avg_movie, by = "movieId") 


