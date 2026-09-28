test_data <- read.csv("C:/Users/FRT/Downloads/Fall-2026/QuantathonV326/Quantathon-V3-Schrodingers-Cats/Data/earthquakeq_test.csv")
train_data <- read.csv("C:/Users/FRT/Downloads/Fall-2026/QuantathonV326/Quantathon-V3-Schrodingers-Cats/Data/earthquakeq_train.csv")


# Data prepocessing

train_data |> ggplot(aes(x=magnitude)) +
  geom_histogram()

test_data |> select(depth_km) |> count(is.na=TRUE) # THIS DOESN4T WORK

train_data |> select(depth_km) |> count(is.na=TRUE)
