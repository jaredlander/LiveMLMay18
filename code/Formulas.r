library(useful)

boros <- tibble::tribble(
    ~ Boro, ~ Pop, ~ Size, ~ Random,
    'Manhattan', 1600000, 23, 7,
    'Brooklyn', 2600000, 78, 24,
    'Queens', 2330000, 104, pi,
    'Bronx', 1455000, 42, 21,
    'Staten Island', 475000, 60, 3
)

boros

build.x( ~ Pop, data=boros)
build.x( ~ Pop + Size, data=boros)
build.x( ~ Pop * Size, data=boros)
build.x( ~ Pop : Size, data=boros)
build.x( ~ Pop + Size - 1, data=boros)

build.x(~ Boro, data=boros)
build.x(~ Boro, data=boros, contrasts=FALSE)

build.x( ~ Pop + Size + Boro, data=boros, contrasts=FALSE)
build.x( ~ Pop + Size + Boro, data=boros, contrasts=FALSE, sparse=TRUE)
