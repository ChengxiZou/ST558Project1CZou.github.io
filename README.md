ST558 Project 1
================
Chengxi Zou
2022-06-25

# Initial Settings

``` r
# have a section that notes the required packages needed to run the code to create vignette near the top
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
```

# Reading Data from Pokemon data api

This is a consumption-only API — only the HTTP GET method is available
on resources.

No authentication is required to access this API, and all resources are
fully open and available.

There are lots of data from the pokemon api, pokemons are species that
helps you in a battle, and they can eat different kinds of berries to
gain health, strength, etc.

Let’s grab some berry data by creating query functions using GET.

Notice that I use tolower(x) in the function. It convert letters to
lower case so the path would be right even if users used upper case.
This is good **use-ability**. If input ID numbers, it still works fine.

There is no abbreviation of berries or pokemons, every one of them has a
simple and unique ID and a name, so the tolower(x) function is all that
I’ve done for the **use-ability**.

``` r
initialpath = "https://pokeapi.co/api/v2/berry/"

# search berry data by typing name or id.
# endpoint 1: berries
getberrydata = function(x){
  fullpath = paste(initialpath, tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  berryinfo <- data.frame(berry.id = b$id,
                          berry.name = b$name,
                          max_harvest = b$max_harvest,
                          growth_time = b$growth_time,
                          power = b$natural_gift_power,
                          size = b$size,
                          smoothness = b$smoothness,
                          soil_dryness = b$soil_dryness,
                          firmness.name = b$firmness$name,
                          flavor.name = b$flavors$flavor$name,
                          item = b$item,
                          natural_gift_power= b$natural_gift_power)
berryinfotbl <- as_tibble(berryinfo)
  return(berryinfotbl)
}
```

`getberrydata` is a function which you can get berry data by simply
typing the berry name or ID.

Let’s take a look at combined berry data:

``` r
# the tibble of all berry data
  allberry <- data.frame()
  for (i in 1:64) {
    fullpath = paste(initialpath, i, sep = "")
    a <- GET(fullpath)
    b <- fromJSON(rawToChar(a$content))
    berryinfo <- data.frame(berry.id = b$id,
                            berry.name = b$name,
                            max_harvest = b$max_harvest,
                            growth_time = b$growth_time,
                            power = b$natural_gift_power,
                            size = b$size,
                            smoothness = b$smoothness,
                            soil_dryness = b$soil_dryness,
                            firmness.name = b$firmness$name,
                            flavor.name = b$flavors$flavor$name,
                            item = b$item,
                            natural_gift_power= b$natural_gift_power)
    allberry <- rbind(allberry, berryinfo)
  }
  allberryinfotbl <- as_tibble(allberry)
  allberryinfotbl
```

    ## # A tibble: 320 x 13
    ##    berry.id berry.name max_harvest growth_time power  size smoothness soil_dryness firmness.name flavor.name item.name    item.url        
    ##       <int> <chr>            <int>       <int> <int> <int>      <int>        <int> <chr>         <chr>       <chr>        <chr>           
    ##  1        1 cheri                5           3    60    20         25           15 soft          spicy       cheri-berry  https://pokeapi~
    ##  2        1 cheri                5           3    60    20         25           15 soft          dry         cheri-berry  https://pokeapi~
    ##  3        1 cheri                5           3    60    20         25           15 soft          sweet       cheri-berry  https://pokeapi~
    ##  4        1 cheri                5           3    60    20         25           15 soft          bitter      cheri-berry  https://pokeapi~
    ##  5        1 cheri                5           3    60    20         25           15 soft          sour        cheri-berry  https://pokeapi~
    ##  6        2 chesto               5           3    60    80         25           15 super-hard    spicy       chesto-berry https://pokeapi~
    ##  7        2 chesto               5           3    60    80         25           15 super-hard    dry         chesto-berry https://pokeapi~
    ##  8        2 chesto               5           3    60    80         25           15 super-hard    sweet       chesto-berry https://pokeapi~
    ##  9        2 chesto               5           3    60    80         25           15 super-hard    bitter      chesto-berry https://pokeapi~
    ## 10        2 chesto               5           3    60    80         25           15 super-hard    sour        chesto-berry https://pokeapi~
    ## # ... with 310 more rows, and 1 more variable: natural_gift_power <int>

Berry has its firmness: soft, hard, etc.

`getberryfirmdata` is a function which you can get berry firmness data
by simply typing the berry name or ID.

``` r
# search berry firmness data by typing name or id.
# endpoint 2: Berry Firmness
getberryfirmdata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/berry-firmness/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  berryinfo <- data.frame(firmness.id = b$id,
                          firmness.name = b$name,
                          berry.name = b$berries$name)
  berryinfotbl <- as_tibble(berryinfo)
  return(berryinfotbl)
}
```

Berry has its flavor: sour, spicy, etc.

`getberryflavordata` is a function which you can get berry flavor data
by simply typing the berry name or ID.

``` r
# search berry flavor data by typing name or id.
# endpoint 3: Berry Flavor
getberryflavordata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/berry-flavor/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  berryinfo <- data.frame(flavor.id = b$id,
                          flavor.Name= b$name,
                          berry.name = b$berries$berry$name,
                          potency = b$berries$potency,
                          ct.name = b$contest_type$name)
  berryinfotbl <- as_tibble(berryinfo)
  return(berryinfotbl)
}
```

Pokemon has its moves, which means different way to deal damage or
affect targets.

`getmovedata` is a function which you can get moves data by simply
typing the move name or ID.

``` r
# search Moves
# endpoint 4: Moves
getmovedata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/move/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  moveinfo <- data.frame(move.id = b$id,
                         move.name = b$name,
                         ct.name = b$contest_type$name,
                         move.power = b$power,
                         move.pp = b$pp,
                         move.priority = b$priority,
                         move.accuracy = b$accuracy,
                         target.name = b$target$name,
                         damage.class = b$damage_class$name,
                         generation.name = b$generation$name)
  moveinfotbl <- as_tibble(moveinfo)
  return(moveinfotbl)
}
```

Moves would cause ailments, the buff that affect targets.

`getmadata` is a function which you can get move ailments data by simply
typing the ailment’s name or ID.

``` r
# search Move Ailments
# endpoint 5: Move Ailments
getmadata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/move-ailment/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  mainfo <- data.frame(ailment.id = b$id,
                       ailment.name = b$name,
                       move.name = b$moves$name
                       )
  mainfotbl <- as_tibble(mainfo)
  return(mainfotbl)
}
```

Moves have categories.

`getmcdata` is a function which you can get category data by simply
typing the category name or ID.

``` r
# search Move Categories
# endpoint 6: Move Categories
getmcdata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/move-category/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  mcinfo <- data.frame(category.id = b$id,
                       category.name = b$name,
                       move.name = b$moves$name,
                       description = b$descriptions$description)
  mcinfotbl <- as_tibble(mcinfo)
  return(mcinfotbl)
}
```

We can search pokemon data from this api, which is the most fun part to
me.

`getpokemondata` is a function which you can get pokemon data by simply
typing the pokemon’s name or ID.

``` r
# search Pokemon
# endpoint 7: Pokemon
getpokemondata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/pokemon/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  pokemoninfo <- data.frame(pokemon.id = b$id,
                            pokemon.name = b$name,
                            pokemon.height = b$height,
                            pokemon.weight = b$weight,
                            base_experience = b$base_experience,
                            species.name = b$species$name,
                            pokemon.type = b$types$type$name)
  pokemoninfotbl <- as_tibble(pokemoninfo)
  return(pokemoninfotbl)
}
```

Pokemons have colors.

`getpokemoncolordata` is a function which you can get color data by
simply typing the color name or ID.

``` r
# search Pokemon Colors
# endpoint 8: Pokemon Colors
getpokemoncolordata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/pokemon-color/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  pokemoncolorinfo <- data.frame(color.id = b$id,
                            color.name = b$name,
                            species.name = b$pokemon_species$name)
  pokemoncolorinfotbl <- as_tibble(pokemoncolorinfo)
  return(pokemoncolorinfotbl)
}
```

There are different species of pokemon.

`getpokemonspeciesdata` is a function which you can get species data by
simply typing the species name or ID.

``` r
# search Pokemon Species
# endpoint 8: Pokemon Species
getpokemonspeciesdata = function(x){
  fullpath = paste("https://pokeapi.co/api/v2/pokemon-species/", tolower(x), sep = "")
  a <- GET(fullpath)
  b <- fromJSON(rawToChar(a$content))
  pokemonspeciesinfo <- data.frame(species.id = b$id,
                            species.order = b$order,
                            species.name = bb$name,
                            habitat.name = b$habitat$name,
                            capture.rate = b$capture_rate,
                            base.happiness = b$base_happiness,
                            gender.rate = b$gender_rate,
                            color = b$color$name,
                            shape = b$shape$name,
                            legendary = b$is_legendary)
  pokemonspeciesinfotbl <- as_tibble(pokemonspeciesinfo)
  return(pokemonspeciesinfotbl)
}
```

**Now let’s try to write an overall function that allow users to ask for
info in a single function.**

**Good User-ability!**

Just type the endpoint and the ID or name then you get the data.

``` r
GetPokemonApiData = function(endpoint, x){
  endpoint2 <- tolower(endpoint)
  switch(endpoint2,
         "berry" = getberrydata(x),
         "berry-firmness" = getberryfirmdata(x),
         "berry-flavor" = getberryflavordata(x),
         "move" = getmovedata(x),
         "move-ailment" = getmadata(x),
         "move-category" = getmcdata(x),
         "pokemon" = getpokemondata(x),
         "pokemon-color" = getpokemoncolordata(x)
         )
}

GetPokemonApiData("Berry",1)
```

    ## # A tibble: 5 x 13
    ##   berry.id berry.name max_harvest growth_time power  size smoothness soil_dryness firmness.name flavor.name item.name   item.url          
    ##      <int> <chr>            <int>       <int> <int> <int>      <int>        <int> <chr>         <chr>       <chr>       <chr>             
    ## 1        1 cheri                5           3    60    20         25           15 soft          spicy       cheri-berry https://pokeapi.c~
    ## 2        1 cheri                5           3    60    20         25           15 soft          dry         cheri-berry https://pokeapi.c~
    ## 3        1 cheri                5           3    60    20         25           15 soft          sweet       cheri-berry https://pokeapi.c~
    ## 4        1 cheri                5           3    60    20         25           15 soft          bitter      cheri-berry https://pokeapi.c~
    ## 5        1 cheri                5           3    60    20         25           15 soft          sour        cheri-berry https://pokeapi.c~
    ## # ... with 1 more variable: natural_gift_power <int>

``` r
# it works!
```

# A basic exploratory data analysis

## Pull data from two endpoints: pokemoncolor and getpokemon. Explore the relationship between color, weight, length.

``` r
colordata = data.frame()
for (i in 1:10) {colordata <- bind_rows(colordata,getpokemoncolordata(i)[,c(2:3)])}
# now the firmdata object contains all firmness info of berries

hwdata = data.frame()
for (i in 1:898) {hwdata <- bind_rows(hwdata,getpokemondata(i)[,c(1:4,6)])}
# now the flavordata object contains all flavor info of berries

# look at first 6 rows of those two data to get a feeling of the structure.
head(colordata)
```

    ##   color.name species.name
    ## 1      black      snorlax
    ## 2      black      murkrow
    ## 3      black        unown
    ## 4      black      sneasel
    ## 5      black      umbreon
    ## 6      black     houndour

``` r
head(hwdata)
```

    ##   pokemon.id pokemon.name pokemon.height pokemon.weight species.name
    ## 1          1    bulbasaur              7             69    bulbasaur
    ## 2          1    bulbasaur              7             69    bulbasaur
    ## 3          2      ivysaur             10            130      ivysaur
    ## 4          2      ivysaur             10            130      ivysaur
    ## 5          3     venusaur             20           1000     venusaur
    ## 6          3     venusaur             20           1000     venusaur

Notice that some species have multiplr types so they were counted
multiple times. That makes sense.

Combining those two data frame into one in order to do analysis and
plot:

``` r
joindata <- left_join(colordata,hwdata)

# inspect its structure.
as.tibble(head(joindata))
```

    ## # A tibble: 6 x 6
    ##   color.name species.name pokemon.id pokemon.name pokemon.height pokemon.weight
    ##   <chr>      <chr>             <int> <chr>                 <int>          <int>
    ## 1 black      snorlax             143 snorlax                  21           4600
    ## 2 black      murkrow             198 murkrow                   5             21
    ## 3 black      murkrow             198 murkrow                   5             21
    ## 4 black      unown               201 unown                     5             50
    ## 5 black      sneasel             215 sneasel                   9            280
    ## 6 black      sneasel             215 sneasel                   9            280

## Create a new numeric variable: BMI = weight/(height ^ 2)

BMI is a statistic to determine whether a pokemon is strong or thin.

``` r
joindata$BMI = joindata$pokemon.weight/(joindata$pokemon.height * joindata$pokemon.height)
as.tibble(head(joindata))
```

    ## # A tibble: 6 x 7
    ##   color.name species.name pokemon.id pokemon.name pokemon.height pokemon.weight   BMI
    ##   <chr>      <chr>             <int> <chr>                 <int>          <int> <dbl>
    ## 1 black      snorlax             143 snorlax                  21           4600 10.4 
    ## 2 black      murkrow             198 murkrow                   5             21  0.84
    ## 3 black      murkrow             198 murkrow                   5             21  0.84
    ## 4 black      unown               201 unown                     5             50  2   
    ## 5 black      sneasel             215 sneasel                   9            280  3.46
    ## 6 black      sneasel             215 sneasel                   9            280  3.46

Create other new variables: BMI.level, height.level and weight.level.

I created those categorical variables to determine whether they are tall
or short, heavy or light, etc.

Since they are different species from human, I choose 5 to be the BMI
boundary after inspecting the joined data.

``` r
# drop the na value in the data
joindata2 <- drop_na(joindata,c("BMI","pokemon.weight","pokemon.height","pokemon.name","pokemon.id","species.name","color.name"))

# let's see how the BMI looks like.
arrangebmi <- joindata2 %>% arrange(desc(BMI))
as.tibble(head(arrangebmi))
```

    ## # A tibble: 6 x 7
    ##   color.name species.name pokemon.id pokemon.name      pokemon.height pokemon.weight    BMI
    ##   <chr>      <chr>             <int> <chr>                      <int>          <int>  <dbl>
    ## 1 blue       cosmoem             790 cosmoem                        1           9999 9999  
    ## 2 brown      minior              774 minior-red-meteor              3            400   44.4
    ## 3 brown      minior              774 minior-red-meteor              3            400   44.4
    ## 4 gray       aron                304 aron                           4            600   37.5
    ## 5 gray       aron                304 aron                           4            600   37.5
    ## 6 gray       durant              632 durant                         3            330   36.7

``` r
# this "cosmoem" has an extremly high BMI, better exclude it in order to do better analysis.
joindata2 <- arrangebmi[-1,]

# Create other new variables: BMI.level, height.level, weight.level since the original data is not suitable to generate contingency tables and plots.
for (i in 1:1339) {
  if (joindata2$BMI[i] > 5) {joindata2$BMI.level[i] = "strong"}
  if (joindata2$BMI[i] <= 5) {joindata2$BMI.level[i] = "thin"}
  if (joindata2$pokemon.height[i] > 30) {joindata2$height.level[i] = "tall"}
  if (joindata2$pokemon.height[i] <= 30) {joindata2$height.level[i] = "short"}
  if (joindata2$pokemon.weight[i] > 2000) {joindata2$weight.level[i] = "heavy"}
  if (joindata2$pokemon.weight[i] <= 2000) {joindata2$weight.level[i] = "light"}
}

as.tibble(head(joindata2))
```

    ## # A tibble: 6 x 10
    ##   color.name species.name pokemon.id pokemon.name      pokemon.height pokemon.weight   BMI BMI.level height.level weight.level
    ##   <chr>      <chr>             <int> <chr>                      <int>          <int> <dbl> <chr>     <chr>        <chr>       
    ## 1 brown      minior              774 minior-red-meteor              3            400  44.4 strong    short        light       
    ## 2 brown      minior              774 minior-red-meteor              3            400  44.4 strong    short        light       
    ## 3 gray       aron                304 aron                           4            600  37.5 strong    short        light       
    ## 4 gray       aron                304 aron                           4            600  37.5 strong    short        light       
    ## 5 gray       durant              632 durant                         3            330  36.7 strong    short        light       
    ## 6 gray       durant              632 durant                         3            330  36.7 strong    short        light

## Create some contingency tables

**1. Create a contingency table showing the count of BMI level.**

``` r
table(joindata2$BMI.level)
```

    ## 
    ## strong   thin 
    ##    242   1097

We can see that if we set the BMI boundary to be 5, there would be 242
strong pokemons and 1097 thin pokemons.

**2. Create a contingency table showing the count of color and BMI
level.**

``` r
table(joindata2$color.name,joindata2$BMI.level)
```

    ##         
    ##          strong thin
    ##   black       7   59
    ##   blue       38  190
    ##   brown      36  155
    ##   gray       52   79
    ##   green      30  136
    ##   pink        6   67
    ##   purple     17  111
    ##   red        30  103
    ##   white       9  105
    ##   yellow     17   92

We can see that whether pokemons are strong or thin doesn’t depend on
the color much.

**3. Create a contingency table showing the count between color and
weight level.**

``` r
table(joindata2$weight.level,joindata2$color.name)
```

    ##        
    ##         black blue brown gray green pink purple red white yellow
    ##   heavy    10   17    14   20    17    0      6  14    12      0
    ##   light    56  211   177  111   149   73    122 119   102    109

We can see that there is no heavy pink pokemons or heavy yellow
pokemons.

**4. Create a contingency table showing the count of height and
weight.**

``` r
table(joindata2$height.level,joindata2$weight.level)
```

    ##        
    ##         heavy light
    ##   short    71  1217
    ##   tall     39    12

We can see that the majority of pokemons are light and short.

## Create numerical summaries for some quantitative variables at each setting of some of your categorical variables

**Summary of the joined data object.**

``` r
summary(joindata2)
```

    ##   color.name        species.name         pokemon.id    pokemon.name       pokemon.height   pokemon.weight        BMI          
    ##  Length:1339        Length:1339        Min.   :  1.0   Length:1339        Min.   :  1.00   Min.   :   1.0   Min.   : 0.00391  
    ##  Class :character   Class :character   1st Qu.:228.5   Class :character   1st Qu.:  6.00   1st Qu.:  90.0   1st Qu.: 1.87500  
    ##  Mode  :character   Mode  :character   Median :454.0   Mode  :character   Median : 10.00   Median : 285.0   Median : 2.87654  
    ##                                        Mean   :453.9                      Mean   : 12.35   Mean   : 662.6   Mean   : 4.02886  
    ##                                        3rd Qu.:681.5                      3rd Qu.: 15.00   3rd Qu.: 702.5   3rd Qu.: 4.29959  
    ##                                        Max.   :898.0                      Max.   :200.00   Max.   :9999.0   Max.   :44.44444  
    ##   BMI.level         height.level       weight.level      
    ##  Length:1339        Length:1339        Length:1339       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ## 

There are 1339 pokemons when excluding the data with NA and extrem
outliers. Notice that some species have multiple types.

There are pokemons weigh more than 9000, however if you look at the
data, this situation is not rare.

**Summary of BMI at each settings of color**

``` r
joindata2 %>% group_by(color.name) %>% summarise(Min = min(BMI), Median = median(BMI),
                                                 Mean = mean(BMI), Max = max(BMI), IQR = IQR(BMI))
```

    ## # A tibble: 10 x 6
    ##    color.name     Min Median  Mean   Max   IQR
    ##    <chr>        <dbl>  <dbl> <dbl> <dbl> <dbl>
    ##  1 black      0.6       2.94  3.46 29.2   2.56
    ##  2 blue       0.102     3.10  4.07 32.8   2.59
    ##  3 brown      0.188     3.2   4.90 44.4   2.04
    ##  4 gray       0.204     3.7   6.73 37.5   5.50
    ##  5 green      0.157     2.5   3.65 32.1   2.91
    ##  6 pink       0.116     2.35  2.55  6.78  1.66
    ##  7 purple     0.00391   2.64  3.06 12     2.38
    ##  8 red        0.204     2.95  3.94 25.5   2.46
    ##  9 white      0.04      2.46  2.92 18.3   1.69
    ## 10 yellow     0.333     2.78  3.50 16.8   1.42

The heaviest pokemon is brown, the lightest pokemon is purple.

Pink pokemons are the lightest polemons among all the other color.

**Summary of BMI at each settings of BMI level**

Let’s see how BMI behaves at each settings of BMI level.

``` r
joindata2 %>% group_by(BMI.level) %>% summarise(Min = min(BMI), Median = median(BMI),
                                                 Mean = mean(BMI), Max = max(BMI), IQR = IQR(BMI))
```

    ## # A tibble: 2 x 6
    ##   BMI.level     Min Median  Mean   Max   IQR
    ##   <chr>       <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 strong    5.05      8.16 10.8   44.4  6.36
    ## 2 thin      0.00391   2.55  2.53   5    1.70

The strong pokemons have the mean BMI of 10.81, while the thin pokemons
have the mean BMI of 2.53.

**Summary of BMI at each settings of height level**

Let’s see how BMI behaves at each settings of height level.

``` r
joindata2 %>% group_by(height.level) %>% summarise(Min = min(BMI), Median = median(BMI),
                                                 Mean = mean(BMI), Max = max(BMI), IQR = IQR(BMI))
```

    ## # A tibble: 2 x 6
    ##   height.level     Min Median  Mean   Max   IQR
    ##   <chr>          <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 short        0.00391   2.92  4.13 44.4   2.39
    ## 2 tall         0.103     1.18  1.51  7.76  1.62

The short pokemons have the mean BMI of 4.13, while the tall pokemons
have the mean BMI of 1.51.

**Summary of BMI at each settings of weight level**

Let’s see how BMI behaves at each settings of weight level.

``` r
joindata2 %>% group_by(weight.level) %>% summarise(Min = min(BMI), Median = median(BMI),
                                                 Mean = mean(BMI), Max = max(BMI), IQR = IQR(BMI))
```

    ## # A tibble: 2 x 6
    ##   weight.level     Min Median  Mean   Max   IQR
    ##   <chr>          <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 heavy        0.189     4.10  5.55  21.5  4.65
    ## 2 light        0.00391   2.79  3.89  44.4  2.32

The heavy pokemons have the mean BMI of 5.55, while the light pokemons
have the mean BMI of 3.89.

**Summary of weight at each settings of BMI level**

Let’s see how weight behaves at each settings of BMI level.

``` r
joindata2 %>% group_by(BMI.level) %>% summarise(Min = min(pokemon.weight), Median = median(pokemon.weight),
                                                 Mean = mean(pokemon.weight), Max = max(pokemon.height), IQR = IQR(pokemon.weight))
```

    ## # A tibble: 2 x 6
    ##   BMI.level   Min Median  Mean   Max   IQR
    ##   <chr>     <int>  <dbl> <dbl> <int> <dbl>
    ## 1 strong        6   708. 1176.    35  1291
    ## 2 thin          1   240   549.   200   478

The strong pokemons have the mean weight of 1176.32, while the thin
pokemons have the mean weight of 549.24.

## Create plots.

**Generate a jitter plot of BMI level counts through color.**

``` r
ggplot(joindata2, aes(x = color.name, BMI)) +
geom_point(aes(color = color.name), position = "jitter") + scale_color_discrete(name = "color") +
  ggtitle("Jitter Plot of BMI level counts through color") + xlab("color")
```

![](D:\5th%20semester\ST558\Rrepo\ST558Project1CZou.github.io\README_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

We can see that the majority of polemons have the BMI value under 10.
There are no pink pokemon whose BMI is larger than 10.

The pokemons who have large BMI tend to have color of blue, brown, grey
or green.

**Generate a scatter plot of weight and height.**

``` r
ggplot(joindata2, aes(pokemon.weight, pokemon.height)) +
geom_point(aes(color = factor(color.name))) + scale_color_discrete(name = "color") +
  ggtitle("Scatter plot of weight and height.") + geom_smooth(method = 'lm')
```

![](D:\5th%20semester\ST558\Rrepo\ST558Project1CZou.github.io\README_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

In general, the pokemon with larger weight would have larger height, but
there is no clear linear relationship between them since we could see
that the variance are huge.

**Generate a scatter plot of height and BMI.**

``` r
ggplot(joindata2, aes(pokemon.height, BMI)) +
geom_point(aes(color = factor(color.name))) + scale_color_discrete(name = "color") +
  ggtitle("Scatter plot of height and BMI.") + geom_smooth(method = 'lm')
```

![](D:\5th%20semester\ST558\Rrepo\ST558Project1CZou.github.io\README_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

Although the plot shows a down-ward linear regression model, there is
absolutely no clear or convincing linear model to describe the
relationship between height and BMI since the data points spread with
huge variance and without a clear trend.

**Generate a histogram of BMI level counts through color.**

``` r
ggplot(joindata2, aes(x = BMI.level)) +
geom_bar(aes(fill = color.name), position = "dodge") +
scale_fill_discrete(name = "color") + ggtitle("Histogram Plot of BMI level counts through color")
```

![](D:\5th%20semester\ST558\Rrepo\ST558Project1CZou.github.io\README_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

We can see that the number of thin blue pokemons is the biggest among
other thin pokemons. The number of strong grey pokemons is the biggest
among other strong pokemons.

**Generate a box plot of BMI through color.**

``` r
ggplot(joindata2, aes(x = BMI, y = color.name)) + ylab("color") +
geom_boxplot(aes(color = color.name)) + ggtitle("Box Plot of BMI level counts through color") + coord_flip()
```

![](D:\5th%20semester\ST558\Rrepo\ST558Project1CZou.github.io\README_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

All the distributions seem to be right-skewed, which means there exist
outliers with huge BMIs, especially among black, blue, brown, grey,
green and red pokemons. The pink polemons tend to be thin since none of
them has a BMI greater than 10.

The gray pokemons tend to have larger BMIs comparing to pokemons with
other colors.
