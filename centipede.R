library(tidyverse)
# Import submissions
submissions <- read_csv("submissions.csv")
# Pull a random strategy from the submissions
random_strat <- function(p1 = FALSE, tbl = submissions){
    if(p1) {
        return(sample(tbl$p1,1) %>% as.character)
    } else {
        return(sample(tbl$p2,1) %>% as.character)
    }
}
# Playoffs for the game
payoffs <- tibble(P1 <- c(2,1,4,3,6,5,8,7,10), P2 <- c(0,3,2,5,4,7,6,9,8))
# Combine two strategies into a sequence of moves
condense_game <- function(strat1, strat2){
    S1 <- str_split(strat1,"") %>% unlist
    S2 <- str_split(strat2,"") %>% unlist
    turns <- length(S1) + length(S2)
    move <- character(turns)
    for(t in 1:turns){
        move[t] <- ifelse(t %% 2 == 0,S2[t/2],S1[ceiling(t/2)])
    }
    return(move)
}
# Give payouts for two players
play_game <- function(strat1,strat2){
    end <- condense_game(strat1,strat2) %>% 
        match("D",.) # find the first "down"
    if(is.na(end)){ end <- 9 }
    return(payoffs[end,])
}
# Play a given strategy against a random strategy
play_one_side <- function(strat, p1 = FALSE, tbl = submissions){
    if(p1){
        outcome <- play_game(strat, random_strat(p1, tbl))[1,1]
    } else {
        outcome <- play_game(random_strat(p1, tbl), strat)[1,2]
    }
    outcome %>% as.numeric %>% return
}
# Play a given strategy multiple times
play_repeat <- function(strat, rounds = 100, p1 = FALSE, tbl = submissions){
    1:rounds %>%
        map_dbl(function(x){play_one_side(strat, p1, tbl)}) %>%
        reduce(sum) / rounds %>% 
        return
}
# Run a tournament
rounds <- 1000
submissions <- submissions %>%
    mutate(
        s1=map_dbl(p1,function(x){play_repeat(x,rounds,T)}),
        s2=map_dbl(p2,function(x){play_repeat(x,rounds,F)}),
        score = s1 + s2
    ) %>%
    group_by(p1,p2) %>% 
    mutate(top_score = max(score))
write_csv(submissions,"scores.csv")
anonymous_results <- submissions %>%
    select(-name) %>% arrange(score)
