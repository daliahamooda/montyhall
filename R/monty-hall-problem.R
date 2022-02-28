#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}


#' @title
#'   select door
#'
#' @description'
#'   `select_door`opens a door with the possibility of having a goat behind or a car behind
#'
#' @details'
#'   The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by this function
#'
#' @return
#'    The function returns a length 3 character vector
#'    indicating the positions of goats and the car.
#'
#' @examples
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' @title
#'   open goat door
#'
#' @description
#'   The contestant opens a goat door which is one of the 2 available doors
#'
#' @details
#'   The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param
#'   if contestant selected car,
#'   randomly select one of two goats
#'
#' @return
#'   The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   change _door
#'
#' @description
#'   The contestant changes a door
#'
#' @details
#'   the contestant is given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param
#'   if contestant selected car,
#'   randomly select one of two goats
#'
#' @return
#'   one door, car or goat
#'
#' @examples
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#'   determine_winner according to the final pick
#'
#' @description
#'   the contestant whose final pick is a car is a winner and the contestant whose final pick is a goat loses
#'
#' @details
#'   The game determines the winner according to the contestant final pick of the door
#'
#' @param
#'   if final pick is a car is a winner
#'   if  final pick is a goat loses
#'
#' @return
#'   no return
#'
#' @examples
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#'   Play_game for creating anew game with 3 doors
#'
#' @description
#'   New game is created with 3 doors , 2 goats and one car
#'
#' @details
#'   The game shows the 3 options of 2 goats and one car doors and the contestants have to choose one of thesr doors
#'
#' @param
#'   if final pick is a car is a winner
#'   if final pick is a goat loses
#'
#' @return
#'   switch or stay
#'
#' @examples
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}


#' @title
#'   Result list
#'
#' @description
#'   a table of the results of playing the game for 100 times
#'
#' @details
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'   print out
#'
#' @examples
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
