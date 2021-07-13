#### Point based modle functions



#Probabitly of winning a point in a gamem, can be for server or returner, 

p_server_wins_game <- function(p_server_wins_point){
  p_game <- p_server_wins_point^4 + 
    4*(p_server_wins_point)^4*(1-p_server_wins_point) +
    10*(p_server_wins_point)^4 * (1-p_server_wins_point)^2 +
    (
      (20*(p_server_wins_point)^5*(1-p_server_wins_point)^3)/
        (1 - 2*p_server_wins_point*(1-p_server_wins_point))
    )
  
  return(p_game)
  
}

#Probabitly of winning a tie breaker, this is used within p_wins_set

p_wins_tie_break <- function(outcome, A, p_server_wins_point, p_wins_point_on_return)
  
{
  i = outcome 
  p = p_server_wins_point
  q = p_wins_point_on_return
  part_a = A[i,1] * p^A[i,2] * (1-p)^A[i,3] *q^A[i,4] * (1-q)^A[i,5]
  part_b = (p*q)*((1-(p*(1-q)+(1-p)*q))^-1)
  result <- part_a * part_b^A[i,6]
  
  return(result)
}


#Probabitly of winning a tie breaker, this is used within p_wins_set


p_wins_a_set <- function(outcome, B, p_server_wins_point, 
                         p_wins_point_on_return, p_wins_tie_break_for_set)
  
{ 
  i = outcome 
  p = p_server_wins_point
  q = p_wins_point_on_return
  
  part_a = (B[i, 1] * p_server_wins_game(p)^B[i,2]) *
    (1-p_server_wins_game(p))^B[i, 3] *
    p_server_wins_game(q)^B[i, 4]*
    (1-p_server_wins_game(q))^B[i, 5]
  
  part_b = (
    p_server_wins_game(p) * p_server_wins_game(q) +
      ( 
        p_server_wins_game(p)*(1 -  p_server_wins_game(q)) +
          (1 -  p_server_wins_game(p)) *  p_server_wins_game(q) ) * p_wins_tie_break_for_set
  )^B[i,6]
  
  result <- part_a * part_b
  
  return(result)
  
}


p_wins_a_match <- function(p_server_wins_point,
                           p_wins_point_on_return,
                           match_length,
                           matrix_A_tb = matrix_A,
                           matrix_B_set = matrix_B)
  
{
  
  p = p_server_wins_point
  q = p_wins_point_on_return 
  
  p_wins_a_tie_break <- 
    purrr::map_df(.x = 1:28, ~p_wins_tie_break(outcome = .x, A = matrix_A_tb,
                                               p_server_wins_point = p,
                                               p_wins_point_on_return = q)) %>% 
    sum()
  
  p_wins_set <- purrr::map_df(.x = 1:21, 
                              ~p_wins_a_set(outcome = .x, 
                                            B = matrix_B_set, 
                                            p_server_wins_point = p, 
                                            p_wins_point_on_return = q,
                                            p_wins_tie_break_for_set = p_wins_a_tie_break)) %>% 
    sum()
  
  if (match_length == 3) 
  { 
    p_wins_set^2 *  (1 + 2 * (1 - p_wins_set)) 
  } else
    
    p_wins_set^3 * 
    ( 1 + 3 * (1 - p_wins_set) + 6 *  (1 - p_wins_set)^2 )
  
}
