map_chr(c(5, 3, 4), int_to_string)


gt<-map_lgl(c(1, 2, 3, 4, 5), function(x){
  x > 3
})

map_lgl(c(1, 2, 3, 4, 5), gt, x = 3)


gt<-function(b)
{
  b>3
}


map_lgl(c(1, 2, 3, 4, 5),gt,b=3)

gt<-function(x,b){x>b}
gt(5,2)
library(purrr)
map_lgl(c(1, 2, 3, 4, 5),gt,b=3)


is_even<-function(x){x %% 2 == 0}
is_even(5)  

map_if(1:5, function(x){
  x %% 2 == 0
},
function(y){
  y^2
}) %>% unlist()
square<-function(x){x^2}


map_if(c(1, 2, 3, 4), is_even, square)

map_at(seq(100, 500, 100), c(1, 3, 5), function(x){
  x - 10
}) %>% unlist()


map_at(c(4, 6,2, 3, 8),c(1,3,4),square)
map2_chr(letters, 1:26, paste)
reduce(c(1, 3, 5, 7),add_talk)

reduce_right(c("a", "b", "c", "d"), paste_talk)
contains(random_ints,45)

detect(random_ints,is_even)

detect_index(random_ints,is_even)

keep(random_ints,is_even)
discard(random_ints,is_even)
every(random_ints,100)



gt<-function(x){x>10}

gt_10<-partial(x){x>10}

walk(mark_antony, message)
