#Play function will do the following:
get_symbols10<-function(){
  wheel<-c("DD","7","BBB","BB","B","C","0")
  sample(wheel,size = 3,replace = TRUE, prob =c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols10()
# Trying this function my self
score10<-function(symbols10){
  same10<-symbols10[1]==symbols10[2] & symbols10[2]==symbols10[3]
  bars10<-symbols10 %in% c("BBB","BB","B")
  cherries10<-sum(symbols10=="C")
  # if it is of either the same kind or all(bars) Look up prize 
   if(same10){
     symbols101<-symbols10[1]
    if(symbols101[1]=="DD"){
      prize<-100
    } else if(symbols101[1]=="7"){
      prize<-80
    } else if(symbols101[1]=="BBB"){
      prize<-40
    } else if(symbols101[1]=="BB"){
      prize<-25
    } else if (symbols101[1]=="B"){
      prize<-10
    } else if (symbols101[1]=="C"){
      prize<-10
    } else if (symbols101[1]=="0"){
      prize<-0
    }
  } else if(all(bars10)){
    prize<-5
  } else if(cherries10==1){   ####counting cherries and assing their respecting prizes
      prize<-2
  } else if(cherries10==2){
      prize<-5
  } else {              #### iyo udahaye machine else statement ishobora kuguha ibisubizo utateganyaga. cg c iyo uyihaye else statement utagombaga kuyiha nabwo biba gutyo.
      prize<-0
    }
  diamonds10<-sum(symbols10=="DD")
  prize*2^diamonds10
  }



play10<-function(){
  symbols10<-get_symbols10()
  print(symbols10)
  score10(symbols10)
}
play10()
# to test how R behaves when we have not provided else statement.
k<-function(){
Y<-c("c","a","b")
y<-sample(Y,size = 2)
if(all(y %in% c("a","b"))){
  print(y)
  y<-20
} else if(all(y%in%c("b","c"))){
  print(y)
  y<-100
} else{
  print(y)
  y<-0
}
y
}
### learning how to checkout if slot machine was payingout as claimed
# there is a need to create a vector named combos that contains the three symbols as per the play function.
combos<-expand.grid(wheel,wheel,wheel, stringsAsFactors = FALSE)
combos
prob<-c("DD"=0.03, "7"=0.03, "BBB"=0.06, "BB"=0.1, "B"=0.25, "C"=0.01, "0"=0.52)
combos$prob1<-prob[combos$Var1]
combos$prob2<-prob[combos$Var2]
combos$prob3<-prob[combos$Var3]
combos$Prob<-combos$prob1*combos$prob2*combos$prob3
combos$logic<-sum(combos$var1==combos$var2 & combos$var2==combos$var3)

#this is to be well checked.  
b<-sum(combos$var1==combos$var2 & combos$var2==combos$var3)
b
combos[c(combos$logic==TRUE), ]
# how do make correctly the above lines??
combos$logic<-NA
head(combos)
combos$logic[i]<-sum(combos$var1==combos$var2 & combos$var2==combos$var3)
sum(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[i, 3])


combos$logic<-NA
combos[combos$logic[i]]<-c(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[i, 3])
head(combos)
combos[combos$logic[1, c(1,2)]]
combos[1:5,c(1:4)]
combos$logic[i]<-c(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[1, 3])

head(combos)


# wrong code is the following, it was searching to do the work of the for loop.
if (combos$logic[i]==TRUE){
  if(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[1, 3]){
    combos$logic[i]<-TRUE
  } else {
    combos$logic[i]<-FALSE
  } else {
    combos$logic[i]<-FALSE
  }
}

ay<-sum(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[i, 3])
ay
combos$logic[i]<-ay[i]
head(combos)
tail(combos)

# the following for loop function serves to assign values to the rows whose outputs are similar for all the 3 Variables.

for(i in 1:343){
  combos$logic[i]<-sum(combos[i, 1]==combos[i, 2] & combos[i, 2]==combos[i, 3])
}
tail(combos)


for(i in 1:343){
  combos$logic[i]<-sum(combos$Var1[i]==combos$Var2[i] & combos$Var2[i]==combos$Var3[i])
}
tail(combos)
head(combos)



c(combos$Var1, combos$logic)

combos$Var1



# the following are the codes used to lookup values that have same values:
combos[(combos$logic==1), ]

combos[c(combos$Var1=="C" & combos$Var2=="C" & combos$Var3=="C"), ]

combos[c(combos$Var1=="0" & combos$Var2=="0" & combos$Var3=="0"), ]

combos[c(combos$Var1=="DD" & combos$Var2=="DD" & combos$Var3=="DD"), ]

combos[c(combos$Var1=="BBB" & combos$Var2=="BBB" & combos$Var3=="BBB"), ]

combos[c(combos$Var1=="BB" & combos$Var2=="BB" & combos$Var3=="BB"), ]

combos[c(combos$Var1=="B" & combos$Var2=="B" & combos$Var3=="B"), ]

combos[c(combos$Var1=="7" & combos$Var2=="7" & combos$Var3=="7"), ]





## how do we create the for loops that work correctly?
# first we create an empty vector or a list
# then we use a for loop to fill up the created vector or list.
empty<-vector(length=4)
empty
word<-c("inka","ingurube","impyisi","ihene")
for(i in c(1:4)){
  empty[i]<-word[i]}
empty[c(2,3)]






for(value in c("my","first","second","third","fourth")){
  print(value)
}
value
for (i in combos[1:5]){
  print(combos[1])
}
names(combos$logic)<-prize
head(combos)
combos$prize
prize
combos$prob<-NA
combos$prize<-NA
head(combos)


combos$prob<-prob[combos$Var1]*prob[combos$Var2]*prob[combos$Var3]
head(combos)
prob

#how to score all the output of the combos by using the already created score function?

symbols10<-c(combos$Var1[i],combos$Var2[i],combos$Var3[i])
symbols10

# the following for loop function is to be well checked for it to yield results as expected.
combos$symbols10<-NA
head(combos)
# creating a for loop that will compile values for all the 3 variables.
for(i in 1:343){
  combos$symbols10[i]<-score10(symbols10)
}
head(combos)
tail(combos)


symbols10
score10(symbols10[1])
symbols10

combos$prize<-NA
# formula imported from the book, on page 166
for (i in 1:nrow(combos)) {
  symbols10 <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score10(symbols10)
}
symbols10 <- c(combos[i, 1], combos[i, 2], combos[i, 3])
symbols10



score5<-function(symbols){
  # identify case
  same<-symbols[1]==symbols[2] & symbols[2]==symbols[3]
  bars<-symbols %in% c("B","BBB","BB")
  #get prize
  if (same) {
    symbol<-symbols[1]
    prize<-unname(payout[symbol])
  } else if(all(bars)){
    prize <- 5
  } else {
    cherries<-sum(symbols=="C")
    prize <- c(0,2,5)[cherries+1]
  }
  # adjust for diamonds
  diamonds<-sum(symbols=="DD")
  prize*2^diamonds
}

play5<- function() {
  symbols <- get_symbols()
  print(symbols)
  score5(symbols)
}
play5()

g<-letters
g$new<-LETTERS
rm(g)
b<-rep(NA,26)
c=b
wangu<-c(b,c)
rm(wangu)
matrix(data=NA,nrow = 26,ncol =2)
wangu<-matrix(data=NA,nrow = 26,ncol =2)
names(wangu[,1])<-"A"

names(wangu[, 1])<-LETTERS
wangu[, 2]<-letters
wangu[21,2]
wangu
names(wangu[, 1])<-birasa # this for adding attributes names....


## how do we use S3 system and its role
num<--1000001
class(num) <- c("POSIXct", "POSIXt")
print(num)
unclass(num)
birasa<-"1969-12-20 12:13:19 SAST"
birasa
unclass(birasa)
num
num==birasa
unclass(num)
unclass(birasa)

# how to Does R's S3 SYSTEM work?
one_play<-play()
one_play
# the following code is used to assign a new attribute to a given R object
attr(one_play, "symbols")<-c("B","0","B")
# the following codes are to retrieve or to checkout if the assigned attribute has worked
attributes(one_play)
# to lookup the value of an attribute, give attr an R object and the name of attribute you would like to look up:
attr(one_play,"symbols")
# if you give an attribute to an atomic vector, like one_play, R will display the attribute beneath the vector's value.However, if the attribute changes the vector’s class,
#R may display all of the information in the vector in a new way (as we saw with POSIXct objects):


# Modify play to return a prize that contains the symbols associated with it as an attribute named symbols. Remove the redundant call to print(symbols):
play20<-function(){
  symbols<-get_symbols()
  prize<-score(symbols)
  attr(prize,"symbols")<-symbols
  prize
}
play20()
birasa<-play20()
birasa
# how can we extract a set of attributes and store them as a new object?
symbols20<-attr(prize,"symbols")
symbols20
attributes(prize)


slot_display <- function(prize){
  # extract symbols
  symbols <- attr(prize, "symbols")
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  # display regular expression in console without quotes
  cat(string)
}


# other means of adjusting the display of our Slot Machine
# how to use both structure and USEMETHOD to adjust the display of our slot machine?
play<- function() {
  symbols <- get_symbols()
  score(symbols)
  structure(score(symbols),symbols=symbols,class="slots")
}
class(play())

class(one_play) <- "slots"





play20()


# how to check the functionality of the above slot_display function?

one_play
symbols<-attr(one_play,"symbols")
symbols<-paste(symbols,collapse = " ")
symbols
cat(symbols)
symbols

# how to modify play function to display the expected output of the slot machine?
play25<-function() {
  symbols <- get_symbols()
  prize<-score(symbols)
  symbols <- paste(symbols, collapse = " ")
  string <- paste(symbols, prize, sep = "\n$")
  cat(string)
}
play25()

string
cat(string)
play25()

# these are very useful functions to be well known regarding their use. they elaborate the steps taken in designing the above play25() function.
symbols<-get_symbols()
symbols
prize<-score(symbols)
prize
    # turning symbols' output into one value. this essential to avoid multiple values in output.
symbols <- paste(symbols, collapse = " ")
    #combining the outputs of both prize and symbols with a separate \n and a sign of $ to symbolize prize.
string<-paste(symbols,prize,sep="\n$")
string
# the following cat function separate the output of string using the separate \n provided.
cat(string)




print.slots <- function(x, ...) {
  slot_display(x)
}

class(play())
play()

# Notice that if two objects with assigned classes are combined together, R will drop their respective classes.
#e.g:
one_play
two_play<-play()
g<-c(one_play,two_play)
#Here, R stops using print.slots to display the vector because the vector c(play1,play2) no longer has a “slots” class attribute.
class(g)      
#Next, R will drop the attributes of an object (like class) when you subset the object:
play1[1]
one_play[1]

# how can I simulate play function for a million time to check whether the manufacturer's claim was true or not?
rm(prize)


# The following code create a vector named score_new with 1 million values, the values resulted from 
a<-0
score_new<-rep(NA,1000000)
for(i in 1:1000000){
  symbols10<-get_symbols10()
  prize<-score10(symbols10)
  if(score10(symbols10)>=0){
   ayatanzwe<-a+prize
    score_new[i]<-ayatanzwe
  }
}
score_new[score_new==40]
head(score_new,20)
tail(score_new,20)

# another for loop function that adds up 2 numbers a and b until to five iteration,
a<-0
b<-1
for(i in 1:5){
  c=a+b
  print(c)
  a<-b
  b<-c
}

#how do we calculate the expected Value of occurrency among the rolling a pair of loaded dies
die
rolls<-expand.grid(die, die)
rolls
rolls$value<-rolls$Var1 + rolls$Var2
rolls

prob<-c("1"=1/8,"2"=1/8,"3"=1/8,"4"=1/8,"5"=1/8,"6"=3/8)
prob
rolls$prob1<-prob[rolls$Var1]
rolls$prob2<-prob[rolls$Var2]
rolls$prob<-prob[rolls$Var1]*prob[rolls$Var2]
sum(rolls$prob)
rolls
rolls$final<-rolls$value*rolls$prob
rolls
sum(rolls$final)
# what if we want to calculate the expected Value of rolling a pair of fair dies?
die
rolls<-expand.grid(die, die)
rolls
rolls$value<-rolls$Var1 + rolls$Var2
rolls
  # the probabilities of a fair die and their corresponding value will change to look like the following:
prob<-c("1"=1/6,"2"=1/6,"3"=1/6,"4"=1/6,"5"=1/6,"6"=1/6)
prob
rolls$prob1<-prob[rolls$Var1]
rolls$prob2<-prob[rolls$Var2]
rolls$prob<-prob[rolls$Var1]*prob[rolls$Var2]
sum(rolls$prob)
rolls
rolls$final<-rolls$value*rolls$prob
rolls
sum(rolls$final)



# Now that we have already gotten an information on how to calculate the expected value of rolling pair of dies, let's try to calculate the expected Value of playing a  Slot Machine, using the above acquired knowledge:
slots<-expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE )
slots
prob<-c("DD"=0.03,"7"=0.03,"BBB"=0.06,"BB"=0.1,"B"=0.25,"C"=0.01,"0"=0.52)
prob
head(slots)
slots$prob1<-prob[slots$Var1]
slots$prob2<-prob[slots$Var2]
slots$prob3<-prob[slots$Var3]
slots
slots$prob<-prob[slots$Var1]*prob[slots$Var2]*prob[slots$Var3]
slots
head(slots)

symbol<-c(slots[1,1],slots[1,2],slots[1,3])
symbol<-c("7","7","7")
symbol
#to check if the score function will work while filling up the slots%prize column
score(symbol)
rm(symbol)
# how to create lookup values for prize in the corresponding slots values?

slots$prize_mine<-NA
slots$VALUE_mine<-NA
head(slots)
# this is how we fillup the above created prize column, which is very essential in calculating the Expected Value.
for(i in 1:343){
  symbol<-c(slots[i, 1], slots[i, 2], slots[i, 3])
  slots$prize_book[i]<-score(symbol)  
}
head(slots)
# Calculating the individual Expected Values by multiplying the occurrence probability with the related prizes
slots$VALUE_book<-slots$prob*slots$prize_book
# the expected value which is the payout rate is finally calculated as the following:
sum(slots$prob)

view(slots)



#### comparison of the codes in the book and mine code:
mine_code<-slots$VALUE
view(mine_code)

book_code<-slots$VALUE
slots[book_code==mine_code, ]
slots$book<-book_code
slots$mine<-mine_code
# TO DISPLAY THE DATA whose codes of mine and that of the book are contrary....
view(slots[!slots$prize_mine==slots$prize_book, ])

# Kureba data ukoresheje table view muri Studio, we use the following code
view(slots)



# this is to be well explored, for further details.
# this function does not work, but it has been replaced by the above function.
for(i in 1:343){
slots$Var1[i]==slots$Var2[i] && slots$Var2[i]==slots$Var3[i]
symbol<-slots$var1[i]
# not that If statements are not used to fill up sets of objects, we rather use for loops.
if(symbol=="DD"){
  slots$prize[i]<-800
} else if(symbol=="7"){
  slots$prize[i]<-80
} else if(symbol=="BB"){
  slots$prize[i]<-40
} else if(symbol=="BB"){
  slots$prize[i]<-25
} else if(symbol=="B"){
  slots$prize[i]<-10
} else if(symbol=="C"){
  slots$prize[i]<-10
} else if(symbol=="0"){
  slots$prize[i]<-0
} 
}
# until here, the above function does not properly work.



# ANOTHER SCORE FUNCTION FROM THE BOOK
score<- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  # identify case
  # since diamonds are wild, only nondiamonds
  # matter for three of a kind and all bars
  slats <- symbols[symbols != "DD"]
  same <- length(unique(slats)) == 1
  bars <- slats %in% c("B", "BB", "BBB")
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slats[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0,2, 5)[cherries + diamonds]
  } else {
    prize <- 0
  }
  # double for each diamond
  prize * 2^diamonds
}
sum(slots$VALUE)
symbols<-c("BB","B","DD")
symbols<-c("B","BBB","DD")
symbols<-c("BB","BBB","DD")
score11(symbols)
score(symbols)



symbols<-c("7","C","C")
score(symbols)



# how do I use while loop to assess how long it takes to broke while playing on the slot machine?

play_broke<-function(cash){
  n<-0
  while (cash>1) {
    cash<-cash-1+play()
    n<-n+1
  }
  n
}
play_broke(100)


# how do I use repeat loops to play until broke on the slot machines?
play_broke_rp<-function(cash){
  n<-0
  repeat{
    cash<-cash-1+play()
    n<-n+1
    if(cash<=0){
      break
    }
  }
  n
  }
play_broke_rp(100)




# how to check which day corresponds to the given number x?
weekday<-function(x){
switch(x,
        '1'=print("monday"),
        '2'=print("tuesday"),
        '3'=print("wednesday"),
        '4'=print("thursday"),
        '5'=print("friday"),
        '6'=print("saturday"),
        '7'=print("sunday"),
        print("Not a week day")
  )
}
weekday(5)


## to check if the for loop can create an object that is not outside it:...
igihembo<-rep(0,10)

for(i in 1:10){
  n<-1
  igihembo[i]<-igihembo[i]+igihembo[i+1]
  print(igihembo)
}


### how do we speed up the execution of our code in R?
abs_loop
abs_set
