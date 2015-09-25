(*
  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 1: Tuples, functions, basic pattern matching
  ------------------------------------
  Name:
  Student ID:
  ------------------------------------
  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will be graded. It has to be submitted to the TUT git system using
  the instructions on the course web page by September 25, 2015.
*)

// 1. Make an instance of a tuple with 3 elements, where all elements are of different types.
let someTuple  = "This is a string", 24, 30.5
// 2. Make an instance of a tuple of type int * int * int * int .
let someIntTuple (a,b,c) = int a,int b, int c
// 3. Make a function that takes two arguments, an integer and a five element tuple of integers
// that returns true if the sum of any three elements of the tuple is greater than the first argument, else false.
let function1 (someNum:int) aTuple = 
    match aTuple with
    |(a,b,c,_,_) when a+b+c > someNum -> true | (a,b,_,d,_) when a+b+d > someNum -> true
    |(a,b,_,_,e) when a+b+e > someNum -> true | (a,_,c,d,_) when a+c+d > someNum -> true
    |(a,_,c,_,e) when a+c+e > someNum -> true | (a,_,d,_,e) when a+d+e > someNum -> true
    |(_,b,c,d,_) when b+c+d > someNum -> true | (_,b,c,_,e) when b+c+e > someNum -> true 
    |(_,b,d,_,e) when b+d+e > someNum -> true | (_,_,c,d,e) when c+d+e > someNum -> true 
    |_ -> false
    //
// 4. Make a function that takes a tuple of five integers and returns a tuple of five integers where at the 
// position of the largest element would be the result of division of the largest element with the smallest one greater than one.
let finderfun tuple1 =

    let findMin num (a,b,c,d,e) =
        if   num < b && num < c && num < d && num < e && num >1 then true
        elif num < a && num < c && num < d && num < e && num >1 then true
        elif num < b && num < a && num < d && num < e && num >1 then true
        elif num < b && num < c && num < a && num < e && num >1 then true
        elif num < b && num < c && num < d && num < a && num >1 then true
        else false

     let findMax num (a,b,c,d,e)=
       if   num>a && num>c && num>d && num>e    then true
       elif num>a && num>c && num>d && num>e    then true
       elif num>d && num>b && num>d && num>e    then true
       elif num>a && num>b && num>c && num>e    then true
       elif num>a && num>b && num>c && num>d    then true
       else  false 
     
     let methodone  tuple=  
          match  tuple with
          | (a,b,c,d,e) when findMax a tuple && findMin b  tuple ->  (a/b,b,c,d,e)
          | (a,b,c,d,e) when findMax a tuple && findMin c  tuple ->  (a/c,b,c,d,e)
          | (a,b,c,d,e) when findMax a tuple && findMin d  tuple ->  (a/d,b,c,d,e)
          | (a,b,c,d,e) when findMax a tuple && findMin e  tuple ->  (a/e,b,c,d,e)

          | (a,b,c,d,e) when findMax b tuple && findMin a  tuple ->  (a,b/a,c,d,e)
          | (a,b,c,d,e) when findMax b tuple && findMin c  tuple ->  (a,b/c,c,d,e)
          | (a,b,c,d,e) when findMax b tuple && findMin d  tuple ->  (a,b/d,c,d,e)
          | (a,b,c,d,e) when findMax b tuple && findMin e  tuple ->  (a,b/e,c,d,e)

          | (a,b,c,d,e) when findMax c tuple && findMin a  tuple ->  (a,b,c/a,d,e)
          | (a,b,c,d,e) when findMax c tuple && findMin b  tuple ->  (a,b,c/b,d,e)
          | (a,b,c,d,e) when findMax c tuple && findMin d  tuple ->  (a,b,c/d,d,e)
          | (a,b,c,d,e) when findMax c tuple && findMin e  tuple ->  (a,b,c/e,d,e)

          | (a,b,c,d,e) when findMax d tuple && findMin a  tuple ->  (a,b,c,d/a,e)
          | (a,b,c,d,e) when findMax d tuple && findMin b  tuple ->  (a,b,c,d/b,e)
          | (a,b,c,d,e) when findMax d tuple && findMin c  tuple ->  (a,b,c,d/c,e)
          | (a,b,c,d,e) when findMax d tuple && findMin e  tuple ->  (a,b,c,d/e,e)

          | (a,b,c,d,e) when findMax e tuple && findMin a  tuple ->  (a,b,c,d,e/a)
          | (a,b,c,d,e) when findMax e tuple && findMin b  tuple ->  (a,b,c,d,e/b)
          | (a,b,c,d,e) when findMax e tuple && findMin c  tuple ->  (a,b,c,d,e/c)
          | (a,b,c,d,e) when findMax e tuple && findMin d  tuple ->  (a,b,c,d,e/d)

          |_->  tuple
// 6. Make 3 functions that return the average of the value of elements in
// tuples containing 2, 3, or 4 elements of float type.
let of2elts tuple1 =
    match tuple1 with 
    | (a,b) -> (a+b)/2.0  
//    | _ -> -1.0

let of3elts  tuple1 =
    match tuple1 with
    | (a,b,c) -> (a+b+c)/3.0 
//    |_ -> -1.0

let of4elts tuple1 =
    match tuple1 with
    | (a,b,c,d) -> (a+b+c+d)/4.0 
//    |_ -> -1.0
//of4elts (1.0,2.0,3.0,4.0)
// 7. Write 3 functions 'allcaps2', 'allcaps3' and 'allcaps4' which capitalize
// the strings in a tuple of strings where the size of the tuple is either 2,3, or 4.
// Hint: "abc".ToUpper ()

let allcaps3 tupleOfstrings =
    match tupleOfstrings with
   |(a:string, b:string, c:string) -> (a.ToUpper(),b.ToUpper(),c.ToUpper())
//    |_-> "oops"

let allcaps4 tupleOfstrings =
     match tupleOfstrings with
    |(a:string, b:string, c:string, d:string) -> (a.ToUpper(),b.ToUpper(),c.ToUpper(),d.ToUpper())

let allcaps5 tupleOfstrings =
     match tupleOfstrings with
    |(a:string, b:string, c:string, d:string, e:string) -> (a.ToUpper(),b.ToUpper(),c.ToUpper(),d.ToUpper(), e.ToUpper())
 



