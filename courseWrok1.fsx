(*
  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 1: Tuples, functions, basic pattern matching
  ------------------------------------
  Name: Fortunat Mutunda
  Student ID: fomutu
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
let tuple_calculator  tuple_1 =
    let find_max num (el1,el2,el3,el4,el5)=
       if   num>el2 && num>el3 && num>el4 && num>el5    then true
       elif num>el1 && num>el3 && num>el4 && num>el5    then true
       elif num>el1 && num>el2 && num>el4 && num>el5    then true
       elif num>el1 && num>el2 && num>el3 && num>el5    then true
       elif num>el1 && num>el2 && num>el3 && num>el4    then true
       else  false

    let find_min num (el1,el2,el3,el4,el5) =
       if   num<el2 && num<el3 && num<el4 && num<el5 && num>1   then true
       elif num<el1 && num<el3 && num<el4 && num<el5 && num>1   then true
       elif num<el1 && num<el2 && num<el4 && num<el5 && num>1   then true
       elif num<el1 && num<el2 && num<el3 && num<el5 && num>1   then true
       elif num<el1 && num<el2 && num<el3 && num<el4 && num>1   then true
       else  false

    let operation_computation  tuple=  
          match  tuple with
          | (el1,el2,el3,el4,el5) when find_max el1 tuple && find_min el2  tuple ->  (el1/el2,el2,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el1 tuple && find_min el3  tuple ->  (el1/el3,el2,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el1 tuple && find_min el4  tuple ->  (el1/el4,el2,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el1 tuple && find_min el5  tuple ->  (el1/el5,el2,el3,el4,el5)

          | (el1,el2,el3,el4,el5) when find_max el2 tuple && find_min el1  tuple ->  (el1,el2/el1,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el2 tuple && find_min el3  tuple ->  (el1,el2/el3,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el2 tuple && find_min el4  tuple ->  (el1,el2/el4,el3,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el2 tuple && find_min el5  tuple ->  (el1,el2/el5,el3,el4,el5)

          | (el1,el2,el3,el4,el5) when find_max el3 tuple && find_min el1  tuple ->  (el1,el2,el3/el1,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el3 tuple && find_min el2  tuple ->  (el1,el2,el3/el2,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el3 tuple && find_min el4  tuple ->  (el1,el2,el3/el4,el4,el5)
          | (el1,el2,el3,el4,el5) when find_max el3 tuple && find_min el5  tuple ->  (el1,el2,el3/el5,el4,el5)

          | (el1,el2,el3,el4,el5) when find_max el4 tuple && find_min el1  tuple ->  (el1,el2,el3,el4/el1,el5)
          | (el1,el2,el3,el4,el5) when find_max el4 tuple && find_min el2  tuple ->  (el1,el2,el3,el4/el2,el5)
          | (el1,el2,el3,el4,el5) when find_max el4 tuple && find_min el3  tuple ->  (el1,el2,el3,el4/el3,el5)
          | (el1,el2,el3,el4,el5) when find_max el4 tuple && find_min el5  tuple ->  (el1,el2,el3,el4/el5,el5)

          | (el1,el2,el3,el4,el5) when find_max el5 tuple && find_min el1  tuple ->  (el1,el2,el3,el4,el5/el1)
          | (el1,el2,el3,el4,el5) when find_max el5 tuple && find_min el2  tuple ->  (el1,el2,el3,el4,el5/el2)
          | (el1,el2,el3,el4,el5) when find_max el5 tuple && find_min el3  tuple ->  (el1,el2,el3,el4,el5/el3)
          | (el1,el2,el3,el4,el5) when find_max el5 tuple && find_min el4  tuple ->  (el1,el2,el3,el4,el5/el4)

          |_->  tuple
      
    operation_computation tuple_1

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
 



