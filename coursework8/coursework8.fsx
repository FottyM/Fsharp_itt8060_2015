(*

  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 8: Sequences and computation expressions

  ------------------------------------------------
  Name:Fortunat Mutunda
  Student ID:fomutu
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file coursework8.fsx in
  directory coursework8.

  The deadline for completing the above procedure is Friday, December 4, 2015.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

// 1. Make a declaration for the sequence of odd numbers.

let oddNumbers = Seq.filter(fun x-> x%2<>0) (Seq.initInfinite(fun x -> x))
oddNumbers

// 2. Declare a function that, for given i and n, selects the sublist [a_i;a_i+1;...;a_i+n-1]
//    of sequence [a_0; a_1; ...]

let sublist sequ (i:int) (n:int) = 
    sequ |> Seq.skip i |> Seq.take (n-1)

sublist oddNumbers  5 10




// 3. Use the functions in the Seq library to declare a function cartesian sqx sqy that gives
// a sequence containing all pairs (x,y) where x is a member of sqx and y is a member of sqy.
//let cartesian sqx sqy =
  //  match (sqx,sqy) with
let seq1 = seq{1..3}
let seq2 = seq{4..6}

let cartesian sqx sqy =
    seq{for x in sqx do 
            for y in sqy do 
                yield (x,y)}

cartesian seq1 seq2
// 4. Make an alternative solution to 3 using sequence expressions.


let cartesianr sqx sqy = 
    sqx |> Seq.collect (fun x -> sqy |> Seq.map (fun y -> x, y))
 
cartesianr seq1 seq2
// 5. Extend the logging workflow explained in the lecture to support downloading web pages.
// Log the http response code for each URL. Make a function that takes a sequence of URLs and 
// downloads them while logging the response codes using the logging workflow.

