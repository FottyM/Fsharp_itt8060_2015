(*

  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, unit of measure

  ------------------------------------
  Name:Fortunat Mutunda
  TUT Student ID:fomutu
  ------------------------------------


  Answer the questions below.  You answers to questions 1--5 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 2, 2015.
*)

// 1. Make a value sl containing empty list of type string list.

let (sl:string list) = []



// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]

let rec shuffle (numbers:int list) =
        match numbers with
        |head::tail -> head :: shuffle( List.rev( tail))
        |[] -> []
shuffle [1;2;3;4;5;6;7;8]

let  shuffley (numbers:int list) =
    let rec loop numbers acc =
        match numbers with
        |head::tail -> loop (List.rev(tail)) (head::acc)
        |[] -> List.rev(acc)
    loop numbers []
shuffley [1;2;3;4;5;6;7;8]

     

// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  [] 
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]

let rec breaker (list:int list) =
    match list with
    |head::tail when head <= tail.Head -> head:: breaker tail
    |head::tail when head > tail.Head -> head :: []
breaker [3;4;5;5;1;2;3]

let rec breaker2 (list:int list) =
    match list with
    |head::tail when head <= tail.Head -> breaker2 tail 
    |head::tail when head > tail.Head -> tail 
breaker2 [3;4;5;6;1;2;3]

let rec segments (list:int list) =
        match list with
        |head::tail when head < tail.Head -> (breaker list) :: [breaker2 list]
        |head::tail when head >= tail.Head -> []
        
segments [3;4;5;5;1;2;3;1;3;4]


// 4. Write a function convertSpeeds: float<miles/hour> list -> float<km/hour> list that
// converts all values in mph in the initial list to km/h in the returned list.

[<Measure>] type km
[<Measure>] type mile
[<Measure>] type h

let milesForKm = 1.60934<km/mile>
let rec converter (speed: float<mile/h> list) = 
    match speed with
    |head::tail -> (head * milesForKm ) :: converter tail 
    |[] -> []

converter [18.0<mile/h>]
// 5. Write a function that from a list of values in degrees Celsius filters
// out values that exceed a predefined limit.


[<Measure>] type C
let theTerminator (list:float<C> list) (limit:float<C>) = 
    List.filter (fun x -> x < limit ) list

theTerminator [0.0<C>;1.2<C>;5.6<C>;2.6<C>;3.5<C>;4.5<C>] 4.2<C>

