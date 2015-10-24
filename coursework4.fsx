(*

  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name:Fortunat Mutunda
  Student ID:fomutu
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 23, 2015.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1


let flattenOption opt =
    match opt with
    |(Some (Some a)) -> Some a
    |Some(None)      -> None
    |None            -> None

flattenOption (None)
flattenOption (Some(Some "Oops"))
flattenOption(Some(Some 2))



// 2. Can flattenOption by implemented using bind? If so, do it!

let bindflattenOption opt = opt |> Option.bind ( fun x -> x) 

bindflattenOption (None)
bindflattenOption (Some(Some "Oops"))
bindflattenOption (Some(Some 2))


// 3. Write a function
//
//    defeatist : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.
//    E.g. [Some 1 ; Some 2] -> Some [1; 2]

let rec defeatist (opt:list<option<'a>>) : option<list<'a>> = 
    let rec loop opt acc =
        match opt with
        |[None] -> None
        |hd::tl -> 
            match hd with
            |None -> None
            |Some a -> loop tl (a::acc)
        |[] -> Some (List.rev(acc))
    loop opt []

let list = [Some "mm"; Some "ll"; Some "ff"]
let list2 = [Some[Some 2];Some[Some 3]; Some[Some 5];Some[None]]

defeatist list
defeatist list2

// 4. Write a function
//
//    optimist : 'a -> list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes with all failures replaced
//    by the first parameter of the function. Pay close attention to the type.
//    E.g. optimist 0 [Some 1; None] -> [1; 0]

let rec mapu2 list value =
 match list with 
 |None :: tail  -> value :: mapu2 tail value
 |Some list :: tail  -> list :: mapu2 tail value
 |_ -> []

let rec explode list uni =
    match list with
    |hd::tl ->
        match hd with
        |None   -> uni :: explode tl uni
        |Some list -> list :: explode tl uni
    |_ -> []

let rec optimist a (list:option<'a> List) = 
  match explode list a with 
  |[]-> [] 
  |hd::tl-> hd::tl

optimist 6 [Some 1; None]


// 5. Write a function
//    chars : list<string> -> list<char>
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

let chars (ch:list<string>) : list<char> = ch |> List.collect(fun i ->  (i |> List.ofSeq))

let listo = ["F#";"IS";"NOT";"A";"PROGRAMING";"WE";"NEED";"BUT";"THE";"ONE";"WE";"DESERVE"]
chars listo


// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"

let iprint (list: int list)  = List.foldBack (fun  elem acc ->  elem.ToString()+ "," + acc) list ("")
let to_s_list = [1..7]
iprint to_s_list 
