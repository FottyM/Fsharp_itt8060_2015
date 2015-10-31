

(*ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Records, List.collect and Charting

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
  coursework5.fsx in directory coursework5. The downloaded data should go in
  atoms.xml in the coursework5 directory.

  The deadline for completing the above procedure is Friday,
  October 30, 2014.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.*)

// 1) Define a record type with the name FileMetaData representing the name
//    of the file and the size of it. 
open System.IO
type FileMetadata = {
    name:string
    size:int64 }

// 2) Make a function that takes a directory as input  
//    and produces a list of instances of the FileMetaData record type defined in Q 1.

let directory dir = dir|> Directory.GetFiles|> List.ofSeq|> List.map(fun z -> {name = Path.GetFileName (z) ; size = FileInfo(z).Length})

directory "c:\Windows"


// 3) Make a function called getFileMetadata of type string list -> FileMetaData list
//    that takes a list of directories as strings and returns a list of FileMetaData records of
//    all files contained in the directories. You are encouraged to use List.collect
//    in the solution

let ls = ["c:\Windows";"c:\Program files"]

let getFileMetaData (lisy:list<string>): list<FileMetadata> = List.collect(fun x -> x|>Directory.GetFiles |> List.ofSeq|> List.map(fun z -> {name = Path.GetFileName (z) ; size = FileInfo(z).Length})) lisy
getFileMetaData ls

// 4) Make a function that displays a histogram chart showing the distribution of
//    the file sizes given a list of FileMetaData records.

let getFileSizes (lisy:list<string>)= List.collect(fun x -> x|>Directory.GetFiles |> List.ofSeq|> List.map(fun z -> FileInfo(z).Length)) lisy
getFileSizes ls

#r @"..\packages\FSharp.Charting.0.90.13\lib\net40\FSharp.Charting.dll"
#load @"..\packages\FSharp.Charting.0.90.13\FSharp.Charting.fsx"

open FSharp.Charting
open FSharp.Charting.ChartTypes

let countGreaterThan0 list =
   let mutable counter = 0
   for i in list do
       if i > 0L && i < 1000L then counter <- counter + 1
   counter

let countGreaterThan1000 list = 
   let mutable counter = 0
   for i in list do
       if i > 1000L && i < 10000L then counter <- counter + 1
   counter

let countGreaterThan10000 list = 
   let mutable counter = 0
   for i in list do
       if i > 10000L && i < 50000L then counter <- counter + 1
   counter

let countGreaterThan50000 list = 
   let mutable counter = 0
   for i in list do
       if i > 50000L && i < 100000L then counter <- counter + 1
   counter

let countGreaterThan100000 list =  
   let mutable counter = 0
   for i in list do
       if i > 100000L && i < 1000000L then counter <- counter + 1
   counter

let resultsData = 
    [ ">0", (countGreaterThan0 (getFileSizes ls)); 
      ">1000", countGreaterThan1000 (getFileSizes ls); 
      ">10000", countGreaterThan10000 (getFileSizes ls); 
      ">50000", countGreaterThan50000 (getFileSizes ls); 
      ">100000", countGreaterThan100000 (getFileSizes ls) ]

Chart.Column resultsData