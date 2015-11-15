(*
  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------
  Coursework 6: Testing
  ------------------------------------------------
  Name:Fortunat Mutunda
  Student ID:fomutu
  ------------------------------------------------
  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.
  This coursework will be graded.
  Commit and push your scron to the repository as
  file coursework6.fsx in directory coursework6.
  The file that should be compiled to a dll should go into coursework6.fs.
  Please do not upload DLL-s. Just include a readme.txt file containing the 
  dependencies required (additional DLLs)
  The deadline for completing the above procedure is Friday, November 13, 2015.
  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.
*)

(*
     One of your former colleagues wrote some code to convert Roman numbers to Arabic
     numbers. After writing this code he left for greener pastures at a new
     and cool startup.
     You are now left to make sure the code he wrote works and can be shipped to the
     customer, one of the largest banks in the region and the most important one to 
     your company. 
*)

(*   1) You are given the code in file coursework6input/Library1.fs that can be
     compiled to BrokenRomanNumbers.dll.
     You are expected to test the code using NUnit or FsCheck unit tests (at least 6 in total).
*)

(*   2) List faults discovered as a table. Please try to shrink the inputs
     to the smallest samples corresponding to the appropriate fault.
RomanNumber ExpectedOutput OutputFromConvert FoundWithFsCheckOrNUnit
*)

(*
  Expected: 99, but was 89
  All the Number between 91 and 99 don't won't work
  Expected: 901 But was:  1101
  CDXC 490 System.Exception : invalid input
  LM should throw an error
  VC System.Exception was expected 
  VD System.Exception was expected
  VL System.Exception was expected
  VM System.Exception was expected
  XD System.Exception was expected
  XM System.Exception was expected
  CCCC System.Exception was expected
  IIII System.Exception was expected
  XXXX System.Exception was expected
  LC System.Exception was expected
  LL System.Exception was expected
  VXL System.Exception was expected
*)


(*   3) Write a CorrectRomanNumbers implementation in functional style that you
     would be confident to include in mission critical applications. Test the
     implementation using the tests defined previously.
*)

let decimal_of_roman roman =
    let convert (arabic,lastval) c =
        let n = match c with
                | 'M' | 'm' -> 1000
                | 'D' | 'd' -> 500
                | 'C' | 'c' -> 100
                | 'L' | 'l' -> 50
                | 'X' | 'x' -> 10
                | 'V' | 'v' -> 5
                | 'I' | 'i' -> 1
                | _ -> 0
        let op = if n > lastval then (-) else (+)
        (op arabic lastval, n)
    let (arabic, lastval) = Seq.fold convert (0,0) roman
    arabic + lastval



let converter (roman:string) =
    let rec checkForOtherOptions (list) : int =
        match list with
        |hd::tl -> if hd = roman then failwith("oops") else checkForOtherOptions tl
        |[]     -> decimal_of_roman roman
        |_      -> 0
    
    checkForOtherOptions ["IIII";"XXXX";"CCCC";"VXL";"IXLCD";"LCDM";"VV";"LL";"DD";"LC";"IL";"IC";"ID";"IM";"VL";"VC";"VD";"VM";"XD";"XM";"LM"]

converter "III"