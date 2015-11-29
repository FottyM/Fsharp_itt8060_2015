(*

  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Generating data for testing

  ------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as
  file coursework7.fsx in directory coursework7.

  Please do not upload any DLL-s. Just include a readme.md file containing the 
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 20, 2015.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
You should now have a BrokenRomanNumbers implementation and your own CorrectRomanNumbers
implementation from coursework 6.
*)
#r @"..\packages\FsCheck.2.2.3\lib\net45\FsCheck.dll"
#r @"..\BrokenRomanNumbers.dll"
open FsCheck
open BrokenRomanNumbers

//CorrectRomanNumbers
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
    
    checkForOtherOptions ["IIII";"XXXX";"CCCC";"VXL";"IXLCD";"LCDM";"VV";"LL";"DD";"LC";"IL";"IC";"ID";"IM";"VL";"VC";"VD";"VM";"XD";"XM";"LM";""]
(*   1) Write at least 3 FsCheck properties that you can use to check both
     implementations of Roman numeral conversions. The properties should return
     "OK" in the case of correct implementation, and "Falsifyable" in the
     broken implementation.
*)

let createStringFromChars (cs : char list) = 
    gen {
       let! chars = Gen.arrayOf (Gen.elements cs)
       return System.String.Concat(chars)
    }

let chooseFromList xs =
   gen {
       let! idx = Gen.choose(0, List.length xs - 1)
       return (List.nth xs idx)
   }

let testBroken1 =
    Prop.forAll (Arb.fromGen (chooseFromList ["IIII"; "VV"; "XXXX"; "LL"; "CCCC"; "DD"; "MMMMMM"; "I"; "V"; "X"; "L"; "C"; "D"; "M"] ))
                 (fun p -> try if p <> "IIII" && p <> "VV" && p <> "XXXX" && p <> "LL" && p <> "CCCC" && p <> "DD" && p <> "MMMMMM" then (box (BrokenRomanNumbers.convert(p)) :? int) else false with | _ -> true)

Check.Quick testBroken1

let testCorrect1 =
    Prop.forAll (Arb.fromGen (chooseFromList ["IIII"; "VV"; "XXXX"; "LL"; "CCCC"; "DD"; "MMMMMM"; "I"; "V"; "X"; "L"; "C"; "D"; "M"] ))
                 (fun p -> try (box (converter(p)) :? int) with | _ -> true)

let testBroken2 =
    Prop.forAll (Arb.fromGen (chooseFromList ["IX"; "LIX"; "XCI"; "CDXCI"; "CMI";] ))
                 (fun p -> try if p <> "IX" && p <> "LIX" && p <> "XCI" && p <> "CDXCI" && p <> "CMI" then (box (BrokenRomanNumbers.convert(p)) :? int) else false with | _ -> true)

Check.Quick testBroken2

let testCorrect2 =
    Prop.forAll (Arb.fromGen (chooseFromList ["IX"; "LIX"; "XCI"; "CDXCI"; "CMI";] ))
                 (fun p -> try (box (converter(p)) :? int) with | _ -> true)
Check.Quick testCorrect2

let testBroken3 =
    Prop.forAll (Arb.fromGen (chooseFromList ["LC"; "IL"; "IC"; "ID"; "IM";"VL";"VM";"VD";"XD";"XM";"LM"] ))
                 (fun p -> try if p <> "LC" && p <> "IL" && p <> "IC" && p <> "ID" && p <> "IM" && p <> "VL" && p <> "VL" && p <> "VM" && p <> "VD" && p <> "XD" && p <> "XM" && p <> "LM" then (box (BrokenRomanNumbers.convert(p)) :? int) else false with | _ -> true)

Check.Quick testBroken3

let testCorrect3 =
    Prop.forAll (Arb.fromGen (chooseFromList ["LC"; "IL"; "IC"; "ID"; "IM";"VL";"VM";"VD";"XD";"XM";"LM"] ))
                 (fun p -> try (box (converter(p)) :? int) with | _ -> true)
Check.Quick testCorrect3
 
Check.Verbose testCorrect3



(*   2) Write at least 1 data generator and at least 1 FsCheck property for the following code: *)

type Client = 
  { Name : string; Income : int ; YearsInJob : int
    UsesCreditCard : bool;  CriminalRecord : bool }

type QueryInfo =
  { Title     : string
    Check     : Client -> bool
    Positive  : Decision
    Negative  : Decision }

and Decision = 
   | Result of string
   | Query  of QueryInfo

let rec tree =
   Query  {Title = "More than €40k"
           Check = (fun cl -> cl.Income > 40000)
           Positive = moreThan40
           Negative = lessThan40}
and moreThan40 =
   Query  {Title = "Has criminal record"
           Check = (fun cl -> cl.CriminalRecord)
           Positive = Result "NO"
           Negative = Result "YES"}
and lessThan40 =
   Query  {Title = "Years in job"
           Check = (fun cl -> cl.YearsInJob > 1)
           Positive = Result "YES"
           Negative = usesCreditCard}
and usesCreditCard =
   Query  {Title = "Uses credit card"
           Check = (fun cl -> cl.UsesCreditCard)
           Positive = Result "YES"
           Negative = Result "NO"}

let rec testClientTree client tree =
    match tree with
    | Result msg  -> printfn " OFFER A LOAN: %s" msg ; msg // Added that msg gets returned, otherwise side effects are hard to test.
    | Query qinfo -> let result, case = 
                         if qinfo.Check(client) then
                             "yes", qinfo.Positive
                         else
                             "no", qinfo.Negative
                     printfn " - %s ? %s" qinfo.Title result
                     testClientTree client case


// this could be one way of doing this
let clientGenerator = Arb.generate<Client>
//Gen.sample 1 2 clientGenerator
//Check.QuickAll clientGenerator
let rnd = Random();
let getRandArrElement' (arr :list<'a>) =
  arr |> Seq.item (rnd.Next arr.Length)

let chooseFromList' xs =
    gen {
        return getRandArrElement' xs
    }


let person =
   gen {
      let! name = (Gen.oneof [ gen {return "Mutunda"};gen {return "Fortunat"};gen {return "Le Duc"}])
      let! income = (chooseFromList' [100..40000])
      let! yearsOfexperiance = chooseFromList' [0..30]
      let! creditCardStatus = (Gen.oneof [ gen { return true }; gen { return false } ])
      let! criminialRecord = (Gen.oneof [ gen { return true }; gen { return false } ])
      return { 
         Name = name; 
         Income = income;
         YearsInJob = yearsOfexperiance;
         UsesCreditCard = creditCardStatus;
         CriminalRecord = criminialRecord }
   }

let checkClient = Prop.forAll(  Arb.fromGen ( person ))  (fun x ->testClientTree x tree )

Check.Quick checkClient

