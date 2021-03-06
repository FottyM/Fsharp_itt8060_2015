(*

  ITT8060 -- Advanced Programming 2015
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name:Fortunat Mutunda
  TUT Student ID:fomutu
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Make a function rearrange: Fexpr -> Fexpr that will rearrange a finite expression tree
// (as defined in the lecture in the differentiation example) in such a way that constants
// always are to the left of variables in addition and multiplication and the variables with
// higher power in multiplication are pushed to the right.
// Add(X, Const 1.0) -> Add(Const 1.0, X)
// Mul(X, Const 1.0) -> Mul(Const 1.0, X)
//Add(Mul (Const 0.0,Mul (Mul (X,X),X)),Mul(Const 2.0,Add(Mul (Const 1.0,Mul (X,X)),Mul (X,Add (Mul (Const 1.0,X),Mul (X,Const 1.0))))))
// -> 
//Add(Mul (Const 0.0,Mul (X,Mul (X,X))),Mul(Const 2.0,Add(Mul (Const 1.0,Mul (X,X)), (X,Add (Mul (Const 1.0,X),Mul (Const 1.0,X))))))

type Fexpr = | Const of float
             | X
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr

let rec rearrange fxp =
    match fxp with
    |Const a -> Const a
    |X -> X
    |Sin fxp1 -> Sin(rearrange fxp1)
    |Cos fxp1 -> Cos(rearrange fxp1)
    |Log fxp1 -> Log(rearrange fxp1)
    |Exp fxp1 -> Exp(rearrange fxp1)
    |Sub (fxp1,fxp2) -> Sub(rearrange fxp1, rearrange fxp2)
    |Div (fxp1,fxp2) -> Div(rearrange fxp1, rearrange fxp2)
    |Add(X, Const a ) -> Add (Const a,X)
    |Add(fxp1,Const a) -> Add(Const a, rearrange fxp1)
    |Add(X, fxp1) -> Add(X,rearrange fxp1)
    |Add(fexp1,fexp2) -> Add(rearrange fexp1, rearrange fexp2)
    |Mul(fexp1,X)-> Mul(X, rearrange fexp1)
    |Mul(X, Const a) -> Mul(Const a, X)
    |Mul(fexp1,Const a) -> Mul(Const a, rearrange fexp1)
    |Mul(fexp1,fexp2) -> Mul(rearrange fexp1, rearrange fexp2)

let a = Add(Mul (Const 0.0,Mul (Mul (X,X),X)),Mul(Const 2.0,Add(Mul (Const 1.0,Mul (X,X)),Mul (X,Add (Mul (Const 1.0,X),Mul (X,Const 1.0))))))
rearrange a

let a2 = Mul (Mul (X,X),X)
rearrange a2



// 2. Make a function simplify: Fexpr -> Fexpr that will simplify a finite expression tree by removing
// terms that evaluate to zero.
//
// For example:
// Add (Const 0.0, X) -> X
// Mul (Const 1.0, X) -> X
//Add
//    (Mul (Const 0.0,Mul (X,Mul (X,X))),
//     Mul
//       (Const 2.0,
//        Add
//          (Mul (Const 1.0,Mul (X,X)),
//           Mul (X,Add (Mul (Const 1.0,X),Mul (Const 1.0,X))))))
// ->
//     Mul
//       (Const 2.0,
//        Add
//          (Mul (X,X),
//           Mul (X,Add (X,X)))




// 3-4: Given the type definition:
 type BList =
  | BEmpty
  | Snoc of BList * int
// 
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.
let prop a = (=) 1

let rec filterB (prop: int -> bool) (list:BList) =
    match list with
    |BEmpty -> BEmpty
    |Snoc (head,tail) -> let filteredHead = filterB prop head
                         if prop tail
                         then Snoc (filteredHead, tail)
                         else filteredHead



// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.



let rec mapB (trans: int -> int) list:BList = 
    match list with
    |BEmpty -> BEmpty
    |Snoc (tl,hd) -> Snoc((mapB trans tl), trans hd)


// 5-7. Given the type definition
 type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree
// 
// 5. Define the value exampleTree : Tree that represents the following
//    tree:


let tree : Tree = Nil

//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *

let thisTree = Branch2(Nil,2,Branch3(Nil,3,Nil,5,Nil))


// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.

let rec sumTree tree = 
    match tree with
    |Nil -> 0
    |Branch2(tree1,ite,tree2) -> ite + sumTree tree1 + sumTree tree2
    |Branch3(tree1,item1,tree2,item2,tree3) -> item1 + item2 + sumTree tree1 + sumTree tree2 + sumTree tree3


// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.
let rec productTree tree =
    match tree with
    |Nil -> 0
    |Branch2(tree1,item,tree2) -> if (item <> 0) then item * productTree tree1 * productTree tree2 else 0
    |Branch3(tree1,item1,tree2,item2,tree3) -> if(item1 <> 0 && item2 <> 0) then item1 * item2 * productTree tree1 * productTree tree2 * productTree tree3 else 0
// ** Bonus questions **

// 8. Define a function mapTree : (int -> int) -> Tree -> Tree that
//    applies the given function to every label of the given tree.

// 9. Use mapTree to implement a function negateAll : Tree -> Tree that
//    negates all labels of a given tree.
