﻿// =============================================
// Exercise:
//
// For each of the following definitions,
// say whether it is a simple value or a function,
// and if a function, what is the function signature?
//
// If in doubt, run them to find out!
// =============================================

let testA = 2

let testB x = 2 + x

let testC x = 2.0 + x

let testD = "hello"

let testE = printfn "hello"

let testF () = 42

let testG () = printfn "hello"

let testH x = String.length x

let testI x = sprintf "%i" x

let testJ x = printfn "%i" x

let testK x =
    printfn "x is %f" x
    x // return x

let testL (f: int -> string) x = f x

let testM f (x:int) :string =
    f x

let testN x: string = x 1 // hint: what does :string modify?

let testO x = 1

// val testO : x:'a -> int  // F# signature with a generic type
// int testO<T>(T x)        // C# signature with a generic type

let testP x = x 1 // hint: what kind of thing is x?

let testQ x y = x

let testR x y z = z

let testS x = x = x


// =============================================
// Exercise:
//
// For each of the following signatures, create a function
// that will be *inferred* to have that signature.
//
// Avoid using explicit type annotations!
// =============================================

// val sigA = int -> int
// example of a possible answer:
(*
let sigA x = x + 1
*)

// val sigB = int -> unit
// example of a possible answer:
(*
let sigB x = printfn "%i" x
*)

// val sigC = int -> string
let sigC x = sprintf "%i" x

// val sigD = unit -> string
let sigC () = "test"


// val sigE = string -> string
let sigE x = sprintf "%s" x

// val sigF = int -> bool -> float -> string
let sigF i b f = sprintf "%i %b %f" i b f


// =============================================
// Exercise:
//
// Make the following functions compile by adding
// type annotations. Try to use the minimum annotations possible!
//
// Here's how to do type annotations
// let aFunction (param1:string) (param2:bool) :string =
//                ^1st param      ^2nd param    ^return type
//
// =============================================

// Remove spaces from front and back of a string
// The "s" parameter is a string and it returns a string
let trim (s: string) = s.Trim()

// Return the length of string
// The "s" parameter is a string and it returns an int
let len (s: string) = s.Length

// Replace a substring with new substring
// The s, oldStr,newStr parameters are all string
// and it returns a string
let replace (oldStr: string) newStr (s: string) = s.Replace(oldStr, newStr)
(*
This one is tricky because .NET has two overloads
  Replace(string,string)
  Replace(char,char)
So we need to say what type the parameters are too.
*)


// =============================================
// Exercise (HARD):
// Below are four generic signatures.
// Try to create four functions that are inferred to have these signatures.
// =============================================

// val sigW = 'a -> int
let sigW x = 3

// val sigX = int -> 'a
// let sigX (i: int) = ignore i

// val sigY = 'a -> 'a
let sigY x = x

// val sigZ = 'a -> 'b
// let sigZ x = ignore x
