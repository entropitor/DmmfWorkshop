﻿// =================================
// This file demonstrates how to define map the error track
// so that the error types match
//
// Exercise:
//    look at, execute, and understand all the code in this file
// =================================

//----------------------
// A silly domain
//----------------------

type Apple = RedApple | GreenApple | YellowApple
type Banana = LargeBanana | SmallBanana
type Cherry = SweetCherry | TartCherry

type AppleError =
    | AppleIsBrown

type BananaError =
    | BananaIsBrown

// ----------------------------------
// Two functions with different error types
// ----------------------------------

let functionA apple =
    match apple with
    | RedApple -> Ok LargeBanana
    | YellowApple -> Ok SmallBanana
    | GreenApple -> Error AppleIsBrown

let functionB banana =
    match banana with
    | LargeBanana -> Ok SweetCherry
    | SmallBanana -> Error BananaIsBrown


// ----------------------------------
// Now try to chain them together

// The functions can not be chained because the error type doesn't match
(*
let functionAThenB apple =
    apple
    |> functionA
    |> Result.bind functionB  // AppleError does not match BananaError
*)


// ==============================
// Solution
// ==============================

// ----------------------------------
// 1. define a common type with both errors

type FruitError =
| AppleErrorCase of AppleError
| BananaErrorCase of BananaError


// ----------------------------------
// 2. redefine the functions to use the common error type

// NOTE ' can be used in a function name,
// often to indicate a variant/modification of the original

let functionA' apple =   // "functionA'" means a modification of "functionA"
    apple
    |> functionA
    |> Result.mapError AppleErrorCase

let functionB' banana =
    banana
    |> functionB
    |> Result.mapError BananaErrorCase
    // same as
    // functionB >> Result.mapError BananaErrorCase

// ----------------------------------
// 3. now they can be chained

let functionAThenB' apple =
    apple
    |> functionA'
    |> Result.bind functionB'


// test
functionAThenB' Apple.RedApple
functionAThenB' Apple.GreenApple
functionAThenB' Apple.YellowApple

// ----------------------------------
// Alternative -- redefine the new functions inside the pipeline

let functionAThenB_v2 apple =
    // redefine the functions here
    let functionA' = functionA >> Result.mapError AppleErrorCase
    let functionB' = functionB >> Result.mapError BananaErrorCase

    // now build the pipeline
    apple
    |> functionA'
    |> Result.bind functionB'

let functionAThenB_v3 apple =
    // now build the pipeline
    apple
    |> (functionA >> Result.mapError AppleErrorCase)
    |> Result.bind (functionB >> Result.mapError BananaErrorCase)

let functionAThenB_v4 apple =
    // now build the pipeline
    apple
    |> functionA 
    |> Result.mapError AppleErrorCase
    |> Result.bind (functionB >> Result.mapError BananaErrorCase)
