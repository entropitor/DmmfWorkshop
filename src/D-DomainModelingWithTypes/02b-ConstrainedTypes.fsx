﻿// =================================
// This file demonstrates how to define and construct constrained types
//
// Exercise:
//    look at, execute, and understand all the code in this file
// =================================

open System

module ConstrainedTypes =

    /// Define a wrapper type with a *private* constructor.
    /// Only code in the same module can use this constructor now.
    type String10 = private String10 of string

    /// Define a helper module for String10 that
    /// has access to the private constructor
    module String10 =

        /// Expose a public "factory" function
        /// to construct a value, or return an error
        let create str =
            if String.IsNullOrEmpty(str) then
                None
            else if str.Length > 10 then
                None
            else
                Some (String10 str)

        /// Expose a public function
        /// to extract the wrapped value
        let value (String10 str) = str



    /// Define a wrapper type with a *private* constructor.
    /// Only code in the same module can use this constructor now.
    type EmailAddress = private EmailAddress of string

    /// Define a helper module for EmailAddress that
    /// has access to the private constructor
    module EmailAddress =

        /// Expose a public "factory" function
        /// to construct a value, or return an error
        let create str =
            if String.IsNullOrEmpty(str) then
                None
            else if not (str.Contains("@")) then
                None
            else
                Some (EmailAddress str)

        /// Expose a public function
        /// to extract the wrapped value
        let value (EmailAddress str) = str

open ConstrainedTypes

//TODO uncomment to see the compiler error
// let compileError = String10 "1234567890"

// create using the exposed constructor
let validString10 = String10.create("1234567890")
let invalidString10 = String10.create("12345678901")

// create using the exposed constructor
let validEmail = EmailAddress.create("a@example.com")
let invalidEmail = EmailAddress.create("example.com")

// --------------------------------------------
// using constrained types in a workflow
// --------------------------------------------
module MyWorkflows =

    // define a dummy workflow
    let mainWorkflow (str10:String10) : bool =
        // values are immutable and not null
        // so no defensive programming is needed.

        // is the str10 null? NOT NEEDED
        // is the str10.Length <= 10? NOT NEEDED
        true


// --------------------------------------------
// Using a workflow with constrained types
// --------------------------------------------

module Example1 =

    // the main public API that wraps the workflow
    let api input =

        // create a value from the input (eg JSON)
        let str10option = String10.create input

        // If you try to call the workflow without checking if it is valid
        // you will get a compile-time error
        MyWorkflows.mainWorkflow str10option

// test
Example1.api "1234567890"


// Instead, you need to check first
module Example2 =

    // the main public API that wraps the workflow
    let api input =

        // create a value from the input (eg JSON)
        let str10option = String10.create input

        match str10option with
        | Some str10 ->
            // the input is valid, so call the workflow
            let result = MyWorkflows.mainWorkflow str10
            match result with
            | true -> "200 OK"
            | false -> "500 ServerError"
        | None ->
            // the input is not valid, so return an error
            "400 BadRequest"


// test
Example2.api "1234567890"


// --------------------------------------------
// compare two ways of constraining a value
// 1. using a type
// 2. using a validation attribute
// --------------------------------------------

type Contact = {
    // 1. putting the validation in the type
    Email: EmailAddress

    // 2. putting the validation in a property attribute
    //[Validation(EmailAddress)]
    Email2: string

    }
