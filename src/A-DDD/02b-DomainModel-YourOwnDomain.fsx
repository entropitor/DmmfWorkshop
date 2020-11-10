// define a domain model for a your own domain
module rec YourOwnDomain


// ==============================
// Basic F# Syntax for types
// (see also fsharp-basic-syntax.fsx in parent directory)
// ==============================

(*

// --------------------
// Use Functions for workflows
// --------------------

// a single input and a single output
type Workflow = InputData -> OutputData

// a pair of inputs and a pair of outputs
type Workflow2 = InputData * State -> OutputData * State

// --------------------
// Use Records or tuples for AND
// --------------------

// a record with named fields
type ContactInfo = {
    // FieldName : FieldType
    Name : Name
    Address : Address
    }

// a pair
type ContactInfo = Name * Address

// a triplet
type ContactInfo = Name * Address * Email


// --------------------
// Use Choices for OR
// --------------------

type MyChoice =
    | Choice1 of Choice1Data
    | Choice2 of Choice2Data

// --------------------
// Use type aliases for primitives
// --------------------

// document constraints in a comment
type OrderQuantity = int // must be > 0
type EmailAddress = string // must contain @ symbol

// --------------------
// Use list and option if needed
// --------------------

type Order = {
    OrderLines : OrderLine list
    DeliveryAddress : Address option // optional data
    }


*)


//============================================
// Your code starts here


(* type DiceNumber = One | Two | Three | Four | Five | Six *)
type DiceNumber = Eyes of int // at most 6
type Color = Red | Blue | Green | Yellow

type Dice = {
  Color: Color;
}
type ThrownDice = {
  Number: DiceNumber;
  Color: Color;
}
type DiceInHand = Dice list
type DiceOnTable = ThrownDice list
type PlayerDice = Inhand of DiceInHand | OnTable of DiceOnTable
type Player = {
  Name: string;
  StartColor: Color;
  Dice: PlayerDice;
}

type Bid = {
  Count: int;
  Number: DiceNumber;
}
type AnswerToBid = Rejection of Bid | NewBid of Bid

type ThrowDice = DiceInHand -> DiceOnTable
type Answer = Player list -> Bid -> AnswerToBid
type CountDice = DiceOnTable -> DiceNumber -> int * DiceInHand

(* let throw: ThrowDice = exn *)
let count: CountDice = fun diceOnTable number ->
  let c = diceOnTable |> List.filter (fun d -> d.Number = number) |> List.length
  (c, [])
