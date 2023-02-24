module Verification.Components

type Signedness =
    | Signed
    | Unsigned

type Comparison =
    | LessThan of Signedness
    | LessThanOrEqual of Signedness
    | GreaterThan of Signedness
    | GreaterThanOrEqual of Signedness
    | Equal
    
type AssertionOutput =
    | AssertHIGH
    | AssertLOW

type Width = int

// TODO(jpnock): add more verification components.
type Type =
    | SignExtend of Width
    | ZeroExtend of Width
    | Add
    | Subtract
    | Multiply of Signedness
    | Divide of Signedness
    | Modulo of Signedness
    | Power of Signedness
    | Comparison of Comparison
    | AssertionOutput of AssertionOutput


type Descriptor =
    { Name: string
      Description: string
      Type: Type }

let basicTwoInputDescription (action: string) (signedness: Signedness) =
    match signedness with
    | Signed -> $"{action} the two inputs, treating them as signed values"
    | Unsigned -> $"{action} the two inputs, treating them as unsigned values"

let comparatorDescription (comparison: string) (signedness: Signedness) =
    match signedness with
    | Signed -> $"Outputs HIGH if input A is {comparison} input B, treating them as signed values"
    | Unsigned -> $"Outputs HIGH if input A is {comparison} input B, treating them as unsigned values"

let createComparatorDescriptors (name: string) typ (comparison: string) =
    [ { Name = $"{name} (Unsigned)"
        Description = comparatorDescription comparison Unsigned
        Type = Comparison(typ Unsigned) }
      { Name = $"{name} (Signed)"
        Description = comparatorDescription comparison Signed
        Type = Comparison(typ Signed) } ]

let components: Descriptor list =
    ([ { Name = "Sign Extend"
         Description = "Sign extends the input to the given width"
         Type = SignExtend 0 }
       { Name = "Zero Extend"
         Description = "Zero extends the input to the given width"
         Type = ZeroExtend 0 }
       { Name = "Assert HIGH"
         Description = "Causes an assertion if the input is not HIGH"
         Type = AssertionOutput AssertHIGH }
       { Name = "Assert LOW"
         Description = "Causes an assertion if the input is not LOW"
         Type = AssertionOutput AssertLOW }
       { Name = "Add"
         Description = "Adds the two inputs"
         Type = Add }
       { Name = "Subtract"
         Description = "Subtracts input B from input A"
         Type = Subtract }
       { Name = "Multiply (Unsigned)"
         Description = basicTwoInputDescription "Multiplies" Unsigned
         Type = Multiply Unsigned }
       { Name = "Multiply (Signed)"
         Description = basicTwoInputDescription "Multiplies" Signed
         Type = Multiply Signed }
       { Name = "Divide (Unsigned)"
         Description = basicTwoInputDescription "Divides" Unsigned
         Type = Divide Unsigned }
       { Name = "Divide (Signed)"
         Description = basicTwoInputDescription "Divides" Signed
         Type = Divide Signed }
       { Name = $"Equal"
         Description = "Outputs HIGH if both inputs are equal"
         Type = Comparison(Equal) } ]
     @ (createComparatorDescriptors "Less than" LessThan "less than"
        @ createComparatorDescriptors "Less than or equal" LessThanOrEqual "less than or equal to"
          @ createComparatorDescriptors "Greater than" GreaterThan "greater than"
            @ createComparatorDescriptors "Greater than or equal" GreaterThanOrEqual "greater than or equal to"))

let symbolName (typ:Type) =
    match typ with
    | SignExtend _ -> "Sign\nExtend"
    | ZeroExtend _ -> "Zero\nExtend"
    | Comparison comparison ->
        match comparison with
        | LessThan _ -> "A < B"
        | GreaterThan _ -> "A > B" 
        | LessThanOrEqual _ -> "A <= B" 
        | GreaterThanOrEqual _ -> "A >= B"
        | Equal -> "A==B"
    | Add -> "+"
    | Subtract -> "-"
    | Multiply _ -> "*"
    | Divide _ -> "/"
    | Modulo _ -> "%"
    | Power _ -> "pow(A,B)"
    | AssertionOutput out ->
        match out with
        | AssertLOW -> "Assert\nLOW"
        | AssertHIGH -> "Assert\nHIGH"

let getPrefix (typ:Type) =
    match typ with
    | _ -> "VERIFICATION"

let generateVerilog (typ:Type) =
    // TODO(jpnock): Support verilog implementation of verification components
    match typ with
    | _ -> ""

let numInputs (typ:Type) =
    match typ with
    | AssertionOutput _ | SignExtend _ | ZeroExtend _ -> 1
    | _ -> 2

let numOutputs (typ:Type) =
    match typ with
    | AssertionOutput _ -> 0
    | _ -> 1
    
let hasNumberOfBitsField (typ:Type) =
    match typ with
    | SignExtend bits
    | ZeroExtend bits -> Some bits
    | _ -> None

let outputPortWidths (typ:Type) (inputPortWidths: Map<int, int>) =
    match typ with
    | AssertionOutput _ -> [1]
    | Comparison _ -> [1]
    | SignExtend width | ZeroExtend width -> [width]
    // TODO(jpnock): Confirm this is the correct behaviour
    | _ ->
        printf $"got input ports of {inputPortWidths}"
        [Map.values inputPortWidths |> Seq.max]
