/// Defines helper defaults that are useful when constructing Component IO ports.
///
/// Authored by jpn119 (James Nock)
module IOConstants

open AssertionTypes
open VerificationTypes

/// A single input named A, with no initialised fixed with.
let InputA: ComponentInput = { Name = "A"; FixedWidth = None; DataType = DataTypeUInt; }

/// A single input named B, with no initialised fixed with.
let InputB: ComponentInput = { InputA with Name = "B" }

/// A single input named A at port 0, with no initialised fixed width.
let OneInput: Map<InputPortNumber, ComponentInput> = Map [
    (0, InputA)
]

/// Two inputs named A and B, at ports 0 and 1 respectively, with no
/// initialised fixed width.
let TwoInputs = OneInput.Add (1, InputB)

/// A single input named A at port 0, with fixed width of `width`.
let OneFixedWidthInputA width =
    Map [ (0, {InputA with FixedWidth = Some width}) ]

let OneAssertionInputA =
    Map [ (0, {InputA with FixedWidth = Some 1; DataType = DataTypeAssertionInput}) ]

/// A single output named X, with no initialised fixed width.
let OutputX: ComponentOutput = { Name = "X"; FixedWidth = None; HostLabel = ""; HostSheet = ""}

/// A single output named X at port 0, with no initialised fixed width.
let OneOutput: Map<OutputPortNumber, ComponentOutput> = Map [
    (0, OutputX)
]

/// A single output named X at port 0, with fixed width of `width`.
let OneFixedWidthOutputX width =
    Map [ (0, {OutputX with FixedWidth = Some width}) ]
