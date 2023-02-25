module Verification.Components

open Thoth.Json

// type Comparison =
//     | LessThan
//     | LessThanOrEqual
//     | GreaterThan
//     | GreaterThanOrEqual
//     | Equal
//
// type AssertionOutput =
//     | AssertHIGH
//     | AssertLOW

// TODO(jpnock): add more verification components.
// type Type =
//     | SignExtend
//     | ZeroExtend
//     | Add
//     | Subtract
//     | Multiply
//     | Divide
//     | Modulo
//     | Power
//     | Comparison of Comparison
//     | AssertionOutput of AssertionOutput

type LibraryID = string

type ComponentInput =
    { Name: string
      FixedWidth: int option
      Signed: bool option }

type ComponentOutput =
    { Name: string
      FixedWidth: int option
      Signed: bool option }

type InputPortNumber = int
type OutputPortNumber = int

type ComponentState =
    { LibraryID: LibraryID
      Inputs: Map<InputPortNumber, ComponentInput>
      Outputs: Map<OutputPortNumber, ComponentOutput> }
    static member Default : ComponentState = {
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
    }

// type Getter<'t> = ComponentState -> 't

type SymbolDetails =
    { Name: string
      Prefix: string 
      Height: float
      Width: float }

type IComponent =
    abstract member GetLibraryID: LibraryID
    abstract member GetName: string
    abstract member GetTooltipText: string
    abstract member GetDefaultState : ComponentState
    abstract member GetSymbolDetails : ComponentState -> SymbolDetails
    abstract member GetDescription : ComponentState -> string
    abstract member GetOutputWidths : ComponentState -> Map<OutputPortNumber, uint>
    

module IODefaults =
    let InputA: ComponentInput = { Name = "A"; FixedWidth = None; Signed = None; }
    let InputB: ComponentInput = { InputA with Name = "B" }
    let OneInput: Map<InputPortNumber, ComponentInput> = Map [
        (0, InputA)
    ]
    let TwoInputs = OneInput.Add (1, InputB)
    
    let OutputX: ComponentOutput = { Name = "X"; FixedWidth = None; Signed = None; }
    let OneOutput: Map<OutputPortNumber, ComponentOutput> = Map [
        (0, OutputX)
    ]

module SymbolDefaults =
    let GridSize = 30.0
    let Width = 3.0 * GridSize
    let Height = 2.0 * GridSize
    let Prefix = "VERI"

type SimpleComponent =
    { 
      Name: string
      SymbolName: string
      DescriptionFunc: SimpleComponent -> ComponentState -> string
      TooltipText: string
      DefaultState: ComponentState }
    interface IComponent with
        member this.GetLibraryID = this.DefaultState.LibraryID
        member this.GetName = this.Name
        member this.GetTooltipText = this.TooltipText
        member this.GetDefaultState = this.DefaultState
        member this.GetSymbolDetails _ =
            { Name = this.SymbolName
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width }
        member this.GetDescription state = this.DescriptionFunc this state
        member this.GetOutputWidths _ =
            Map.empty

let signedDescription _ state =
    let signedOperation =
        state.Inputs.Values
        |> Seq.exists (fun el -> Option.defaultValue false el.Signed)
    match signedOperation with
    | true -> "treating them as signed values"
    | false -> "treating them as unsigned values"

let comparatorDescription comparison comp state =
    $"Outputs HIGH when Input A is {comparison} Output B, {signedDescription comp state}"

let actionDescription action comp state =
    $"{action} the two inputs, {signedDescription comp state}"

let makeIOSigned signed state  =
    {
     state with
        Inputs = state.Inputs |> Map.map (fun _ v -> {v with Signed = Some signed})
        Outputs = state.Outputs |> Map.map (fun _ v -> {v with Signed = Some signed})
    }

let make2In1OutOperators name baseLibraryID symbolName action : IComponent * IComponent =
    let signedName = $"{name} (Signed)"
    let unsignedName = $"{name} (Unsigned)"
    
    let extendLibraryID id signed =
        match signed with
        | true -> $"{id}_SIGNED"
        | false -> $"{id}_UNSIGNED"

    let defaultState = {
        ComponentState.Default with
            Inputs = IODefaults.TwoInputs
            Outputs = IODefaults.OneOutput
            LibraryID = baseLibraryID
    }
    
    let unsignedState =
        { defaultState with LibraryID = extendLibraryID defaultState.LibraryID false }
        |> makeIOSigned false
    
    let signedState =
        { defaultState with LibraryID = extendLibraryID defaultState.LibraryID true }
        |> makeIOSigned true
    
    let unsignedComp = {
        Name = unsignedName
        SymbolName = symbolName
        DescriptionFunc = actionDescription action
        TooltipText = ""
        DefaultState = unsignedState
    }
    
    let signedComp = {
        unsignedComp with
            Name = signedName
            DefaultState = signedState
    }
    
    unsignedComp, signedComp

let components: IComponent list =
    // let signExtend = {
    //         Base = makeGenericOneOutputBase "Sign Extend" SignExtend "Sign\nExtend" IODefaults.OneInput
    //         Width = 0}
    //
    // let zeroExtend = {
    //         Base = makeGenericOneOutputBase "Zero Extend" ZeroExtend "Zero\nExtend" IODefaults.OneInput
    //         Width = 0}
    //
    
    
    let operatorPairs = [
        make2In1OutOperators "Multiply" "PLUGIN_MULTIPLY" "A*B" "Multiplies"
        make2In1OutOperators "Divide" "PLUGIN_DIVIDE" "A/B" "Divides" 
        make2In1OutOperators "Modulo" "PLUGIN_MODULO" "A % B" "Applies A mod B using" 
        make2In1OutOperators "Power" "PLUGIN_POWER" "pow(A, B)" "Applies pow(A, B) using" 
        make2In1OutOperators "Less than" "PLUGIN_COMPARISON_LESS_THAN" "A < B" "less than"
        make2In1OutOperators "Less than or equal" "PLUGIN_COMPARISON_LESS_THAN_OR_EQUAL_TO" "A <= B" "less than or equal to"
        make2In1OutOperators "Greater than" "PLUGIN_COMPARISON_GREATER_THAN" "A > B" "greater than"
        make2In1OutOperators "Greater than or equal" "PLUGIN_COMPARISON_GREATER_THAN_OR_EQUAL_TO" "A >= B" "greater than or equal to"
    ]
    
    let operators =
        operatorPairs
        |> List.collect (fun (unsigned, signed) -> [unsigned; signed])
    
   // { Name = "Assert HIGH"
   //   Description = "Causes an assertion if the input is not HIGH"
   //   Type = AssertionOutput AssertHIGH }
   // { Name = "Assert LOW"
   //   Description = "Causes an assertion if the input is not LOW"
   //   Type = AssertionOutput AssertLOW }


   // { Name = $"Equal"
   //   Description = "Outputs HIGH if both inputs are equal"
   //   Type = Comparison(Equal) } ]
    
    [
       // signExtend
       // zeroExtend
       // make2In1OutComponent "Add" "A+B" "PLUGIN_ADD" "Adds the two inputs"
       // make2In1OutComponent "Sub" "A-B" "PLUGIN_SUB" "Subtracts input B from input A"
    ] @ operators



// let symbolName (typ:Type) =
//     match typ with
//     | SignExtend _ -> "Sign\nExtend"
//     | ZeroExtend _ -> "Zero\nExtend"
//     | Comparison comparison ->
//         match comparison with
//         | LessThan _ -> "A < B"
//         | GreaterThan _ -> "A > B" 
//         | LessThanOrEqual _ -> "A <= B" 
//         | GreaterThanOrEqual _ -> "A >= B"
//         | Equal -> "A==B"
//     | Add -> "+"
//     | Subtract -> "-"
//     | Multiply _ -> "*"
//     | Divide _ -> "/"
//     | Modulo _ -> "%"
//     | Power _ -> "pow(A,B)"
//     | AssertionOutput out ->
//         match out with
//         | AssertLOW -> "Assert\nLOW"
//         | AssertHIGH -> "Assert\nHIGH"

// let outputPortWidths (typ:Type) (inputPortWidths: Map<int, int>) =
//     match typ with
//     | AssertionOutput _ -> [1]
//     | Comparison _ -> [1]
//     | SignExtend | ZeroExtend -> [width]
//     // TODO(jpnock): Confirm this is the correct behaviour
//     | _ ->
//         printf $"got input ports of {inputPortWidths}"
//         [Map.values inputPortWidths |> Seq.max]

type ComponentLibrary =
    { Components: Map<LibraryID, IComponent> }
    member this.register (comp:IComponent) = this.Components.Add (comp.GetLibraryID, comp)

let mutable library: ComponentLibrary = {
    Components =
        components
        |> List.map (fun comp -> (comp.GetLibraryID, comp))
        |> Map.ofList
}

let implementsVariableWidth (state : ComponentState) =
    match state.Inputs.TryFind 0 with
    | Some input when input.FixedWidth.IsSome -> input.FixedWidth
    | _ -> None