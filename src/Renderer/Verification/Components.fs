module Verification.Components

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
type LibraryID = string

type ComponentState =
    { LibraryID: LibraryID
      Inputs: Map<InputPortNumber, ComponentInput>
      Outputs: Map<OutputPortNumber, ComponentOutput> }
    static member Default : ComponentState = {
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
    }

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
    abstract member GetOutputWidths : ComponentState -> Map<InputPortNumber, int> -> Map<OutputPortNumber, int>

module IODefaults =
    let InputA: ComponentInput = { Name = "A"; FixedWidth = None; Signed = None; }
    let InputB: ComponentInput = { InputA with Name = "B" }
    let OneInput: Map<InputPortNumber, ComponentInput> = Map [
        (0, InputA)
    ]
    let OneFixedWidthInputA width =
        Map [ (0, {InputA with FixedWidth = Some width}) ]
    let TwoInputs = OneInput.Add (1, InputB)
    
    let OutputX: ComponentOutput = { Name = "X"; FixedWidth = None; Signed = None; }
    let OneOutput: Map<OutputPortNumber, ComponentOutput> = Map [
        (0, OutputX)
    ]
    let OneFixedWidthOutputX width =
        Map [ (0, {OutputX with FixedWidth = Some width}) ]

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
        member this.GetSymbolDetails state =
            { Name = this.SymbolName
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width }
        member this.GetDescription state = this.DescriptionFunc this state
        member this.GetOutputWidths state inputPortWidths =
            let maxInputWidth = Map.values inputPortWidths |> Seq.max
            state.Outputs
            |> Map.map (fun outputPortNum output ->
                match output.FixedWidth with
                | Some w -> w
                | _ -> maxInputWidth)

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

let makeState outputs inputs libraryID =
    { ComponentState.Default with Inputs = inputs; Outputs = outputs; LibraryID = libraryID }   
   
let makeOneOutputState = makeState IODefaults.OneOutput

let makeSimpleComponent outputs inputs name baseLibraryID symbolName description : IComponent =
    {
        Name = name
        SymbolName = symbolName
        DescriptionFunc = (fun _ _ -> description)
        TooltipText = description
        DefaultState = makeState outputs inputs baseLibraryID
    }

let makeOneOutputComponent = makeSimpleComponent IODefaults.OneOutput
let makeOneInputOneBitComponent = makeSimpleComponent Map.empty (IODefaults.OneFixedWidthInputA 1)
let makeOneInputOneOutputComponent = makeOneOutputComponent IODefaults.OneInput
let makeTwoInputOneOutputComponent = makeOneOutputComponent IODefaults.TwoInputs

let makeSignedPairOfComponents outputs description name baseLibraryID symbolName : IComponent * IComponent =
    let signedName = $"{name} (Signed)"
    let unsignedName = $"{name} (Unsigned)"
    
    let extendLibraryID id signed =
        match signed with
        | true -> $"{id}_SIGNED"
        | false -> $"{id}_UNSIGNED"

    let defaultState = makeState outputs IODefaults.TwoInputs baseLibraryID
    
    let unsignedState =
        { defaultState with LibraryID = extendLibraryID defaultState.LibraryID false }
        |> makeIOSigned false
    
    let signedState =
        { defaultState with LibraryID = extendLibraryID defaultState.LibraryID true }
        |> makeIOSigned true
    
    let unsignedComp = {
        Name = unsignedName
        SymbolName = symbolName
        DescriptionFunc = description
        TooltipText = ""
        DefaultState = unsignedState
    }
    
    let signedComp = { unsignedComp with Name = signedName; DefaultState = signedState }
    unsignedComp, signedComp
    
let makeSignedPairOfOperators actionText =
    makeSignedPairOfComponents IODefaults.OneOutput (actionDescription actionText)
let makeSignedPairOfComparators comparisonText =
    makeSignedPairOfComponents (IODefaults.OneFixedWidthOutputX 1) (comparatorDescription comparisonText)

// TODO(jpnock): add more verification components.
let private components: IComponent list =    
    let signExtend =
        makeOneInputOneOutputComponent
            "Sign Extend" "PLUGIN_SIGN_EXTEND" "Sign\nExtend" "Sign extends the input to the specified width"

    let zeroExtend =
        makeOneInputOneOutputComponent
            "Zero Extend" "PLUGIN_ZERO_EXTEND" "Zero\nExtend" "Zero extends the input to the specified width"
    
    let assertHigh =
        makeOneInputOneBitComponent
            "Assert HIGH" "PLUGIN_ASSERT_HIGH" "Assert\nHIGH" "Raises an assertion if the input is not HIGH"
    
    let assertLow =
        makeOneInputOneBitComponent
            "Assert LOW" "PLUGIN_ASSERT_LOW" "Assert\nLOW" "Raises an assertion if the input is not LOW"
     
    let operatorPairs = [
        makeSignedPairOfOperators "Multiplies" "Multiply" "PLUGIN_MULTIPLY" "A*B" 
        makeSignedPairOfOperators "Divides" "Divide" "PLUGIN_DIVIDE" "A/B"
        makeSignedPairOfOperators "Applies A mod B using" "Modulo" "PLUGIN_MODULO" "A % B"
        makeSignedPairOfOperators "Applies pow(A, B) using" "Power" "PLUGIN_POWER" "pow(A, B)" 
        makeSignedPairOfComparators "less than" "Less than" "PLUGIN_COMPARISON_LESS_THAN" "A < B"
        makeSignedPairOfComparators "less than or equal to" "Less than or equal" "PLUGIN_COMPARISON_LESS_THAN_OR_EQUAL_TO" "A <= B" 
        makeSignedPairOfComparators "greater than" "Greater than" "PLUGIN_COMPARISON_GREATER_THAN" "A > B"
        makeSignedPairOfComparators "greater than or equal to" "Greater than or equal" "PLUGIN_COMPARISON_GREATER_THAN_OR_EQUAL_TO" "A >= B"
    ]
    
    let operators =
        operatorPairs
        |> List.collect (fun (unsigned, signed) -> [unsigned; signed])
        
    [
        assertHigh
        assertLow
        signExtend
        zeroExtend
        makeSimpleComponent
            (IODefaults.OneFixedWidthOutputX 1) IODefaults.TwoInputs "Equals" "PLUGIN_EQUALS" "A == B"
            "Outputs HIGH when the two inputs are equal" 
        makeTwoInputOneOutputComponent "Add" "PLUGIN_ADD" "A+B" "Adds the two inputs"
        makeTwoInputOneOutputComponent "Subtract" "PLUGIN_SUBTRACT" "A-B" "Subtracts input B from input A"
    ] @ operators

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
