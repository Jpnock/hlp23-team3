module Verification.Components

open System
open AssertionTypes
open ASTMap

type ComponentInput =
    { Name: string
      FixedWidth: int option
      Signed: bool option }

type ComponentOutput =
    { Name: string
      FixedWidth: int option
      Signed: bool option }


type LibraryID = string

type ComponentState =
    { InstanceID: string option
      LibraryID: LibraryID
      Inputs: Map<InputPortNumber, ComponentInput>
      Outputs: Map<OutputPortNumber, ComponentOutput>
      AssertionText: string option
      IsInput: bool option }
    static member Default : ComponentState = {
        InstanceID = Some (Guid.NewGuid().ToString())
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
        AssertionText = None
        IsInput = None
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
    abstract member Build : Map<InputPortNumber, Expr> -> Expr
    
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

let collectionMaxWithDefault<'t when 't: comparison> defaultValue (sequence:Collections.Generic.ICollection<'t>) =
    match sequence.Count with
    | 0 -> defaultValue
    | _ -> Seq.max sequence

type SimpleComponent =
    { 
      Name: string
      SymbolName: string
      DescriptionFunc: SimpleComponent -> ComponentState -> string
      TooltipText: string
      DefaultState: ComponentState
      AssertionBuilder: ASTBuilder }
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
            // TODO(jpnock): Check logic
            let maxInputWidth = collectionMaxWithDefault 0 inputPortWidths.Values
            state.Outputs
            |> Map.map (fun outputPortNum output ->
                match output.FixedWidth with
                | Some w -> w
                | _ -> maxInputWidth)
        member this.Build exprPortMap =
            // TODO(jpnock): Add logic here
            let built = this.AssertionBuilder exprPortMap
            match built with
            | Some b -> b
            | _ -> failwithf $"Unable to build for {this.Name}"

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

type InputType =
    | InputTypeVerification
    | InputTypeExternal

let mustGetOperands (state:ComponentState) =
    let i1, i2 = state.Inputs.TryFind 0, state.Inputs.TryFind 1
    match i1, i2 with
    | Some input1, Some input2 ->
        input1, input2
    | _ -> failwithf "Expected this component to have two operands"

let makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description builder : SimpleComponent =
    {
        Name = name
        SymbolName = symbolName
        DescriptionFunc = (fun _ _ -> description)
        TooltipText = description
        DefaultState = makeState outputs inputs baseLibraryID
        AssertionBuilder = builder
    }

let makeSimpleComponent outputs inputs name baseLibraryID symbolName description builder : IComponent =
    makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description builder

let makeOneOutputComponent = makeSimpleComponent IODefaults.OneOutput
let makeOneInputOneBitComponent = makeSimpleComponent Map.empty (IODefaults.OneFixedWidthInputA 1)
let makeOneInputOneOutputComponent = makeOneOutputComponent IODefaults.OneInput
let makeTwoInputOneOutputComponent = makeOneOutputComponent IODefaults.TwoInputs

let makeSignedPairOfComponents outputs description name baseLibraryID symbolName builder : IComponent * IComponent =
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
        AssertionBuilder = builder
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
        makeSignedPairOfOperators "Multiplies" "Multiply" "PLUGIN_MULTIPLY" "A*B" (astMapper TMul)
        makeSignedPairOfOperators "Divides" "Divide" "PLUGIN_DIVIDE" "A/B" (astMapper TDiv)
        makeSignedPairOfOperators "Applies A mod B using" "Modulo" "PLUGIN_MODULO" "A % B" (astMapper TRem)
        makeSignedPairOfOperators "Applies pow(A, B) using" "Power" "PLUGIN_POWER" "pow(A, B)" (astMapper TSigned)
        makeSignedPairOfComparators "less than" "Less than" "PLUGIN_COMPARISON_LESS_THAN" "A < B" (astMapper TLt)
        makeSignedPairOfComparators "less than or equal to" "Less than or equal" "PLUGIN_COMPARISON_LESS_THAN_OR_EQUAL_TO" "A <= B" (astMapper TLte)
        makeSignedPairOfComparators "greater than" "Greater than" "PLUGIN_COMPARISON_GREATER_THAN" "A > B" (astMapper TGt)
        makeSignedPairOfComparators "greater than or equal to" "Greater than or equal" "PLUGIN_COMPARISON_GREATER_THAN_OR_EQUAL_TO" "A >= B" (astMapper TGte)
    ]
    
    let operators =
        operatorPairs
        |> List.collect (fun (unsigned, signed) -> [unsigned; signed])
    
    let textAssertionBase =
        makeSimpleComponentConcrete
            Map.empty Map.empty "Text Assertion" "PLUGIN_TEXT_ASSERTION" "Assertion" "Verifies the logic of your sheet using text based expressions" noAssertion
    
    let textAssertion =
        {
            textAssertionBase with DefaultState = {textAssertionBase.DefaultState with AssertionText = Some ""}
        }
    [
        textAssertion
        assertHigh noAssertion
        assertLow noAssertion
        signExtend noAssertion
        zeroExtend noAssertion
        makeSimpleComponent
            (IODefaults.OneFixedWidthOutputX 1) IODefaults.TwoInputs "Equals" "PLUGIN_EQUALS" "A == B"
            "Outputs HIGH when the two inputs are equal" (astMapper TEq)
        makeTwoInputOneOutputComponent "Add" "PLUGIN_ADD" "A+B" "Adds the two inputs" (astMapper TAdd)
        makeTwoInputOneOutputComponent "Subtract" "PLUGIN_SUBTRACT" "A-B" "Subtracts input B from input A" (astMapper TSub)
    ] @ operators

type ComponentLibrary =
    { Components: Map<LibraryID, IComponent> }
    member this.register (comp:IComponent) = this.Components.Add (comp.GetLibraryID, comp)

let library: ComponentLibrary = {
    Components =
        components
        |> List.map (fun comp -> (comp.GetLibraryID, comp))
        |> Map.ofList
}


/// ConnectionID to Component
type SheetConnections = Map<string, ComponentState> 

let getStateForInput (portToSource: Map<int, ComponentState>) inputNum =
    // TODO(jpnock): safety first!
    printf $"Getting state for: {inputNum}"
    portToSource[inputNum]
    
let getComp (state: ComponentState) : IComponent =
    library.Components[state.LibraryID]

let rec generateAST (componentPortSources: Map<string, Map<int, ComponentState>>) (state: ComponentState) : Expr =
    match state.IsInput with
    | Some true -> Lit (Id state.Outputs[0].Name)
    | _ ->
        printf $"Getting state for {state}"
        let componentPortMap = componentPortSources[state.InstanceID.Value]
        printf $"Looking up state {state}"
        state.Inputs
        |> Map.map (fun k _ -> getStateForInput componentPortMap k)
        |> Map.map (fun _ -> generateAST componentPortSources)
        |> (getComp state).Build

let implementsVariableWidth (state : ComponentState) =
    match state.Inputs.TryFind 0 with
    | Some input when input.FixedWidth.IsSome -> input.FixedWidth
    | _ -> None

let makeStateFromExternalInputComponent id inputName width : ComponentState =
    {
        ComponentState.Default with
            InstanceID = Some id
            Outputs = Map [
                (0, {IODefaults.OutputX with Name = inputName; FixedWidth = width})
            ]
            IsInput = Some true
    }
