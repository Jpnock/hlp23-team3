/// Author: James Nock
module VerificationComponents

open System
open AssertionTypes
open AssertionASTMap

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
    member this.ToInterface : IComponent = this
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

let basicDescription description _ _ =
    description

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

let makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description tooltip builder : SimpleComponent =
    {
        Name = name
        SymbolName = symbolName
        DescriptionFunc = description
        TooltipText = tooltip
        DefaultState = makeState outputs inputs baseLibraryID
        AssertionBuilder = builder
    }

let makeSimpleComponent outputs inputs name baseLibraryID symbolName description tooltip builder : IComponent =
    makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description tooltip builder

let makeOneOutputComponent = makeSimpleComponent IODefaults.OneOutput
let makeOneInputOneBitComponent = makeSimpleComponent Map.empty (IODefaults.OneFixedWidthInputA 1)
let makeOneInputOneOutputComponent = makeOneOutputComponent IODefaults.OneInput
let makeTwoInputOneOutputComponent = makeOneOutputComponent IODefaults.TwoInputs

let makeSignedPairOfComponents outputs description tooltip name baseLibraryID symbolName builder : SimpleComponent * SimpleComponent =
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
        TooltipText = tooltip
        DefaultState = unsignedState
        AssertionBuilder = builder
    }
    
    let signedComp = { unsignedComp with Name = signedName; DefaultState = signedState }
    unsignedComp, signedComp

let getStateForInput (portToSource: Map<int, ComponentState>) inputNum =
    // TODO(jpnock): safety first!
    printf $"Getting state for: {inputNum}"
    portToSource[inputNum]
    
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
