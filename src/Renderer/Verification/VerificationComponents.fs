/// Represents types and logic for verification component creation,
/// as well as internal component logic.
///
/// Authored by jpn119 (James Nock)
module VerificationComponents

open System
open AssertionTypes
open AssertionASTMap
open VerificationTypes
  

let convertAllToDataTypeAssertion inputs =
    inputs
    |> Map.map (fun _ input -> {input with DataType = DataTypeAssertionInput})


/// Returns the maximum value of a collection, or the default value if
/// the list is empty.
let private collectionMaxWithDefault<'t when 't: comparison> defaultValue (sequence:Collections.Generic.ICollection<'t>) =
    match sequence.Count with
    | 0 -> defaultValue
    | _ -> Seq.max sequence

/// Updates the config of a component, such that
/// all inputs are set to `signed`.
let makeIOSigned cfg  =
    {
     cfg with
        Inputs = cfg.Inputs |> Map.map (fun _ v -> {v with DataType = DataTypeInt})
    }

let inputPortLabels (cfg: ComponentConfig) =
    match cfg.Inputs.IsEmpty with
    | true -> []
    | false ->
        let highestPort =
            cfg.Inputs.Keys
            |> Seq.max
        [0..highestPort]
        |> List.choose (fun idx ->
            match cfg.Inputs.TryFind idx with
            | Some port -> Some port.Name
            | None -> None)

let emptyExprInfo expr compId=
    expr, { Line = 0; Col = 0; Length = 0; CompId = compId}

let addSignInfoToAST (cfg : ComponentConfig) (exprPortMap : PortExprs) =
    let addSignInfoToPort inputPortNum expr =
        match cfg.Inputs.TryFind inputPortNum with
        | Some inputPort ->
            match inputPort.DataType with
            | DataTypeInt -> Cast (ToSigned (emptyExprInfo expr cfg.InstanceID.Value))
            | DataTypeUInt -> expr
            | DataTypeFloat32 -> Cast (ToFloat (emptyExprInfo expr cfg.InstanceID.Value))
            | DataTypeAssertionInput -> expr
        | _ -> failwith $"what? could not find port #{inputPortNum} for {cfg.LibraryID} : {cfg.InstanceID}"
    
    exprPortMap
    |> Map.map addSignInfoToPort
    
let largestOutputWidthForAllPorts cfg (inputPortWidths: Map<InputPortNumber, int>) =
    let maxInputWidth = collectionMaxWithDefault 0 inputPortWidths.Values
    cfg.Outputs
    |> Map.map (fun _ output ->
        match output.FixedWidth with
        | Some w -> w
        | _ -> maxInputWidth)
    
/// Represents a generic component that implements all features required for
/// Assertion components. Implements IComponent.
type SimpleComponent =
    { 
      Name: string
      SymbolName: string
      DescriptionFunc: SimpleComponent -> ComponentConfig -> string
      TooltipText: string
      DefaultConfig: ComponentConfig
      AssertionBuilder: ASTBuilder }
    member this.ToInterface : IComponent = this
    interface IComponent with
        member this.GetLibraryID = this.DefaultConfig.LibraryID
        member this.GetName = this.Name
        member this.GetTooltipText = this.TooltipText
        member this.GetDefaultConfig = this.DefaultConfig
        member this.GetSymbolDetails cfg =
            { Name = this.SymbolName
              Prefix = SymbolConstants.Prefix
              Height = SymbolConstants.Height
              Width = SymbolConstants.Width
              Colour = SymbolConstants.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription cfg = this.DescriptionFunc this cfg
        member this.GetOutputWidths cfg inputPortWidths =
            largestOutputWidthForAllPorts cfg inputPortWidths
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            let built = this.AssertionBuilder cfg.InstanceID.Value signedExprPortMap
            printfn "verification port name %A" this.DefaultConfig.Outputs
            match built with
            | Some b -> b
            | _ -> failwithf $"Unable to build for {this.Name}"

type ComparatorComponent =
    { LibraryID: string }
    member this.ToInterface : IComponent = this
    interface IComponent with
        member this.GetLibraryID = this.LibraryID
        member this.GetName = "Comparator"
        member this.GetTooltipText =
            "Performs comparisons between two busses, such as checking a bus contains a greater value than another."
        member this.GetDefaultConfig = {
            ComponentConfig.Default with
                MultiComponentType = Some (ComparatorType ComparatorTypeEq)
                Inputs = IOConstants.TwoInputs
                Outputs = IOConstants.OneFixedWidthOutputX 1
                LibraryID = this.LibraryID }
        member this.GetSymbolDetails cfg =
            let comparatorTyp =
                match cfg.MultiComponentType with
                | Some (ComparatorType typ) -> typ
                | _ -> failwithf "Tried to get the comparator type of a non-comparator"
            { Name = comparatorTypeSymbolName comparatorTyp
              Prefix = SymbolConstants.Prefix
              Height = SymbolConstants.Height
              Width = SymbolConstants.Width
              Colour = SymbolConstants.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription _ =
            "Performs comparisons between two busses, such as checking a bus contains a greater value than another."
        member this.GetOutputWidths cfg _ = Map.map (fun _ _ -> 1) cfg.Outputs
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            let built =
                match cfg.MultiComponentType with
                | None -> failwith "Tried to build assertion AST for comparator without config set"
                | Some (ComparatorType typ) ->
                    match typ with
                    | ComparatorTypeEq -> astMapper TEq cfg.InstanceID.Value signedExprPortMap 
                    | ComparatorTypeLt -> astMapper TLt cfg.InstanceID.Value signedExprPortMap 
                    | ComparatorTypeGt -> astMapper TGt cfg.InstanceID.Value signedExprPortMap 
                    | ComparatorTypeLte -> astMapper TLte cfg.InstanceID.Value signedExprPortMap
                    | ComparatorTypeGte -> astMapper TGte cfg.InstanceID.Value signedExprPortMap
                | _ -> None
            match built with
            | Some b -> b
            | _ -> failwithf $"Unable to CreateAST for comparator {cfg.MultiComponentType}"

type LogicalOpComponent =
    { LibraryID: string }
    member this.ToInterface : IComponent = this
    interface IComponent with
        member this.GetLibraryID = this.LibraryID
        member this.GetName = "Logical Operator"
        member this.GetTooltipText =
            "Applies logical operations to the inputs, such as the AND (&&); OR (||) and NOT (!) operators"
        member this.GetDefaultConfig = {
            ComponentConfig.Default with
                Inputs = IOConstants.TwoInputs |> convertAllToDataTypeAssertion
                Outputs = IOConstants.OneFixedWidthOutputX 1
                LibraryID = this.LibraryID
                MultiComponentType = Some (LogicalOpType LogicalOpTypeAnd)}
        member this.GetSymbolDetails cfg =
            let typ =
                match cfg.MultiComponentType with
                | Some (LogicalOpType logicalOpType) -> logicalOpType
                | _ -> failwithf "Tried to get the logical-operator type of a non-logical-operator"
            { Name = logicalOpTypeName typ
              Prefix = SymbolConstants.Prefix
              Height = SymbolConstants.Height
              Width = SymbolConstants.Width
              Colour = SymbolConstants.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription _ =
            "Applies logical operations to the inputs, such as the AND (&&); OR (||) and NOT (!) operators"
        member this.GetOutputWidths cfg _ = Map.map (fun _ _ -> 1) cfg.Outputs
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            match cfg.MultiComponentType with
            | Some (LogicalOpType logicalOpType) ->
                match logicalOpType with
                | LogicalOpTypeAnd -> astMapper TLogAnd cfg.InstanceID.Value signedExprPortMap
                | LogicalOpTypeOr -> astMapper TLogOr cfg.InstanceID.Value signedExprPortMap
                | LogicalOpTypeNot -> astMapper TLogNot cfg.InstanceID.Value signedExprPortMap
            | None -> failwith "Tried to build assertion AST for logical operator without config set"
            | _ -> failwith "what? Tried to build assertion AST for logical operator but wrong multi-component type was set"
            |> Option.defaultValue (failwithf $"Unable to CreateAST for logical operator {cfg.MultiComponentType}")

type BitwiseOpComponent =
    { LibraryID: string }
    member this.ToInterface : IComponent = this
    interface IComponent with
        member this.GetLibraryID = this.LibraryID
        member this.GetName = "Bitwise Operator"
        member this.GetTooltipText =
            "Applies bitwise operations to the inputs, such as the AND (&); OR (|) and NOT (~) operators. \
            Zero extends the smaller of the two inputs."
        member this.GetDefaultConfig = {
            ComponentConfig.Default with
                Inputs = IOConstants.TwoInputs |> convertAllToDataTypeAssertion
                Outputs = IOConstants.OneOutput
                LibraryID = this.LibraryID
                MultiComponentType = Some (BitwiseOpType BitwiseOpTypeAnd)}
        member this.GetSymbolDetails cfg =
            let typ =
                match cfg.MultiComponentType with
                | Some (BitwiseOpType bitwiseTyp) -> bitwiseTyp
                | _ -> failwithf "Tried to get the bitwise-operator type of a non-bitwise-operator"
            { Name = bitwiseOpTypeName typ
              Prefix = SymbolConstants.Prefix
              Height = SymbolConstants.Height
              Width = SymbolConstants.Width
              Colour = SymbolConstants.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription _ =
            "Applies bitwise operations to the inputs, such as the AND (&); OR (|) and NOT (~) operators. \
            Zero extends the smaller of the two inputs."
        member this.GetOutputWidths cfg inputPortWidths = largestOutputWidthForAllPorts cfg inputPortWidths
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            match cfg.MultiComponentType with
            | Some (BitwiseOpType bitwiseOpType) ->
                match bitwiseOpType with
                | BitwiseOpTypeAnd -> astMapper TBitAnd cfg.InstanceID.Value signedExprPortMap
                | BitwiseOpTypeOr -> astMapper TBitOr cfg.InstanceID.Value signedExprPortMap
                | BitwiseOpTypeNot -> astMapper TBitNot cfg.InstanceID.Value signedExprPortMap
            | None -> failwith "Tried to build assertion AST for bitwise without config set"
            | _ -> failwith "what? Tried to build assertion AST for bitwise operator but wrong multi-component type was set"
            |> Option.defaultWith (fun () -> failwithf $"Unable to CreateAST for bitwise operator {cfg.MultiComponentType}")

// Helper function for dynamically generating the description of a
// signed component, based on its config.
let signedDescription _ cfg =
    let dt =
        cfg.Inputs.Values
        |> Seq.exists (fun el -> el.DataType = DataTypeInt)
    match dt with
    | true -> "treating them as signed values"
    | false -> "treating them as unsigned values"

/// Helper function for creating a static description (regardless of config).
let basicDescription description _ _ =
    description

/// Helper function for creating component config, with
/// defaults applied.
let makeConfig outputs inputs libraryID =
    { ComponentConfig.Default with Inputs = inputs; Outputs = outputs; LibraryID = libraryID }   

/// Returns inputs 0 and 1 of a component, or fails if they
/// are not both present.
let mustGetOperands (cfg: ComponentConfig) =
    let i1, i2 = cfg.Inputs.TryFind 0, cfg.Inputs.TryFind 1
    match i1, i2 with
    | Some input1, Some input2 ->
        input1, input2
    | _ -> failwithf "Expected this component to have two operands"

/// Helper function for constructing a new SimpleComponent. Returns
/// the concrete component, rather than IComponent. Use `makeSimpleComponent`
/// if an IComponent is required.
let makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description tooltip builder : SimpleComponent =
    {
        Name = name
        SymbolName = symbolName
        DescriptionFunc = description
        TooltipText = tooltip
        DefaultConfig = makeConfig outputs inputs baseLibraryID
        AssertionBuilder = builder
    }

/// Helper function for constructing a new SimpleComponent (returned as an IComponent).
/// Use `makeSimpleComponentConcrete` if a SimpleComponent is required. 
let makeSimpleComponent outputs inputs name baseLibraryID symbolName description tooltip builder : IComponent =
    makeSimpleComponentConcrete outputs inputs name baseLibraryID symbolName description tooltip builder

/// Helper functions, which are partial applications of makeSimpleComponent, to
/// allow for easier creation of common component configurations.
module ComponentDefaults =
    let makeOneOutputComponent = makeSimpleComponent IOConstants.OneOutput
    let makeOneInputOneBitComponent = makeSimpleComponent Map.empty (IOConstants.OneFixedWidthInputA 1)
    let makeOneInputOneOutputComponent = makeOneOutputComponent IOConstants.OneInput
    let makeTwoInputOneOutputComponent = makeOneOutputComponent IOConstants.TwoInputs

/// Returns whether the component specifies a user-controllable
/// input port 0 width.
let implementsVariableWidth (cfg : ComponentConfig) =
    let inputIsAssertion input = input.DataType <> DataTypeAssertionInput
    
    match cfg.Inputs.TryFind 0 with
    | Some input when
        input.FixedWidth.IsSome && not (inputIsAssertion input) -> input.FixedWidth
    | _ -> None

let makeOutputs portIds widths hostLabel hostSheet : Map<OutputPortNumber, ComponentOutput> =
    portIds
    |> List.zip widths 
    |> List.mapi (fun i (width, name) -> (
        i, {IOConstants.OutputX with Name = name; FixedWidth = width; HostLabel = hostLabel; HostSheet = hostSheet}))
    |> Map.ofList 

/// Allows for components not registered in the library to be
/// represented as Plugin components (useful for simulation)
/// TODO(jpnock): refactor this such that these components are
/// supported by natively.
// TODO ln220 make it so that it actually holds the component outputs 
let makeConfigFromExternalInputComponent id widths portsIds hostLabel hostSheet: ComponentConfig =
    {
        ComponentConfig.Default with
            InstanceID = Some id
            Outputs = makeOutputs portsIds widths hostLabel hostSheet
            IsInput = Some true
    }
