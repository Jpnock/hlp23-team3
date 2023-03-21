/// Represents types and logic for verification component creation,
/// as well as internal component logic.
///
/// Authored by jpn119 (James Nock)
module VerificationComponents

open System
open AssertionTypes
open AssertionASTMap

type ComparatorType =
    | Eq
    | Lt
    | Lte
    | Gt
    | Gte
    
let comparatorTypeName typ =
    match typ with
    | Lt -> "Less than"
    | Lte -> "Less than or equals"
    | Gt -> "Greater than"
    | Gte -> "Greater than or equals"
    | Eq -> "Equal"

let comparatorTypeSymbolName typ =
    match typ with
    | Lt -> "A < B"
    | Lte -> "A <= B"
    | Gt -> "A > B"
    | Gte -> "A >= B"
    | Eq -> "A == B"

type DataType =
    | DataTypeInt
    | DataTypeUInt
    | DataTypeFloat32
    | DataTypeAssertionInput

let dataTypeName =
    function
    | DataTypeInt -> "Signed Integer"
    | DataTypeUInt -> "Unsigned Integer (default)"
    | DataTypeFloat32 -> "32-bit Float (IEEE-754)"
    | DataTypeAssertionInput -> "Assertion Input"

type LogicalOpType =
    | LogicalOpTypeAnd
    | LogicalOpTypeOr
    | LogicalOpTypeNot

let logicalOpTypeName =
    function
    | LogicalOpTypeAnd -> "L. AND"
    | LogicalOpTypeOr -> "L. OR"
    | LogicalOpTypeNot -> "L. NOT"

type BitwiseOpType =
    | BitwiseOpTypeAnd
    | BitwiseOpTypeOr
    | BitwiseOpTypeNot

let bitwiseOpTypeName =
    function
    | BitwiseOpTypeAnd -> "B. AND"
    | BitwiseOpTypeOr -> "B. OR"
    | BitwiseOpTypeNot -> "B. NOT"

type MultiComponentType =
    | ComparatorType of ComparatorType
    | BitwiseOpType of BitwiseOpType
    | LogicalOpType of LogicalOpType

/// Represents stored data about a component input.
type ComponentInput =
    {
      // Port label / name
      Name: string
      // The output width; set to Some (value) for static width-inference.
      // If None, then automatic width-inference is applied.
      FixedWidth: int option
      // The type of the data represented by this input
      DataType: DataType
    }

/// Represents stored data about a component output.
type ComponentOutput =
    {
      // Port label / name
      Name: string
      // The output width; set to Some (value) for static width-inference.
      // If None, then automatic width-inference is applied.
      FixedWidth: int option
      // Used in evaluation 
      HostLabel: string
    }

/// Represents the ID of a component in the Component Library.
type LibraryID = string

/// Represents all stored data for a component. All newly added
/// fields should be optional where possible, such that they can
/// be initialised to None in the default constructor; this
/// allows for new fields to be added without changing the code
/// in many places.
type ComponentConfig =
    { InstanceID: string option
      LibraryID: LibraryID
      Inputs: Map<InputPortNumber, ComponentInput>
      Outputs: Map<OutputPortNumber, ComponentOutput>
      AssertionText: string option
      AssertionDescription: string option
      IsInput: bool option
      ComparatorType: ComparatorType option
      MultiComponentType: MultiComponentType option }
    static member Default : ComponentConfig = {
        InstanceID = Some (Guid.NewGuid().ToString())
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
        AssertionText = None
        AssertionDescription = None
        IsInput = None
        ComparatorType = None
        MultiComponentType = None
    }

/// Represents stored data about a Symbol for a component.
type SymbolDetails =
    {
      // The text displayed on a symbol.
      Name: string
      // The prefix used above the symbol, when not given a name manually.
      Prefix: string
      // The height of the symbol.
      Height: float
      // The width of the symbol.
      Width: float
      // The colour of the symbol.
      Colour: string
      // The port labels for each input.
      InputLabels: string list
      // The port labels for each output.
      OutputLabels: string list
    }
    
/// Abstracts the behaviour of any component behind an interface. This allows
/// for maximum flexibility -- including the ability to define components outside
/// of this module -- such that components with complex behaviour do not pollute
/// a single file. This was chosen to avoid the current Issie model of matching
/// on the ComponentType DU, which forces you to currently change more than 10
/// files in multiple places to add a single component.
type IComponent =
    /// Returns the ID of the library entry describing this component.
    abstract member GetLibraryID: LibraryID
    /// Returns the catalogue name of the component
    abstract member GetName: string
    /// Returns tooltip text displayed when creating a component.
    abstract member GetTooltipText: string
    /// Returns the initial config of a component, which determines its
    /// behaviour and the type of component. User modifications are
    /// not reflected here.
    abstract member GetDefaultConfig : ComponentConfig
    /// Retrieves the details of the component symbol, given the config,
    /// such that this can dynamically be generated with different
    /// behaviour per each component-type instance.
    abstract member GetSymbolDetails : ComponentConfig -> SymbolDetails
    /// Retrieves the description of the component based on the config.
    /// Providing the config is useful if the description needs to be
    /// dynamically updated based on the component config, such as
    /// changing "N-bit adder" to "10-bit adder" when the user configures this.
    abstract member GetDescription : ComponentConfig -> string
    /// Returns the output widths of each output port. Used by the width-inference
    /// engine to automatically deduce bus-widths.
    abstract member GetOutputWidths : ComponentConfig -> Map<InputPortNumber, int> -> Map<OutputPortNumber, int>
    /// Creates the Assertion AST for this component, when the ASTs
    /// corresponding to each input port are provided.
    abstract member CreateAST : ComponentConfig -> Map<InputPortNumber, Expr> -> Expr
  
/// Defines helper defaults that are useful when constructing Component IO ports.
module IODefaults =
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
    let OutputX: ComponentOutput = { Name = "X"; FixedWidth = None; HostLabel = ""}
    
    /// A single output named X at port 0, with no initialised fixed width.
    let OneOutput: Map<OutputPortNumber, ComponentOutput> = Map [
        (0, OutputX)
    ]
    
    /// A single output named X at port 0, with fixed width of `width`.
    let OneFixedWidthOutputX width =
        Map [ (0, {OutputX with FixedWidth = Some width}) ]

let convertAllToDataTypeAssertion inputs =
    inputs
    |> Map.map (fun _ input -> {input with DataType = DataTypeAssertionInput})

/// Defines helper defaults that are useful when constructing Component symbols.
module SymbolDefaults =
    let GridSize = 30.0
    let Width = 3.5 * GridSize
    let Height = 2.0 * GridSize
    let Prefix = "VERI"
    let Colour = "rgb(175,220,120)"

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

let emptyExprInfo expr =
    expr, { Line = 0; Col = 0; Length = 0 }

let addSignInfoToAST (cfg : ComponentConfig) (exprPortMap : PortExprs) =
    let addSignInfoToPort inputPortNum expr =
        match cfg.Inputs.TryFind inputPortNum with
        | Some inputPort ->
            match inputPort.DataType with
            | DataTypeInt -> Cast (ToSigned (emptyExprInfo expr))
            | DataTypeUInt -> expr
            | DataTypeFloat32 -> Cast (ToFloat (emptyExprInfo expr))
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
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription cfg = this.DescriptionFunc this cfg
        member this.GetOutputWidths cfg inputPortWidths =
            largestOutputWidthForAllPorts cfg inputPortWidths
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            let built = this.AssertionBuilder signedExprPortMap
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
                ComparatorType = Some Eq
                Inputs = IODefaults.TwoInputs
                Outputs = IODefaults.OneFixedWidthOutputX 1
                LibraryID = this.LibraryID }
        member this.GetSymbolDetails cfg =
            let comparatorTyp =
                match cfg.ComparatorType with
                | Some typ -> typ
                | _ -> failwithf "Tried to get the comparator type of a non-comparator"
            { Name = comparatorTypeSymbolName comparatorTyp
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
              InputLabels = inputPortLabels cfg
              OutputLabels = [] }
        member this.GetDescription _ =
            "Performs comparisons between two busses, such as checking a bus contains a greater value than another."
        member this.GetOutputWidths cfg _ = Map.map (fun _ _ -> 1) cfg.Outputs
        member this.CreateAST cfg exprPortMap =
            let signedExprPortMap = addSignInfoToAST cfg exprPortMap
            let built =
                match cfg.ComparatorType with
                | None -> failwith "Tried to build assertion AST for comparator without config set"
                | Some typ ->
                    match typ with
                    | Eq -> astMapper TEq signedExprPortMap
                    | Lt -> astMapper TLt signedExprPortMap
                    | Gt -> astMapper TGt signedExprPortMap
                    | Lte -> astMapper TLte signedExprPortMap
                    | Gte -> astMapper TGte signedExprPortMap
            match built with
            | Some b -> b
            | _ -> failwithf $"Unable to CreateAST for comparator {cfg.ComparatorType}"

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
                Inputs = IODefaults.TwoInputs |> convertAllToDataTypeAssertion
                Outputs = IODefaults.OneFixedWidthOutputX 1
                LibraryID = this.LibraryID
                MultiComponentType = Some (LogicalOpType LogicalOpTypeAnd)}
        member this.GetSymbolDetails cfg =
            let typ =
                match cfg.MultiComponentType with
                | Some (LogicalOpType logicalOpType) -> logicalOpType
                | _ -> failwithf "Tried to get the logical-operator type of a non-logical-operator"
            { Name = logicalOpTypeName typ
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
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
                | LogicalOpTypeAnd -> astMapper TLogAnd signedExprPortMap
                | LogicalOpTypeOr -> astMapper TLogOr signedExprPortMap
                | LogicalOpTypeNot -> astMapper TLogNot signedExprPortMap
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
                Inputs = IODefaults.TwoInputs |> convertAllToDataTypeAssertion
                Outputs = IODefaults.OneOutput
                LibraryID = this.LibraryID
                MultiComponentType = Some (BitwiseOpType BitwiseOpTypeAnd)}
        member this.GetSymbolDetails cfg =
            let typ =
                match cfg.MultiComponentType with
                | Some (BitwiseOpType bitwiseTyp) -> bitwiseTyp
                | _ -> failwithf "Tried to get the bitwise-operator type of a non-bitwise-operator"
            { Name = bitwiseOpTypeName typ
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
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
                | BitwiseOpTypeAnd -> astMapper TBitAnd signedExprPortMap
                | BitwiseOpTypeOr -> astMapper TBitOr signedExprPortMap
                | BitwiseOpTypeNot -> astMapper TBitNot signedExprPortMap
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
    let makeOneOutputComponent = makeSimpleComponent IODefaults.OneOutput
    let makeOneInputOneBitComponent = makeSimpleComponent Map.empty (IODefaults.OneFixedWidthInputA 1)
    let makeOneInputOneOutputComponent = makeOneOutputComponent IODefaults.OneInput
    let makeTwoInputOneOutputComponent = makeOneOutputComponent IODefaults.TwoInputs

/// Returns whether the component specifies a user-controllable
/// input port 0 width.
let implementsVariableWidth (cfg : ComponentConfig) =
    let inputIsAssertion input = input.DataType <> DataTypeAssertionInput
    
    match cfg.Inputs.TryFind 0 with
    | Some input when
        input.FixedWidth.IsSome && not (inputIsAssertion input) -> input.FixedWidth
    | _ -> None

let makeOutputs portIds widths hostLabel : Map<OutputPortNumber, ComponentOutput> =
    portIds
    |> List.zip widths 
    |> List.mapi (fun i (width, name) -> (i, {IODefaults.OutputX with Name = name; FixedWidth = width; HostLabel = hostLabel}))
    |> Map.ofList 

/// Allows for components not registered in the library to be
/// represented as Plugin components (useful for simulation)
/// TODO(jpnock): refactor this such that these components are
/// supported by natively.
// TODO ln220 make it so that it actually holds the component outputs 
let makeConfigFromExternalInputComponent id widths portsIds hostLabel: ComponentConfig =
    {
        ComponentConfig.Default with
            InstanceID = Some id
            Outputs = makeOutputs portsIds widths hostLabel
            IsInput = Some true
    }
