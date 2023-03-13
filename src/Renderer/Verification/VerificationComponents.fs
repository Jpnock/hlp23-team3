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

/// Represents stored data about a component input.
type ComponentInput =
    {
      // Port label / name
      Name: string
      // The output width; set to Some (value) for static width-inference.
      // If None, then automatic width-inference is applied.
      FixedWidth: int option
      // Optional field for whether the output is signed or unsigned.
      Signed: bool option
    }

/// Represents stored data about a component output.
type ComponentOutput =
    {
      // Port label / name
      Name: string
      // The output width; set to Some (value) for static width-inference.
      // If None, then automatic width-inference is applied.
      FixedWidth: int option
      // Optional field for whether the output is signed or unsigned.
      Signed: bool option
    }

/// Represents the ID of a component in the Component Library.
type LibraryID = string

/// Represents all stored data for a component. All newly added
/// fields should be optional where possible, such that they can
/// be initialised to None in the default constructor; this
/// allows for new fields to be added without changing the code
/// in many places.
type ComponentState =
    { InstanceID: string option
      LibraryID: LibraryID
      Inputs: Map<InputPortNumber, ComponentInput>
      Outputs: Map<OutputPortNumber, ComponentOutput>
      AssertionText: string option
      AssertionDescription: string option
      IsInput: bool option
      ComparatorType: ComparatorType option }
    static member Default : ComponentState = {
        InstanceID = Some (Guid.NewGuid().ToString())
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
        AssertionText = None
        AssertionDescription = None
        IsInput = None
        ComparatorType = None
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
    /// Returns the initial state of a component, which determines its
    /// behaviour and the type of component. User modifications are
    /// not reflected here.
    abstract member GetDefaultState : ComponentState
    /// Retrieves the details of the component symbol, given the state
    /// such that this can dynamically be generated with different
    /// behaviour per each component-type instance.
    abstract member GetSymbolDetails : ComponentState -> SymbolDetails
    /// Retrieves the description of the component based on the state.
    /// Providing the state is useful if the description needs to be
    /// dynamically updated based on the component state, such as
    /// changing "N-bit adder" to "10-bit adder" when the user configures this.
    abstract member GetDescription : ComponentState -> string
    /// Returns the output widths of each output port. Used by the width-inference
    /// engine to automatically deduce bus-widths.
    abstract member GetOutputWidths : ComponentState -> Map<InputPortNumber, int> -> Map<OutputPortNumber, int>
    /// Builds the Assertion AST for this component, when the ASTs
    /// corresponding to each input port are provided.
    abstract member Build : ComponentState -> Map<InputPortNumber, Expr> -> Expr
  
/// Defines helper defaults that are useful when constructing Component IO ports.
module IODefaults =
    /// A single input named A, with no initialised fixed with.
    let InputA: ComponentInput = { Name = "A"; FixedWidth = None; Signed = None; }
    
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

    /// A single output named X, with no initialised fixed width.
    let OutputX: ComponentOutput = { Name = "X"; FixedWidth = None; Signed = None; }
    
    /// A single output named X at port 0, with no initialised fixed width.
    let OneOutput: Map<OutputPortNumber, ComponentOutput> = Map [
        (0, OutputX)
    ]
    
    /// A single output named X at port 0, with fixed width of `width`.
    let OneFixedWidthOutputX width =
        Map [ (0, {OutputX with FixedWidth = Some width}) ]

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

/// Updates the state of a component, such that
/// all inputs are set to `signed`.
let makeIOSigned signed state  =
    {
     state with
        Inputs = state.Inputs |> Map.map (fun _ v -> {v with Signed = Some signed})
        Outputs = state.Outputs |> Map.map (fun _ v -> {v with Signed = Some signed})
    }

let inputPortLabels (state:ComponentState) =
    match state.Inputs.IsEmpty with
    | true -> []
    | false ->
        let highestPort =
            state.Inputs.Keys
            |> Seq.max
        [0..highestPort]
        |> List.choose (fun idx ->
            match state.Inputs.TryFind idx with
            | Some port -> Some port.Name
            | None -> None)

/// Represents a generic component that implements all features required for
/// Assertion components. Implements IComponent.
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
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
              InputLabels = inputPortLabels state
              OutputLabels = [] }
        member this.GetDescription state = this.DescriptionFunc this state
        member this.GetOutputWidths state inputPortWidths =
            // TODO(jpnock): Check logic
            let maxInputWidth = collectionMaxWithDefault 0 inputPortWidths.Values
            state.Outputs
            |> Map.map (fun outputPortNum output ->
                match output.FixedWidth with
                | Some w -> w
                | _ -> maxInputWidth)
        member this.Build state exprPortMap =
            let built = this.AssertionBuilder exprPortMap
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
        member this.GetDefaultState = makeIOSigned false <| {
            ComponentState.Default with
                ComparatorType = Some Eq
                Inputs = IODefaults.TwoInputs
                Outputs = IODefaults.OneFixedWidthOutputX 1
                LibraryID = this.LibraryID }
        member this.GetSymbolDetails state =
            let comparatorTyp =
                match state.ComparatorType with
                | Some typ -> typ
                | _ -> failwithf "Tried to get the comparator type of a non-comparator"
            { Name = comparatorTypeSymbolName comparatorTyp
              Prefix = SymbolDefaults.Prefix
              Height = SymbolDefaults.Height
              Width = SymbolDefaults.Width
              Colour = SymbolDefaults.Colour
              InputLabels = inputPortLabels state
              OutputLabels = [] }
        member this.GetDescription state =
            "Performs comparisons between two busses, such as checking a bus contains a greater value than another."
        member this.GetOutputWidths state inputPortWidths = Map.map (fun _ _ -> 1) state.Outputs
        member this.Build state exprPortMap =
            let built =
                match state.ComparatorType with
                | None -> failwith "Tried to build assertion AST for comparator without state set"
                | Some typ ->
                    match typ with
                    | Eq -> astMapper TEq exprPortMap
                    | Lt -> astMapper TLt exprPortMap
                    | Gt -> astMapper TGt exprPortMap
                    | Lte -> astMapper TLte exprPortMap
                    | Gte -> astMapper TGte exprPortMap
            match built with
            | Some b -> b
            | _ -> failwithf $"Unable to build for comparator {state.ComparatorType}"


// Helper function for dynamically generating the description of a
// signed component, based on its state.
let signedDescription _ state =
    let signedOperation =
        state.Inputs.Values
        |> Seq.exists (fun el -> Option.defaultValue false el.Signed)
    match signedOperation with
    | true -> "treating them as signed values"
    | false -> "treating them as unsigned values"

/// Helper function for creating a static description (regardless of state).
let basicDescription description _ _ =
    description

/// Helper function for creating component state, with
/// defaults applied.
let makeState outputs inputs libraryID =
    { ComponentState.Default with Inputs = inputs; Outputs = outputs; LibraryID = libraryID }   

/// Returns inputs 0 and 1 of a component, or fails if they
/// are not both present.
let mustGetOperands (state:ComponentState) =
    let i1, i2 = state.Inputs.TryFind 0, state.Inputs.TryFind 1
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
        DefaultState = makeState outputs inputs baseLibraryID
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
let implementsVariableWidth (state : ComponentState) =
    match state.Inputs.TryFind 0 with
    | Some input when input.FixedWidth.IsSome -> input.FixedWidth
    | _ -> None

/// Allows for components not registered in the library to be
/// represented as Plugin components (useful for simulation)
/// TODO(jpnock): refactor this such that these components are
/// supported by natively.
let makeStateFromExternalInputComponent id inputName width : ComponentState =
    {
        ComponentState.Default with
            InstanceID = Some id
            Outputs = Map [
                (0, {IODefaults.OutputX with Name = inputName; FixedWidth = width})
            ]
            IsInput = Some true
    }
