module VerificationTypes

open System
open AssertionTypes

type ComparatorType =
    | ComparatorTypeEq
    | ComparatorTypeLt
    | ComparatorTypeLte
    | ComparatorTypeGt
    | ComparatorTypeGte
    
let comparatorTypeName typ =
    match typ with
    | ComparatorTypeLt -> "Less than"
    | ComparatorTypeLte -> "Less than or equals"
    | ComparatorTypeGt -> "Greater than"
    | ComparatorTypeGte -> "Greater than or equals"
    | ComparatorTypeEq -> "Equal"

let comparatorTypeSymbolName typ =
    match typ with
    | ComparatorTypeLt -> "A < B"
    | ComparatorTypeLte -> "A <= B"
    | ComparatorTypeGt -> "A > B"
    | ComparatorTypeGte -> "A >= B"
    | ComparatorTypeEq -> "A == B"

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
      // TODO(jpnock): Move elsewhere... Used in evaluation 
      HostLabel: string
      // TODO(jpnock): Move elsewhere... Used in evaluation 
      HostSheet: string
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
      MultiComponentType: MultiComponentType option }
    static member Default : ComponentConfig = {
        InstanceID = Some (Guid.NewGuid().ToString())
        LibraryID = ""
        Inputs = Map.empty
        Outputs = Map.empty
        AssertionText = None
        AssertionDescription = None
        IsInput = None
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

