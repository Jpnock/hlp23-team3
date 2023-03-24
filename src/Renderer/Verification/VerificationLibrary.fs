/// Contains logic for representing the collection
/// of verification components (the library).
///
/// Authored by jpn119 (James Nock)
module VerificationLibrary

open AssertionTypes
open AssertionASTMap
open VerificationTypes
open VerificationComponents

type ComponentClass =
    | ClassComparator of string
    | ClassTwoInputOperator of string
    | ClassNoIO of string
    | ClassAssert of string
    | ClassMultiComponent of string * MultiComponentType

let makeLibraryID (cls:ComponentClass) (name:string) =
    let normalised = name.Replace(" ", "_").Replace("(", "_").Replace(")", "_").ToUpper()
    let className = cls.ToString().ToUpper().Split(" ")[0]
    $"PLUGIN_{className}_{normalised.ToUpper()}"

let inline defaultIO<'T when 'T : (member ToInterface : IComponent)> (comp : 'T)=
    let compDefaults = comp.ToInterface.GetDefaultConfig
    compDefaults.Inputs, compDefaults.Outputs

type DefaultComponent = {
    Name: string
    Class: ComponentClass
    SymbolName: string
    Builder: ASTBuilder
    SignedAndUnsigned: bool
} with
    member this.MakeID = makeLibraryID this.Class this.Name
    member this.makeSimpleComponents : SimpleComponent list =
        let inputs, outputs =
            match this.Class with
            | ClassComparator _ -> IOConstants.TwoInputs, IOConstants.OneFixedWidthOutputX 1
            | ClassTwoInputOperator _ -> IOConstants.TwoInputs, IOConstants.OneOutput
            | ClassNoIO _ -> Map.empty, Map.empty
            | ClassAssert _ -> IOConstants.OneAssertionInputA, Map.empty
            | ClassMultiComponent (_, multiTyp) ->
                match multiTyp with
                | ComparatorType _ -> defaultIO ({ LibraryID = "" } : ComparatorComponent)
                | LogicalOpType _ -> defaultIO ({ LibraryID = "" } : LogicalOpComponent)
                | BitwiseOpType _ -> defaultIO ({ LibraryID = "" } : BitwiseOpComponent)
        
        let tooltip =
            match this.Class with
            | ClassComparator desc -> desc
            | ClassTwoInputOperator desc -> desc
            | ClassNoIO desc -> desc
            | ClassAssert desc -> desc
            | ClassMultiComponent (desc, _) -> desc
        
        let description = basicDescription tooltip
        
        let baseComp =
            makeSimpleComponentConcrete outputs inputs this.Name this.MakeID this.SymbolName description tooltip this.Builder
        
        let assertCfg =
            match this.Class with
            | ClassAssert _ ->
                {baseComp with DefaultConfig = {baseComp.DefaultConfig with AssertionDescription = Some ""}}
            | _ -> baseComp
        
        match this.SignedAndUnsigned with
        | false -> [assertCfg]
        | true -> [{assertCfg with DefaultConfig = assertCfg.DefaultConfig}]

let private defaultComps : DefaultComponent list = [
     {
        Name = "Multiply"
        Class = ClassTwoInputOperator "Multiplies the two inputs"
        SymbolName = "A*B"
        Builder = astMapper TMul 
        SignedAndUnsigned = true
     }
     {
        Name = "Divide"
        Class = ClassTwoInputOperator "Divides A by B"
        SymbolName = "A / B"
        Builder = astMapper TDiv 
        SignedAndUnsigned = true
     }
     {
        Name = "Modulo"
        Class = ClassTwoInputOperator "Returns the remainder of A divided by B"
        SymbolName = "A % B"
        Builder = astMapper TRem
        SignedAndUnsigned = true
     }
     {
         Name = "Add"
         Class = ClassTwoInputOperator "Outputs the sum of A and B"
         SymbolName = "A + B"
         Builder = astMapper TAdd 
         SignedAndUnsigned = false
     }
     {
         Name = "Subtract"
         Class = ClassTwoInputOperator "Outputs the value of B subtracted from A"
         SymbolName = "A - B"
         Builder = astMapper TSub 
         SignedAndUnsigned = false
     }
     {
         Name = "Assert HIGH (Visual)"
         Class = ClassAssert "Raises an assertion if the input is not HIGH"
         SymbolName = "Assert\nHIGH"
         Builder = noAssertion
         SignedAndUnsigned = false
     }
]

/// Added by jls20
let private makeTextAssertion : IComponent =
    let comp =
        {
            Name = "Assert HIGH (Text)"
            Class = ClassNoIO "Evaluates the provided expression and raises an assertion if the result is not HIGH"
            SymbolName = "Text Assert"
            Builder = noAssertion
            SignedAndUnsigned = false
        }.makeSimpleComponents[0]
    let defaultText = 
        "// Get started by writing your assertions below! Here's a few examples: \n\ninput a; input b; \n a == b \n//a >= b + 5 \n//signed(5'b) >= (a - 25) * 2 \n\n"
    {
        comp with DefaultConfig = { comp.DefaultConfig with AssertionText = Some defaultText ; AssertionDescription = Some "" }
    }

let private comparatorComp : IComponent =
    {
        LibraryID = makeLibraryID (ClassComparator "") "Comparator"
    } : ComparatorComponent

let private bitwiseOpComp : IComponent =
    {
        LibraryID = makeLibraryID (ClassMultiComponent ("", BitwiseOpType BitwiseOpTypeAnd)) "Bitwise Operator"
    } : BitwiseOpComponent

let private logicalOpComponent : IComponent =
    {
        LibraryID = makeLibraryID (ClassMultiComponent ("", LogicalOpType LogicalOpTypeAnd)) "Logical Operator"
    } : LogicalOpComponent

let private components: IComponent list =
    let allDefaultComps =
        defaultComps
        |> List.collect (fun el -> el.makeSimpleComponents)
        |> List.map (fun el -> el.ToInterface)
    List.concat [allDefaultComps; [makeTextAssertion; comparatorComp; bitwiseOpComp; logicalOpComponent]]

type ComponentLibrary =
    { Components: Map<LibraryID, IComponent> }
    member this.register (comp:IComponent) = this.Components.Add (comp.GetLibraryID, comp)

// TODO(jpnock): think about how to expose this, so other parts of the code
// can dynamically register components. One way would be making this mutable.
// Alternatively, the library can be passed around on some existing Issie
// data-structure.
let library: ComponentLibrary = {
    Components =
        components
        |> List.map (fun comp -> (comp.GetLibraryID, comp))
        |> Map.ofList
}
