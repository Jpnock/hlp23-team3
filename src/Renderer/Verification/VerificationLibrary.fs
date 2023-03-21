/// Contains logic for representing the collection
/// of verification components (the library).
///
/// Authored by jpn119 (James Nock)
module VerificationLibrary

open AssertionTypes
open AssertionASTMap
open VerificationComponents

type ComponentClass =
    | ClassComparator of string
    | ClassTwoInputOperator of string
    | ClassNoIO of string
    | ClassAssert of string

let makeLibraryID (cls:ComponentClass) (name:string) =
    let normalised = name.Replace(" ", "_").Replace("(", "_").Replace(")", "_").ToUpper()
    let className = cls.ToString().ToUpper().Split(" ")[0]
    $"PLUGIN_{className}_{normalised.ToUpper()}"

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
            | ClassComparator _ -> IODefaults.TwoInputs, IODefaults.OneFixedWidthOutputX 1
            | ClassTwoInputOperator _ -> IODefaults.TwoInputs, IODefaults.OneOutput
            | ClassNoIO _ -> Map.empty, Map.empty
            | ClassAssert _ -> IODefaults.OneAssertionInputA, Map.empty
            
        let tooltip =
            match this.Class with
            | ClassComparator desc -> desc
            | ClassTwoInputOperator desc -> desc
            | ClassNoIO desc -> desc
            | ClassAssert desc -> desc
        
        let description = basicDescription tooltip
        
        let baseComp =
            makeSimpleComponentConcrete outputs inputs this.Name this.MakeID this.SymbolName description tooltip this.Builder
        
        let assertState =
            match this.Class with
            | ClassAssert _ -> {baseComp with DefaultConfig = {baseComp.DefaultConfig with AssertionDescription = Some ""}}
            | _ -> baseComp
        
        match this.SignedAndUnsigned with
        | false -> [assertState]
        | true -> [{assertState with DefaultConfig = assertState.DefaultConfig}]

// let lessThan =
//      {
//         Name = "Less Than"
//         Class = ClassComparator "Outputs HIGH when A is less than B"
//         SymbolName = "A < B"
//         Builder = astMapper TLt
//         SignedAndUnsigned = true
//      }
//
// let lessThanOrEqual =
//      {
//         Name = "Less Than or Equal"
//         Class = ClassComparator "Outputs HIGH when A is less than or equal to B"
//         SymbolName = "A <= B"
//         Builder = astMapper TLte
//         SignedAndUnsigned = true
//      }
//
// let greaterThan = 
//      {
//         Name = "Greater Than"
//         Class = ClassComparator "Outputs HIGH when A is greater than B"
//         SymbolName = "A > B"
//         Builder = astMapper TGt
//         SignedAndUnsigned = true
//      }
//
// let greaterThanOrEqual =
//      {
//         Name = "Greater Than or Equal"
//         Class = ClassComparator "Outputs HIGH when A is greater than or equal to B"
//         SymbolName = "A >= B"
//         Builder = astMapper TGte
//         SignedAndUnsigned = true
//      }
//      
// let equals =
//      {
//          Name = "Equals"
//          Class = ClassComparator "Outputs HIGH when A equals B"
//          SymbolName = "A == B"
//          Builder = astMapper TEq
//          SignedAndUnsigned = false
//      }

// TODO(jpnock): Add more components in group phase
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
        Name = "Power"
        Class = ClassTwoInputOperator "Raises A to the power of B"
        SymbolName = "pow(A, B)"
        Builder = noAssertion
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
            SymbolName = "Text\nAssertion"
            Builder = noAssertion
            SignedAndUnsigned = false
        }.makeSimpleComponents[0]
    {
        comp with DefaultConfig = { comp.DefaultConfig with AssertionText = Some ""; AssertionDescription = Some "" }
    }

let private comparatorComp : IComponent =
    {
        LibraryID = makeLibraryID (ClassComparator "") "Comparator"
    } : ComparatorComponent

let private components: IComponent list =
    let allDefaultComps =
        defaultComps
        |> List.collect (fun el -> el.makeSimpleComponents)
        |> List.map (fun el -> el.ToInterface)
    List.concat [allDefaultComps; [makeTextAssertion; comparatorComp]]

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
