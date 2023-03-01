/// Author: James Nock
module VerificationLibrary

open AssertionTypes
open AssertionASTMap
open VerificationComponents

type ComponentClass =
    | ClassComparator of string
    | ClassTwoInputOperator of string
    | ClassNoIO of string
    | ClassAssert of string

type DefaultComponent = {
    Name: string
    Class: ComponentClass
    SymbolName: string
    Builder: ASTBuilder
    SignedAndUnsigned: bool
} with
    member this.MakeID =
        let normalised = this.Name.Replace(" ", "_").Replace("(", "_").Replace(")", "_").ToUpper()
        let className = this.Class.ToString().ToUpper()
        $"PLUGIN_{className}_{normalised.ToUpper()}"
    
    member this.makeSimpleComponents : SimpleComponent list =
        let inputs, outputs =
            match this.Class with
            | ClassComparator _ -> IODefaults.TwoInputs, IODefaults.OneFixedWidthOutputX 1
            | ClassTwoInputOperator _ -> IODefaults.TwoInputs, IODefaults.OneOutput
            | ClassNoIO _ -> Map.empty, Map.empty
            | ClassAssert _ -> IODefaults.OneFixedWidthInputA 1, Map.empty
            
        let tooltip =
            match this.Class with
            | ClassComparator desc -> desc
            | ClassTwoInputOperator desc -> desc
            | ClassNoIO desc -> desc
            | ClassAssert desc -> desc
        
        let description = basicDescription tooltip
        
        match this.SignedAndUnsigned with
        | false -> [makeSimpleComponentConcrete outputs inputs this.Name this.MakeID this.SymbolName description tooltip this.Builder]
        | true ->
            let u, s = makeSignedPairOfComponents outputs description tooltip this.Name this.MakeID this.SymbolName this.Builder
            [u; s]

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
        Name = "Less Than"
        Class = ClassComparator "Outputs HIGH when A is less than B"
        SymbolName = "A < B"
        Builder = astMapper TLt
        SignedAndUnsigned = true
     }
     {
        Name = "Less Than or Equal"
        Class = ClassComparator "Outputs HIGH when A is less than or equal to B"
        SymbolName = "A <= B"
        Builder = astMapper TLte
        SignedAndUnsigned = true
     }
     {
        Name = "Greater Than"
        Class = ClassComparator "Outputs HIGH when A is greater than B"
        SymbolName = "A > B"
        Builder = astMapper TGt
        SignedAndUnsigned = true
     }
     {
        Name = "Greater Than or Equal"
        Class = ClassComparator "Outputs HIGH when A is greater than or equal to B"
        SymbolName = "A >= B"
        Builder = astMapper TGte
        SignedAndUnsigned = true
     }
     {
         Name = "Equals"
         Class = ClassComparator "Outputs HIGH when A equals B"
         SymbolName = "A == B"
         Builder = astMapper TEq
         SignedAndUnsigned = false
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
         Builder = astMapper TAdd
         SignedAndUnsigned = false
     }
     {
         Name = "Assert HIGH"
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
            Name = "Text Assert HIGH"
            Class = ClassNoIO "Evaluates the provided expression and raises an assertion if the result is not HIGH"
            SymbolName = "Text\nAssertion"
            Builder = noAssertion
            SignedAndUnsigned = false
        }.makeSimpleComponents[0]
    {comp with DefaultState = {comp.DefaultState with AssertionText = Some ""}}

let private components: IComponent list =
    let allDefaultComps =
        defaultComps
        |> List.collect (fun el -> el.makeSimpleComponents)
        |> List.map (fun el -> el.ToInterface)
    List.concat [allDefaultComps; [makeTextAssertion]]

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