module Verification.Components

open Thoth.Json

type Signedness =
    | Signed
    | Unsigned

type Comparison =
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    
type AssertionOutput =
    | AssertHIGH
    | AssertLOW

type Width = int

// TODO(jpnock): add more verification components.
type Type =
    | SignExtend
    | ZeroExtend
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Power
    | Comparison of Comparison
    | AssertionOutput of AssertionOutput

type ComponentInput = { Name: string }

let oneInputDefaults = [{Name = "A"}]
let twoInputDefaults = [{Name = "A"}; {Name = "B"}]

type ComponentOutput = { Name: string; }

type BaseComponent =
    { CatalogueName : string
      Type: Type
      SymbolName: string
      SymbolPrefix: string
      Inputs : ComponentInput list
      Outputs : ComponentOutput list }

type IComponent =
    abstract member GetBase: BaseComponent
    abstract member GetDescription: string
    abstract member GenerateVerilog: string
    abstract member GetOutputWidths: int list
    abstract member JSONTypeName: string
    abstract member MakeJSONEncoder: (IComponent -> JsonValue)

let makeJSONEncoder (typName: string) (toEncode: IComponent) =
    Encode.object [
        "typ", typName
        "impl", toEncode
    ]

type ISignedComponent =
    abstract member GetSignedness : Signedness
    abstract member ImplementsISignedComponent : bool

type IVariableWidthComponent =
    abstract member GetWidth : int
    abstract member SetWidth : int -> IComponent
    abstract member GetWidthDescription : string
    abstract member ImplementsIVariableWidthComponent : bool

type GenericComponent =
    { Base: BaseComponent
      Description: string }
    static member JSONTypeName = "__GenericComponent"
    static member MakeEncoder = makeJSONEncoder GenericComponent.JSONTypeName
    interface IComponent with
        member this.GetBase = this.Base
        member this.GetDescription = this.Description
        member this.GenerateVerilog = ""
        member this.GetOutputWidths = []
        member this.JSONTypeName = GenericComponent.JSONTypeName
        member this.MakeJSONEncoder = GenericComponent.MakeEncoder
        

let makeGenericOneOutputBase name typ symbolName inputs: BaseComponent =
      {
        CatalogueName = name
        Type = typ
        SymbolName = symbolName
        SymbolPrefix = "VERIFICATION"
        Inputs = inputs
        Outputs = [ {Name = "X"} ]
      }

type TwoInputSignedOperatorComponent =
    { Base: BaseComponent
      OperatorAction: string
      Signedness: Signedness
      IsComparator: bool }
      static member JSONTypeName = "__TwoInputSignedOperatorComponent"
      static member MakeEncoder = makeJSONEncoder TwoInputSignedOperatorComponent.JSONTypeName
      interface ISignedComponent with
        member this.GetSignedness = this.Signedness
        member this.ImplementsISignedComponent = true
      interface IComponent with
        member this.GetBase = this.Base
        member this.GetDescription =
            let signWording =
                match this.Signedness with
                | Signed -> $"treating them as signed values"
                | Unsigned -> $"treating them as unsigned values"
            
            match this.IsComparator with
            | true -> $"Outputs HIGH when Input A is {this.OperatorAction} Output B, {signWording}"
            | false -> $"{this.OperatorAction} the two inputs, {signWording}"
        member this.GenerateVerilog = ""
        member this.GetOutputWidths = []
        member this.JSONTypeName = TwoInputSignedOperatorComponent.JSONTypeName
        member this.MakeJSONEncoder = TwoInputSignedOperatorComponent.MakeEncoder

let make1In1OutComponent name symbolName typ description : IComponent =
    {
        Base = makeGenericOneOutputBase name typ symbolName [ {Name = "A"} ]
        Description = description
    }

let make2In1OutComponent name symbolName typ description : IComponent =
    {
        Base = makeGenericOneOutputBase name typ symbolName twoInputDefaults
        Description = description
    }

let make2In1OutOperators action isComparator compBase : IComponent * IComponent =
    let signedName = $"{compBase.CatalogueName} (Signed)"
    let unsignedName = $"{compBase.CatalogueName} (Unsigned)"
    let unsignedComp = { Base = compBase; OperatorAction = action; Signedness = Unsigned; IsComparator = isComparator }
    ({unsignedComp with Base = {compBase with CatalogueName = unsignedName}},
     {unsignedComp with Base = {compBase with CatalogueName = signedName}; Signedness = Signed})

type VariableWidthComponent =
    { Base: BaseComponent
      Width: int }
    static member JSONTypeName = "__VariableWidthComponent"
    static member MakeEncoder = makeJSONEncoder VariableWidthComponent.JSONTypeName
    interface IComponent with
        member this.GetBase = this.Base
        member this.GetDescription = ""
        member this.GenerateVerilog = ""
        member this.GetOutputWidths = []
        member this.JSONTypeName = VariableWidthComponent.JSONTypeName
        member this.MakeJSONEncoder = VariableWidthComponent.MakeEncoder
    interface IVariableWidthComponent with
        member this.GetWidthDescription = ""
        member this.GetWidth = this.Width
        member this.SetWidth newWidth = {this with Width = newWidth}
        member this.ImplementsIVariableWidthComponent = true        

let components: IComponent list =
    let signExtend = {
            Base = makeGenericOneOutputBase "Sign Extend" SignExtend "Sign\nExtend" oneInputDefaults
            Width = 0}
    
    let zeroExtend = {
            Base = makeGenericOneOutputBase "Zero Extend" ZeroExtend "Zero\nExtend" oneInputDefaults
            Width = 0}
    
    let unsignedMultiply, signedMultiply =
        makeGenericOneOutputBase "Multiply" Multiply "A*B" twoInputDefaults
        |> make2In1OutOperators "Multiplies" false
    
    let unsignedDivide, signedDivide =
        makeGenericOneOutputBase "Divide" Multiply "A/B" twoInputDefaults
        |> make2In1OutOperators "Divides" false
        
    let unsignedMod, signedMod =
        makeGenericOneOutputBase "Modulo" Multiply "A % B" twoInputDefaults
        |> make2In1OutOperators "Applies A mod B using" false
    
    let unsignedPower, signedPower =
        makeGenericOneOutputBase "Power" Multiply "pow(A, B)" twoInputDefaults
        |> make2In1OutOperators "Applies pow(A, B) using" false
        
    let unsignedLessThan, signedLessThan =
        makeGenericOneOutputBase "Less than" (Comparison LessThan) "A < B" twoInputDefaults
        |> make2In1OutOperators "less than" true
   
    let unsignedLessThanEq, signedLessThanEq =
        makeGenericOneOutputBase "Less than or equal" (Comparison LessThanOrEqual) "A <= B" twoInputDefaults
        |> make2In1OutOperators "less than or equal to" true

    let unsignedGreaterThan, signedGreaterThan =
        makeGenericOneOutputBase "Greater than" (Comparison GreaterThan) "A > B" twoInputDefaults
        |> make2In1OutOperators "greater than" true
   
    let unsignedGreaterThanEq, signedGreaterThanEq =
        makeGenericOneOutputBase "Greater than or equal" (Comparison GreaterThanOrEqual) "A >= B" twoInputDefaults
        |> make2In1OutOperators "greater than or equal to" true
    
    [
       signExtend
       zeroExtend
       make2In1OutComponent "Add" "A+B" Add "Adds the two inputs"
       make2In1OutComponent "Sub" "A-B" Subtract "Subtracts input B from input A"
       unsignedMultiply
       signedMultiply
       unsignedDivide
       signedDivide
       unsignedMod
       signedMod
       unsignedPower
       signedPower
       unsignedLessThan
       signedLessThan
       unsignedLessThanEq
       signedLessThanEq
       unsignedGreaterThan
       signedGreaterThan
       unsignedGreaterThanEq
       signedGreaterThanEq
    ]

       // { Name = "Assert HIGH"
       //   Description = "Causes an assertion if the input is not HIGH"
       //   Type = AssertionOutput AssertHIGH }
       // { Name = "Assert LOW"
       //   Description = "Causes an assertion if the input is not LOW"
       //   Type = AssertionOutput AssertLOW }


       // { Name = $"Equal"
       //   Description = "Outputs HIGH if both inputs are equal"
       //   Type = Comparison(Equal) } ]


// let symbolName (typ:Type) =
//     match typ with
//     | SignExtend _ -> "Sign\nExtend"
//     | ZeroExtend _ -> "Zero\nExtend"
//     | Comparison comparison ->
//         match comparison with
//         | LessThan _ -> "A < B"
//         | GreaterThan _ -> "A > B" 
//         | LessThanOrEqual _ -> "A <= B" 
//         | GreaterThanOrEqual _ -> "A >= B"
//         | Equal -> "A==B"
//     | Add -> "+"
//     | Subtract -> "-"
//     | Multiply _ -> "*"
//     | Divide _ -> "/"
//     | Modulo _ -> "%"
//     | Power _ -> "pow(A,B)"
//     | AssertionOutput out ->
//         match out with
//         | AssertLOW -> "Assert\nLOW"
//         | AssertHIGH -> "Assert\nHIGH"

// let outputPortWidths (typ:Type) (inputPortWidths: Map<int, int>) =
//     match typ with
//     | AssertionOutput _ -> [1]
//     | Comparison _ -> [1]
//     | SignExtend | ZeroExtend -> [width]
//     // TODO(jpnock): Confirm this is the correct behaviour
//     | _ ->
//         printf $"got input ports of {inputPortWidths}"
//         [Map.values inputPortWidths |> Seq.max]

let private unsafeCastToIVariableWidthComponent (plugin: IComponent) : IVariableWidthComponent =
    downcast plugin
    
let castToIVariableWidthComponent (plugin: IComponent) : IVariableWidthComponent option =
    try Some (unsafeCastToIVariableWidthComponent plugin)
    with _ -> None

let implementsVariableWidth (plugin:IComponent) : bool =
    try (unsafeCastToIVariableWidthComponent plugin).ImplementsIVariableWidthComponent
    with _ -> false

module IComponentCoder =
    let decoder : Decoder<IComponent> =
        let decTwoInputSignedOperatorComponent = Decode.Auto.generateDecoder<TwoInputSignedOperatorComponent>()
        let decGenericComponent = Decode.Auto.generateDecoder<GenericComponent>()
        let decVariableWidthComponent = Decode.Auto.generateDecoder<VariableWidthComponent>()
        
        Decode.object (fun get -> (
            let typ = get.Required.Field "typ" Decode.string
            match typ with
            | s when s = TwoInputSignedOperatorComponent.JSONTypeName ->
                let impl = get.Required.Field "impl" decTwoInputSignedOperatorComponent
                (impl : IComponent)
            | s when s = GenericComponent.JSONTypeName ->
                let impl = get.Required.Field "impl" decGenericComponent
                (impl : IComponent)
            | s when s = VariableWidthComponent.JSONTypeName ->
                let impl = get.Required.Field "impl" decVariableWidthComponent
                (impl : IComponent)
            | _ -> failwithf "Unknown concrete plugin type name"
        ))
    
    let encoder (comp : IComponent) =
        comp.MakeJSONEncoder comp

// let inline makeDecoder<'t when 't :> IComponent> (typName: string) : string -> obj -> Result<IComponent,DecoderError> =
//     let res = Decode.Auto.generateDecoder<'t>()
//     Decode.object (fun get -> (
//         get.Required.Raw res
//     ))

// let decoderMap: Map<string, string -> obj -> Result<IComponent,DecoderError>> =
//     Map.empty
//     |> Map.add VariableWidthComponent.JSONTypeName (
//         makeDecoder<VariableWidthComponent> VariableWidthComponent.JSONTypeName)
//     |> Map.add TwoInputSignedOperatorComponent.JSONTypeName (
//         makeDecoder<TwoInputSignedOperatorComponent> TwoInputSignedOperatorComponent.JSONTypeName)
//     |> Map.add GenericComponent.JSONTypeName (
//         makeDecoder<GenericComponent> GenericComponent.JSONTypeName)
//
// let encoderMap: Map<string, (IComponent -> JsonValue)> =
//     Map.empty
//     |> Map.add VariableWidthComponent.JSONTypeName VariableWidthComponent.MakeEncoder
//     |> Map.add TwoInputSignedOperatorComponent.JSONTypeName TwoInputSignedOperatorComponent.MakeEncoder
//     |> Map.add GenericComponent.JSONTypeName GenericComponent.MakeEncoder
