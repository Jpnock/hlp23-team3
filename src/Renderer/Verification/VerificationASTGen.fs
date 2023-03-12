/// Contains logic for building an Assertion AST
/// from a set of visual verification components.
///
/// Authored by jpn119 (James Nock)
module VerificationASTGen

open AssertionTypes
open VerificationComponents
open VerificationLibrary

/// Returns the component state at a given input port number.
let getStateForInput (portToSource: Map<int, ComponentState>) inputNum =
    // TODO(jpnock): use try-catch here
    printf $"Getting state for: {inputNum}"
    portToSource[inputNum]

/// Helper function for recursively generating an Assertion AST.
/// The `componentPortSources` arg contains a map of component IDs
/// too a map of input port component states. This allows a component ID
/// to be looked up and the state returned for any component connected to
/// its input ports. The `state` arg represents the state of the component
/// the AST is currently being built for.
let rec generateAST (componentPortSources: Map<string, Map<int, ComponentState>>) (state: ComponentState) : Expr =
    match state.IsInput with
    | Some true -> Lit (Id state.Outputs[0].Name)
    | _ ->
        printf $"Getting state for {state}"
        let componentPortMap = componentPortSources[state.InstanceID.Value]
        let componentBuilder = library.Components[state.LibraryID]
        printf $"Looking up state {state}"
        
        state.Inputs
        |> Map.map (fun k _ -> getStateForInput componentPortMap k)
        |> Map.map (fun _ -> generateAST componentPortSources)
        |> componentBuilder.Build state
