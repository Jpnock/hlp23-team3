/// Contains logic for building an Assertion AST
/// from a set of visual verification components.
///
/// Authored by jpn119 (James Nock)
module VerificationASTGen

open AssertionTypes
open VerificationComponents
open VerificationLibrary

/// Returns the component config at a given input port number.
let getConfigForInput (portToSource: Map<int, ComponentConfig>) inputNum =
    // TODO(jpnock): use try-catch here
    printf $"Getting component config for: {inputNum}"
    portToSource[inputNum]

/// Helper function for recursively generating an Assertion AST.
/// The `componentPortSources` arg contains a map of component IDs
/// too a map of input port component configs. This allows a component ID
/// to be looked up and the state returned for any component connected to
/// its input ports. The `cfg` arg represents the config of the component
/// that the AST is currently being built for.
let rec generateAST (componentPortSources: Map<string, Map<int, (ComponentConfig * int * string)>>) sourcePortN (connId: string) (cfg: ComponentConfig) : Expr =
    match cfg.IsInput with
    | Some true -> 
        printf $"making lit : {cfg}"

        Lit (Id (cfg.Outputs[sourcePortN].HostLabel, sourcePortN, connId))
    | _ ->
        printf $"Getting state for {cfg}"
        let componentPortMap = 
            componentPortSources[cfg.InstanceID.Value]
            |> Map.map (fun _ (state, _, _) -> state)
        let componentBuilder = library.Components[cfg.LibraryID]
        printf $"Looking up state {cfg}"
        
        cfg.Inputs
        |> Map.map (fun k _ -> getConfigForInput componentPortMap k)
        |> Map.map (fun k cs -> 
            let (_, portN, connId) = componentPortSources[cfg.InstanceID.Value].[k] 
            generateAST componentPortSources portN connId cs)
        |> componentBuilder.CreateAST cfg 
