// Author: James Nock
module VerificationASTGen

open AssertionTypes
open VerificationComponents
open VerificationLibrary

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
        |> componentBuilder.Build
