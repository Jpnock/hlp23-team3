(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open SimulationBuilder
open SimulationRunner
open DependencyMerger
open SimulationGraphAnalyser

// Simulating a circuit has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...
// 4. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

/// Builds the graph and simulates it with all inputs zeroed.


/// upper case a string
let cap (sheet:string) = sheet.ToUpper()

/// look up a sheet in a set of loaded components
let getSheet (ldcs: LoadedComponent list) (openSheet: string)  =
    match List.tryFind (fun ldc -> cap ldc.Name = cap openSheet) ldcs with
    | None -> failwithf $"getSheet failed to look up '{openSheet}' in {ldcs |> List.map (fun ldc -> ldc.Name)}"
    | Some name -> name

/// look up a sheet in a set of loaded components, return [] or a list of the matching LoadedComponent
let getLdcList (ldcs: LoadedComponent list) (openSheet: string)  =
    match List.tryFind (fun ldc -> cap ldc.Name = cap openSheet) ldcs with
    | None -> failwithf $"getSheet failed to look up '{openSheet}' in {ldcs |> List.map (fun ldc -> ldc.Name)}"
    | Some name -> name

let getDirectDependencies (cs:CanvasState) =
    fst cs
    |> List.collect (fun comp -> match comp.Type with | Custom ct-> [comp.Label, ct.Name] | _ -> [])

let childrenOf (ldcs: LoadedComponent list) (sheet:string) =
    getSheet ldcs sheet
    |> (fun ldc -> getDirectDependencies ldc.CanvasState)


/// Sheets needed to simulate sheet with name sheet.
/// Sheets form a dependency tree. 
/// ldcs is a list of loaded components which must include sheet
let rec sheetsNeeded (ldcs: LoadedComponent list) (sheet:string): string list =
    let children = childrenOf ldcs sheet |> List.map snd
    children
    |> List.map (sheetsNeeded ldcs)
    |> List.concat
    |> List.append children
    |> List.append [sheet]
    |> List.distinct


/// canvasState: extracted canvasState from draw block.
/// projLdcs: ldcs from project (current sheet ldc may be outofdate)
/// diagramName: name of current open sheet.
/// return updated list of all LDCs
let getUpdatedLoadedComponentState diagramName canvasState projLdcs =
    let ldc' = Extractor.extractLoadedSimulatorComponent canvasState diagramName
    let ldcs = 
        let ldcIsOpen ldc = ldc.Name = diagramName
        projLdcs 
        |> List.map (fun ldc -> if ldcIsOpen ldc then ldc' else ldc)
        |> (fun ldcs -> if not <| List.exists ldcIsOpen ldcs then ldc' :: ldcs else ldcs)
    ldcs


/// gets the status of the simulation given current canvasState and project
let getCurrentSimulationState (canvState: CanvasState) (project: Project option) (fs:FastSimulation) : SimulationRunStatus =
    match project with
    | None -> 
        SimNoProject
    | _ when fs.SimulatedTopSheet = "" ->
        SimEmpty
    | Some p -> 
        let simIsUpToDate =
            fs.SimulatedCanvasState
            |> List.forall (fun ldc -> 
                match p.OpenFileName = ldc.Name, List.tryFind (fun (ldc':LoadedComponent) -> ldc'.Name = ldc.Name) p.LoadedComponents with
                | _, None -> false
                | false, Some ldc' -> Extractor.loadedComponentIsEqual ldc ldc'
                | true, Some _ -> Extractor.stateIsEqual ldc.CanvasState canvState)
        match simIsUpToDate, p.OpenFileName = fs.SimulatedTopSheet with
        | false, _ -> 
            SimOutOfDate
        | true, true -> 
            SimValidSameSheet
        | true, false -> 
            SimValidDifferentSheet


/// Helper used convert port into SheetPort for use by wave simulator determining connectivity
/// within a design sheet.
/// name is the name of the containing sheet.
let portSheetPort (compsWithIds:Map<ComponentId,Component>) (name:string)  port  =
    let comp = compsWithIds[ComponentId port.HostId]
    let compPort = comp.getPort (PortId port.Id)
    match compPort with
    | None -> None
    | Some cPort ->
        {
            PortOnComp = cPort
            Sheet = name
        } |> Some

/// canvasState: extracted canvasState from draw block.
/// loadedComponents: from project
/// diagramName: name of current open sheet.
/// save all needed by simulation ldcs in the FastSimulation record.
/// The top sheet name must be saved separately - since if a simulation is being refreshed it must not change
let saveStateInSimulation (canvasState:CanvasState) (openFileName: string) (loadedComponents: LoadedComponent list) (fs:FastSimulation) =
    let diagramName = openFileName
    let ldcs = getUpdatedLoadedComponentState diagramName canvasState loadedComponents
    //printfn $"diagramName={diagramName}, sheetNames = {ldcs |> List.map (fun ldc -> ldc.Name)}"
    sheetsNeeded ldcs diagramName
    |> List.map (getSheet ldcs)
    |> (fun updatedLdcs -> 

        let compMap, portMap =
            updatedLdcs
            |> List.map (
                fun ldc -> 
                    let comps,conns = ldc.CanvasState
                    let compsWithIds = 
                        comps 
                        |> List.map (fun comp -> ComponentId comp.Id, comp)
                        |> Map.ofList
                    
                    let portSheetPort = portSheetPort compsWithIds ldc.Name 
                    
                    let addConnToPort (portOpt: SheetPort option) conn pMap: Map<SheetPort,Connection list> =
                        match portOpt with
                        | None -> pMap
                        | Some port ->
                            pMap
                            |> Map.change port (function | Some conns -> Some (conn :: conns) | None -> Some [conn])
                    let portsToConnections =
                        (Map.empty, conns)
                        ||> List.fold (fun pMap conn ->
                            pMap
                            |> addConnToPort (portSheetPort conn.Source) conn
                            |> addConnToPort (portSheetPort conn.Target) conn)
                        |> Map.toList
                    ((ldc.Name,compsWithIds), portsToConnections))
            |> List.unzip  


        let compMap = compMap|> Map.ofList
        let portMap = portMap |> List.concat |> Map.ofList
    
            
        {fs with 
            SimulatedCanvasState = updatedLdcs
            ComponentsById = compMap
            ConnectionsByPort = portMap 
        })
     

/// Extract circuit data from inputs and return a checked SimulationData object or an error
/// SimulationData has some technical debt, it wraps FastSimulation adding some redundant data
let rec startCircuitSimulation
        (simulationArraySize: int)
        (diagramName : string)
        (fullCanvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =

    // Remove all verification components from the simulation
    // as this logic is performed outside of the simulator.
    let canvasComps, canvasConnections = fullCanvasState 
    
    let isTypeVerificationComp =
        function
        | Plugin _ -> true
        | _ -> false
    
    let verificationCompMap =
        canvasComps
        |> List.map (fun comp -> comp.Id, isTypeVerificationComp comp.Type)
        |> Map.ofList
    
    // TODO(jpnock): we might also want to remove constants that are solely 
    // connected to verification components, otherwise these will error.
    let canvasNonVerificationComps =
        canvasComps
        |> List.filter (fun comp -> not (isTypeVerificationComp comp.Type))
    
    let isVerificationComp compId = verificationCompMap[compId]
    
    let componentMap =
        canvasComps
        |> List.map (fun el -> el.Id, el)
        |> Map.ofList
    
    let textAssertions =
        canvasComps
        |> List.filter (fun c ->
            match c.Type with
            | Plugin state when state.AssertionText.IsSome -> true
            | _ -> false)
        
    let makeState =
        VerificationComponents.makeStateFromExternalInputComponent

    let getPortNames comp : string List = 
        comp.OutputPorts 
        |> List.map (fun oP -> oP.Id) 

    let getnWidths n = 
        [0..n]
        |
    let getSourceComponentState componentId =
        let source = componentMap[componentId]
        match source.Type with
        | Plugin state -> state
        | Input1 (busWidth, _) -> makeState source.Id source.Label [(Some busWidth)] [source.OutputPorts.Head.Id]
        | IOLabel -> makeState source.Id source.Label [None] [source.OutputPorts.Head.Id]
        | Viewer busWidth -> makeState source.Id source.Label [(Some busWidth)] [source.OutputPorts.Head.Id]
        | Constant (busWidth, _) -> makeState source.Id source.Label [(Some busWidth)] [source.OutputPorts.Head.Id]
        | _ -> makeState source.Id source.Label ([0.]) (getPortNames source)
        
    // Problem : port number is None on components
    // Solution : make a mapping between port Id and number
    let inputPortIDToNumber =
        canvasComps
        |> List.map (fun comp -> comp.InputPorts)
        |> List.concat
        |> List.map (fun port -> (port.Id, port.PortNumber))
        |> Map.ofList
    
    let targetIDtoPortNum id =
        printf $"Looking up {id}"
        inputPortIDToNumber[id]
    
    // HostId -> Map<input port number, state>
    let componentIDToInputPortState =
        canvasConnections
        |> List.map (fun conn -> (conn.Target.HostId, targetIDtoPortNum conn.Target.Id, getSourceComponentState conn.Source.HostId))
        |> List.groupBy (fun (hostId, _, _) -> hostId)
        |> List.map (fun (k, v) ->
            let inputMap =
                v |> List.choose (
                        fun (_, inputNumber, state) ->
                        match inputNumber.IsSome with
                        | true -> Some (inputNumber.Value, state)
                        | false -> None )
            k, Map.ofList inputMap)
        |> Map.ofList

    let assertionComps =
        canvasComps
        |> List.choose (fun el ->
            match el.Type with
            | Plugin state when state.Inputs.Count = 1 && state.Outputs.Count = 0 ->
                Some state
            | _ -> None)
    
    let assertionTexts =
        canvasComps
        |> List.choose (fun el ->
            match el.Type with
            | Plugin state when state.AssertionText.IsSome -> state.AssertionText
            | _ -> None)
    
    let assertionCompASTs : AssertionTypes.Assertion list =
        assertionComps
        |> List.map (fun el ->
            // TODO(jpnock): Currently assuming assert HIGH
            let compConnectedToAssert = componentIDToInputPortState[el.InstanceID.Value][0]
            let ast = VerificationASTGen.generateAST componentIDToInputPortState compConnectedToAssert
            let exprPos = {Line = 0; Col = 0; Length = 0} : AssertionTypes.Pos
            let assertion = ast, exprPos
            {AST = assertion})
    
    let assertionParseResults = List.map AssertionParser.parseAssertion assertionTexts
    
    let resultFolder state result =
        match result with 
        | Ok expr -> (List.append [expr] <| fst state, snd state)
        | Error e -> (fst state, List.append [e] <| snd state)

    let (assertionExprs, assertionErrors) = List.fold resultFolder ([], []) assertionParseResults

    if assertionErrors.IsEmpty = false then
        printf $"Not all assertions could be parsed:"
        List.iter ( fun e -> printf $"{e}") assertionErrors

    let emptyPos = {AssertionTypes.Line = 1; AssertionTypes.Col = 1; AssertionTypes.Length = 1; }
    let assertionASTs = List.map (fun expr -> {AssertionTypes.AST = expr, emptyPos}) assertionExprs
    let assertionTextASTs : AssertionTypes.Assertion list = assertionASTs
    
    let allASTs = List.concat [assertionCompASTs; assertionTextASTs]
    
    printf $"Got assertion ASTs {allASTs}"
    
    allASTs
    |> List.map (fun el ->
        let pretty = AssertionParser.prettyPrintAST (fst el.AST) "" false
        printf $"Got AST:\n{pretty}")
    |> ignore
    
    let checkedASTs =
        allASTs
        |> List.map (fun el -> AssertionCheck.checkAST el.AST canvasComps)
    
    let goodASTs =
        checkedASTs
        |> List.choose (
            function
            | AssertionTypes.Properties properties -> Some properties
            | _ -> None)
    
    let finalAssertions =
        if goodASTs.Length = checkedASTs.Length then allASTs
        else
            printf $"ERROR in ASTs assertionASTs {checkedASTs} {goodASTs}"
            []
    
    // Remove all connections that are connected to verification components
    // at either end.
    let canvasNonVerificationConns =
        canvasConnections
        |> List.filter (fun conn ->
            (not (isVerificationComp conn.Source.HostId)) && (not (isVerificationComp conn.Target.HostId)))
    
    let canvasState = canvasNonVerificationComps, canvasNonVerificationConns
    
    /// Tune for performance of initial zero-length simulation versus longer run.
    /// Probably this is not critical.
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph
                                canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it
            let components, connections = canvasState
            let inputs, outputs = getSimulationIOs components
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None -> 
                try
                    match FastRun.buildFastSimulation simulationArraySize diagramName graph finalAssertions with
                    | Ok fs ->
                        let fs = saveStateInSimulation canvasState diagramName loadedDependencies fs   
                        Ok {
                            FastSim = fs                           
                            Graph = graph // NB graph is now not initialised with data
                            Inputs = inputs;
                            Outputs = outputs
                            IsSynchronous = hasSynchronousComponents graph
                            NumberBase = Hex
                            ClockTickNumber = 0
                        }
                    | Error  e -> Error  e
                with
                | AlgebraNotImplemented e -> Error e
                | e -> 
                    printfn "\nEXCEPTION:\n\n%A\n%A\n\n" e.Message e.StackTrace
                    Error {
                        Msg = sprintf "\nInternal ERROR in Issie fast simulation: %A\n\n%A\n" e.Message e.StackTrace
                        InDependency = None
                        ComponentsAffected = []
                        ConnectionsAffected = []
                    }
                |> Result.map (fun sd ->
                    //Fast.compareFastWithGraph sd |> ignore
                    sd)





/// Expose the extractSimulationIOs function from SimulationRunner.
let extractSimulationIOs = SimulationRunner.extractSimulationIOs

/// Get some info and the state of all stateful components in a graph.
let extractStatefulComponents
        (graph : SimulationGraph)
        : SimulationComponent list =
    graph
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp -> comp.State <> NoState)
    // TODO: recursively search custom components?


