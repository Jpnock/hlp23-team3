(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open AssertionTypes
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
    |> List.filter (fun dep -> dep <> sheet)
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
let saveStateInSimulation (canvasState:CanvasState) (openFileName: string) (loadedComponents: LoadedComponent list) =
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
    
        updatedLdcs, compMap, portMap)

let saveStateInSimulationFastSim (canvasState:CanvasState) (openFileName: string) (loadedComponents: LoadedComponent list) (fs:FastSimulation) =
    let updatedLdcs, compMap, portMap = saveStateInSimulation canvasState openFileName loadedComponents
    {
        fs with 
            SimulatedCanvasState = updatedLdcs
            ComponentsById = compMap
            ConnectionsByPort = portMap 
    }

/// Extract circuit data from inputs and return a checked SimulationData object or an error
/// SimulationData has some technical debt, it wraps FastSimulation adding some redundant data
let rec startCircuitSimulation
        (simulationArraySize: int)
        (diagramName : string)
        (fullCanvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =
    
    let _, sheetComponentMap, sheetCanvasConnections = saveStateInSimulation fullCanvasState diagramName loadedDependencies
    // printf $"Got all sheet ldcs \n {allSheetLoadedComps}"
    // printf $"Got all sheet comps \n {sheetComponentMap}"
    // printf $"Got all sheet conns \n {sheetCanvasConnections}"
    
    let canvasConnections =
        sheetCanvasConnections
        |> Map.values
        |> Seq.concat
        |> List.ofSeq
    
    let mergeMaps (maps: seq<Map<_, _>>) =
        maps
        |> Seq.fold (fun accMap map ->
            Map.fold (fun newMap key value -> Map.add key value newMap) accMap map
        ) Map.empty
    
    let componentMap =
        sheetComponentMap
        |> Map.values
        |> mergeMaps

    let canvasComps =
        sheetComponentMap.Values
        |> Seq.map Map.values
        |> Seq.concat
        |> List.ofSeq
    
    let isAssertionComponent (comp:Component) =
        match comp.Type with
        | Plugin state when state.Inputs.Count = 1 && state.Outputs.Count = 0 ->
            Some (comp.Id, state)
        | _ -> None
    
    let assertionCompsAndIDs =
        List.choose isAssertionComponent canvasComps
    
    let makeState =
        VerificationComponents.makeConfigFromExternalInputComponent

    let getPortNames comp : string list = 
        comp.OutputPorts 
        |> List.map (fun oP -> oP.Id)

    let getSourceComponentState componentId =
        let source = componentMap[componentId]
        match source.Type with
        | Plugin state -> state
        | Input1 (busWidth, _) -> makeState source.Id [(Some busWidth)] [source.OutputPorts.Head.Id] source.Label
        | IOLabel -> makeState source.Id [None] [source.OutputPorts.Head.Id] source.Label
        | Viewer busWidth -> makeState source.Id [(Some busWidth)] [source.OutputPorts.Head.Id] source.Label
        | Constant (busWidth, _) -> makeState source.Id [(Some busWidth)] [source.OutputPorts.Head.Id] source.Label
        | _ -> makeState source.Id ([0..source.OutputPorts.Length] |> List.map (fun _ -> None)) (getPortNames source) source.Label
        
    // Problem : port number is None on components
    // Solution : make a mapping between port Id and number
    let inputPortIDToNumber =
        canvasComps
        |> List.map (fun comp -> [comp.InputPorts; comp.OutputPorts])
        |> List.concat |> List.concat
        |> List.map (fun port -> (port.Id, port.PortNumber))
        |> Map.ofList
    
    let targetIDtoPortNum id =
        // printf $"Looking up {id}"
        inputPortIDToNumber[id]
    
    // HostId -> Map<input port number, state>
    let componentIDToInputPortState =
        canvasConnections
        |> List.map (fun conn -> (
                conn.Target.HostId,
                targetIDtoPortNum conn.Target.Id,
                (
                    getSourceComponentState (ComponentId conn.Source.HostId),
                    targetIDtoPortNum conn.Source.Id, conn.Id)
                )
        )
        |> List.groupBy (fun (hostId, _, _) -> hostId)
        |> List.map (fun (k, v) ->
            let inputMap =
                v |> List.choose (
                        fun (_, inputNumber, (state, sourceNumber, connId)) ->
                        match inputNumber.IsSome, sourceNumber.IsSome with
                        | true, true -> Some (inputNumber.Value, (state, sourceNumber.Value, connId))
                        | _ -> None )
            k, Map.ofList inputMap)
        |> Map.ofList
    
    // TODO(jpnock): Fix uses of this
    let emptyPos = {AssertionTypes.Line = 1; AssertionTypes.Col = 1; AssertionTypes.Length = 1; }
    
    let assertionComps: VerificationComponents.ComponentConfig list = List.map snd assertionCompsAndIDs
    
    let componentToSheet =
        componentMap
        |> Map.map (fun id _ ->
            sheetComponentMap
            |> Map.findKey (fun _ compMap -> compMap.ContainsKey id))

    let assertionCompASTs : Result<AssertionTypes.Assertion, CodeError> list =
        assertionComps
        |> List.map (fun el ->
            // TODO(jpnock): Currently assuming assert HIGH
            let connectedToPort = componentIDToInputPortState.TryFind el.InstanceID.Value
            match connectedToPort with
            | None -> Error {
                Msg = "An assertion component was not driven by any inputs"
                Pos = emptyPos
                ExtraErrors = None }
            | Some connectedTo ->               
                let (assertionInput, _, _) = connectedTo[0]
                let ast = VerificationASTGen.generateAST componentIDToInputPortState 0 "" assertionInput
                let assertion = ast, emptyPos
                let componentId = 
                    match el.InstanceID with
                    | Some id -> id
                    | _ -> failwithf "What - assertion comps should have ids at this point"
                let assertionLabel = componentMap[ComponentId componentId].Label
                let assertionSheet = componentToSheet[ComponentId componentId]
                Ok {AssertExpr = assertion; InputNames = Set.empty; Name = Some assertionLabel; Id = Some componentId; Sheet = Some assertionSheet; Description = el.AssertionDescription})
    
    let isAssertionTextComp (comp:Component) =
        match comp.Type with
        | Plugin state -> state.AssertionText
        | _ -> None
    
    let assertionTexts = canvasComps |> List.choose isAssertionTextComp
    
    let assertionTextASTs =
        List.map AssertionParser.parseAssertion assertionTexts
    
    let allASTs = List.concat [assertionCompASTs; assertionTextASTs]
    
    let resultFolder state result =
        match result with 
        | Ok expr -> (List.append [expr] <| fst state, snd state)
        | Error e -> (fst state, List.append [e] <| snd state)

    let (validASTs, astErrors) = List.fold resultFolder ([], []) allASTs
    
    if not astErrors.IsEmpty then
        // TODO(jpnock): improve error output
        printf $"Not all assertions could be parsed:"
        List.iter ( fun e -> printf $"{e}") astErrors
   
    printf $"Got valid ASTs: {validASTs}"
    
    validASTs
    |> List.map (fun el ->
        let pretty = AssertionParser.prettyPrintAST (fst el.AssertExpr) "" false
        printf $"Got AST:\n{pretty}")
    |> ignore
    
    let checkedASTs =
        validASTs
        |> List.map (fun el -> AssertionCheck.checkAST el.AssertExpr canvasComps)
        
    let goodASTs =
        checkedASTs
        |> List.choose (
            function
            | AssertionTypes.TypeInfo typeInfo-> Some typeInfo 
            | _ -> None)
    
    let finalAssertions =
        if goodASTs.Length = checkedASTs.Length then validASTs
        else
            // TODO(jpnock): improve error output
            printf $"ERROR in ASTs assertionASTs {checkedASTs} {goodASTs}"
            []
    
    let canvasState = fullCanvasState
    
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
                        let fs = saveStateInSimulationFastSim canvasState diagramName loadedDependencies fs   
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


