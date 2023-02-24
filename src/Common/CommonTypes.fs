
    /// Messages that will be sent from JS code.
    /// This is a define here as a hack to deal with the F# requirement of no forward references.
    /// This type must be defined before Draw2dwrapper, however Draw2dWrapper must be defined before ModelType
    /// Therefore we must define this here rather than where it should be, which is in ModelType
    /// The type parameters allow us to keep JSCanvas and JSComponent in JSTypes where they should be.
    /// whenever JSDiagramMsg is actually used: TCanvas = JSCanvas, TComponent = JSComponent
    type JSDiagramMsg<'TCanvas,'TComponent> =
        | InitCanvas of 'TCanvas // Has to be dispatched only once.
        | SelectComponent of 'TComponent
        | UnselectComponent of unit
        | InferWidths of unit
        | SetHasUnsavedChanges of bool

    type CCForm =
        |User
        |Library
        |ProtectedTopLevel
        |ProtectedSubSheet
        |Verilog of string

    type SheetInfo = {
        Form: CCForm option 
        Description: string option
    }