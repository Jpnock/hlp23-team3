# Team 3 Verification Project

This project implements assertion logic, improving how designs can be verified within Issie.

# Implemented functionality

## SINCE DEMO

* The ability to set whether assertions will be raised based on an inclusive from-cycle to an exclusive to-cycle has been implemented. 

## Text based assertions 

* Supports a simple assertion language 
* Deals with inputs in a similar way to Verilog component
* Features code examples for the language
* Performs code compilation before saving, so invalid assertions cannot be saved
* Performs simulation-time checking to make sure inputs are valid
* Allow a description to be set
* Displays parsing and compilation errors in the code editor 

## Display and handling of failed assertions

* When an assertion fails, its name and (optional) user defined description are displayed in the simulator interface
* When an assertion fails, it is highlighted in yellow on the sheet
* If the failing assertion is not on the current sheet, a button links to the sheet containing it
* If there are multiple failing assertions, they are grouped in a dropdown menu and each of them can be selected to reveal their description

## Visual Assertions 

* Dropdown menu to configure similar kinds of blocks (such as comparators or logic blocks)
* Can be driven by normal Issie components (not just Inputs/Constants)
* Are forbidden from driving normal Issie components (with user friendly errors)
* Supports different input widths, the "shorter" input will be sign/zero extended to match the longer one 
* Supports different interpretations of the input bits on each operator ()
* Supports IEEE-754 single-precision floats
* Allows for a user-defined description to be set
* If the compilation fails, starting the simulation is forbidden and the failing blocks are highlighted

## Component Interface Design

* All verification components implemented using a new component interface design
* Implementation is tied to individual records which implement member functions

## Changes made to Issie internals

* New Plugin type added to CommonTypes.ComponentType, to support the addition of the Plugin component interface system
* New Float32 type added to CommonTypes.NumberBase, to support viewing of floats in the step simulator
* Hooks added in the width inferer, such that width-inference for Plugins can be supported
* Many messages added in SymbolT and Sheet interfaces, to update the Plugin component configuration and handle assertion messages
* Updates to SheetUpdate to help display failed assertions
* Hooks to Symbol added to support the Plugin interface
* Deleted lots of unused files!
* Refactored parts of CanvasStateAnalyser to support rules about what verification components can drive
* Added IEEE-754 int to float and vice-versa helpers to NumberHelpers, along with support for formatting float32
* Fixed a bug in NumberHelpers, where numbers were formatted with commas but then rejected by strToInt
* Edited Simulator.fs to support evaluation of assertions
* Added Assertions to evaluate to the FastSimulator record (since our evaluator uses FastSim data)
* Many changes to CatalogueView to support reconfigurable components
* Refactored the Verilog component popup to be a generic editor, which is also used by the assertion text comp
* Changes to SimulationView and WaveSim to support displaying of failed assertions
* Additional helpers added to EEExtensions


## Work breakdown 

All commits can be found and reviewed [here](https://github.com/Jpnock/hlp23-team3/pull/24).


### James Nock (jpn119)

- Author format: `Authored by jpn119 (James Nock)`
- TODOs are marked with: `TODO(jpnock):`
- [VerificationASTGen.fs](src/Renderer/Verification/VerificationASTGen.fs)
- [AssertionASTMap.fs](src/Renderer/Verification/AssertionASTMap.fs)
- [VerificationComponents.fs](src/Renderer/Verification/VerificationComponents.fs)
- [VerificationLibrary.fs](src/Renderer/Verification/VerificationLibrary.fs)

### Joachim Sand (jls20)

- Author format: `Authored by jls20/jlsand (Joachim Sand)`
- TODOs are marked with: `TODO(jlsand):`
- [AssertionTypes.fs](src/Renderer/Verification/AssertionTypes.fs)
Authored 4 types: Token, TokenType, ParseResult and Precedence.
- [AssertionParser.fs](src/Renderer/AssertionParser.fs)
Full operator-precedence parser. A unique mix between a operator-precedence parser and a recursive descent parser.
- [CatalogueView.fs](src/Renderer/UI/CatalogueView.fs)
Made several modifications to clean up the code editor popup and generalize it sufficiently
to be used for the assertion expressions as well.
- [ModelType.fs](src/Renderer/UI/ModelType.fs)
Modified and added types to support the new generalized code editor.

### Lucia Necchi (ln220)

- Author format: `Authored by ln220`
- [AssertionCheck.fs](src/Renderer/Simulator/AssertionCheck.fs)
- [AsserionEvaluation.fs](src/Renderer/Simulator/AssertionEvaluation.fs)
- [AssertionTests.fs](src/Renderer/Simulator/AssertionTests.fs)
- [AssertionTypes.fs](src/Renderer/Verification/AssertionTypes.fs)

### Dominic Justice-Konec (djj120)

- Author format: `Authored by djj120`
- [AssertionEvaluation.fs](src/Renderer/Simulator/AssertionEvaluation.fs)
- [SimulationView.fs](src/Renderer/UI/SimulationView.fs)
- [WaveSim.fs](src/Renderer/UI/WaveSim/WaveSim.fs)
- [WaveSimStyle.fs](src/Renderer/UI/WaveSim/WaveSimStyle.fs)
- [WaveSimStyle.fs](src/Renderer/UI/WaveSim/WaveSimStyle.fs)
