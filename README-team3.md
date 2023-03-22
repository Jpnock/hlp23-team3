# Team 3 Verification Project

This project implements assertion logic, improving how designs can be verified inside Issie.



# Implemented functionality

The following features are implemented:

## Text based assertions 

* supports simple assertion language 
* deals with inputs in a similar way to Verilog component
* features code examples 
* performs compilation before save, so invalid assertions can't be saved 
* can have an optional description 
* display parsing and compilation errors in the text editor 

## Display and handling of failed assertions

* when an assertion fails, its name and (optional) user defined description are displayed in the simulator interface
* when an assertion fails it is highlighted in yellow on its sheet
* if the failing assertion is not on the current sheet a button can link to the sheet that contains it 
* if there are multiple failing assertions they are grouped in a dropdown menu and each of them can be selected to visualise their description and access the button linking to their sheet (if it's not the current one)

## Visual Assertions 

* feature drop down menu to configure similar kinds of blocks (such as comparators or logic blocks)
* support any kind of driver 
* are not allowed to drive a normal issie component 
* support different input widths, the "shorter" input will be sign/ zero (depending on signedness) extended to match the longer one 
* support different signedness for each port 
* support floating point operations
* can have an optional description
* if the compilation fails starting the simulation is not allowed and the failing blocks are highlighted



## Work breakdown 


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
