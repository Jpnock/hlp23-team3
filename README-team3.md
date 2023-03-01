# Team 3 Verification Project

This project implements assertion logic, improving how designs can be verified inside Issie.

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
