namespace StructureDiscovery

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open System.IO
open System
open System.Text.RegularExpressions

module FileProcessor = 
    type LineSpan = pos * pos
    
    // the second is a zero-relative character position within the line.
    // The second sub-pair points one past the end of the construct,
    // so the interval is [). It may either point past the end on the
    // same line, or may point to the zeroeth character position on the
    // next line, this depends on whether the construct is contained
    // within one line or spans several.
    type CharacterSpan = int * int // [] interval using zero-relative character position.
    
    let emptyCharacterSpan = (0, -1)
    
    type Terminal = 
        { Type: string // Description of the programming language construct.
          Name: string // Identifier declared by the programming language construct.
          LocationSpan: LineSpan
          Span: CharacterSpan }
    
    type Section = 
        | Container of Container
        | Terminal of Terminal
        member this.LocationSpan = 
            match this with
            | Container { LocationSpan = locationSpan } -> locationSpan
            | Terminal { LocationSpan = locationSpan } -> locationSpan
    
    and Container = 
        { Type: string // Description of the programming language construct.
          Name: string // Identifier declared by the programming language construct.
          LocationSpan: LineSpan
          HeaderSpan: CharacterSpan
          FooterSpan: CharacterSpan
          Children: List<Section> }
    
    type ParsingError = 
        { Location: pos
          Message: string }
    
    type OverallStructure = 
        { Name: string // Path to file.
          LocationSpan: LineSpan
          Children: List<Section>
          ParsingErrors: array<ParsingError> }
    
    let private lineRegex = Regex(@"^.*$", RegexOptions.Multiline)
    
    let DiscoverStructure(pathOfInputFile: string, 
                          pathOfOutputFileForYamlResult: string) = 
        let contentsOfInputFile = File.ReadAllText(pathOfInputFile)
        let interactiveChecker = InteractiveChecker.Create()
        
        let onePastLineBreakPositionsInInputFile = 
            // HACK: I'm not aware of any way of prising open the 'System.IO.File.ReadAllLines()' abstraction
            // that would give a platform- / file line ending - independent way of seeing where the lines start
            // within the complete string of characters in the file, including any multi-character EOLs. So this
            // is my best attempt at doing something pithy. Hmm.
            let lineMatches = lineRegex.Matches contentsOfInputFile
            [| for lineMatch in lineMatches do
                   yield lineMatch.Index |]
        
        let numberOfLinesInInputFile = 
            onePastLineBreakPositionsInInputFile.Length
        let startLocation = mkPos 1 0
        let endLocation = 
            mkPos numberOfLinesInInputFile 
                (contentsOfInputFile.Length 
                 - onePastLineBreakPositionsInInputFile.[numberOfLinesInInputFile 
                                                         - 1])
        
        let characterPositionFrom (line, column) = 
            let onePastLineBreakPositionInInputFile = 
                onePastLineBreakPositionsInInputFile.[line - 1]
            onePastLineBreakPositionInInputFile + column
        
        let characterPositionFor (position: pos) = 
            characterPositionFrom (position.Line, position.Column)
        let locationSpanFor (range: range) = 
            mkPos range.StartLine range.StartColumn, 
            mkPos range.EndLine range.EndColumn
        let characterSpanFor ((startLocation, endLocation): LineSpan) = 
            characterPositionFor startLocation, 
            characterPositionFor endLocation - 1
        
        let textFor (range: range) = 
            let startCharacterPosition = characterPositionFor range.Start
            let onePastEndCharacterPosition = characterPositionFor range.End
            contentsOfInputFile.Substring
                (startCharacterPosition, 
                 onePastEndCharacterPosition - startCharacterPosition)
        
        let adjustLineAndColumnToPreceedingCharacterPosition (line, column) = 
            if 0 < column then line, column - 1
            else if 1 = line then 
                failwith 
                    "Precondition violation: there is no character position preceeding the start of the input file."
            else 
                let preceedingLine = line - 1
                let onePastPreceedingLineBreakPositionInInputFile = 
                    onePastLineBreakPositionsInInputFile.[preceedingLine - 1]
                
                let lengthOfPreceedingLine = 
                    if 1 = preceedingLine then 
                        onePastPreceedingLineBreakPositionInInputFile
                    else 
                        onePastPreceedingLineBreakPositionInInputFile 
                        - onePastLineBreakPositionsInInputFile.[preceedingLine 
                                                                - 2]
                preceedingLine, lengthOfPreceedingLine - 1
        
        let sections, parsingErrors = 
            let fakeProjectOptions = 
                interactiveChecker.GetProjectOptionsFromScript
                    (pathOfInputFile, contentsOfInputFile) 
                |> Async.RunSynchronously
            let resultsFromParsing, _ = 
                interactiveChecker.ParseAndCheckFileInProject
                    (pathOfInputFile, 0, contentsOfInputFile, fakeProjectOptions) 
                |> Async.RunSynchronously
            
            let parsingErrors = 
                resultsFromParsing.Errors |> Array.map (fun error -> 
                                                 let location = 
                                                     mkPos 
                                                         error.StartLineAlternate 
                                                         error.StartColumn
                                                 let message = error.Message
                                                 { Location = location
                                                   Message = message })
            
            let longIdentifierFrom (longIdentifierPieces: LongIdent) = 
                String.Join(".", longIdentifierPieces)
            match resultsFromParsing.ParseTree with
            | Some(ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, 
                                                            modulesOrNamespaces, 
                                                            _))) -> 
                let sectionFromModuleOrNamespace moduleOrNameSpace = 
                    let overallRange = 
                        (moduleOrNameSpace: SynModuleOrNamespace).Range
                    match moduleOrNameSpace with
                    | SynModuleOrNamespace(longIdentifierPieces, isModule, 
                                           containedDeclarations, _, _, _, _) -> 
                        let childrenFrom declaration = 
                            match declaration with
                            | SynModuleDecl.Let(_, bindings, _) -> 
                                let childFrom (Binding(_, _, _, _, _, _, _, 
                                                       patternOnLhsOfBinding, _, 
                                                       _, _, _) as binding: SynBinding) = 
                                    let name = 
                                        match patternOnLhsOfBinding with
                                        | SynPat.Named(_, name, _, _, _) -> 
                                            name.idText
                                        | SynPat.LongIdent(name, _, _, _, _, _) -> 
                                            textFor name.Range
                                        | _ -> textFor binding.RangeOfHeadPat
                                    
                                    let overallRange = 
                                        binding.RangeOfBindingAndRhs
                                    let locationSpan = 
                                        locationSpanFor overallRange
                                    
                                    let terminal = 
                                        { Type = "let"
                                          Name = name
                                          LocationSpan = locationSpan
                                          Span = characterSpanFor locationSpan }
                                    Terminal terminal
                                bindings |> List.map childFrom
                            | _ -> List.empty
                        
                        let children = 
                            containedDeclarations |> List.collect childrenFrom
                        let endOfTheModuleNamePosition = 
                            (Seq.last longIdentifierPieces).idRange.End
                        
                        let container = 
                            { Type = 
                                  if isModule then "module"
                                  else "namespace"
                              Name = longIdentifierFrom longIdentifierPieces
                              LocationSpan = locationSpanFor overallRange
                              HeaderSpan = 
                                  characterPositionFor overallRange.Start, 
                                  
                                  characterPositionFor 
                                      endOfTheModuleNamePosition - 1
                              FooterSpan = emptyCharacterSpan
                              Children = children }
                        Container container
                modulesOrNamespaces |> List.map sectionFromModuleOrNamespace, 
                parsingErrors
            | _ -> 
                raise 
                    (Exception
                         (String.Format
                              ("Cannot parse file: {0}.", pathOfInputFile)))
        
        let locationSpanForFile = 
            match sections with
            | [] -> pos0, pos0
            | _ -> 
                fst (List.head sections).LocationSpan, 
                snd (Seq.last sections).LocationSpan
        
        let overallStructure = 
            { Name = pathOfInputFile
              LocationSpan = locationSpanForFile
              Children = sections
              ParsingErrors = parsingErrors }
        
        let adjustSpansToCoverInputFile ({ LocationSpan = unadjustedStartLocation, 
                                                          unadjustedEndLocation; 
                                           Children = children } as overallStructure: OverallStructure) = 
            let adjustedLocationSpan = startLocation, endLocation
            let adjustStartOfSectionToAlignWith (section, alignmentPosition) = 
                section
            
            let adjustedChildren = 
                match children with
                | [] -> []
                | _ -> 
                    let childrenPairedOffWithTheirAlignments = 
                        [ yield children |> List.head, unadjustedStartLocation
                          
                          yield! children
                                 |> Seq.pairwise
                                 |> Seq.map 
                                        (function 
                                        | predecessor, successor -> 
                                            successor, 
                                            snd predecessor.LocationSpan) ]
                    childrenPairedOffWithTheirAlignments 
                    |> List.map adjustStartOfSectionToAlignWith
            
            let syntheticChildToAlignWithStartOfFile = 
                if posLt startLocation unadjustedStartLocation then 
                    let locationSpan = startLocation, unadjustedStartLocation
                    { Type = "fragment"
                      Name = "Beginning of file."
                      LocationSpan = locationSpan
                      Span = characterSpanFor locationSpan }
                    |> Terminal
                    |> Some
                else None
            
            let syntheticChildToAlignWithEndOfFile = 
                if posLt unadjustedEndLocation endLocation then 
                    let locationSpan = unadjustedEndLocation, endLocation
                    { Type = "fragment"
                      Name = "End of file."
                      LocationSpan = locationSpan
                      Span = characterSpanFor locationSpan }
                    |> Terminal
                    |> Some
                else None
            
            { overallStructure with LocationSpan = adjustedLocationSpan
                                    Children = 
                                        match syntheticChildToAlignWithStartOfFile, 
                                              syntheticChildToAlignWithEndOfFile with
                                        | Some syntheticChildToAlignWithStartOfFile, 
                                          Some syntheticChildToAlignWithEndOfFile -> 
                                            [ yield syntheticChildToAlignWithStartOfFile
                                              yield! adjustedChildren
                                              
                                              yield syntheticChildToAlignWithEndOfFile ]
                                        | Some syntheticChildToAlignWithStartOfFile, 
                                          None -> 
                                            [ yield syntheticChildToAlignWithStartOfFile
                                              yield! adjustedChildren ]
                                        | None, 
                                          Some syntheticChildToAlignWithEndOfFile -> 
                                            [ yield! adjustedChildren
                                              
                                              yield syntheticChildToAlignWithEndOfFile ]
                                        | None, None -> adjustedChildren }
        
        let yamlForOverallStructure { Name = name; LocationSpan = locationSpan; 
                                      Children = children; 
                                      ParsingErrors = parsingErrors } = 
            let yamlForLineSpan (start: pos, onePastEnd: pos) = 
                String.Format
                    ("{{start: [{0},{1}], end: [{2},{3}]}}", start.Line, 
                     start.Column, onePastEnd.Line, onePastEnd.Column)
            let yamlForCharacterSpan (indexOfFirstCharacter, 
                                      indexOfLastCharacter) = 
                String.Format
                    ("[{0}, {1}]", indexOfFirstCharacter, indexOfLastCharacter)
            let yamlForEmptyCharacterSpan = "[0, -1]"
            let indent indentationLevel line = 
                String.replicate indentationLevel " " + line
            let indentPieces = Seq.map (indent 2)
            let joinPiecesOnSeparateLines (pieces: seq<string>) = 
                String.Join("\n", pieces)
            let rec yamlForSubpieces yamlForSubpiece pieces = 
                pieces |> Seq.collect (yamlForSubpiece >> indentPieces)
            
            and yamlForSection section = 
                let yamlForContainer { Type = typeName; Name = name; 
                                       LocationSpan = locationSpan; 
                                       HeaderSpan = headerSpan; 
                                       FooterSpan = footerSpan; 
                                       Children = children } = 
                    [ yield String.Format("- type : {0}", typeName)
                      yield String.Format("  name : {0}", name)
                      
                      yield String.Format
                                ("  locationSpan : {0}", 
                                 yamlForLineSpan locationSpan)
                      
                      yield String.Format
                                ("  headerSpan : {0}", 
                                 yamlForCharacterSpan headerSpan)
                      
                      yield String.Format
                                ("  footerSpan : {0}", 
                                 yamlForCharacterSpan footerSpan)
                      if not children.IsEmpty then 
                          yield "  children :"
                          yield! children |> yamlForSubpieces yamlForSection ]
                
                let yamlForTerminal { Type = typeName; Name = name; 
                                      LocationSpan = locationSpan; Span = span } = 
                    [ yield String.Format("- type : {0}", typeName)
                      yield String.Format("  name : {0}", name)
                      
                      yield String.Format
                                ("  locationSpan : {0}", 
                                 yamlForLineSpan locationSpan)
                      
                      yield String.Format
                                ("  span : {0}", yamlForCharacterSpan span) ]
                
                match section with
                | Container container -> yamlForContainer container
                | Terminal terminal -> yamlForTerminal terminal
            
            let yamlForParsingError { Location = location; Message = message } = 
                [ yield String.Format
                            ("- location: [{0},{1}]", location.Line, 
                             location.Column)
                  
                  yield String.Format
                            ("  message: \"{0}\"", message.Replace("\"", "\\\"")) ]
            
            let pieces = 
                [ yield "---"
                  yield "type : file"
                  yield String.Format("name : {0}", pathOfInputFile)
                  
                  yield String.Format
                            ("locationSpan : {0}", yamlForLineSpan locationSpan)
                  
                  yield String.Format
                            ("footerSpan : {0}", yamlForEmptyCharacterSpan)
                  
                  let parsingErrorsDetected = 
                      not (parsingErrors |> Array.isEmpty)
                  yield String.Format
                            ("parsingErrorsDetected : {0}", 
                             parsingErrorsDetected)
                  if not children.IsEmpty then 
                      yield "children :"
                      yield! children |> yamlForSubpieces yamlForSection
                  if parsingErrorsDetected then 
                      yield "parsingErrors :"
                      yield! parsingErrors 
                             |> yamlForSubpieces yamlForParsingError ]
            
            joinPiecesOnSeparateLines pieces
        
        let yamlResult = 
            overallStructure
            |> adjustSpansToCoverInputFile
            |> yamlForOverallStructure
        
        File.WriteAllText(pathOfOutputFileForYamlResult, yamlResult)
        File.WriteAllText
            (String.Format
                 (@"F:\{0}.txt", 
                  Path.GetFileNameWithoutExtension(pathOfInputFile)), yamlResult)
