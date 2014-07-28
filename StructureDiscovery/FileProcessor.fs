namespace StructureDiscovery

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open System.IO
open System
open System.Text.RegularExpressions

module FileProcessor = 
    type LineSpan = (int * int) * (int * int) // First item in sub-pair is a one-relative line position,
    
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
        { Location: int * int // First item is a one-relative line position,
          // the second is a zero-relative character position within the line.
          Message: string }
    
    type OverallStructure = 
        { Name: string // Path to file.
          LocationSpan: LineSpan
          FooterSpan: CharacterSpan
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
        
        //        let lineAndColumnIndicesFor characterPosition =
        //            let searchResult = Array.BinarySearch(onePastLineBreakPositionsInInputFile, characterPosition)
        //            let lineIndex =
        //                if 0 > searchResult
        //                then
        //                    ~~~searchResult - 1
        //                else
        //                    searchResult
        //            lineIndex, characterPosition - onePastLineBreakPositionsInInputFile.[lineIndex]
        let totalNumberOfLines = onePastLineBreakPositionsInInputFile.Length
        let characterSpanFor (range: range) = 
            let characterPositionFor oneRelativeLinePosition columnIndex = 
                let onePastLineBreakPositionInInputFile = 
                    onePastLineBreakPositionsInInputFile.[oneRelativeLinePosition - 1]
                onePastLineBreakPositionInInputFile + columnIndex
            characterPositionFor range.StartLine range.StartColumn, 
            characterPositionFor range.EndLine range.EndColumn - 1
        
        let locationSpanFor (range: range) = 
            (range.StartLine, range.StartColumn), 
            (range.EndLine, range.EndColumn)
        
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
                                                     error.StartLineAlternate, 
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
                    match moduleOrNameSpace with
                    | SynModuleOrNamespace(longIdentifierPieces, isModule, 
                                           containedDeclarations, _, _, _, overallRange) -> 
//                        let rangeOfContainedDeclarations = 
//                            (containedDeclarations
//                             |> List.map (fun declaration -> declaration.Range)
//                             |> List.reduce unionRanges)

                        let headerRange = mkRange String.Empty overallRange.Start (Seq.last longIdentifierPieces).idRange.End
                        
                        let container = 
                            { Type = 
                                  if isModule then "module"
                                  else "namespace"
                              Name = longIdentifierFrom longIdentifierPieces
                              LocationSpan = locationSpanFor overallRange
                              HeaderSpan = characterSpanFor headerRange
                              FooterSpan = emptyCharacterSpan
                              Children = List.Empty }
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
            | [] -> (0, 0), (0, 0)
            | _ -> 
                fst (List.head sections).LocationSpan, 
                snd (Seq.last sections).LocationSpan
        
        let overallStructure = 
            { Name = pathOfInputFile
              LocationSpan = locationSpanForFile
              FooterSpan = emptyCharacterSpan
              Children = sections
              ParsingErrors = parsingErrors }
        
        let yamlForOverallStructure { Name = name; LocationSpan = locationSpan; 
                                      FooterSpan = footerSpan; 
                                      Children = children; 
                                      ParsingErrors = parsingErrors } = 
            let yamlForLineSpan ((startLine, indexOfFirstCharacter), 
                                 (endLine, indexOfOnePastLastCharacter)) = 
                String.Format
                    ("{{start: [{0},{1}], end: [{2},{3}]}}", startLine, 
                     indexOfFirstCharacter, endLine, indexOfOnePastLastCharacter)
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
                                ("  footerSpan : {0}", yamlForEmptyCharacterSpan)
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
                                ("  span : {0}", yamlForEmptyCharacterSpan) ]
                
                match section with
                | Container container -> yamlForContainer container
                | Terminal terminal -> yamlForTerminal terminal
            
            let yamlForParsingError { Location = line, indexOfCharacter; 
                                      Message = message } = 
                [ yield String.Format
                            ("- location: [{0},{1}]", line, indexOfCharacter)
                  
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
        
        let yamlResult = yamlForOverallStructure overallStructure
        File.WriteAllText(pathOfOutputFileForYamlResult, yamlResult)
        File.WriteAllText
            (String.Format
                 (@"F:\{0}.txt", 
                  Path.GetFileNameWithoutExtension(pathOfInputFile)), yamlResult)
