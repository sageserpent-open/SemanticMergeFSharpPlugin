namespace StructureDiscovery

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open System.IO
open System

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
        { Type: string // Must be "file'.
          Name: string // Path to file.
          LocationSpan: LineSpan
          FooterSpan: CharacterSpan
          Children: List<Section>
          ParsingErrors: array<ParsingError> // TODO: be careful - the YAML specification would have this called 'ParsingError'.
                                             }
    
    let DiscoverStructure(pathOfInputFile: string, 
                          pathOfOutputFileForYamlResult: string) = 
        let contentsOfInputFile = File.ReadAllText(pathOfInputFile)
        let interactiveChecker = InteractiveChecker.Create()
        
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
            match resultsFromParsing.ParseTree with
            | Some(ParsedInput.ImplFile _) -> 
                (List.empty: List<Section>), parsingErrors
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
            { Type = "file"
              Name = pathOfInputFile
              LocationSpan = locationSpanForFile
              FooterSpan = emptyCharacterSpan
              Children = sections
              ParsingErrors = parsingErrors }
        
        let yamlForOverallStructure { Type = typeName; Name = name; 
                                      LocationSpan = locationSpan; 
                                      FooterSpan = footerSpan; 
                                      Children = children; 
                                      ParsingErrors = parsingErrors } = 
            let yamlForLineSpan ((startLine, indexOfFirstCharacter), 
                                 (endLine, indexOfOnePastLastCharacter)) = 
                String.Format
                    ("{{start: [{0},{1}], end: [{0},{1}]}}", startLine, 
                     indexOfFirstCharacter, endLine, indexOfOnePastLastCharacter)
            let yamlForEmptyCharacterSpan = "[0, -1]"
            let indent indentationLevel line = 
                String.replicate indentationLevel " " + line
            let joinPiecesOnSeparateLines (pieces: seq<string>) = 
                String.Join("\n", pieces)
            
            let rec yamlForSection section = 
                let rec yamlForContainer container = [ String.Empty ]
                let yamlForTerminal container = [ String.Empty ]
                String.Empty
            
            let yamlForParsingError { Location = line, indexOfCharacter; 
                                      Message = message } = 
                let pieces = 
                    [ yield String.Format
                                ("Location: [{0}],[{1}]", line, indexOfCharacter)
                      yield String.Format("Message: \"{0}\"", message) ]
                joinPiecesOnSeparateLines pieces
            
            let pieces = 
                [ yield "---"
                  yield String.Format("type : {0}", typeName)
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
                      yield! children |> List.map (yamlForSection >> indent 2)
                  if parsingErrorsDetected then 
                      yield "parsingError :"
                      yield! parsingErrors 
                             |> Array.map (yamlForParsingError >> indent 2) ]
            
            joinPiecesOnSeparateLines pieces
        
        ()
