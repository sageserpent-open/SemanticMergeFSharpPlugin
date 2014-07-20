
#I @"C:\PlasticSCMWorkspaces\SemanticMergeFSharpPlugin\packages\FSharp.Compiler.Service.0.0.54\lib\net40"


#r "FSharp.Compiler.Service"


#load "C:\PlasticSCMWorkspaces\SemanticMergeFSharpPlugin\StructureDiscovery\FileProcessor.fs"


open StructureDiscovery

FileProcessor.DiscoverStructure (@"C:\PlasticSCMWorkspaces\NTestCaseBuilder\development\solution\SageSerpent.Infrastructure\BargainBasement.fs", @"F:\foo.txt")

open System.IO

File.ReadAllText(@"F:\Foo.txt")
