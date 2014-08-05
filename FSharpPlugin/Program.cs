using System;
using System.IO;
using StructureDiscovery;

namespace FSharpPlugin
{
    internal class Program
    {
        private static void Main(string[] arguments)
        {
            var numberOfArguments = arguments.Length;

            const int numberOfArgumentsExpected = 2;

            if (numberOfArgumentsExpected != numberOfArguments)
            {
                throw new Exception(
                    string.Format("ERROR: expected {0} arguments, got {1}.",
                        numberOfArgumentsExpected, numberOfArguments));
            }

            var mode = arguments[0];

            const string theOnlyModeHandled = "shell";

            if (
                !string.Equals(theOnlyModeHandled, mode,
                    StringComparison.OrdinalIgnoreCase))
            {
                throw new Exception(
                    string.Format("ERROR, expect mode: {0}, requested mode was: {1}.",
                        theOnlyModeHandled, mode));
            }

            var acknowledgementFilePath = arguments[1];

            File.WriteAllText(acknowledgementFilePath, "READY");


            do
            {
                var pathOfFileToBeProcessed = Console.ReadLine();

                if (null == pathOfFileToBeProcessed)
                {
                    SignalFailedRequestToProcessFile();
                    return;
                }

                if ("end".Equals(pathOfFileToBeProcessed, StringComparison.OrdinalIgnoreCase))
                    return;

                var pathOfFileForYamlResult = Console.ReadLine();

                if (null == pathOfFileForYamlResult)
                {
                    SignalFailedRequestToProcessFile();
                    return;
                }

                try
                {
                    FileProcessor.DiscoverStructure(pathOfFileToBeProcessed, pathOfFileForYamlResult);
                    SignalSuccessfulRequestToProcessFile();
                }
                catch (Exception)
                {
                    SignalFailedRequestToProcessFile();
                }
                
            } while (true);
        }

        private static void SignalSuccessfulRequestToProcessFile()
        {
            Console.WriteLine("OK");
        }

        private static void SignalFailedRequestToProcessFile()
        {
            Console.WriteLine("KO");
        }
    }
}