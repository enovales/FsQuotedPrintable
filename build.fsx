#!/usr/bin/fsharp
// Simple build script using standard dotnet CLI commands
// This replaces the complex FAKE-based build system

open System
open System.IO

let execute command arguments =
    printfn "Executing: %s %s" command (String.concat " " arguments)
    let startInfo = Diagnostics.ProcessStartInfo()
    startInfo.FileName <- command
    startInfo.Arguments <- String.concat " " arguments
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    
    let process = Diagnostics.Process.Start(startInfo)
    process.WaitForExit()
    
    if process.ExitCode <> 0 then
        printfn "STDERR: %s" (process.StandardError.ReadToEnd())
        printfn "STDOUT: %s" (process.StandardOutput.ReadToEnd())
        failwithf "Command failed with exit code %d" process.ExitCode

[<EntryPoint>]
let main argv =
    let commandsAndArgs =
        if argv.Length = 0 then
            // Default: build and test
            [| [| "dotnet"; "restore" |]; [| "dotnet"; "build"; "FsQuotedPrintable.sln"; "--configuration"; "Release" |]; [| "dotnet"; "test"; "FsQuotedPrintable.sln"; "--configuration"; "Release"; "--no-build" |] |]
        else
            // Pass through to dotnet CLI
            [| argv |]
    
    for cmdArgs in commandsAndArgs do
        execute cmdArgs.[0] (Array.toList cmdArgs.[1..])
    
    0
