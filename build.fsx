// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake.FscHelper
open Fake
//let buildDir = "./bin/"

// Target "Clean" (fun _ -> 
//     CleanDir buildDir
// )
// Default target
Target "Default" (fun _ ->
    trace "Beginning compilation"
)

Target "Fsray.exe" (fun _ ->
    ["Program.fs"] 
    |> FscHelper.Compile [FscParam.Target TargetType.Exe]
)

//Dependencies


// start build
RunTargetOrDefault "Fsray.exe"