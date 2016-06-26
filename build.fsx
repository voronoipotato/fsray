// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake.FscHelper
open Fake

let buildDir = "./bin/"

Target "Clean" (fun _ -> 
    CleanDir buildDir
)
// Default target
Target "Intro" (fun _ ->
    trace "Beginning compilation"
)

Target "Compile" (fun _ ->
    ["Program.fs"] 
    |> FscHelper.Compile [FscParam.Out "./bin/Fsray.exe"; FscParam.Target TargetType.Exe]
)

//Dependencies

"Clean"
    ==> "Intro"
    ==> "Compile"

// start build
RunTargetOrDefault "Compile"