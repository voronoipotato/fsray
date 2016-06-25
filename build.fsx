// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake

let buildDir = "./bin/"

Target "Clean" (fun _ -> 
    CleanDir buildDir
)


// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

//Dependencies
"Clean"
    ==> "Default"
// start build
RunTargetOrDefault "Default"