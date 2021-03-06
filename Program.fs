﻿module FSray
open System.IO
open System.Drawing

//open FSharp.Collections.ParallelSeq
[<AutoOpen>]
module DTypes =
    type V3 = 
        {x:float; y:float; z:float}
        static member FMap2 (f :float->float->float) (v, k) = {x=f v.x k; y=f v.y k; z=f v.z k }
        static member FMapV2 (f :float->float->float) (v1, v2) = {x=f v1.x v2.x; y=f v1.y v2.y; z=f v1.z v2.z;}
        static member (~-) (v) = {x=(-1.0 *v.x);y=(-1.0 *v.y);z=(-1.0 *v.z)}    
        static member (*) (v, k) = V3.FMap2 (*) (v, k)
        static member (/) (v, k) = V3.FMap2 (/) (v, k)
        static member (+) (v, k) = V3.FMap2 (+) (v, k) 
        static member (-) (v, k) = V3.FMap2 (-) (v, k)                        
        static member (*) (k, v) :V3= v * k
        static member (+) (k:float, v:V3) :V3= v + k
        static member (+) (v1, v2) = V3.FMapV2 (+) (v1,v2)
        static member (-) (v1, v2) = V3.FMapV2 (-) (v1,v2)
        static member (*) (v1, v2) = V3.FMapV2 (*) (v1,v2)
        static member (/) (v1, v2) = V3.FMapV2 (/) (v1,v2)
        override v.ToString() = sprintf "%f %f %f" v.x v.y v.z
    type Color =
        {r:float; g:float; b:float}
        static member FMap2 (f: float -> float -> float) (c, k) = {r=f c.r k; g = f c.g k; b = f c.b k}
        static member FMap2r (f: float -> float -> float) (c, k) = {r=f k c.r; g = f k c.g ; b = f k c.b}
        static member FMap (f: float -> float) c1 = {r=f c1.r; g = f c1.g; b = f c1.b}
        static member FMapC2 (f: float -> float -> float) (c1, c2) = {r=f c1.r c2.r; g = f c1.g c2.g; b = f c1.b c2.b}
        static member (*) (c, k) = Color.FMap2 (*) (c, k)
        static member (*) (k, c) =  c*k
        static member (*) (c1, c2) = Color.FMapC2 (*) (c1,c2)
        static member (/) (c, k) = Color.FMap2 (/) (c, k)
        static member (/) (k, c) = Color.FMap2r (/) (c, k)
        static member (+) (c1, c2) = Color.FMapC2 (+) (c1,c2)
        override this.ToString() = 
            if (this.r > 1.0 || this.g > 1.0 || this.b > 1.0) then failwith "Colors can't be more than 100%"
            int(this.r* 255.99).ToString() + " " + int(this.g*255.99).ToString() + " " + int(this.b*255.99).ToString()
    type HitRecord = {wasHit: bool; shotDistance: float; point: V3; normal: V3}
    type Ray= 
        {origin:V3; direction:V3}
        member this.PointAt(t:float) = this.origin + t*this.direction
    type Sphere = {center:V3; r:float}

    type Triangle = 
        {v0:V3;v1:V3;v2:V3}
        member this.e0 = this.v1 - this.v0
        member this.e1 = this.v2 - this.v1  
        member this.e2 = this.v0 - this.v2

module V3 =
    let squaredLength v = v.x * v.x + v.y * v.y + v.z * v.z
    let length v =  squaredLength v |> sqrt
    let dot (v1: V3, v2: V3) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
    let cross(v1: V3, v2: V3) = {x=(v1.y * v2.z - v1.z * v2.y); y= -(v1.x*v2.z - v1.z * v2.x); z=(v1.x*v2.y - v1.y * v2.x)}
    let reflect(v1: V3, v2: V3) = v1 - 2.0 * dot(v1,v2)*v2
    let unit(v:V3) =  v/ length v
    let create a b c = {x=a; y=b; z=c}
    let origin = {x=0.;y=0.;z=0.}
module Color = 
    let mono k = {r=k;g=k;b=k}
    let white = mono(1.0)
    let black = mono(0.)
    let DColorFromRgb r g b = System.Drawing.Color.FromArgb(255,r,g,b)
    let toDcolor (c: Color) = DColorFromRgb (int(c.r * 255.99)) (int(c.g*255.99)) (int(c.b*255.99))

module Sphere = 
    let hit s ray tmin tmax= 
        let rdir = ray.direction
        let oc = ray.origin - s.center
        let a = V3.dot(rdir, rdir)
        let b = V3.dot(oc, rdir)
        let c = V3.dot(oc,oc) - s.r*s.r
        let descriminant = b*b - a*c
        if descriminant > 0.0 then
            let temp = (-b - sqrt(descriminant))/a
            let tempb = (-b + sqrt(descriminant))/a
            if (tmax > temp && temp > tmin )then 
                {
                wasHit=true;
                shotDistance=temp;
                point=ray.PointAt(temp);
                normal=ray.PointAt(temp) - s.center / s.r
                }
            else if (tmax > tempb && tempb > tmin) then
                {
                wasHit=true;
                shotDistance=temp;
                point=ray.PointAt(tempb);
                normal=ray.PointAt(tempb) - s.center / s.r
                }
            else 
                {wasHit=false;shotDistance=0.0;point=V3.origin;normal=V3.origin}
        else
            {wasHit=false;shotDistance=0.0;point=V3.origin;normal=V3.origin}
module Triangle = 
    let normal (t :Triangle) = V3.cross(t.e0, -t.e2)
    let hit t ray tmin tmax = 
        let n = normal t
        let planeHit (t: Triangle) (ray :Ray) = 
            let D = V3.dot(n, t.v0)
            let t = (V3.dot(n, ray.origin) + D) / V3.dot(n, ray.direction)
            ray.origin + t * ray.direction
        let P = planeHit t ray
        let dist = V3.dot(P,P) |> sqrt
        let paralell = false
        let behind = false
        let inside _ = 
            let c0 = P - t.v0
            let c1 = P - t.v1
            let c2 = P - t.v2
            V3.dot(n, V3.cross(t.e0,c0)) > 0.0 &&
            V3.dot(n, V3.cross(t.e1,c1)) > 0.0 &&
            V3.dot(n, V3.cross(t.e2,c2)) > 0.0 
        if inside() then
            {wasHit=true;shotDistance=dist;point=P;normal=n}
        else
            {wasHit=false;shotDistance=0.0;point=V3.origin;normal=n}
module Camera = 
    [<Literal>]
    let Width = 800 
    [<Literal>]
    let Height = 400
    let bitmap = new Bitmap(Width, Height)
    let setPixel i j c = bitmap.SetPixel(i,j,c)

    let save () = bitmap.Save(Path.Combine(__SOURCE_DIRECTORY__, "bitmap.png"))
type Hitable = Triangle of Triangle | Sphere of Sphere
let rand = System.Random()
let toStr (x) = x.ToString()

//**************************
//* Settings               *
//**************************

[<Literal>]
let MaxBounce = 7
[<Literal>]
let Antialias = 4
let LowerLeftCorner = V3.create -4. -2. -2.
let Horizontal = V3.create 8. 0. 0.
let Vertical = V3.create 0. 4. 0.
let Tmin = 0.0
let Tmax = 100000.0
[<EntryPoint>]
let main argv = 
    //let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let tv1 = V3.create 0.0 3.0 -1.0
    let tv2 = V3.create 2.0 0.0 0.0
    let tv3 = V3.create -1.0 0.0 -1.0 
    let tri1 = Triangle {v0=tv1; v1=tv2; v2=tv3}
    let s1 = Sphere {center={V3.origin with z = -1.}; r= 0.5}
    let s2 = Sphere {center={V3.origin with y=(-100.5); z = (-1.)}; r=100.}
    let s3 = Sphere {center={V3.origin with x=1.; z = (-1.)}; r=0.5}
    let hit = function
        | Sphere s -> Sphere.hit s 
        | Triangle t-> Triangle.hit t
    let hitables = [tri1;s1;s2;s3] 
    let sb = System.Text.StringBuilder()
    let outN l = sb.Append( l + "\n") |> ignore
    // let header = 
    //     outN "P3" 
    //     outN (sprintf "%i %i" Camera.Width Camera.Height) 
    //     outN "255" 
    let rec randomInUnitSphere (p :obj) = 
        let q = {x=rand.NextDouble(); y=rand.NextDouble(); z=rand.NextDouble()} - 1. 
        match p with
        | :? V3 as p when V3.dot(p,p) < 1.0 -> p
        | _ -> randomInUnitSphere q
    let skybox (c1:Color) (c2:Color) (r :Ray) = 
        let unitDirection = r.direction |> V3.unit
        let t = 0.5 * (unitDirection.y + 1.0)
        c1*(1.0-t)+ c2*t
    let sky = skybox Color.white {r=0.5;g=0.7;b=1.0}
    let sunset = skybox {r=0.7;g=0.2;b=0.2} {r=0.5;g=0.7;b=1.0}
    let rec fireRay (r:Ray) n :Color = 
        let results = 
            match n with
            | n when n < MaxBounce -> 
                hitables |> List.map(fun s -> (s, hit s r Tmin Tmax) ) 
                |> List.filter(fun (_,(s:HitRecord)) -> s.wasHit ) 
                |> List.sortBy(fun (_,(s:HitRecord)) -> s.shotDistance)
            | _ -> []
        match results with
        | head :: _ -> 
            let result = snd head
            let target = result.point + result.normal + randomInUnitSphere ()
            let r2 = {origin = result.point; direction= target - result.point}
            0.65 * fireRay r2 (n+1) 
        | _ -> sky r
    let loading i j = 
        if i = 600 && j = 300  then printfn "Quarter of the way done!"
        if i = 400 && j = 200  then printfn "Halfway done!"
        if i = 200 && j = 100  then printfn "Three Quarters of the way done!"
    let camera i j =
        loading i j |> ignore
        let c = [1..Antialias] |> List.fold(fun acc _ -> 
            let u = (float(i) + rand.NextDouble()) / float(Camera.Width) //these variables have to be initialized here because they need to be generated each time
            let v = (float(j) + rand.NextDouble()) / float(Camera.Height)
            let r = {origin=V3.origin; direction=LowerLeftCorner + u*Horizontal + v*Vertical}
            fireRay r 0 + acc) Color.black
        c/float(Antialias) |> Color.FMap sqrt //average the sum of the passes
    // header //adds the file header for PPM files
    let columns = [0..(Camera.Height-1)] |> List.rev
    let rows = [0..(Camera.Width-1)]
    columns |> List.iter(fun j ->
        (rows |> List.iter(fun i -> (
            camera i j 
                |> Color.toDcolor 
                |> Camera.setPixel i j
        )))
    )
    Camera.save ()
    0 // return an integer exit code