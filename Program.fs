open System.IO
//open FSharp.Collections.ParallelSeq


type Vec3(x: float, y:float, z:float) = 
    member this.x = x
    member this.y = y
    member this.z = z
    static member (~-) (v: Vec3) = Vec3(-1.0 *v.x,-1.0 *v.y,-1.0 *v.z )
    static member (*) (v: Vec3, k) = Vec3(v.x*k, v.y*k, v.z*k)
    static member (/) (v: Vec3, k) = Vec3(v.x/k, v.y/k, v.z/k)
    static member (*) (k, v:Vec3) = Vec3(v.x*k, v.y*k, v.z*k)
    static member (/) (k, v:Vec3) = Vec3(v.x/k, v.y/k, v.z/k)
    static member (+) (v1: Vec3, v2: Vec3) = Vec3(v1.x + v2.x, v1.y + v2.y , v1.z + v2.z)
    static member (-) (v1: Vec3, v2: Vec3) = Vec3(v1.x - v2.x, v1.y - v2.y , v1.z - v2.z)
    static member (*) (v1: Vec3, v2: Vec3) = Vec3(v1.x * v2.x, v1.y * v2.y , v1.z * v2.z)
    static member (/) (v1: Vec3, v2: Vec3) = Vec3(v1.x / v2.x, v1.y / v2.y , v1.z / v2.z)
    static member dot (v1: Vec3, v2: Vec3) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
    static member cross(v1: Vec3, v2: Vec3) = Vec3((v1.y * v2.z - v1.z * v2.y),
                                                   -(v1.x*v2.z - v1.z * v2.x),
                                                   (v1.x*v2.y - v1.y * v2.x))
    static member reflect(v1: Vec3, v2: Vec3) = v1 - 2.0 * Vec3.dot(v1,v2)*v2
    static member Unit(v:Vec3) =  v/v.Length()
    member this.Unit() = this/this.Length()
    member this.Length() = sqrt (this.SquaredLength())
    member this.SquaredLength() = this.x * this.x + this.y * this.y + this.z * this.z
    override this.ToString() = this.x.ToString() + " " + this.y.ToString() + " " + this.z.ToString()

type Color(r: float, g:float, b:float) =
    member this.r = r
    member this.g = g
    member this.b = b
    static member (*) (c: Color, k) = Color(c.r*k, c.g*k, c.b*k)
    static member (*) (k, c: Color) = Color(k*c.r, k*c.g, k*c.b)
    static member (*) (c1: Color, c2: Color) = Color(c1.r * c2.r, c1.g * c2.g , c1.b * c2.b)
    static member (/) (c: Color, k) = Color(c.r/k, c.g/k, c.b/k) 
    static member (/) (k, c: Color) = Color(k/c.r, k/c.g, k/c.b)   
    static member (+) (c1: Color, c2: Color) = Color(c1.r + c2.r, c1.g + c2.g , c1.b + c2.b)
    static member mono (k:float) = Color(k,k,k)
    static member white = Color.mono(1.0)
    static member black = Color.mono(0.0)
    static member Sqrt (c: Color) = Color(sqrt(c.r),sqrt(c.g),sqrt(c.b))
    override this.ToString() = 
        if (this.r > 1.0 || this.g > 1.0 || this.b > 1.0) then failwith "Colors can't be more than 100%"
        int(this.r* 255.99).ToString() + " " + int(this.g*255.99).ToString() + " " + int(this.b*255.99).ToString()

type HitRecord(h: bool, shotDistance: float, point: Vec3, normal: Vec3) =
    member this.h = h
    member this.shotDistance = shotDistance
    member this.point = point
    member this.normal = normal

type Ray(a: Vec3, b:Vec3) = 
   member this.a = a
   member this.b = b
   member this.origin = this.a
   member this.direction = this.b
   member this.point_at(t:float) = this.a + t*this.b

type IHitable = abstract member hit: Ray -> float -> float -> HitRecord

type Sphere(center:Vec3,r:float) =
    member this.r = r
    member this.center = center
    interface IHitable with
        member this.hit ray tmin tmax= 
            let oc = ray.origin - this.center
            let a = Vec3.dot(ray.direction, ray.direction)
            let b = Vec3.dot(oc, ray.direction)
            let c = Vec3.dot(oc,oc) - this.r*this.r
            let descriminant = b*b - a*c
            if descriminant > 0.0 then
                let temp = (-b - sqrt(b*b-a*c))/a
                let tempb = (-b + sqrt(b*b-a*c))/a
                if (tmax > temp && temp > tmin )then 
                    HitRecord(true,temp,ray.point_at(temp),ray.point_at(temp) - this.center / this.r)
                else if (tmax > tempb && tempb > tmin) then
                    HitRecord(true,temp,ray.point_at(tempb),ray.point_at(tempb) - this.center / this.r)
                else 
                    HitRecord(false,0.0,Vec3(0.0,0.0,0.0),Vec3(0.0,0.0,0.0))
            else
                HitRecord(false,0.0,Vec3(0.0,0.0,0.0),Vec3(0.0,0.0,0.0))


type Triangle(v0:Vec3,v1:Vec3,v2:Vec3) = 
    member this.v0 = v0
    member this.v1 = v1
    member this.v2 = v2
    member this.e0 = v1 - v0
    member this.e1 = v2 - v1  
    member this.e2 = v0 - v2
    member this.normal = Vec3.cross(this.e0, -this.e2)
    member this.planeHit (ray :Ray) = 
        let D = Vec3.dot(this.normal, v0)
        let t = (Vec3.dot(this.normal, ray.origin) + D) / Vec3.dot(this.normal, ray.direction)
        ray.origin + t * ray.direction
    interface IHitable with
        member this.hit ray tmin tmax = 

            let P = this.planeHit(ray)
            let dist = Vec3.dot(P,P) |> sqrt
            let paralell = false
            let behind = false
            let inside _ = 
                let c0 = P - this.v0
                let c1 = P - this.v1
                let c2 = P - this.v2
                Vec3.dot(this.normal, Vec3.cross(this.e0,c0)) > 0.0 &&
                Vec3.dot(this.normal, Vec3.cross(this.e1,c1)) > 0.0 &&
                Vec3.dot(this.normal, Vec3.cross(this.e2,c2)) > 0.0 
            if inside() then
                HitRecord(true,dist,P,this.normal)
            else
                HitRecord(false,0.0,Vec3(0.0,0.0,0.0),this.normal)


type IBounce<'T> = 
    abstract member bounce: HitRecord -> Color
    //abstract member bind:  IHitable -> 'T

type Metal<'T>(hitable:IHitable) = 
    member this.hitable = hitable
    member this.Return() = this.hitable
    interface IBounce<Metal<'T>>  with 
        member this.bounce(result:HitRecord) = Color.black
        //member this.bind(f) = Metal(this.hitable |> f)

  //  member this.thing = 
  //  interface IHitable with
  //      member this.hit (ray :Ray) (tmin :float) (tmax :float) = HitRecord(false,0.0,Vec3(0.0,0.0,0.0),Vec3(0.0,0.0,0.0))


let rand = System.Random()
let __ = ()
let toStr (x) = x.ToString()

//**************************
//* Settings               *
//**************************
[<Literal>]
let Width = 800 
[<Literal>]
let Height = 400
[<Literal>]
let MaxBounce = 3
[<Literal>]
let Antialias = 1
let UserName = "Alan"

let LowerLeftCorner = Vec3(-4.0, -2.0, -2.0)
let Horizontal = Vec3(8.0, 0.0, 0.0)
let Vertical = Vec3(0.0, 4.0, 0.0)
let Origin = Vec3(0.0, 0.0, 0.0)
let Tmin = 0.0
let Tmax = 100000.0

[<EntryPoint>]

let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let tv1 = Vec3(0.0,3.0,-1.0)
    let tv2 =  Vec3(2.0,0.0,0.0)
    let tv3 = Vec3(-1.0,0.0,-1.0)
    let hitables =
        [Metal(Sphere(Vec3(0.0,0.0,-1.0),0.5)) ]
        |> List.append([Metal(Sphere(Vec3(0.0,-100.5,-1.0),100.0))])
        |> List.append([Metal(Sphere(Vec3(1.0,0.0,-1.0),0.5))])
        //|> List.append([Metal(Sphere(Vec3(-1.0,0.0,-1.0),0.5))])
        |> List.append([Metal(Triangle(tv1, tv2,tv3))])
 
    let sb = new System.Text.StringBuilder()
    let outN l = sb.Append( l + "\n") |> ignore

    let header = 
        outN "P3" 
        outN (sprintf "%i %i" Width Height) 
        outN "255" 
    
    let rec randomInUnitSphere (p:obj) = 
        let q = Vec3(rand.NextDouble(), rand.NextDouble(), rand.NextDouble()) - Vec3(1.0,1.0,1.0)
        match p with
        | :? Vec3 as p when Vec3.dot(p,p) < 1.0 -> p
        | _ -> randomInUnitSphere q

    let sky (r:Ray) = 
        let unitDirection = r.direction.Unit()
        let t = 0.5 * (unitDirection.y + 1.0)
        (1.0-t)*Color.white + t*Color(0.5, 0.7, 1.0)

    let sunset (r:Ray) = 
        let unitDirection = r.direction.Unit()
        let t = 0.5 * (unitDirection.y + 1.0)
        (1.0-t)*Color(0.7,0.2,0.2) + t*Color(0.5, 0.7, 1.0)

    let rec fireRay (r:Ray) n :Color = 
        let results = 
            match n with
            | n when n < MaxBounce -> 
                hitables |> List.map(fun s -> (s , s.hitable.hit r Tmin Tmax) ) 
                |> List.filter(fun s -> snd(s).h ) 
                |> List.sortBy(fun s -> snd(s).shotDistance)
            | _ -> []

        match results with
        | head :: _ -> 
            let result = snd head
            let target = result.point + result.normal + randomInUnitSphere(__)
            let r2 = Ray(result.point, target - result.point)
            0.65 * fireRay r2 (n+1) 
        | _ -> sky r

    let camera i j =
        if i = 600 && j = 300  then printfn "Quarter of the way done!"
        if i = 400 && j = 200  then printfn "Halfway done!"
        if i = 200 && j = 100  then printfn "Three Quarters of the way done!"

        let c = [1..Antialias] |> List.fold(fun acc l -> 
            let u = (float(i) + rand.NextDouble()) / float(Width) //these variables have to be initialized here because they need to be generated each time
            let v = (float(j) + rand.NextDouble()) / float(Height)
            let r = Ray(Origin, LowerLeftCorner + u*Horizontal + v*Vertical)
            let p = r.point_at(2.0)
            fireRay r 0 + acc) Color.black
        c/float(Antialias) |> Color.Sqrt //average the sum of the passes
    
    header //adds the file header for PPM files
    
    let columns = [0..(Height-1)] |> List.rev
    let rows = [0..(Width-1)]
    columns |> List.iter(fun j ->
        (rows |> List.iter(fun i -> (
            camera i j |> toStr |> outN
        )))
    )
        
    System.IO.File.WriteAllText(sprintf @"C:\Users\%s\Desktop\testing.ppm" UserName, sb.ToString())
    stopWatch.Stop()
    let p = new System.Diagnostics.Process();
    p.StartInfo.FileName <- @"C:\Program Files (x86)\IrfanView\i_view32.exe"
    p.StartInfo.Arguments <- sprintf @"C:\Users\%s\Desktop\testing.ppm" UserName
    p.Start() |> ignore
    0 // return an integer exit code