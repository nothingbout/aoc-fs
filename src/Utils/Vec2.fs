namespace Utils
open Globals

[<Struct>]
type Vec2<'T> = {X : 'T; Y : 'T}
    with 
        static member inline get_Zero () = { X = makeZero (); Y = makeZero () }
        static member inline ( ~- ) (a) = {X = -a.X; Y = -a.Y}
        static member inline ( + ) (a, b) = {X = a.X + b.X; Y = a.Y + b.Y}
        static member inline ( - ) (a, b) = {X = a.X - b.X; Y = a.Y - b.Y}
        static member inline ( * ) (a, s) = {X = a.X * s; Y = a.Y * s}
        static member inline ( * ) (s, a) = {X = a.X * s; Y = a.Y * s}
        static member inline ( / ) (a, s) = {X = a.X / s; Y = a.Y / s}

module Vec2 = 
    let inline make x y = {X = x; Y = y}
    let inline makeZero () = make (makeZero ()) (makeZero ())
    let inline fromTuple (x, y) = make x y
    let inline map f a = make (f a.X) (f a.Y)
    let inline min a b = make (min a.X b.X) (min a.Y b.Y)
    let inline max a b = make (max a.X b.X) (max a.Y b.Y)
    let inline minElem a = min a.X a.Y
    let inline maxElem a = max a.X a.Y
    let inline sign a = make (sign a.X) (sign a.Y)
    let inline abs a = make (abs a.X) (abs a.Y)
    let inline sum a = a.X + a.Y
    let inline mul a b = make (a.X * b.X) (a.Y * b.Y)
    let inline div a b = make (a.X / b.X) (a.Y / b.Y)
    let inline dot a b = a.X * b.X + a.Y * b.Y

    let inline turnCCW a = make -a.Y a.X
    let inline turnCW a = make a.Y -a.X
    let inline fromDegrees d = make (System.Math.Cos(d), System.Math.Sin(d))
    let inline degrees a = System.Math.Atan2(a.Y, a.X)
    let inline degreesDeltaPositive src dst = (dst - src) %+ 360.0
    let inline degreesDeltaNegative src dst = degreesDeltaPositive src dst - 360.0
    let inline degreesDelta src dst = degreesDeltaPositive src dst |> fun d -> if d <= 180.0 then d else d - 360.0

[<Struct>]
type Rect<'T> = { XR : Range<'T>; YR : Range<'T> }

module Rect = 
    let inline make start finish = { XR = Range.make start.X finish.X; YR = Range.make start.Y finish.Y }
    let inline start r = Vec2.make (Range.start r.XR) (Range.start r.YR)
    let inline finish r = Vec2.make (Range.finish r.XR) (Range.finish r.YR)

    let inline contains p r = Range.contains r.XR p.X && Range.contains r.YR p.Y

    let inline intersect a b = 
        maybe {
            let! xr = Range.intersect a.XR b.XR
            let! yr = Range.intersect a.YR b.YR
            return { XR = xr; YR = yr }
        }

    let encapsulating points = make (points |> List.foldHead Vec2.min) (points |> List.foldHead Vec2.max)

    let inline expand amount r = make (start r - amount) (finish r + amount)

module IntRect =
    let inline withSize start size = { XR = IntRange.withSize start.X size.X; YR = IntRange.withSize start.Y size.Y }

    let pointsSeq r = Seq.allPairs (IntRange.toSeq r.XR) (IntRange.toSeq r.YR) |> Seq.map Vec2.fromTuple

// module TestVec2Range =
//     let testInt = 
//         let range = Range.make (Vec2.make 0 0) (Vec2.make 10 10)
//         let range2 = Range.make (Vec2.make 5 5) (Vec2.make 15 15)
//         let range3 = Range.intersect range range2
//         ()
