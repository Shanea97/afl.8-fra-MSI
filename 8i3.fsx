open ImgUtil
type point = int * int // A point (x,y) in the plane
type colour = int * int * int //(red, green, blue), 0...255
type figure =
  | Circle of point * int * colour // Defined by center, radius and colour
  | Rectangle of point * point * colour // Defined by corners bottom-left, top-right and colour
  | Mix of figure * figure // Combine figures with mixed colours at overlap

///let CheckColour (c:colour) : bool =
let a = (1,2,4)
let b =  third a
printfn "%A" b
let rec CheckFigure (f:figure): bool =
  match f with
  | Circle ((cx,cy),r,col) ->
      r > 0
  | Rectangle ((x0,y0), (x1,y1), col) ->
      x0 > x1 && y0 > y1
  | Mix (f1,f2) ->
      CheckFigure(f1) && CheckFigure(f2)


let circle = Circle((45,45), (41), (0,0,255))
let rrectangle = Rectangle((81,99), (36,36), (255,0,0))
let mix = Mix (circle, rrectangle)
printfn "Circle: %b" (CheckFigure (Circle((125,125), (125), (0,0,255))))
printfn "Rectangle: %b" (CheckFigure (Rectangle((81,99), (36,36), (255,0,0))))
printfn "Mix: %b" (CheckFigure (mix))
