open ImgUtil
type point = int * int // A point (x,y) in the plane
type colour = int * int * int //(red, green, blue), 0...255
type figure =
  | Circle of point * int * colour // Defined by center, radius and colour
  | Rectangle of point * point * colour // Defined by corners bottom-left, top-right and colour
  | Mix of figure * figure // Combine figures with mixed colours at overlap

// Finds the colour of figure at point
//let rec colourAt (x,y) figure =
//  match figure with
  //| Circle ((cx,cy), r, col) ->
    //  if (x-cx)*(x-xc)+(y-cy)*(y-cy) <= r*r // Uses pythagoras to dertermine distance to center
      //then Some col else None
  //| Rectangle ((x0,y0), (x1,y1) col) ->
    //  if x0 <=x && x <= x1 && y0 <= y && y <= y1 // within corners
    //  then Some col else None
  //| Mic (f1,f2) ->
    //  match (colourAt (x,y) f1, colourAt (x,y) f2) with
    //  | (None, c) -> c // No overlap
    //  | (c, None) -> c // No overlap
    //  | (Some (r1,g1,b1), Some (r2,g2,b2)) -> // average colour
    //     Some ((r1+r2)/2, (g1+g2+)/2, (b1+b2)/2)



let pic = ImgUtil.mk 141 141
//let figO = Circle ((40,40), 45, (255,0,0))
//let figBox = Rectangle ((40,40),(90,110), (0,0,255))
let rec colourAt (x,y) figure =
  match figure with
  | Circle ((cx,cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r // Uses pythagoras to dertermine distance to center
      then Some col else None
  | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0 <=x && x <= x1 && y0 <= y && y <= y1 // within corners
      then Some col else None
  | Mix (f1,f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c // No overlap
      | (c, None) -> c // No overlap
      | (Some (r1,g1,b1), Some (r2,g2,b2)) -> // average colour
         Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)

//let box =
  //for x = 0 to 120 do
    //for y = 0 to 100 do
      //if x >= 40 && x <= 90 && y >= 40 && y <= 110 then
        //ImgUtil.setPixel(ImgUtil.fromRgb (0,0,255)) (x,y) pic

let circle = Circle((50,50), (45), (0,0,255))
//let rekt = Rectangle((40,40), (90,110), (255,0,0)
let b = (colourAt (50,50) circle)
//let round =
  //if colourAt( (50,50) (Circle(50,50) (45) (ImgUtil.fromRgb (0,0,255)))) = Some then
    //ImgUtil.setPixel(ImgUtil.fromRgb (0,0,255)) (x,y) pic
printfn "%A" b
let mix = Mix ((Rectangle((40,40), (90,110), (255,0,0))), (Circle((50,50), (45), (0,0,255))))
// Equation of the Circle: (x-x1)^2 + (y-y2)^2 = r^2
let trial = (colourAt (50,50) mix)
printfn "%A" (colourAt (50,50) mix)
let square = Rectangle((40,40), (90,110), (255,0,0))

let round bmp fig =
  for x = 0 to 140 do
    for y = 0 to 140 do
      match (colourAt (x,y) fig) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb (0,120,0)) (x,y) pic
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) pic

printfn "%A" (round pic mix)
//do ImgUtil.setLine (ImgUtil.fromRgb (0,0,255)) (40,40) (90,110) pic
//do ImgUtil.setLine (ImgUtil.fromRgb (0,0,255))  (90,110) (40,40) pic
do ImgUtil.toPngFile "8i-Test.png" pic
