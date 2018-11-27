type point = int * int // A point (x,y) in the plane
type colour = int * int * int //(red, green, blue), 0...255
type figure =
  | Circle of point * int * colour // Defined by center, radius and colour
  | Rectangle of point * point * colour // Defined by corners bottom-left, top-right and colour
  | Mix of figure * figure // Combine figures with mixed colours at overlap

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

let circle = Circle((125,125), (125), (0,0,255))

let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk h b
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb (255,120,0)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) bmp
  do ImgUtil.toPngFile filnavn bmp

printfn "%A" (MakePicture "8i1 billede.png" circle 250 250)
