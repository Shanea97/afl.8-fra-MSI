open ImgUtil

let rec boundingBox (fig:figure) : point*point =
  match fig with
  | Circle ((cx,cy), r, col) ->
      ((cx-r,cy+r),(cx+r,cy-r))
  | Rectangle ((x0,y0), (x1,y1), col) ->
      ((x0,y0), (x1,y1))
  | Mix (f1,f2) ->
     ((boundingBox(f1)), (boundingBox(f2)))


let circle = Circle((50,50), (45), (0,0,255))

printfn "%A" (boundingBox circle)