open ImgUtil

let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk b h
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) figur).Value)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (125,125,125)) (x,y) bmp
  do ImgUtil.toPngFile filnavn bmp

// let circle = Circle((50,50), (45), (0,0,255))
// let rectangle = Rectangle((5,95), (95,5), (255,0,0))
// let square = Rectangle((5,5),(95,95), (255,0,0))
// let mix = Mix (circle, square)

let rec boundingBox (fig:figure) : point*point =
  match fig with
  | Circle ((cx,cy), r, col) ->
      ((cx-r,cy-r),(cx+r,cy+r))
  | Rectangle ((x0,y0), (x1,y1), col) ->
      ((x0,y0), (x1,y1))
  | Mix (f1,f2) ->
     ((fst(boundingBox(f1)),(snd(boundingBox(f2)))))

let square = Rectangle((40,40), (90,110), (255,0,0))

let circle = Circle((50,50), (45), (0,0,255))
let mix = Mix ((circle,square))

MakePicture "test af opgave 5.png" square 110 110

printfn "Mix:%A" (boundingBox mix)
printfn "Square:%A" (boundingBox square)
printfn "Circle:%A" (boundingBox circle)

let sq = ((40,40),(90,110))
let circ = ((5,5),(95,95))
let mixx = 
    if (fst(fst sq) || snd(fst sq)) > (fst(fst circ) || snd(fst circ)) && (fst(snd sq) || snd(snd sq)) < (fst(snd circ) || snd(snd circ)) then 
        ((fst(fst circ), snd(fst circ)), (fst(snd circ), snd(snd circ))
    else 
        
)