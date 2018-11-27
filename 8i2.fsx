open ImgUtil

let circle = Circle((45,45), (41), (0,0,255))
let rrectangle = Rectangle((36,36), (81,99), (255,0,0))
let mix = Mix (circle, rrectangle)

let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk b h
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) mix).Value)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) bmp
  ImgUtil.toPngFile filnavn bmp

MakePicture "figTest.png" mix 100 150
