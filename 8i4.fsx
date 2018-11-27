open ImgUtil

let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk b h
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) figur).Value)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) bmp
  ImgUtil.toPngFile filnavn bmp

/// <param name "fig" "(z,c)"> 
/// "fig" takes form of the type Figure
/// (z, c) is a tupple, of which you want to move the figure (fig) 
/// </param name>
/// <summary>
/// The function takes a figure, of any of the 3 sorts that have been given, an is able to move the figure around, compared to where it originally would have been
/// </summary>
/// <returns>
/// The function returns another figure 
/// </returns>
let rec move (fig:figure) (z, c) : figure =
  match fig with
  | Circle ((cx,cy),r,col) ->
      Circle ((cx+z,cy+c),r,col)
  | Rectangle ((x0,y0), (x1,y1), col) ->
      Rectangle ((x0+z,y0+c), (x1+z,y1+c), col)
  | Mix (f1,f2) ->
      Mix(move (f1) (z,c),move(f2) (z,c))

let circle = Circle((45,45), (41), (0,0,255))
let rrectangle = Rectangle((36,36), (81,99), (255,0,0))
let figTest = Mix (circle, rrectangle) 
MakePicture "moveTest.png" (move figTest (-20,20)) 100 150
