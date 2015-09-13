open System.Drawing

type Rect = 
    { Left : float32
      Top : float32
      Width : float32
      Height : float32 }

let toRectangleF(original) =
    RectangleF(original.Left, original.Top, original.Width, original.Height)

let deflate original wspace hspace = 
    { Left = original.Left + wspace
      Top = original.Top + hspace
      Width = original.Width - (2.0f * wspace)
      Height = original.Height - (2.0f * hspace)}

type TextContent =
    { Text : string
      Font : Font }

type ScreenElement =
    | TextElement of TextContent * Rect
    | ImageElement of string * Rect

let fontText = new Font("Calibri", 12.0f)
let fontHead = new Font("Calibri", 15.0f)

let elements =
    [ TextElement
        ({ Text = "functonal programming for real world"
           Font = fontHead},
         { Left = 10.0f; Top = 0.0f; Width = 410.0f; Height = 30.0f });
      ImageElement
        (@"C:\Users\Keisuke\Documents\Visual Studio 2015\Projects\Tutorial1\Tutorial1\cover.png",
         { Left = 120.0f; Top = 30.0f; Width = 150.0f; Height = 200.0f });
      TextElement
        ({ Text = "functonal programming for real world" 
           + "dafdal fkfaflfafj fafla fafllllllllllll   dal"
           + "dafdal fkfaflfafj fafla fafllllllllllll   dal"
           + "dafdal fkfaflfafj fafla fafllllllllllll   dal"
           Font = fontText},
         { Left = 10.0f; Top = 230.0f; Width = 400.0f; Height = 400.0f }) ]

let drawElements elements (gr : Graphics) =
    for p in elements do
        match p with 
        | TextElement(text, boudiong) ->
            let boxf = toRectangleF(boudiong)
            gr.DrawString(text.Text, text.Font, Brushes.Black, boxf)
        | ImageElement(imagePath, bouding) ->
            let bmp = new Bitmap(imagePath)
            let wspace, hspace = 
                bouding.Width / 10.0f, bouding.Height / 10.0f
            let rc = toRectangleF(deflate bouding wspace hspace)
            gr.DrawImage(bmp, rc)

let drawImage (width:int, height:int) space coreDrwingFunction = 
    let bmp = new Bitmap(width, height)
    use gr = Graphics.FromImage(bmp)
    gr.Clear(Color.White)
    gr.TranslateTransform(space, space)
    coreDrwingFunction(gr)
    bmp


let docImage = drawImage (450, 400) 20.0f (drawElements elements)