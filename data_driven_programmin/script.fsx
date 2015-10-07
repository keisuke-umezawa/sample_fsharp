// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 13
// --------------------------------------------------------------------------

// Section 13.1 Asynchronous workflows

// Listing 13.1 Writing code using asynchronous workflows 

#r "FSharp.PowerPack.dll"
//#r @"C:\Programs\Development\FSharp-PowerPack-4.0-Beta1\FSharp.PowerPack.dll"

open System.IO
open System.Net

// Namespace containing some of the asynchronous functionality
open Microsoft.FSharp.Control
open Microsoft.FSharp.Control.WebExtensions

// Using the 'async' computation builder
let downloadUrl(url:string) = async { 
  let req = HttpWebRequest.Create(url)
  // Run operation asynchronously
  let! resp = req.AsyncGetResponse()
  use response = resp
  let stream = response.GetResponseStream()
  // Dispose 'StreamReader' when completed
  use reader = new StreamReader(stream)
  // Run asynchronously and then return the result
  return! reader.AsyncReadToEnd() }

// Listing 13.2 Executing asynchronous computations 

// Build the asynchronous workflow
let downloadTask = downloadUrl("http://www.manning.com")
// Run the workflow and wait for the result
Async.RunSynchronously(downloadTask)


// Create collection of workflows
let tasks =
     [ downloadUrl("http://www.tomasp.net");
       downloadUrl("http://www.manning.com") ]
// Join all workflows into one
let all = Async.Parallel(tasks)

// Run the joined workflow
Async.RunSynchronously(all)

// ----------------------------------------------------------------------------
// Section 13.1.3 Creating primitive workflows

// Listing 13.4 Implementing asynchronous waiting

module Async = 
  // Primitive that delays the workflow
  let Sleep(time) = 
    Async.FromContinuations(fun (cont, econt, ccont) ->
      // Initialize timer
      let tmr = new System.Timers.Timer(time, AutoReset=false)
      tmr.Elapsed.Add(fun _ -> 
        // Run the rest of the computation
        cont())
      tmr.Start() )

open System

Async.RunSynchronously(async { 
   printfn "Starting..."
   // Non-blocking waiting using a timer
   do! Async.Sleep(1000.0) 
   printfn "Finished!"
} )

// Section 13.2 world bank

// ----------------------------------------------------------------------------
//Initialize charting libraries
#r @"Q:\umezawa\dev\git\sample_fsharp\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
#load @"Q:\umezawa\dev\git\sample_fsharp\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"
open FSharp.Data
open FSharp.Charting

let data = WorldBankData.GetDataContext()

data.Countries.``United Kingdom``
    .Indicators.``School enrollment, tertiary (% gross)``
|> Chart.Line

// asyncronic access
type WorldBank = WorldBankDataProvider<"World Development Indicators", Asynchronous=true>
WorldBank.GetDataContext()

let wb = WorldBank.GetDataContext()

// 対象とする国のリストを作成
let countries = 
 [| wb.Countries.``Arab World``
    wb.Countries.``European Union``
    wb.Countries.Australia
    wb.Countries.Brazil
    wb.Countries.Canada
    wb.Countries.Chile
    wb.Countries.``Czech Republic``
    wb.Countries.Denmark
    wb.Countries.France
    wb.Countries.Greece
    wb.Countries.``Low income``
    wb.Countries.``High income``
    wb.Countries.``United Kingdom``
    wb.Countries.``United States`` |]

[ for c in countries ->
    c.Indicators.``School enrollment, tertiary (% gross)`` ]
|> Async.Parallel
|> Async.RunSynchronously
|> Array.map Chart.Line
|> Chart.Combine

//Plot birth rate for USA
//Note: run the two plots separately to see their results
data.Countries.``United States``.Indicators.
  ``Birth rate, crude (per 1,000 people)``
|> Chart.Line

//Plot maternal mortality ratio for United Kingdom
//Note: run the two plots separately to see their results
data.Countries.``United Kingdom``.Indicators.
  ``Maternal mortality ratio (modeled estimate, per 100,000 live births)``
|> Chart.Line

//Set up list of countries to compare
let countries = 
  [ data.Countries.``United States``; 
    data.Countries.India;
    data.Countries.``Burkina Faso``; 
    data.Countries.Niger; ]


//Plot birth rates of selected countries
//Note: run the two plots separately to see their results
Chart.Combine([ for country in countries ->
                    let data = country.Indicators.``Birth rate, crude (per 1,000 people)``
                    Chart.Line(data, Name=country.Name) ])
     .WithLegend()

//Plot maternal mortality rates of selected countries
//Note: run the two plots separately to see their results
Chart.Combine([ for country in countries ->
                    let data = country.Indicators.``Maternal mortality ratio (national estimate, per 100,000 live births)``
                    Chart.Line(data, Name=country.Name) ])
     .WithLegend()





(*

// ----------------------------------------------------------------------------
// Section 13.3 Exploring and obtaining the data

// Listing 13.8 Helper functions for reading XML

#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

open System.Xml.Linq

let wb = "http://www.worldbank.org"

// Returns value of the specified attribute 
let xattr s (el:XElement) =     
  el.Attribute(XName.Get(s)).Value
// Returns child node with the specified name
let xelem s (el:XContainer) = 
  el.Element(XName.Get(s, wb))
// Returns child elements with the specified name
let xelems s (el:XContainer) = 
  el.Elements(XName.Get(s, wb))
// Returns the text inside the node
let xvalue (el:XElement) = 
  el.Value

// Return child node specified by a path
let xnested path (el:XContainer) = 
  let res = path |> Seq.fold (fun xn s -> 
    // Upcast element to a container
    let nested = xelem s xn
    nested :> XContainer) el
  // Downcast the result back to an element
  res :?> XElement


// ----------------------------------------------------------------------------
// Section 13.3.2 Extracting region codes

let worldBankRequest(props) = async {
  let! text = worldBankDownload(props)
  return XDocument.Parse(text) }


// Listing 13.9 Exploring the region information
let doc = 
  worldBankRequest (["countries"], ["region", "NA"])
  |> Async.RunSynchronously

module Listing_13_9 =
  // Select the first country element
  let c = doc |> xnested [ "countries"; "country" ]
  // Read the value of the 'id' attribute
  c |> xattr "id" 
  // Get the value of the 'name' child element
  c |> xelem "name" |> xvalue


// Listing 13.10 Creating sequence with region information

let regions = 
  seq { for r in doc |> xelem "countries" |> xelems "country" do
          yield r |> xelem "name" |> xvalue }
          
// Listing 13.11 Obtaining area covered by forests
module Listing_13_11 = 

  // First page of forest area data from 1990
  let ind = "AG.LND.FRST.ZS"
  let date = "1990:1990"  
  let page = 1
  // Build arguments for the request
  let props = 
    [ "countries"; "indicators"; ind ], 
    [ "date", date; "page", string(page)]

  // Get the data and print a preview
  let doc = Async.RunSynchronously(worldBankRequest props)
  printfn "%s..." (doc.ToString().Substring(0, 301))

  // Read the total number of pages
  doc |> xnested [ "data" ] |> xattr "pages" |> int 
  // Read the ID of the first country
  doc |> xnested [ "data"; "data"; "country" ] |> xvalue 


// Listing 13.12 Downloading all indicator data asynchronously

let rec getIndicatorData(date, ind, page) = async {
  // Get the number of pages 
  let! doc = worldBankRequest ([ "countries"; "indicators"; ind ], [ "date", date; "page", string(page)])
  let pages = doc |> xnested [ "data"; ] |> xattr "pages" |> int
  if (pages = page) then 
    // Data from the last page
    return [doc]
  else 
    // Download the remaining pages
    let! rest = getIndicatorData(date, ind, page + 1)
    return doc::rest }

// Listing 13.13 Downloading multiple indicators for multiple years in parallel

// Return workflow for each indicator and a year
let downloadAll = seq { 
   for ind in [ "AG.SRF.TOTL.K2"; "AG.LND.FRST.ZS" ] do 
      for year in [ "1990:1990"; "2000:2000"; "2005:2005" ] do 
         yield getIndicatorData(year, ind, 1) }
          
// Run all workflows in parallel
let data = Async.RunSynchronously(Async.Parallel(downloadAll))

// ----------------------------------------------------------------------------
// Section 13.4 Gathering information from the data

// Listing 13.14 Reading values from the XML data

let readSingleValue format node =
  // Get the value, year and the country ID
  let value = node |> xelem "value" |> xvalue
  let country = node |> xelem "country" |> xvalue
  let date = node |> xelem "date" |> xvalue |> int
  // Value missing - return an empty list
  if (value = "") then [] else [ (date, country), format(value) ]

let readValues format data = seq { 
  for page in data do
    // Find all 'dataPoint' elements for all pages
    for node in page |> xnested [ "data" ] |> xelems "data" do
      // Parse the element and yield returned tuples
      yield! node |> readSingleValue format }        


// Experimenting with the data...
data.[0] |> readValues id

// ----------------------------------------------------------------------------
// Section 13.4.2 Formatting data using units of measure

[<Measure>] type km
[<Measure>] type h

// Listing 13.15 Writing calculations using units of measure

// A constant representing distance
let length = 10.0<km>
// We get an area by multiplying distances
length * length
// Parameter type uses a unit
let distanceInTwoHours(speed:float<km/h>) =
  speed * 2.0<h>
// (Hover) shows the inferred unit of the return type
let dist = distanceInTwoHours(30.0<km/h>)


// Listing 13.16 Converting raw data into a typed data structure
[<Measure>] type percent

let areas = 
  // Concatenate data for the first indicator
  Seq.concat(data.[0..2])
    // Convert to square kilometers
    |> readValues (fun a -> float(a) * 1.0<km^2>) 
    // Return the data as a hashtable
    |> Map.ofSeq 

// Create a hashtable storing the forested area in percents
let forests = 
  Seq.concat(data.[3..5])
    |> readValues (fun a -> float(a) * 1.0<percent>) 
    |> Map.ofSeq  
  
// ----------------------------------------------------------------------------
// Section 13.4.3 Gathering statistics about regions

// Listing 13.17 Calculating information about forested area

// Calculate the total forest area
let calculateForests(area:float<km^2>, forest:float<percent>) =
   area * forest / 100.0<percent>

let years = [| 1990; 2000; 2005 |]

// Is the value available for the specified key?
let dataAvailable(key) = 
   years |> Seq.forall (fun y ->
      (Map.containsKey (y, key) areas) && (Map.containsKey (y, key) forests))
      
// Get the forested area for each monitored year
let getForestData(key) =
   [| for y in years do
         yield calculateForests(areas.[y, key], forests.[y, key]) |]

let stats = seq {
  // Iterate over all regions
  for name in regions do      
    if dataAvailable(name) then    
      // Return title and the data if available
      yield name, getForestData(name) }  

// ----------------------------------------------------------------------------
// Section 13.6 Visualizing data using Excel

// Listing 13.19 Starting Excel and creating worksheet

// Reference the Excel interop assemblies
#r @"C:\Programs\Development\Visual Studio 9.0\Visual Studio Tools for Office\PIA\Office12\Microsoft.Office.Interop.Excel.dll"
#r @"C:\Programs\Development\Visual Studio 9.0\Visual Studio Tools for Office\PIA\Office12\office.dll"

// Workaround when english version of Excel is running on non-english OS:
//
//  open System.Threading
//  open System.Globalization
//  Thread.CurrentThread.CurrentCulture <- CultureInfo.GetCultureInfo("en-US")
//  Thread.CurrentThread.CurrentUICulture <- CultureInfo.GetCultureInfo("en-US")

open Microsoft.Office.Interop.Excel
open System

// Run Excel as a visible application
let app = new ApplicationClass(Visible = true) 
// Create new file using the default template
let workbook = app.Workbooks.Add(XlWBATemplate.xlWBATWorksheet) 
// Get the first worksheet
let worksheet = (workbook.Worksheets.[1] :?> _Worksheet) 

// Write values to the worksheet
worksheet.Range("C2").Value2 <- "1990"
worksheet.Range("C2", "E2").Value2 <- [| "1990"; "2000"; "2005" |]


// Listing 13.19 Exporting data to Excel worksheet 

let statsArray = stats |> Array.ofSeq  // !!!
// Get names of regions as 2D array
let names = Array2D.init statsArray.Length 1 (fun i _ -> 
  let name, _ = statsArray.[i]
  name )
  
// Initialize 2D array with the data
let dataArray = Array2D.init statsArray.Length 3 (fun x y -> 
  // Read value for a year 'y' from the i-th region
  let _, values = statsArray.[x]
  // Display millions of square kilometers
  values.[y] / 1000000.0 )

// Write the data to the worksheet
let endColumn = string(statsArray.Length + 2)
worksheet.Range("B3", "B" + endColumn).Value2 <- names
worksheet.Range("C3", "E" + endColumn).Value2 <- dataArray


// Listing 13.21 Generating Excel chart

// Add new item to the charts collection
let chartobjects = (worksheet.ChartObjects() :?> ChartObjects) 
let chartobject = chartobjects.Add(400.0, 20.0, 550.0, 350.0) 

// Configure the chart using the wizard
chartobject.Chart.ChartWizard
  (Title = "Area covered by forests",
   Source = worksheet.Range("B2", "E" + endColumn),
   Gallery = XlChartType.xl3DColumn, PlotBy = XlRowCol.xlColumns,
   SeriesLabels = 1, CategoryLabels = 1,
   CategoryTitle = "", ValueTitle = "Forests (mil km^2)")

// Set graphical style of the chart
chartobject.Chart.ChartStyle <- 5

// ----------------------------------------------------------------
// Bonus - using the Microsoft chart libraries to draw the same chart

// For Visual Studio 2008
// Download the library from: http://code.msdn.microsoft.com/mschart
// #r @"C:\Program Files\Microsoft Chart Controls\Assemblies\System.Windows.Forms.DataVisualization.dll"

// For Visual Studio 2010
#r @"C:\Program Files\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Windows.Forms.DataVisualization.dll"

open System.Drawing
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

let frm = new Form(ClientSize = Size(800, 600))
let chart = new Chart()
chart.Dock <- DockStyle.Fill

let ch = new ChartArea()
ch.Area3DStyle.Enable3D <- true
ch.Area3DStyle.IsClustered <- true
chart.ChartAreas.Add(ch);

frm.Controls.Add(chart)
frm.Show()

let series name (data:seq<float<'u>>) = 
  let s = new Series(name)
  for (pt:float<_>) in data do
    s.Points.Add(new DataPoint(0.0, float pt))
  s.ChartType <- SeriesChartType.Column
  s.["DrawingStyle"] <- "Cylinder"
  s

for year, index in [| "1990", 0; "2000", 1; "2005", 2 |] do  
  let values = seq { for _, ar in stats -> ar.[index] / 1000000.0 }
  printfn "%A" (values |> List.of_seq)
  chart.Series.Add(series year values)

*)
