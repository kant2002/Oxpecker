﻿open System
open System.Globalization
open System.IO
open System.Net
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Net.Http.Headers
open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Writers
open Oxpecker
open Oxpecker.ViewEngine
open type Microsoft.AspNetCore.Http.TypedResults


type OpenApiResponses with
    static member Init(responses: (string*OpenApiResponse) seq) =
        let obj = OpenApiResponses()
        for code, response in responses do
            obj.Add(code, response)
        obj

let hellloWorldSchema =
    OpenApiOperation(
        Tags = ResizeArray [ OpenApiTag(Name="OpenApiFsharp") ],
        OperationId = "HelloWorld",
        Responses =
            OpenApiResponses.Init([
                ("200", OpenApiResponse(
                    Description = "OK",
                    Content = dict [
                        "text/plain", OpenApiMediaType(Schema = OpenApiSchema(Type = "string"))
                    ]
                ))
            ])
    )

let summaries = [
    "Freezing"; "Bracing"; "Chilly"; "Cool"; "Mild"; "Warm"; "Balmy"; "Hot"; "Sweltering"; "Scorching"
]

type WeatherForecast = {
    Date: DateTime
    TemperatureC: int
    Summary: string
} with
    member this.TemperatureF = 32 +  int (float this.TemperatureC / 0.5556)

let endpoints = [
    GET [
        route "/" (text "Hello World") |> configureEndpoint _.WithMetadata(hellloWorldSchema)
        subRoute "/{city}" [
            routef "/weatherforecast/{%i}" (fun num ->
                [| 1.. num |]
                 |> Array.map (fun i -> {
                         Date= DateTime.Now.AddDays(i)
                         TemperatureC  = Random.Shared.Next(-20, 55)
                         Summary = summaries[Random.Shared.Next(summaries.Length)]
                     })
                 |> json
             )
            |> configureEndpoint _.WithMetadata(typeof<Func<WeatherForecast[]>>.GetMethod("Invoke"))
            |> configureEndpoint _.WithOpenApi(fun o -> o.OperationId <- "GetWeatherForecast"; o)
        ]
    ]
]


let errorView errorCode (errorText: string) =
    html() {
        body(style = "width: 800px; margin: 0 auto") {
            h1(style = "text-align: center; color: red") { raw $"Error <i>%d{errorCode}</i>" }
            p() { errorText }
        }
    }

let notFoundHandler (ctx: HttpContext) =
    let logger = ctx.GetLogger()
    logger.LogWarning("Unhandled 404 error")
    ctx.SetStatusCode 404
    ctx.WriteHtmlView(errorView 404 "Page not found!")

let errorHandler (ctx: HttpContext) (next: RequestDelegate) =
    task {
        try
            return! next.Invoke(ctx)
        with
        | :? ModelBindException
        | :? RouteParseException as ex ->
            let logger = ctx.GetLogger()
            logger.LogWarning(ex, "Unhandled 400 error")
            ctx.SetStatusCode StatusCodes.Status400BadRequest
            return! ctx.WriteHtmlView(errorView 400 (string ex))
        | ex ->
            let logger = ctx.GetLogger()
            logger.LogError(ex, "Unhandled 500 error")
            ctx.SetStatusCode StatusCodes.Status500InternalServerError
            return! ctx.WriteHtmlView(errorView 500 (string ex))
    }
    :> Task

let configureApp (appBuilder: IApplicationBuilder) =
    appBuilder
        .UseRouting()
        .Use(errorHandler)
        .UseOxpecker(endpoints)
        .UseSwagger()
        .Run(notFoundHandler)

let configureServices (services: IServiceCollection) =
    services
        .AddRouting()
        .AddOxpecker()
        .AddEndpointsApiExplorer()
        .AddSwaggerGen()
        .AddSingleton<ILogger>(fun sp ->
            sp.GetRequiredService<ILoggerFactory>().CreateLogger("Oxpecker.Examples.OpenApi"))
    |> ignore


[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    configureServices builder.Services
    let app = builder.Build()
    configureApp app
    app.Run()
    0
