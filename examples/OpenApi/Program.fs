open System
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

let hellloWorldOpenApiSchema =
    OpenApiOperation(
        Tags = ResizeArray([
           OpenApiTag(Name="OpenApiFsharp")
        ]),
        // Parameters = GetOpenApiParameters(methodInfo, pattern, disableInferredBody),
        // RequestBody = GetOpenApiRequestBody(methodInfo, metadata, pattern, disableInferredBody),
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


let endpoints = [
    GET [
        route "/" (text "Hello World") |> addMetadata hellloWorldOpenApiSchema
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

let getOperationType = function
    | "GET" -> OperationType.Get
    | "POST" -> OperationType.Post
    | "PUT" -> OperationType.Put
    | "DELETE" -> OperationType.Delete
    | "PATCH" -> OperationType.Patch
    | "HEAD" -> OperationType.Head
    | "OPTIONS" -> OperationType.Options
    | _ -> failwith "Unknown operation type"

let swagger (ctx: HttpContext) (next: RequestDelegate) =
    task {
        if (ctx.Request.Path = "/swagger.json") then
            let endpointsDataSource = ctx.RequestServices.GetService<EndpointDataSource>()
            let result = OpenApiDocument(
                Info = OpenApiInfo(
                    Title = "OpenApiFsharp",
                    Version = "1.0"
                ),
                Paths = OpenApiPaths()
            )
            let paths =
                endpointsDataSource.Endpoints
                |> Seq.map (fun ep -> ep :?> RouteEndpoint)
                |> Seq.map (fun ep ->
                    let pathItem = OpenApiPathItem()
                    let httpMethodMetadata = ep.Metadata.GetMetadata<HttpMethodMetadata>()
                    let operation = ep.Metadata.GetMetadata<OpenApiOperation>()
                    for httpMethod in httpMethodMetadata.HttpMethods do
                        pathItem.Operations[getOperationType httpMethod] <- operation
                    ep.RoutePattern.RawText, pathItem)
                |> Seq.toArray
            for path, pathItem in paths do
                result.Paths[path] <- pathItem

            use textWriter = new StringWriter(CultureInfo.InvariantCulture)
            let jsonWriter = OpenApiJsonWriter(textWriter)
            result.SerializeAsV3(jsonWriter)
            return! text (string textWriter) ctx
        else
            return! next.Invoke(ctx)
    } :> Task

let configureApp (appBuilder: IApplicationBuilder) =
    appBuilder
        .UseRouting()
        .Use(errorHandler)
        .UseOxpecker(endpoints)
        .Use(swagger)
        .Run(notFoundHandler)

let configureServices (services: IServiceCollection) =
    services
        .AddRouting()
        .AddOxpecker()
        .AddEndpointsApiExplorer()
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
