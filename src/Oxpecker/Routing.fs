namespace Oxpecker

open System
open System.Net
open System.Reflection
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Metadata
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open Microsoft.OpenApi.Models
open Oxpecker

[<AutoOpen>]
module RoutingTypes =
    type HttpVerb =
        | GET
        | POST
        | PUT
        | PATCH
        | DELETE
        | HEAD
        | OPTIONS
        | TRACE
        | CONNECT
        | Any

        override this.ToString() =
            match this with
            | GET -> "GET"
            | POST -> "POST"
            | PUT -> "PUT"
            | PATCH -> "PATCH"
            | DELETE -> "DELETE"
            | HEAD -> "HEAD"
            | OPTIONS -> "OPTIONS"
            | TRACE -> "TRACE"
            | CONNECT -> "CONNECT"
            | _ -> ""

    type RouteTemplate = string
    type Metadata = obj seq

    // This is a hack to prevent generating Func tag in open API
    [<CompilerGenerated>]
    type FakeFunc<'T, 'U> =
        member this.Invoke(_: 'T) = Unchecked.defaultof<'U>
        member this.InvokeUnit() = Unchecked.defaultof<'U>

    let fakeFuncMethod = typeof<FakeFunc<unit, unit>>.GetMethod("InvokeUnit")

    type RequestInfo(?requestType: Type, ?contentType: string) =
        let requestType = requestType |> Option.defaultValue typeof<unit>
        let contentType = contentType |> Option.defaultValue "application/json"
        member this.ToAttribute()=
            AcceptsMetadata([|contentType|], requestType)

    type ResponseInfo(?responseType: Type, ?contentType: string, ?statusCode: int) =
        let responseType = responseType |> Option.defaultValue typeof<unit>
        let contentTypes = contentType |> Option.map (fun ct ->  [|ct|]) |>  Option.defaultValue null
        let statusCode = statusCode |> Option.defaultValue 200
        member this.ToAttribute()=
            ProducesResponseTypeMetadata(statusCode, responseType, contentTypes)


    type OpenApiConfig (?requestInfo : RequestInfo,
                        ?responseInfo : ResponseInfo,
                        ?configureOperation : OpenApiOperation -> OpenApiOperation) =

        member this.Build(builder: IEndpointConventionBuilder) =
            builder.WithMetadata(fakeFuncMethod) |> ignore
            requestInfo |> Option.iter (fun accepts -> builder.WithMetadata(accepts.ToAttribute()) |> ignore)
            responseInfo |> Option.iter (fun produces -> builder.WithMetadata(produces.ToAttribute()) |> ignore)
            let configure = configureOperation |> Option.defaultValue id
            builder.WithOpenApi(configure)

    type ConfigureEndpoint = IEndpointConventionBuilder -> IEndpointConventionBuilder

    type Endpoint =
        | SimpleEndpoint of HttpVerb * RouteTemplate * EndpointHandler * ConfigureEndpoint
        | NestedEndpoint of RouteTemplate * Endpoint seq * ConfigureEndpoint
        | MultiEndpoint of Endpoint seq


module RoutingInternal =


    type ApplyBefore =
        static member Compose(beforeHandler: EndpointHandler, endpoint: Endpoint) =
            match endpoint with
            | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
                SimpleEndpoint(verb, template, beforeHandler >=> handler, configureEndpoint)
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                NestedEndpoint(template, Seq.map (fun e -> ApplyBefore.Compose(beforeHandler, e)) endpoints, configureEndpoint)
            | MultiEndpoint endpoints ->
                MultiEndpoint(Seq.map (fun e -> ApplyBefore.Compose(beforeHandler, e)) endpoints)

        static member Compose(beforeMiddleware: EndpointMiddleware, endpoint: Endpoint) =
            match endpoint with
            | SimpleEndpoint(verb, template, handler,configureEndpoint) ->
                SimpleEndpoint(verb, template, beforeMiddleware >=> handler, configureEndpoint)
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                NestedEndpoint(
                    template,
                    Seq.map (fun e -> ApplyBefore.Compose(beforeMiddleware, e)) endpoints,
                    configureEndpoint
                )
            | MultiEndpoint endpoints ->
                MultiEndpoint(Seq.map (fun e -> ApplyBefore.Compose(beforeMiddleware, e)) endpoints)

module private RouteTemplateBuilder =

    // This function should convert to route template and mappings
    // "api/{%s}/{%i}" -> ("api/{s0}/{i1}", [("s0", 's', None); ("i1", 'i', None)])
    // "api/{%O:guid}/{%s}" -> ("api/{s0:guid}", [("O0", 'O', Some "guid"); ("s1", 's', None)])
    let convertToRouteTemplate (pathValue: string) (parameters: ParameterInfo[]) =
        let placeholderPattern = Regex(@"\{%([sibcdfuO])(:[^}]+)?\}")
        let mutable index = 0
        let mappings = ResizeArray()

        let placeholderEvaluator = MatchEvaluator(fun m ->
            let vtype = m.Groups[1].Value[0] // First capture group is the variable type s, i, or O
            let formatSpecifier = if m.Groups[2].Success then m.Groups[2].Value else ""
            let paramName = parameters[index].Name
            index <- index + 1 // Increment index for next use
            mappings.Add((paramName, vtype, if formatSpecifier = "" then None else (Some <| formatSpecifier.TrimStart(':'))))
            $"{{{paramName}{formatSpecifier}}}" // Construct the new placeholder
        )

        let newRoute = placeholderPattern.Replace(pathValue, placeholderEvaluator)
        (newRoute, mappings.ToArray()) // Convert ResizeArray to Array


module private RequestDelegateBuilder =
    // Kestrel has made the weird decision to
    // partially decode a route argument, which
    // means that a given route argument would get
    // entirely URL decoded except for '%2F' (/).
    // Hence decoding %2F must happen separately as
    // part of the string parsing function.
    //
    // For more information please check:
    // https://github.com/aspnet/Mvc/issues/4599
    let stringParse (s: string) =
        s.Replace("%2F", "/", StringComparison.OrdinalIgnoreCase) |> box
    let intParse (s: string) = int s |> box
    let boolParse (s: string) = bool.Parse s |> box
    let charParse (s: string) = char s[0] |> box
    let int64Parse (s: string) = int64 s |> box
    let floatParse (s: string) = float s |> box
    let uint64Parse (s: string) = uint64 s |> box
    let guidParse (s: string) = Guid.Parse s |> box

    let getSchema (c: char) (modifier: string option) =
        match c with
        | 's' -> OpenApiSchema(Type = "string")
        | 'i' -> OpenApiSchema(Type = "integer", Format = "int32")
        | 'b' -> OpenApiSchema(Type = "boolean")
        | 'c' -> OpenApiSchema(Type = "string")
        | 'd' -> OpenApiSchema(Type = "integer", Format = "int64")
        | 'f' -> OpenApiSchema(Type = "number", Format = "double")
        | 'u' -> OpenApiSchema(Type = "integer", Format = "int64")
        | 'O' ->
            match modifier with
            | Some "guid" ->  OpenApiSchema(Type = "string", Format = "uuid")
            | _ -> OpenApiSchema(Type = "string")
        | _ -> OpenApiSchema(Type = "string")

    let tryGetParser (c: char) (modifier: string option) =
        match c with
        | 's' -> Some stringParse
        | 'i' -> Some intParse
        | 'b' -> Some boolParse
        | 'c' -> Some charParse
        | 'd' -> Some int64Parse
        | 'f' -> Some floatParse
        | 'u' -> Some uint64Parse
        | 'O' ->
            match modifier with
            | Some "guid" -> Some guidParse
            | _ -> None
        | _ -> None

    let private handleResult (result: HttpContext option) (ctx: HttpContext) =
        match result with
        | None -> ctx.SetStatusCode(int HttpStatusCode.UnprocessableEntity)
        | Some _ -> ()


[<AutoOpen>]
module Routers =
    open CoreInternal
    open RoutingInternal

    let rec private applyHttpVerbToEndpoint (verb: HttpVerb) (endpoint: Endpoint) : Endpoint =
        match endpoint with
        | SimpleEndpoint(_, template, handler, configureEndpoint) ->
            SimpleEndpoint(verb, template, handler, configureEndpoint)
        | NestedEndpoint(handler, endpoints, configureEndpoint) ->
            NestedEndpoint(handler, endpoints |> Seq.map(applyHttpVerbToEndpoint verb), configureEndpoint)
        | MultiEndpoint endpoints -> endpoints |> Seq.map(applyHttpVerbToEndpoint verb) |> MultiEndpoint

    let rec private applyHttpVerbToEndpoints (verb: HttpVerb) (endpoints: Endpoint seq) : Endpoint =
        endpoints
        |> Seq.map(fun endpoint ->
            match endpoint with
            | SimpleEndpoint(_, template, handler, configureEndpoint) ->
                SimpleEndpoint(verb, template, handler, configureEndpoint)
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                NestedEndpoint(template, endpoints |> Seq.map(applyHttpVerbToEndpoint verb), configureEndpoint)
            | MultiEndpoint endpoints -> applyHttpVerbToEndpoints verb endpoints)
        |> MultiEndpoint

    let rec private applyHttpVerbsToEndpoints (verbs: HttpVerb seq) (endpoints: Endpoint seq) : Endpoint =
        endpoints
        |> Seq.map (function
            | SimpleEndpoint(_, routeTemplate, requestDelegate, configureEndpoint) ->
                verbs
                |> Seq.map(fun verb -> SimpleEndpoint(verb, routeTemplate, requestDelegate, configureEndpoint))
                |> MultiEndpoint
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                verbs
                |> Seq.map(fun verb ->
                    NestedEndpoint(template, endpoints |> Seq.map(applyHttpVerbToEndpoint verb), configureEndpoint))
                |> MultiEndpoint
            | MultiEndpoint endpoints ->
                verbs
                |> Seq.map(fun verb -> applyHttpVerbToEndpoints verb endpoints)
                |> MultiEndpoint)
        |> MultiEndpoint

    let GET_HEAD: Endpoint seq -> Endpoint = applyHttpVerbsToEndpoints [ GET; HEAD ]

    let GET: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints GET
    let POST: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints POST
    let PUT: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints PUT
    let PATCH: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints PATCH
    let DELETE: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints DELETE
    let HEAD: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints HEAD
    let OPTIONS: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints OPTIONS
    let TRACE: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints TRACE
    let CONNECT: Endpoint seq -> Endpoint = applyHttpVerbToEndpoints CONNECT

    let route (path: string) (handler: EndpointHandler) : Endpoint =
        SimpleEndpoint(HttpVerb.Any, path, handler, id)


    let private invokeHandler<'T>
        (ctx: HttpContext)
        (methodInfo: MethodInfo)
        (handler: 'T)
        (mappings: (string * char * Option<_>) array)
        (parameters: ParameterInfo array)
        =
        let routeData = ctx.GetRouteData()
        let mappingArguments =
            seq {
                for mapping in mappings do
                    let placeholderName, formatChar, modifier = mapping
                    let routeValue = routeData.Values[placeholderName] |> string
                    match RequestDelegateBuilder.tryGetParser formatChar modifier with
                    | Some parseFn ->
                        try
                            parseFn routeValue
                        with :? FormatException as ex ->
                            raise
                            <| RouteParseException($"Url segment value '%s{routeValue}' has invalid format", ex)
                    | None -> routeValue
            }
        let paramCount = parameters.Length
        if paramCount = mappings.Length + 1 then
            methodInfo.Invoke(handler, [| yield! mappingArguments; ctx |]) :?> Task
        elif paramCount = mappings.Length then
            let result =
                methodInfo.Invoke(handler, [| yield! mappingArguments |]) :?> FSharpFunc<HttpContext, Task>
            result ctx
        else
            failwith "Unsupported"

    let routef (path: PrintfFormat<'T, unit, unit, EndpointHandler>) (routeHandler: 'T) : Endpoint =
        let handlerType = routeHandler.GetType()
        let handlerMethod = handlerType.GetMethods()[0]
        let parameters = handlerMethod.GetParameters()
        let template, mappings = RouteTemplateBuilder.convertToRouteTemplate path.Value parameters

        let requestDelegate =
            fun (ctx: HttpContext) -> invokeHandler<'T> ctx handlerMethod routeHandler mappings parameters

        let configureEndpoint =
            fun (endpoint: IEndpointConventionBuilder)->
                endpoint.WithOpenApi(
                    fun (operation: OpenApiOperation) ->
                        operation.Parameters <- ResizeArray (
                            mappings |> Array.map (fun (name, format, modifier) ->
                                OpenApiParameter(
                                    Name = name,
                                    In = ParameterLocation.Path,
                                    Required = true,
                                    Style = ParameterStyle.Simple,
                                    Schema = RequestDelegateBuilder.getSchema format modifier
                                )
                            )
                        )
                        operation
                )

        SimpleEndpoint(HttpVerb.Any, template, requestDelegate, configureEndpoint)

    let subRoute (path: string) (endpoints: Endpoint seq) : Endpoint =
        NestedEndpoint(path, endpoints, id)

    let inline applyBefore (beforeHandler: 'T) (endpoint: Endpoint) =
        compose_opImpl Unchecked.defaultof<ApplyBefore> beforeHandler endpoint

    let rec applyAfter (afterHandler: EndpointHandler) (endpoint: Endpoint) =
        match endpoint with
        | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
            SimpleEndpoint(verb, template, handler >=> afterHandler, configureEndpoint)
        | NestedEndpoint(template, endpoints, configureEndpoint) ->
            NestedEndpoint(template, Seq.map (applyAfter afterHandler) endpoints, configureEndpoint)
        | MultiEndpoint endpoints -> MultiEndpoint(Seq.map (applyAfter afterHandler) endpoints)

    let rec configureEndpoint (f: ConfigureEndpoint) (endpoint: Endpoint) =
        match endpoint with
        | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
            SimpleEndpoint(
                verb,
                template,
                handler,
                configureEndpoint >> f
            )
        | NestedEndpoint(template, endpoints, configureEndpoint) ->
            NestedEndpoint(
                template,
                endpoints,
                configureEndpoint >> f
            )
        | MultiEndpoint endpoints ->
            MultiEndpoint(Seq.map (configureEndpoint f) endpoints)

    let addMetadata (metadata: obj) =
        configureEndpoint _.WithMetadata(metadata)

    let addOpenApi (config: OpenApiConfig) =
        configureEndpoint config.Build

    let addOpenApiSimple<'Req, 'Res> =
        let methodName =
            if typeof<'Req> = typeof<unit> then
                "InvokeUnit"
            else
                "Invoke"
        configureEndpoint
            _.WithMetadata(typeof<FakeFunc<'Req, 'Res>>.GetMethod(methodName))
             .WithOpenApi()



type EndpointRouteBuilderExtensions() =

    [<Extension>]
    static member private MapSingleEndpoint
        (
            builder: IEndpointRouteBuilder,
            verb: HttpVerb,
            routeTemplate: RouteTemplate,
            requestDelegate: RequestDelegate,
            configureEndpoint: ConfigureEndpoint
        ) =
        match verb with
        | Any ->
            builder
                .Map(routeTemplate, requestDelegate)
                |> configureEndpoint
        | _ ->
            builder
                .MapMethods(routeTemplate, [| verb.ToString() |], requestDelegate)
                |> configureEndpoint
        |> ignore

    [<Extension>]
    static member private MapNestedEndpoint
        (
            builder: IEndpointRouteBuilder,
            parentTemplate: RouteTemplate,
            endpoints: Endpoint seq,
            parentConfigureEndpoint: ConfigureEndpoint
        ) =
        let groupBuilder = builder.MapGroup(parentTemplate)
        for endpoint in endpoints do
            match endpoint with
            | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
                groupBuilder.MapSingleEndpoint(
                    verb,
                    template,
                    handler,
                    parentConfigureEndpoint >> configureEndpoint
                )
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                groupBuilder.MapNestedEndpoint(
                    template,
                    endpoints,
                    parentConfigureEndpoint >> configureEndpoint
                )
            | MultiEndpoint endpoints -> groupBuilder.MapMultiEndpoint endpoints

    [<Extension>]
    static member private MapMultiEndpoint(builder: IEndpointRouteBuilder, endpoints: Endpoint seq) =
        for endpoint in endpoints do
            match endpoint with
            | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
                builder.MapSingleEndpoint(verb, template, handler, configureEndpoint)
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                builder.MapNestedEndpoint(template, endpoints, configureEndpoint)
            | MultiEndpoint endpoints ->
                builder.MapMultiEndpoint endpoints

    [<Extension>]
    static member MapOxpeckerEndpoints(builder: IEndpointRouteBuilder, endpoints: Endpoint seq) =

        for endpoint in endpoints do
            match endpoint with
            | SimpleEndpoint(verb, template, handler, configureEndpoint) ->
                builder.MapSingleEndpoint(verb, template, handler, configureEndpoint)
            | NestedEndpoint(template, endpoints, configureEndpoint) ->
                builder.MapNestedEndpoint(template, endpoints, configureEndpoint)
            | MultiEndpoint endpoints -> builder.MapMultiEndpoint endpoints
