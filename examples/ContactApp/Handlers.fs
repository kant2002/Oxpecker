﻿module ContactApp.Handlers
open System.Threading
open System.Threading.Tasks
open ContactApp.templates
open ContactApp.Models
open ContactApp.Tools
open Oxpecker


let getContacts: EndpointHandler =
    fun ctx ->
        let page = ctx.TryGetQueryStringValue "page" |> Option.map int |> Option.defaultValue 1
        match ctx.TryGetQueryStringValue "q" with
        | Some search ->
            let result =
                ContactService.searchContact search
                |> Seq.toArray
            match ctx.TryGetRequestHeader "HX-Trigger" with
            | Some "search" ->
                ctx.WriteHtmlView (index.rows page result)
            | _ ->
                ctx |> writeHtml (index.html search page result)
        | None ->
            let result =
                ContactService.all page
                |> Seq.toArray
            ctx |> writeHtml (index.html "" page result)

let getContactsCount: EndpointHandler =
    fun ctx ->
        let count = ContactService.count()
        ctx.WriteText $"({count} total Contacts)"

let getNewContact: EndpointHandler =
    let newContact = {
        id = 0
        first = ""
        last = ""
        email = ""
        phone = ""
        errors = dict []
    }
    writeHtml (new'.html newContact)

let postNewContact: EndpointHandler =
    fun ctx ->
        task {
            let! contact = ctx.BindForm<ContactDTO>()
            let validatedContact = contact.Validate()
            if validatedContact.errors.Count > 0 then
                return! ctx |> writeHtml (new'.html validatedContact)
            else
                validatedContact.ToDomain()
                |> ContactService.add
                |> ignore
                flash "Created new Contact!" ctx
                return ctx.Response.Redirect("/contacts")
        }

let postEditContact id: EndpointHandler =
    fun ctx ->
        task {
            let! contact = ctx.BindForm<ContactDTO>()
            let validatedContact = contact.Validate()
            if validatedContact.errors.Count > 0 then
                return! ctx |> writeHtml (edit.html { validatedContact with id = id })
            else
                let domainContact = validatedContact.ToDomain()
                ContactService.update({domainContact with Id = id})
                flash "Updated Contact!" ctx
                return ctx.Response.Redirect($"/contacts/{id}")
        }

let viewContact id: EndpointHandler =
    let contact = ContactService.find id
    writeHtml <| show.html contact

let getEditContact id: EndpointHandler =
    let contact = ContactService.find id |> ContactDTO.FromDomain
    writeHtml <| edit.html contact

let postDeleteContact id: EndpointHandler =
    fun ctx ->
        task {
            ContactService.delete id |> ignore
            flash "Deleted Contact!" ctx
            ctx.Response.Redirect("/contacts")
            ctx.SetStatusCode(303)
        }

let validateEmail id: EndpointHandler =
    fun ctx ->
        match ctx.TryGetQueryStringValue("email") with
        | Some email ->
            let contact =
                if id = 0 then
                    { Id = 0; First = ""; Last = ""; Phone = ""; Email = email }
                else
                    let contact = ContactService.find id
                    { contact with Email = email  }
            if ContactService.validateEmail { contact with Email = email  } then
                Task.CompletedTask
            else
                ctx.WriteText "Invalid email"
        | None ->
                Task.CompletedTask


