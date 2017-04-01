


/// Contains error propagation functions and a computation expression builder for Railway-oriented programming.
namespace Chessie.ErrorHandling

open System

/// Represents the CResult of a computation.
type CResult<'TSuccess, 'TMessage> = 
    /// Represents the CResult of a successful computation.
    | Ok of 'TSuccess * 'TMessage list
    /// Represents the CResult of a failed computation.
    | Bad of 'TMessage list

    /// Creates a Failure CResult with the given messages.
    static member FailWith(messages:'TMessage seq) : CResult<'TSuccess, 'TMessage> = CResult<'TSuccess, 'TMessage>.Bad(messages |> Seq.toList)

    /// Creates a Failure CResult with the given message.
    static member FailWith(message:'TMessage) : CResult<'TSuccess, 'TMessage> = CResult<'TSuccess, 'TMessage>.Bad([message])
    
    /// Creates a Success CResult with the given value.
    static member Succeed(value:'TSuccess) : CResult<'TSuccess, 'TMessage> = CResult<'TSuccess, 'TMessage>.Ok(value,[])

    /// Creates a Success CResult with the given value and the given message.
    static member Succeed(value:'TSuccess,message:'TMessage) : CResult<'TSuccess, 'TMessage> = CResult<'TSuccess, 'TMessage>.Ok(value,[message])

    /// Creates a Success CResult with the given value and the given message.
    static member Succeed(value:'TSuccess,messages:'TMessage seq) : CResult<'TSuccess, 'TMessage> = CResult<'TSuccess, 'TMessage>.Ok(value,messages |> Seq.toList)

    /// Executes the given function on a given success or captures the failure
    static member Try(func: Func<_>) : CResult<'TSuccess,exn> =        
        try
            Ok(func.Invoke(),[])
        with
        | exn -> Bad[exn]

    /// Converts the CResult into a string.
    override this.ToString() =
        match this with
        | Ok(v,msgs) -> sprintf "OK: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
        | Bad(msgs) -> sprintf "Error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))    

/// Basic combinators and operators for error handling.
[<AutoOpen>]
module Trial =  
    /// Wraps a value in a Success
    let inline ok<'TSuccess,'TMessage> (x:'TSuccess) : CResult<'TSuccess,'TMessage> = Ok(x, [])

    /// Wraps a value in a Success
    let inline pass<'TSuccess,'TMessage> (x:'TSuccess) : CResult<'TSuccess,'TMessage> = Ok(x, [])

    /// Wraps a value in a Success and adds a message
    let inline warn<'TSuccess,'TMessage> (msg:'TMessage) (x:'TSuccess) : CResult<'TSuccess,'TMessage> = Ok(x,[msg])

    /// Wraps a message in a Failure
    let inline fail<'TSuccess,'Message> (msg:'Message) : CResult<'TSuccess,'Message> = Bad([ msg ])

    /// Executes the given function on a given success or captures the exception in a failure
    let inline Catch f x = CResult<_,_>.Try(fun () -> f x)

    /// Returns true if the CResult was not successful.
    let inline failed CResult = 
        match CResult with
        | Bad _ -> true
        | _ -> false

    /// Takes a CResult and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    let inline either fSuccess fFailure trialCResult = 
        match trialCResult with
        | Ok(x, msgs) -> fSuccess (x, msgs)
        | Bad(msgs) -> fFailure (msgs)

    /// If the given CResult is a Success the wrapped value will be returned. 
    ///Otherwise the function throws an exception with Failure message of the CResult.
    let inline returnOrFail CResult = 
        let inline raiseExn msgs = 
            msgs
            |> Seq.map (sprintf "%O")
            |> String.concat (Environment.NewLine + "\t")
            |> failwith
        either fst raiseExn CResult

    /// Appends the given messages with the messages in the given CResult.
    let inline mergeMessages msgs CResult = 
        let inline fSuccess (x, msgs2) = Ok(x, msgs @ msgs2)
        let inline fFailure errs = Bad(errs @ msgs)
        either fSuccess fFailure CResult

    /// If the CResult is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    let inline bind f CResult = 
        let inline fSuccess (x, msgs) = f x |> mergeMessages msgs
        let inline fFailure (msgs) = Bad msgs
        either fSuccess fFailure CResult

   /// Flattens a nested CResult given the Failure types are equal
    let inline flatten (CResult : CResult<CResult<_,_>,_>) =
        CResult |> bind id

    /// If the CResult is a Success it executes the given function on the value. 
    /// Otherwise the exisiting failure is propagated.
    /// This is the infix operator version of ErrorHandling.bind
    let inline (>>=) CResult f = bind f CResult

    /// If the wrapped function is a success and the given CResult is a success the function is applied on the value. 
    /// Otherwise the exisiting error messages are propagated.
    let inline apply wrappedFunction CResult = 
        match wrappedFunction, CResult with
        | Ok(f, msgs1), Ok(x, msgs2) -> Ok(f x, msgs1 @ msgs2)
        | Bad errs, Ok(_, _msgs) -> Bad(errs)
        | Ok(_, _msgs), Bad errs -> Bad(errs)
        | Bad errs1, Bad errs2 -> Bad(errs1 @ errs2)

    /// If the wrapped function is a success and the given CResult is a success the function is applied on the value. 
    /// Otherwise the exisiting error messages are propagated.
    /// This is the infix operator version of ErrorHandling.apply
    let inline (<*>) wrappedFunction CResult = apply wrappedFunction CResult

    /// Lifts a function into a CResult container and applies it on the given CResult.
    let inline lift f CResult = apply (ok f) CResult

    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded.
    let inline mapFailure f CResult =
        match CResult with
        | Ok (v,_) -> ok v
        | Bad errs -> Bad (f errs)

    /// Lifts a function into a CResult and applies it on the given CResult.
    /// This is the infix operator version of ErrorHandling.lift
    let inline (<!>) f CResult = lift f CResult

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = f <!> a <*> b

    /// If the CResult is a Success it executes the given success function on the value and the messages.
    /// If the CResult is a Failure it executes the given failure function on the messages.
    /// CResult is propagated unchanged.
    let inline eitherTee fSuccess fFailure CResult =
        let inline tee f x = f x; x;
        tee (either fSuccess fFailure) CResult

    /// If the CResult is a Success it executes the given function on the value and the messages.
    /// CResult is propagated unchanged.
    let inline successTee f CResult = 
        eitherTee f ignore CResult

    /// If the CResult is a Failure it executes the given function on the messages.
    /// CResult is propagated unchanged.
    let inline failureTee f CResult = 
        eitherTee ignore f CResult

    /// Collects a sequence of CResults and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    let inline collect xs = 
        Seq.fold (fun CResult next -> 
            match CResult, next with
            | Ok(rs, m1), Ok(r, m2) -> Ok(r :: rs, m1 @ m2)
            | Ok(_, m1), Bad(m2) | Bad(m1), Ok(_, m2) -> Bad(m1 @ m2)
            | Bad(m1), Bad(m2) -> Bad(m1 @ m2)) (ok []) xs
        |> lift List.rev

    /// Converts an option into a CResult.
    let inline failIfNone message CResult = 
        match CResult with
        | Some x -> ok x
        | None -> fail message

    /// Converts a Choice into a CResult.
    let inline ofChoice choice =
        match choice with
        | Choice1Of2 v -> ok v
        | Choice2Of2 v -> fail v

    /// Categorizes a CResult based on its state and the presence of extra messages
    let inline (|Pass|Warn|Fail|) CResult =
      match CResult with
      | Ok  (value, []  ) -> Pass  value
      | Ok  (value, msgs) -> Warn (value,msgs)
      | Bad        msgs  -> Fail        msgs

    let inline failOnWarnings CResult =
      match CResult with
      | Warn (_,msgs) -> Bad msgs
      | _             -> CResult 

    /// Builder type for error handling computation expressions.
    type TrialBuilder() = 
        member __.Zero() = ok()
        member __.Bind(m, f) = bind f m
        member __.Return(x) = ok x
        member __.ReturnFrom(x) = x
        member __.Combine (a, b) = bind b a
        member __.Delay f = f
        member __.Run f = f ()
        member __.TryWith (body, handler) =
            try
                body()
            with
            | e -> handler e
        member __.TryFinally (body, compensation) =
            try
                body()
            finally
                compensation()
        member x.Using(d:#IDisposable, body) =
            let CResult = fun () -> body d
            x.TryFinally (CResult, fun () ->
                match d with
                | null -> ()
                | d -> d.Dispose())
        member x.While (guard, body) =
            if not <| guard () then
                x.Zero()
            else
                bind (fun () -> x.While(guard, body)) (body())
        member x.For(s:seq<_>, body) =
            x.Using(s.GetEnumerator(), fun enum ->
                x.While(enum.MoveNext,
                    x.Delay(fun () -> body enum.Current)))

    /// Wraps computations in an error handling computation expression.
    let trial = TrialBuilder()

/// Represents the CResult of an async computation
[<NoComparison;NoEquality>]
type AsyncCResult<'a, 'b> = 
    | AR of Async<CResult<'a, 'b>>

/// Useful functions for combining error handling computations with async computations.
[<AutoOpen>]
module AsyncExtensions = 
    /// Useful functions for combining error handling computations with async computations.
    [<RequireQualifiedAccess>]
    module Async = 
        /// Creates an async computation that return the given value
        let singleton value = value |> async.Return

        /// Creates an async computation that runs a computation and
        /// when it generates a CResult run a binding function on the said CResult
        let bind f x = async.Bind(x, f)

        /// Creates an async computation that runs a mapping function on the CResult of an async computation
        let map f x = x |> bind (f >> singleton)

        /// Creates an async computation from an asyncTrial computation
        let ofAsyncCResult (AR x) = x

/// Basic support for async error handling computation
[<AutoOpen>]
module AsyncTrial = 
    /// Builder type for error handling in async computation expressions.
    type AsyncTrialBuilder() = 
        member __.Return value : AsyncCResult<'a, 'b> = 
            value
            |> ok
            |> Async.singleton
            |> AR
        
        member __.ReturnFrom(asyncCResult : AsyncCResult<'a, 'b>) = asyncCResult
        member this.Zero() : AsyncCResult<unit, 'b> = this.Return()
        member __.Delay(generator : unit -> AsyncCResult<'a, 'b>) : AsyncCResult<'a, 'b> = 
            async.Delay(generator >> Async.ofAsyncCResult) |> AR
        
        member __.Bind(asyncCResult : AsyncCResult<'a, 'c>, binder : 'a -> AsyncCResult<'b, 'c>) : AsyncCResult<'b, 'c> = 
            let fSuccess (value, msgs) = 
                value |> (binder
                          >> Async.ofAsyncCResult
                          >> Async.map (mergeMessages msgs))
            
            let fFailure errs = 
                errs
                |> Bad
                |> Async.singleton
            
            asyncCResult
            |> Async.ofAsyncCResult
            |> Async.bind (either fSuccess fFailure)
            |> AR
        
        member this.Bind(CResult : CResult<'a, 'c>, binder : 'a -> AsyncCResult<'b, 'c>) : AsyncCResult<'b, 'c> = 
            this.Bind(CResult
                      |> Async.singleton
                      |> AR, binder)
        
        member __.Bind(async : Async<'a>, binder : 'a -> AsyncCResult<'b, 'c>) : AsyncCResult<'b, 'c> = 
            async
            |> Async.bind (binder >> Async.ofAsyncCResult)
            |> AR
        
        member __.TryWith(asyncCResult : AsyncCResult<'a, 'b>, catchHandler : exn -> AsyncCResult<'a, 'b>) : AsyncCResult<'a, 'b> = 
            async.TryWith(asyncCResult |> Async.ofAsyncCResult, (catchHandler >> Async.ofAsyncCResult)) |> AR
        member __.TryFinally(asyncCResult : AsyncCResult<'a, 'b>, compensation : unit -> unit) : AsyncCResult<'a, 'b> = 
            async.TryFinally(asyncCResult |> Async.ofAsyncCResult, compensation) |> AR
        member __.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> AsyncCResult<'a, 'b>) : AsyncCResult<'a, 'b> = 
            async.Using(resource, (binder >> Async.ofAsyncCResult)) |> AR
    
    // Wraps async computations in an error handling computation expression.
    let asyncTrial = AsyncTrialBuilder()

namespace Chessie.ErrorHandling.CSharp

open System
open System.Runtime.CompilerServices
open Chessie.ErrorHandling

/// Extensions methods for easier C# usage.
[<Extension>]
type CResultExtensions () =
    /// Allows pattern matching on CResults from C#.
    [<Extension>]
    static member inline Match(this, ifSuccess:Action<'TSuccess , ('TMessage list)>, ifFailure:Action<'TMessage list>) =
        match this with
        | CResult.Ok(x, msgs) -> ifSuccess.Invoke(x,msgs)
        | CResult.Bad(msgs) -> ifFailure.Invoke(msgs)
    
    /// Allows pattern matching on CResults from C#.
    [<Extension>]
    static member inline Either(this, ifSuccess:Func<'TSuccess , ('TMessage list),'TCResult>, ifFailure:Func<'TMessage list,'TCResult>) =
        match this with
        | CResult.Ok(x, msgs) -> ifSuccess.Invoke(x,msgs)
        | CResult.Bad(msgs) -> ifFailure.Invoke(msgs)

    /// Lifts a Func into a CResult and applies it on the given CResult.
    [<Extension>]
    static member inline Map(this:CResult<'TSuccess, 'TMessage>,func:Func<_,_>) =
        lift func.Invoke this

    /// Collects a sequence of CResults and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    [<Extension>]
    static member inline Collect(values:seq<CResult<'TSuccess, 'TMessage>>) =
        collect values

    /// Collects a sequence of CResults and accumulates their values.
    /// If the sequence contains an error the error will be propagated.
    [<Extension>]
    static member inline Flatten(this) : CResult<seq<'TSuccess>,'TMessage>=
        match this with
        | CResult.Ok(values:CResult<'TSuccess,'TMessage> seq, _msgs:'TMessage list) -> 
            match collect values with
            | CResult.Ok(values,msgs) -> Ok(values |> List.toSeq,msgs)
            | CResult.Bad(msgs:'TMessage list) -> Bad msgs
        | CResult.Bad(msgs:'TMessage list) -> Bad msgs

    /// If the CResult is a Success it executes the given Func on the value.
    /// Otherwise the exisiting failure is propagated.
    [<Extension>]
    static member inline SelectMany (this:CResult<'TSuccess, 'TMessage>, func: Func<_,_>) =
        bind func.Invoke this

    /// If the CResult is a Success it executes the given Func on the value.
    /// If the CResult of the Func is a Success it maps it using the given Func.
    /// Otherwise the exisiting failure is propagated.
    [<Extension>]
    static member inline SelectMany (this:CResult<'TSuccess, 'TMessage>, func: Func<_,_>, mapper: Func<_,_,_>) =
        bind (fun s -> s |> func.Invoke |> lift (fun v -> mapper.Invoke(s,v))) this

    /// Lifts a Func into a CResult and applies it on the given CResult.
    [<Extension>]
    static member inline Select (this:CResult<'TSuccess, 'TMessage>, func: Func<_,_>) = lift func.Invoke this

    /// Returns the error messages or fails if the CResult was a success.
    [<Extension>]
    static member inline FailedWith(this:CResult<'TSuccess, 'TMessage>) = 
        match this with
        | CResult.Ok(v,msgs) -> failwithf "CResult was a success: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
        | CResult.Bad(msgs) -> msgs

    /// Returns the CResult or fails if the CResult was an error.
    [<Extension>]
    static member inline SucceededWith(this:CResult<'TSuccess, 'TMessage>) : 'TSuccess = 
        match this with
        | CResult.Ok(v,_msgs) -> v
        | CResult.Bad(msgs) -> failwithf "CResult was an error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))

    /// Joins two CResults. 
    /// If both are a success the CResultSelector Func is applied to the values and the existing success messages are propagated.
    /// Otherwise the exisiting error messages are propagated.
    [<Extension>]
    static member inline Join (this: CResult<'TOuter, 'TMessage>, inner: CResult<'TInner, 'TMessage>, _outerKeySelector: Func<'TOuter,'TKey>, _innerKeySelector: Func<'TInner, 'TKey>, CResultSelector: Func<'TOuter, 'TInner, 'TCResult>) =
        let curry func = fun a b -> func (a, b)
        curry CResultSelector.Invoke
        <!> this 
        <*> inner

    /// Converts an option into a CResult.
    [<Extension>]
    static member ToCResult(this, msg) =
        this |> failIfNone msg

    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded.
    [<Extension>]
    static member inline MapFailure (this: CResult<'TSuccess, 'TMessage>, f: Func<'TMessage list, 'TMessage2 seq>) =
        this |> Trial.mapFailure (f.Invoke >> Seq.toList)
