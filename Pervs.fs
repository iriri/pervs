// Basially F#+ but worse. Also inspired by cannorin's prelude. Very impure,
// and in practice used to write the moral equivalent of bad GC'd Rust
module Pervs

type u8    = uint8
type u16   = uint16
type u32   = uint32
type u64   = uint64
type usize = unativeint
type i8    = int8
type i16   = int16
type i32   = int32
type i64   = int64
type isize = nativeint
type f32   = single
type f64   = double

type VOption<'a> = ValueOption<'a>
type Array<'a>   = 'a[]
#if !FABLE_COMPILER
type Vec<'a>     = System.Collections.Immutable.ImmutableList<'a>
#endif
type Seq<'a>     = seq<'a>

type VSeq<'a, 'e when 'e :> System.Collections.Generic.IEnumerator<'a>> =
   abstract member GetEnumerator : unit -> 'e

type IEnumerator<'a>                    = System.Collections.Generic.IEnumerator<'a>
type MVec<'a>                           = ResizeArray<'a>
#if !FABLE_COMPILER
type Span<'a>                           = System.ReadOnlySpan<'a>
type MSpan<'a>                          = System.Span<'a>
#endif
type KeyValuePair<'k, 'v>               = System.Collections.Generic.KeyValuePair<'k, 'v>
type HashSet<'k>                        = System.Collections.Generic.HashSet<'k>
type HashMap<'k, 'v when 'k : not null> = System.Collections.Generic.Dictionary<'k, 'v>
type String                             = string

module Lazy =
   let inline force (x : Lazy<_>) = x.Force ()

   let inline ofThunk f = Lazy.Create f
   let inline toThunk x = fun () -> force x
#if !FABLE_COMPILER
   let inline ofAsync x = lazy (Async.RunSynchronously x)
#endif
   let inline toAsync x = x |> toThunk >> async.Return |> async.Delay

   let inline map ([<InlineIfLambda>] f) x  = lazy (x |> force |> f)
   let inline zip x y                       = lazy (force x, force y)
   let inline apply f x                     = lazy (force f (force x))
   let inline flatten x                     = lazy (x |> force |> force)
   let inline bind ([<InlineIfLambda>] f) x = lazy (x |> force |> f |> force)

module Async =
   let inline map ([<InlineIfLambda>] f) x = async.Bind (x, f >> async.Return)
#if !FABLE_COMPILER
   let inline wait x                       = Async.RunSynchronously x
#endif

   let inline never ()  = Async.Sleep -1 |> map (fun _ -> Unchecked.defaultof<_>)
   let        ofValue   = async.Return
   let inline ofThunk f = f >> async.Return |> async.Delay
#if !FABLE_COMPILER
   let inline toThunk x = fun () -> wait x
#endif
   let        ofLazy    = Lazy.toAsync
#if !FABLE_COMPILER
   let        toLazy    = Lazy.ofAsync
#endif

#if !FABLE_COMPILER
   let inline alt x y = [|map Some x; map Some y|] |> Async.Choice |> map Option.get
#endif

   let inline zip (x : Async<^a>) (y : Async<^b>) : Async<^a * ^b> =
      [|map box x; map box y|] |> Async.Parallel |> map (fun xy -> unbox xy.[0], unbox xy.[1])

   let inline apply f x                     = zip f x |> map (fun (g, a) -> g a)
   let inline flatten x                     = async.Bind (x, id)
   let inline bind ([<InlineIfLambda>] f) x = async.Bind (x, f)

module Option =
   let inline wrap x = Some x

   let inline ofBool x  = if x then Some () else None
   let        ofVOption = Option.ofValueOption
   let        toVOption = Option.toValueOption

   let inline ofChoice x =
      match x with
      | Choice1Of2 a -> Some a
      | Choice2Of2 _ -> None

   let inline toChoice x =
      match x with
      | Some a -> Choice1Of2 a
      | None   -> Choice2Of2 ()

   let inline ofResult x =
      match x with
      | Ok a    -> Some a
      | Error _ -> None

   let inline toResult x =
      match x with
      | Some a -> Ok a
      | None   -> Error ()

   let inline ofTuple x =
      match x with
      | true, a  -> Some a
      | false, _ -> None

   let inline toTuple x =
      match x with
      | Some a -> true, a
      | None   -> false, Unchecked.defaultof<_>

   let inline alt x y =
      match x with
      | Some _ -> x
      | None   -> y

   let inline zip x y =
      match x, y with
      | Some a, Some b -> Some (a, b)
      | _              -> None

   let inline unzip x =
      match x with
      | Some (a, b) -> Some a, Some b
      | _           -> None, None

   let inline apply f' x =
      match f', x with
      | Some f, Some a -> Some (f a)
      | _              -> None

#if !FABLE_COMPILER
module VOption =
   let inline ofBool x = if x then ValueSome () else ValueNone

   let ofOption   = ValueOption.ofOption
   let toOption   = ValueOption.toOption
   let toList     = ValueOption.toList
   let toArray    = ValueOption.toArray
   let ofNullable = ValueOption.ofNullable
   let toNullable = ValueOption.toNullable

   let inline ofChoice x =
      match x with
      | Choice1Of2 a -> ValueSome a
      | Choice2Of2 _ -> ValueNone

   let inline toChoice x =
      match x with
      | ValueSome a -> Choice1Of2 a
      | ValueNone   -> Choice2Of2 ()

   let inline ofResult x =
      match x with
      | Ok a    -> ValueSome a
      | Error _ -> ValueNone

   let inline toResult x =
      match x with
      | ValueSome a -> Ok a
      | ValueNone   -> Error ()

   let inline ofTuple x =
      match x with
      | true, a  -> ValueSome a
      | false, _ -> ValueNone

   let inline toTuple x =
      match x with
      | ValueSome a -> true, a
      | ValueNone   -> false, Unchecked.defaultof<_>

   let isSome   = ValueOption.isSome
   let isNone   = ValueOption.isNone
   let get      = ValueOption.get
   let contains = ValueOption.contains

   let inline alt x y =
      match x with
      | ValueSome _ -> x
      | ValueNone   -> y

   let map  = ValueOption.map
   let iter = ValueOption.iter
   let map2 = ValueOption.map2
   let map3 = ValueOption.map3

   let inline zip x y =
      match x, y with
      | ValueSome a, ValueSome b -> ValueSome (a, b)
      | _                        -> ValueNone

   let inline unzip x =
      match x with
      | ValueSome (a, b) -> ValueSome a, ValueSome b
      | _                -> ValueNone, ValueNone

   let inline apply f' x =
      match f', x with
      | ValueSome f, ValueSome a -> ValueSome (f a)
      | _                        -> ValueNone

   let flatten      = ValueOption.flatten
   let bind         = ValueOption.bind
   let defaultValue = ValueOption.defaultValue
   let defaultWith  = ValueOption.defaultWith
   let orElse       = ValueOption.orElse
   let orElseWith   = ValueOption.orElseWith
   let filter       = ValueOption.filter
   let fold         = ValueOption.fold
   let foldBack     = ValueOption.foldBack
   let exists       = ValueOption.exists
   let forall       = ValueOption.forall
#endif

module Choice =
   let inline ofBool x  = if x then Choice1Of2 () else Choice2Of2 ()
   let        ofOption  = Option.toChoice
   let        toOption  = Option.ofChoice
   let        ofVOption = VOption.toChoice
   let        toVOption = VOption.ofChoice

   let inline get x =
      match x with
      | Choice1Of2 a -> a
      | Choice2Of2 b -> invalidArg "source" (sprintf "Value passed in was Choice2Of2: %O" b)

   let inline ofResult x =
      match x with
      | Ok a    -> Choice1Of2 a
      | Error e -> Choice2Of2 e

   let inline toResult x =
      match x with
      | Choice1Of2 a -> Ok a
      | Choice2Of2 b -> Error b

   let isChoice1Of2 (x : Choice<_, _>) = x.IsChoice1Of2
   let isChoice2Of2 (x : Choice<_, _>) = x.IsChoice2Of2

   let inline defaultValue b x =
      match x with
      | Choice1Of2 a -> a
      | Choice2Of2 _ -> b

   let inline defaultWith ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> a
      | Choice2Of2 _ -> f ()

   let inline either ([<InlineIfLambda>] f) g x =
      match x with
      | Choice1Of2 a -> f a
      | Choice2Of2 b -> g b

   let inline map ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> Choice1Of2 (f a)
      | Choice2Of2 b -> Choice2Of2 b

   let inline iter ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> f a
      | Choice2Of2 _ -> ()

   let inline zip x y =
      match x, y with
      | Choice1Of2 a, Choice1Of2 c        -> Choice1Of2 (a, c)
      | Choice2Of2 b, _ | _, Choice2Of2 b -> Choice2Of2 b

   let inline unzip x =
      match x with
      | Choice1Of2 (a, c) -> Choice1Of2 a, Choice1Of2 c
      | Choice2Of2 b      -> Choice2Of2 b, Choice2Of2 b

   let inline apply f x =
      match f, x with
      | Choice1Of2 g, Choice1Of2 a        -> Choice1Of2 (g a)
      | Choice2Of2 b, _ | _, Choice2Of2 b -> Choice2Of2 b

   let inline flatten x =
      match x with
      | Choice1Of2 a -> a
      | Choice2Of2 b -> Choice2Of2 b

   let inline bind ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> f a
      | Choice2Of2 b -> Choice2Of2 b

   let inline map2Of2 ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> Choice1Of2 a
      | Choice2Of2 b -> Choice2Of2 (f b)

   let inline iter2Of2 ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 _ -> ()
      | Choice2Of2 b -> f b

   let inline bimap ([<InlineIfLambda>] f) g x =
      match x with
      | Choice1Of2 a -> Choice1Of2 (f a)
      | Choice2Of2 b -> Choice2Of2 (g b)

   let inline bind2Of2 ([<InlineIfLambda>] f) x =
      match x with
      | Choice1Of2 a -> Choice1Of2 a
      | Choice2Of2 b -> f b

   let inline fold ([<InlineIfLambda>] f) acc x =
      match x with
      | Choice1Of2 a -> f acc a
      | Choice2Of2 _ -> acc

module Result =
   let inline ofBool x  = if x then Ok () else Error ()
   let        ofOption  = Option.toResult
   let        toOption  = Option.ofResult
   let        ofVOption = VOption.toResult
   let        toVOption = VOption.ofResult
   let        ofChoice  = Choice.toResult
   let        toChoice  = Choice.ofResult

   let inline get x =
      match x with
      | Ok a -> a
      | Error e -> invalidArg "source" (sprintf "Value passed in was Error: %O" e)

   let inline defaultWith ([<InlineIfLambda>] f) x =
      match x with
      | Ok a    -> a
      | Error _ -> f ()

   let inline either ([<InlineIfLambda>] f) g x =
      match x with
      | Ok a    -> f a
      | Error e -> g e

   let inline iter ([<InlineIfLambda>] f) x =
      match x with
      | Ok a    -> f a
      | Error _ -> ()

   let inline zip x y =
      match x, y with
      | Ok a, Ok b              -> Ok (a, b)
      | Error e, _ | _, Error e -> Error e

   let inline unzip x =
      match x with
      | Ok (a, b) -> Ok a, Ok b
      | Error e   -> Error e, Error e

   let inline apply f x =
      match f, x with
      | Ok g, Ok a              -> Ok (g a)
      | Error e, _ | _, Error e -> Error e

   let inline flatten x =
      match x with
      | Ok a    -> a
      | Error e -> Error e

   let inline iterError ([<InlineIfLambda>] f) x =
      match x with
      | Ok  _   -> ()
      | Error e -> f e

   let inline bimap ([<InlineIfLambda>] f) g x =
      match x with
      | Ok a    -> Ok (f a)
      | Error e -> Error (g e)

   let inline bindError ([<InlineIfLambda>] f) x =
      match x with
      | Ok a    -> Ok a
      | Error e -> f e

   let inline fold ([<InlineIfLambda>] f) acc x =
      match x with
      | Ok a    -> f acc a
      | Error _ -> acc

module Seq =
   type [<Struct>] VSeq<'a> (xs : Seq<'a>) =
      interface VSeq<'a, IEnumerator<'a>> with
         member _.GetEnumerator () = xs.GetEnumerator ()

   let inline ofVSeq (xs : #VSeq<'a, _>) = {
      new Seq<'a> with
         member _.GetEnumerator () : IEnumerator<'a>                = xs.GetEnumerator ()
         member _.GetEnumerator () : System.Collections.IEnumerator = xs.GetEnumerator () }

   let inline toVSeq xs = VSeq xs

   let inline apply fs (xs : #Seq<_>) = fs |> Seq.collect (fun f -> Seq.map f xs)

   let inline loop f acc (xs : #Seq<_>) =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      use xs                 = xs.GetEnumerator ()
      while again && xs.MoveNext () do
         let acc', again' = f.Invoke (acc, xs.Current)
         acc <- acc'
         again <- again'
      acc

   let inline loopBack f (xs : #Seq<_>) acc =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let xs                 = xs |> Array.ofSeq |> Array.rev
      let mutable i          = 0
      while again && i < xs.Length do
         let acc', again' = f.Invoke (xs.[i], acc)
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

module VSeq' =
   type [<Struct>] UnfoldEnumerator<'a, 'b> =
      val private f : 'a -> VOption<struct('b * 'a)>
      val mutable private state : 'a
      val mutable private x : 'b
      val private init : 'a

      new (f, init) = { f = f; state = init; x = Unchecked.defaultof<_>; init = init }

      interface IEnumerator<'b> with
         member this.Current = this.x
         member this.Current = box this.x

         member this.MoveNext () =
            match this.f this.state with
            | ValueSome (x, s) ->
               this.x <- x
               this.state <- s
               true
            | ValueNone -> false

         member this.Reset () = this.state <- this.init
         member _.Dispose ()  = ()

   type [<Struct>] Unfold<'a, 'b> (f : 'a -> VOption<struct('b * 'a)>, init : 'a) =
      interface VSeq<'b, UnfoldEnumerator<'a, 'b>> with
         member _.GetEnumerator () = new UnfoldEnumerator<_, _> (f, init)

   type [<Struct>] AscendingEnumerator =
      val mutable private m : int
      val private n : int

      new n = { m = n - 1; n = n - 1 }

      interface IEnumerator<int> with
         member this.Current = this.m
         member this.Current = box this.m

         member this.MoveNext () =
            this.m <- this.m + 1
            true

         member this.Reset () = this.m <- this.n
         member _.Dispose ()  = ()

   type [<Struct>] Ascending (n : int) =
      interface VSeq<int, AscendingEnumerator> with
         member _.GetEnumerator () = new AscendingEnumerator (n)

   type [<Struct>] TakeEnumerator<'a, 'e when 'e :> IEnumerator<'a>> =
      val mutable private m : int
      val private n : int
      val mutable private e : 'e

      new (n, e) = { m = n; n = n; e = e }

      interface IEnumerator<'a> with
         member this.Current = this.e.Current
         member this.Current = box this.e.Current

         member this.MoveNext () =
            if this.m <= 0 then false else
               this.m <- this.m - 1
               this.e.MoveNext ()

         member this.Reset () =
            this.m <- this.n
            this.e.Reset ()

         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] Take<'a, 's, 'e when
      's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>> (n : int, xs: 's) =
         interface VSeq<'a, TakeEnumerator<'a, 'e>> with
            member _.GetEnumerator () = new TakeEnumerator<_, _> (n, xs.GetEnumerator ())

   type [<Struct>] TakeWhileEnumerator<'a, 'e when 'e :> IEnumerator<'a>> =
      val mutable private done' : bool
      val mutable private e : 'e
      val private p : 'a -> bool

      new(e, p) = { done' = false; e = e; p = p }

      interface IEnumerator<'a> with
         member this.Current = this.e.Current
         member this.Current = box this.e.Current

         member this.MoveNext () =
            if this.done' then false
            elif this.e.MoveNext () && this.p this.e.Current then true else
               this.done' <- true
               false

         member this.Reset () =
            this.done' <- false
            this.e.Reset ()

         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] TakeWhile<'a, 's, 'e when
      's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>> (xs: 's, p: 'a -> bool) =
         interface VSeq<'a, TakeWhileEnumerator<'a, 'e>> with
            member _.GetEnumerator () = new TakeWhileEnumerator<_, _> (xs.GetEnumerator (), p)

   type [<Struct>] SkipEnumerator<'a, 'e when 'e :> IEnumerator<'a>> =
      val mutable private m : int
      val private n : int
      val mutable private e : 'e

      new (n, e) = { m = n; n = n; e = e }

      interface IEnumerator<'a> with
         member this.Current = this.e.Current
         member this.Current = box this.e.Current

         member this.MoveNext () =
            while this.m > 0 && this.e.MoveNext () do
               this.m <- this.m - 1
            if this.m = 0 then this.e.MoveNext () else false

         member this.Reset () =
            this.m <- this.n
            this.e.Reset ()

         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] Skip<'a, 's, 'e when
      's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>> (n : int, xs: 's) =
         interface VSeq<'a, SkipEnumerator<'a, 'e>> with
            member _.GetEnumerator () = new SkipEnumerator<_, _> (n, xs.GetEnumerator ())

   type [<Struct>] SkipWhileEnumerator<'a, 'e when 'e :> IEnumerator<'a>> =
      val mutable private done' : bool
      val mutable private e : 'e
      val private p : 'a -> bool

      new(e, p) = { done' = false; e = e; p = p }

      interface IEnumerator<'a> with
         member this.Current = this.e.Current
         member this.Current = box this.e.Current

         member this.MoveNext () =
            if this.done' then this.e.MoveNext () else
               let mutable ok = true
               while not this.done' do
                  if not (this.e.MoveNext ()) then
                     ok <- false
                     this.done' <- true
                  elif not (this.p this.e.Current) then
                     this.done' <- true
               ok

         member this.Reset () =
            this.done' <- false
            this.e.Reset ()

         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] SkipWhile<'a, 's, 'e when
      's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>> (xs: 's, p: 'a -> bool) =
         interface VSeq<'a, SkipWhileEnumerator<'a, 'e>> with
            member _.GetEnumerator () = new SkipWhileEnumerator<_, _> (xs.GetEnumerator (), p)

   type [<Struct>] MapEnumerator<'a, 'b, 'e when 'e :> IEnumerator<'a>> =
      val mutable private e : 'e
      val private f : 'a -> 'b

      new (e, f) = { e = e; f = f }

      interface IEnumerator<'b> with
         member this.Current = this.f this.e.Current
         member this.Current = box (this.f this.e.Current)

         member this.MoveNext () = this.e.MoveNext ()

         member this.Reset ()   = this.e.Reset ()
         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] Map<'a, 'b, 's, 'e when
      's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>> (xs : 's, f : 'a -> 'b) =
         interface VSeq<'b, MapEnumerator<'a, 'b, 'e>> with
            member _.GetEnumerator () = new MapEnumerator<_, _, _> (xs.GetEnumerator (), f)

   type [<Struct>] ZipEnumerator<'a, 'b, 'e, 'f when
      'e :> IEnumerator<'a> and 'f :> IEnumerator<'b>> =
         val mutable private e : 'e
         val mutable private f : 'f

         new (e, f) = { e = e; f = f }

         interface IEnumerator<struct('a * 'b)> with
            member this.Current = struct(this.e.Current, this.f.Current)
            member this.Current = box struct(this.e.Current, this.f.Current)

            member this.MoveNext () = this.e.MoveNext () && this.f.MoveNext ()

            member this.Reset () =
               this.e.Reset ()
               this.f.Reset ()

            member this.Dispose () =
               this.e.Dispose ()
               this.f.Dispose ()

   type [<Struct>] Zip<'a, 'b, 's, 't, 'e, 'f when
      's :> VSeq<'a, 'e> and
      't :> VSeq<'b, 'f> and
      'e :> IEnumerator<'a> and
      'f :> IEnumerator<'b>> (xs : 's, ys : 't) =
         interface VSeq<struct('a * 'b), ZipEnumerator<'a, 'b, 'e, 'f>> with
            member _.GetEnumerator () =
               new ZipEnumerator<_, _, _, _> (xs.GetEnumerator (), ys.GetEnumerator ())

   type [<Struct>] FilterEnumerator<'a, 'e when 'e :> IEnumerator<'a>> =
      val mutable private e : 'e
      val private p : 'a -> bool

      new(e, p) = { e = e; p = p }

      interface IEnumerator<'a> with
         member this.Current = this.e.Current
         member this.Current = box this.e.Current

         member this.MoveNext () =
            let mutable ok = this.e.MoveNext ()
            while ok && not (this.p this.e.Current) do
               ok <- this.e.MoveNext ()
            ok

         member this.Reset ()   = this.e.Reset ()
         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] Filter<'a, 's, 'e when 's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>>(
      xs : 's, p: 'a -> bool) =
         interface VSeq<'a, FilterEnumerator<'a, 'e>> with
            member _.GetEnumerator () = new FilterEnumerator<_, _> (xs.GetEnumerator (), p)

   type [<Struct>] ChooseEnumerator<'a, 'b, 'e when 'e :> IEnumerator<'a>> =
      val mutable private e : 'e
      val mutable private x : 'b
      val private f : 'a -> VOption<'b>

      new(e, f) = { e = e; x = Unchecked.defaultof<_>; f = f }

      interface IEnumerator<'b> with
         member this.Current = this.x
         member this.Current = box this.x

         member this.MoveNext () =
            let mutable ok, again = this.e.MoveNext (), true
            while ok && again do
               match this.f this.e.Current with
               | ValueSome x ->
                  this.x <- x
                  again <- false
               | ValueNone -> ok <- this.e.MoveNext ()
            ok

         member this.Reset ()   = this.e.Reset ()
         member this.Dispose () = this.e.Dispose ()

   type [<Struct>] Choose<'a, 'b, 's, 'e when 's :> VSeq<'a, 'e> and 'e :> IEnumerator<'a>>(
      xs : 's, f: 'a -> VOption<'b>) =
         interface VSeq<'b, ChooseEnumerator<'a, 'b, 'e>> with
            member _.GetEnumerator () = new ChooseEnumerator<_, _, _> (xs.GetEnumerator (), f)

   let inline unfold f init = Unfold (f, init)
   let inline ascending n   = Ascending n

   let inline take n xs      = Take (n, xs)
   let inline takeWhile p xs = TakeWhile (xs, p)
   let inline skip n xs      = Skip (n, xs)
   let inline skipWhile p xs = SkipWhile (xs, p)

   let inline map f xs   = Map<_, _, _, _> (xs, f)
   let inline zip xs ys  = Zip (xs, ys)
   let inline indexed xs = Zip (ascending 0, xs)

   let inline filter f xs = Filter (xs, f)
   let inline choose f xs = Choose (xs, f)

   let inline length (xs : #VSeq<_, _>) =
      let mutable n = 0
      use mutable e = xs.GetEnumerator ()
      while e.MoveNext () do n <- n + 1
      n

   let inline iter f (xs : #VSeq<_, _>) =
      use mutable e = xs.GetEnumerator ()
      while e.MoveNext () do f e.Current

   let inline fold f acc (xs : #VSeq<_, _>) =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      use mutable e   = xs.GetEnumerator ()
      while e.MoveNext () do acc <- f.Invoke (acc, e.Current)
      acc

   let inline loop f acc (xs : #VSeq<_, _>) =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      use mutable e          = xs.GetEnumerator ()
      while again && e.MoveNext () do
         let acc', again' = f.Invoke (acc, e.Current)
         acc <- acc'
         again <- again'
      acc

   let inline forall p (xs : #VSeq<_, _>) =
      let mutable ok = true
      use mutable e  = xs.GetEnumerator ()
      while ok && e.MoveNext () do
         ok <- p e.Current
      ok

   let inline exists p (xs : #VSeq<_, _>) =
      let mutable ok = false
      use mutable e  = xs.GetEnumerator ()
      while not ok && e.MoveNext () do
         ok <- p e.Current
      ok

#nowarn 1204
   let inline head (xs : #VSeq<_, _>) =
      use mutable e = xs.GetEnumerator ()
      if e.MoveNext () then e.Current else
         invalidArg "source" FSharp.Core.LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
#warnon 1204

   let inline tryHead (xs : #VSeq<_, _>) =
      use mutable e = xs.GetEnumerator ()
      if e.MoveNext () then Some e.Current else None

   let inline tail xs = skip 1 xs

module List =
   type [<Struct>] VSeq<'a> (xs : List<'a>) =
      interface VSeq<'a, IEnumerator<'a>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline ofVSeq xs = xs |> Seq.ofVSeq |> List.ofSeq
   let inline toVSeq xs = VSeq xs

   let inline add x xs    = x :: xs
   let inline apply fs xs = fs |> List.collect (fun f -> List.map f xs)

   let inline loop f acc (xs : List<_>) =
      let f             = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let rec loop' acc = function
         | []      -> acc
         | x :: xs -> loop' (f.Invoke (acc, x)) xs
      loop' acc xs

   let inline loopBack f (xs : List<_>) acc = Seq.loopBack f xs acc

#if !FABLE_COMPILER
type System.ReadOnlySpan<'a> with
   member inline this.GetSlice (off, end') =
      match off, end' with
      | None, None          -> this
      | Some off, None      -> this.Slice off
      | None, Some end'     -> this.Slice (0, end' + 1)
      | Some off, Some end' -> this.Slice (off, end' - off + 1)

module Span =
   let inline ofArray (xs : _[])      = Span xs
   let inline toArray (xs : Span<_>)  = xs.ToArray ()
   let inline ofMSpan (xs : MSpan<_>) = MSpan.op_Implicit xs

   let inline length (xs : Span<_>) = xs.Length
   let inline isEmpty xs            = length xs = 0
   let inline item i (xs : Span<_>) = xs.[i]
   let inline tryItem i xs          = if i < length xs then Some xs.[i] else None
   let inline head (xs : Span<_>)   = xs.[0]
   let inline tail (xs : Span<_>)   = xs.[1..]
   let inline tryHead xs            = if isEmpty xs then None else Some (head xs)

   let inline map ([<InlineIfLambda>] f) xs =
      let ys = Array.zeroCreate (length xs)
      for i = 0 to length xs - 1 do ys.[i] <- f xs.[i]
      ys

   let inline iter ([<InlineIfLambda>] f) xs = for i = 0 to length xs - 1 do f xs.[i]

   let inline filter ([<InlineIfLambda>] f) xs =
      let xs' = MVec ()
      for i = 0 to length xs - 1 do if f xs.[i] then xs'.Add xs.[i]
      xs'.ToArray ()

   let inline fold f acc xs =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      for i = 0 to length xs - 1 do acc <- f.Invoke (acc, xs.[i])
      acc

   let inline foldBack f xs acc =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      for i = length xs - 1 downto 0 do acc <- f.Invoke (xs.[i], acc)
      acc

   let inline loop f acc xs =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = 0
      while again && i < length xs do
         let acc', again' = f.Invoke (acc, xs.[i])
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

   let inline loopBack f xs acc =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = length xs - 1
      while again && i >= 0 do
         let acc', again' = f.Invoke (xs.[i], acc)
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

type System.Span<'a> with
   member inline this.GetSlice (off, end') =
      match off, end' with
      | None, None          -> this
      | Some off, None      -> this.Slice off
      | None, Some end'     -> this.Slice (0, end' + 1)
      | Some off, Some end' -> this.Slice (off, end' - off + 1)

module MSpan =
   let inline ofArray (xs : _[])      = MSpan xs
   let inline toArray (xs : MSpan<_>) = xs.ToArray ()
   let inline toSpan xs               = Span.ofMSpan xs

   let inline length (xs : MSpan<_>) = xs.Length
   let inline isEmpty xs             = length xs = 0
   let inline item i (xs : MSpan<_>) = xs.[i]
   let inline tryItem i xs           = if i < length xs then Some xs.[i] else None
   let inline head (xs : MSpan<_>)   = xs.[0]
   let inline tail (xs : MSpan<_>)   = xs.[1..]
   let inline tryHead xs             = if isEmpty xs then None else Some (head xs)

   let inline map ([<InlineIfLambda>] f) xs    = Span.map f (MSpan.op_Implicit xs)
   let inline iter ([<InlineIfLambda>] f) xs   = Span.iter f (MSpan.op_Implicit xs)
   let inline filter ([<InlineIfLambda>] f) xs = Span.filter f (MSpan.op_Implicit xs)
   let inline fold f acc xs                    = Span.fold f acc (MSpan.op_Implicit xs)
   let inline foldBack f xs acc                = Span.foldBack f (MSpan.op_Implicit xs) acc
   let inline loop f acc xs                    = Span.loop f acc (MSpan.op_Implicit xs)
   let inline loopBack f xs acc                = Span.loopBack f (MSpan.op_Implicit xs) acc

   let inline transform ([<InlineIfLambda>] f) xs =
      for i = 0 to length xs - 1 do xs.[i] <- f xs.[i]
#endif

module Array =
   type [<Struct>] VEnumerator<'a> =
      val mutable private i : int
      val private xs : 'a[]

      new xs = { i = -1; xs = xs }

      interface IEnumerator<'a> with
         member this.Current = this.xs[this.i]
         member this.Current = box this.xs[this.i]

         member this.MoveNext () =
            if this.i >= this.xs.Length - 1 then false else
               this.i <- this.i + 1
               true

         member this.Reset () = this.i <- -1
         member _.Dispose ()  = ()

   type [<Struct>] VSeq<'a> (xs : 'a[]) =
      interface VSeq<'a, VEnumerator<'a>> with
         member _.GetEnumerator () = new VEnumerator<_> (xs)

   let inline ofVSeq xs = xs |> Seq.ofVSeq |> Array.ofSeq
   let inline toVSeq xs = VSeq xs

#if !FABLE_COMPILER
   let inline ofSpan xs             = Span.toArray xs
   let inline toSpan xs             = Span.ofArray xs
   let inline ofMSpan xs            = MSpan.toArray xs
   let inline toMSpan xs            = MSpan.ofArray xs
#endif

   let inline apply fs xs               = fs |> Array.collect (fun f -> Array.map f xs)
#if !FABLE_COMPILER
   let inline loop f acc (xs : _[])     = Span.loop f acc (Span.ofArray xs)
   let inline loopBack f (xs : _[]) acc = Span.loopBack f (Span.ofArray xs) acc
#else
   let inline loop f acc (xs : _[]) =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = 0
      while again && i < xs.Length do
         let acc', again' = f.Invoke (acc, xs.[i])
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

   let inline loopBack f (xs : _[]) acc =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = xs.Length - 1
      while again && i >= 0 do
         let acc', again' = f.Invoke (xs.[i], acc)
         acc <- acc'
         again <- again'
         i <- i + 1
      acc
#endif

   let inline transform ([<InlineIfLambda>] f) (xs : _[]) =
      for i = 0 to xs.Length - 1 do xs.[i] <- f xs.[i]

#if !FABLE_COMPILER
module Vec =
   type [<Struct>] VSeq<'a> (xs : Vec<'a>) =
      interface VSeq<'a, IEnumerator<'a>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline empty<^a>          = System.Collections.Immutable.ImmutableList<^a>.Empty
   let inline singleton (x : ^a) = System.Collections.Immutable.ImmutableList.Create x

   let        ofSeq                 = System.Collections.Immutable.ImmutableList.CreateRange
   let inline toSeq (xs : Vec<_>)   = xs :> Seq<_>
   let inline ofVSeq xs             = xs |> Seq.ofVSeq |> ofSeq
   let inline toVSeq xs             = VSeq xs
   let inline ofList (xs : List<_>) = ofSeq xs
   let inline toList (xs : Vec<_>)  = List.ofSeq xs
   let inline ofArray (xs : ^a[])   = System.Collections.Immutable.ImmutableList.Create<^a> xs
   let inline toArray (xs : Vec<_>) = System.Linq.Enumerable.ToArray xs

   let inline length (xs : Vec<_>) = xs.Count
   let inline isEmpty xs           = length xs = 0
   let inline item i (xs : Vec<_>) = xs.[i]
   let inline tryItem i xs         = if i < length xs then Some xs.[i] else None
   let inline head (xs : Vec<_>)   = xs.[0]
   let inline tryHead xs           = if isEmpty xs then None else Some (head xs)
   let inline last (xs : Vec<_>)   = xs.[length xs - 1]

   let inline tryLast xs =
      match length xs with
      | 0 -> None
      | n -> Some xs.[n]

   let inline first (xs : Vec<_>) = xs.RemoveAt (length xs - 1)

   let inline tryFirst xs =
      match length xs with
      | 0 -> None
      | n -> Some (xs.RemoveAt (n - 1))

   let inline add x (xs : Vec<_>)                = xs.Add x
   let inline append (xs : Vec<_>) (ys : Vec<_>) = xs.AddRange ys
   let inline updateAt i x (xs : Vec<_>)         = xs.SetItem (i, x)

   let inline iter (f : _ -> _) (xs : Vec<_>) = xs.ForEach (System.Action<_> f)

   let inline map f (xs : Vec<_>) =
      let b = System.Collections.Immutable.ImmutableList.CreateBuilder ()
      xs |> iter (f >> b.Add)
      b.ToImmutable ()

   let inline filter f (xs : Vec<_>) =
      let b = System.Collections.Immutable.ImmutableList.CreateBuilder ()
      xs |> iter (fun x -> if f x then b.Add x)
      b.ToImmutable ()

   let inline fold f acc (xs : Vec<_>) =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      xs |> iter (fun x -> acc <- f.Invoke (acc, x))
      acc

   let inline foldBack f (xs : Vec<_>) acc = Seq.foldBack f xs acc
   let inline loop f acc (xs : Vec<_>)     = Seq.loop f acc xs
   let inline loopBack f (xs : Vec<_>) acc = Seq.loopBack f xs acc
#endif

module MVec =
   type [<Struct>] VEnumerator<'a> =
      val mutable private i : int
      val private xs : MVec<'a>

      new xs = { i = -1; xs = xs }

      interface IEnumerator<'a> with
         member this.Current = this.xs[this.i]
         member this.Current = box this.xs[this.i]

         member this.MoveNext () =
            if this.i >= this.xs.Count - 1 then false else
               this.i <- this.i + 1
               true

         member this.Reset () = this.i <- -1
         member _.Dispose ()  = ()

   type [<Struct>] VSeq<'a> (xs : MVec<'a>) =
      interface VSeq<'a, VEnumerator<'a>> with
         member _.GetEnumerator () = new VEnumerator<_> (xs)

   let inline empty<^a> : MVec<^a> = MVec ()

   let inline singleton x =
      let xs = MVec 1
      xs.Add x
      xs

   let inline ofSeq (xs : Seq<_>)    = MVec xs
   let inline toSeq (xs : MVec<_>)   = xs :> Seq<_>

   let inline ofVSeq (xs : #VSeq<_, _>) =
      let ys = MVec ()
      xs |> VSeq'.iter (fun x -> ys.Add x)
      ys

   let inline toVSeq xs              = VSeq xs
   let inline ofList (xs : List<_>)  = MVec xs
   let inline toList  (xs : MVec<_>) = Seq.toList xs
   let inline ofArray (xs : _[])     = MVec xs
   let inline toArray (xs : MVec<_>) = xs.ToArray ()
#if !FABLE_COMPILER
   let inline ofSpan (xs : Span<_>) =
      let ys = MVec xs.Length
      System.Collections.Generic.CollectionExtensions.AddRange (ys, xs)
      ys

   let inline toSpan xs =
      MSpan.op_Implicit (System.Runtime.InteropServices.CollectionsMarshal.AsSpan xs)

   let inline ofMSpan xs = ofSpan (MSpan.op_Implicit xs)
   let        toMSpan    = System.Runtime.InteropServices.CollectionsMarshal.AsSpan
#endif

   let inline length (xs : MVec<_>)  = xs.Count
   let inline isEmpty (xs : MVec<_>) = length xs = 0
   let inline item i (xs : MVec<_>)  = xs.[i]
   let inline tryItem i xs           = if i < length xs then Some xs.[i] else None
   let inline head (xs : MVec<_>)    = xs.[0]
   let inline tryHead xs             = if isEmpty xs then None else Some (head xs)
   let inline last (xs : MVec<_>)    = xs.[length xs - 1]

   let inline tryLast xs =
      match length xs with
      | 0 -> None
      | n -> Some xs.[n]

   let inline add x (xs : MVec<_>) =
      xs.Add x
      xs

   let inline append (xs : MVec<_>) (ys : MVec<_>) =
      xs.AddRange ys
      xs

   let inline iter ([<InlineIfLambda>] f) (xs : MVec<_>) = for i = 0 to length xs - 1 do f xs.[i]

   let inline map ([<InlineIfLambda>] f) (xs : MVec<_>) =
      let ys = MVec (length xs)
      xs |> iter (f >> ys.Add)
      ys

   let inline filter ([<InlineIfLambda>] f) (xs : MVec<_>) =
      let xs' = MVec ()
      xs |> iter (fun x -> if f x then xs'.Add x)
      xs'

   let inline fold f acc (xs : MVec<_>) =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      xs |> iter (fun x -> acc <- f.Invoke (acc, x))
      acc

   let inline foldBack f (xs : MVec<_>) acc =
      let f           = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc = acc
      for i = length xs - 1 downto 0 do acc <- f.Invoke (xs.[i], acc)
      acc

   let inline loop f acc (xs : MVec<_>) =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = 0
      while again && i < length xs do
         let acc', again' = f.Invoke (acc, xs.[i])
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

   let inline loopBack f (xs : MVec<_>) acc =
      let f                  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
      let mutable acc, again = acc, true
      let mutable i          = length xs - 1
      while again && i >= 0 do
         let acc', again' = f.Invoke (xs.[i], acc)
         acc <- acc'
         again <- again'
         i <- i + 1
      acc

   let inline transform ([<InlineIfLambda>] f) (xs : MVec<_>) =
      for i = 0 to length xs - 1 do xs.[i] <- f xs.[i]

type System.Collections.Generic.List<'a> with
   member inline this.Push x = this.Add x

   member inline this.TryPeek (x: _ outref) =
      match this.Count with
      | 0 -> false
      | n ->
         x <- this.[n - 1]
         true

   member inline this.TryPop (x: _ outref) =
      match this.Count with
      | 0 -> false
      | n ->
         x <- this.[n - 1]
         this.RemoveAt (n - 1)
         true

module Set =
   type [<Struct>] VSeq<'a when 'a : comparison> (xs : Set<'a>) =
      interface VSeq<'a, IEnumerator<'a>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline ofVSeq xs = xs |> Seq.ofVSeq |> Set.ofSeq
   let inline toVSeq xs = VSeq xs

   let        head       = Set.minElement
   let inline tryHead xs = if Set.isEmpty xs then None else Some (head xs)
   let        last       = Set.maxElement
   let inline tryLast xs = if Set.isEmpty xs then None else Some (last xs)

module Map =
   type private VSeq'<'a, 'e when 'e :> IEnumerator<'a>> = VSeq<'a, 'e>

   type [<Struct>] VSeq<'k, 'v when 'k : comparison> (xs : Map<'k, 'v>) =
      interface VSeq'<KeyValuePair<'k, 'v>, IEnumerator<KeyValuePair<'k, 'v>>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline ofVSeq xs = xs |> Seq.ofVSeq |> Map.ofSeq
   let inline toVSeq xs = VSeq xs

   let        head       = Map.minKeyValue
   let inline tryHead xs = if Map.isEmpty xs then None else Some (head xs)
   let        last       = Map.maxKeyValue
   let inline tryLast xs = if Map.isEmpty xs then None else Some (last xs)

module HashSet =
   type [<Struct>] VSeq<'a> (xs : HashSet<'a>) =
      interface VSeq<'a, IEnumerator<'a>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline empty<^a> : HashSet<^a> = HashSet ()

   let inline singleton x =
      let xs = HashSet ()
      xs.Add x |> ignore
      xs

   let ofSeq (xs : Seq<_>)     = HashSet xs
   let toSeq (xs : HashSet<_>) = xs :> Seq<_>
   let inline ofVSeq xs        = xs |> Seq.ofVSeq |> ofSeq
   let inline toVSeq xs        = VSeq xs

   let inline add x (xs : HashSet<_>) =
      xs.Add x |> ignore
      xs

module HashMap =
   type private VSeq'<'a, 'e when 'e :> IEnumerator<'a>> = VSeq<'a, 'e>

   type [<Struct>] VSeq<'k, 'v when 'k : not null> (xs : HashMap<'k, 'v>) =
      interface VSeq'<KeyValuePair<'k, 'v>, IEnumerator<KeyValuePair<'k, 'v>>> with
         member _.GetEnumerator () = (xs :> Seq<_>).GetEnumerator ()

   let inline empty<^k, ^v when ^k : not null and 'k : equality> : HashMap<^k, ^v> = HashMap ()

   let ofSeq (xs : Seq<_>)     = HashSet xs
   let toSeq (xs : HashSet<_>) = xs :> Seq<_>
   let inline ofVSeq xs        = xs |> Seq.ofVSeq |> ofSeq
   let inline toVSeq xs        = VSeq xs

   let inline add k v (xs : HashMap<_, _>) =
      xs[k] = v |> ignore
      xs

module String =
   type [<Struct>] VEnumerator =
      val mutable private i : int
      val private s : String

      new s = { i = -1; s = s }

      interface IEnumerator<char> with
         member this.Current = this.s[this.i]
         member this.Current = box this.s[this.i]

         member this.MoveNext () =
            if this.i >= this.s.Length - 1 then false else
               this.i <- this.i + 1
               true

         member this.Reset () = this.i <- -1
         member _.Dispose ()  = ()

   type [<Struct>] VSeq (s : String) =
      interface VSeq<char, VEnumerator> with
         member _.GetEnumerator () = new VEnumerator (s)

   let inline ofSeq xs                  = String (Array.ofSeq xs)
   let inline toSeq (s : String)        = s :> Seq<_>
   let inline ofVSeq xs                 = xs |> Seq.ofVSeq |> ofSeq
   let inline toVSeq s                  = VSeq s
   let inline ofArray (cs : char[])     = String cs
   let inline toArray (s : String)      = s.ToCharArray ()
#if !FABLE_COMPILER
   let inline ofSpan (s : Span<char>)   = s.ToString ()
   let inline ofMSpan (s : MSpan<char>) = s.ToString ()
   let inline toSpan s                  = System.MemoryExtensions.AsSpan s
#endif

   let inline isEmpty s            = String.length s = 0
   let inline item i (s : String)  = s.[i]
   let inline tryItem i s          = if i < String.length s then Some s.[i] else None
   let inline head (s : String)    = s.[0]
   let inline tryHead (s : String) = if isEmpty s then None else Some (head s)
   let inline last (s : String)    = s.[s.Length - 1]

   let inline tryLast s =
      match String.length s with
      | 0 -> None
      | n -> Some s.[n]

   let inline append (s : String) t         = s + t
   let inline split (c : char) (s : String) = s.Split (c, System.StringSplitOptions.None)

#nowarn 64
module Typeclasses =
   [<AbstractClass>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Priority1 = class end

   [<AbstractClass>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Priority0 =
      inherit Priority1

   type Workaround (_x : unit) =
      member _.Workaround = ()

   type Workaround<'a> (_x : unit) =
      member _.Workaround = ()

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Thunk =
      static member inline Force ([<InlineIfLambda>] f) = f ()
      static member inline Force x                      = Lazy.force x
#if !FABLE_COMPILER
      static member inline Force x                      = Async.wait x
#endif

      static member inline Invoke x =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Force : _ -> _) x)
         call x Unchecked.defaultof<Thunk>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Tuple =
      static member inline Tup (a, b, _ : (_ * _))                 = a, b
      static member inline Tup (a, b, _ : (struct(_ * _)))         = struct(a, b)
      static member inline Tup3 (a, b, c, _ : (_ * _ * _))         = a, b, c
      static member inline Tup3 (a, b, c, _ : (struct(_ * _ * _))) = struct(a, b, c)

      static member inline InvokeTup a b : ^a =
         let inline call (_ : ^w) =
            ((^a or ^w) : (static member Tup : _ * _ * _ -> _) a, b, Unchecked.defaultof<^a>)
         call Unchecked.defaultof<Tuple>

      static member inline InvokeTup3 a b c : ^a =
         let inline call (_ : ^w) =
            ((^a or ^w) :
               (static member Tup3 : _ * _ * _ * _ -> _) a, b, c, Unchecked.defaultof<^a>)
         call Unchecked.defaultof<Tuple>

      static member inline Fst ((a, _))                 = a
#if !FABLE_COMPILER
      static member inline Fst struct(a, _)             = a
#endif
      static member inline Fst ((a, _, _))              = a
#if !FABLE_COMPILER
      static member inline Fst struct(a, _, _)          = a
#endif
      static member inline Fst (x : KeyValuePair<_, _>) = x.Key
      static member inline Snd ((_, b))                 = b
#if !FABLE_COMPILER
      static member inline Snd struct(_, b)             = b
#endif
      static member inline Snd ((_, b, _))              = b
#if !FABLE_COMPILER
      static member inline Snd struct(_, b, _)          = b
#endif
      static member inline Snd (x : KeyValuePair<_, _>) = x.Value
      static member inline Trd ((_, _, c))              = c
#if !FABLE_COMPILER
      static member inline Trd struct(_, _, c)          = c
#endif

      static member inline InvokeFst x =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Fst : _ -> _) x)
         call x Unchecked.defaultof<Tuple>

      static member inline InvokeSnd x =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Snd : _ -> _) x)
         call x Unchecked.defaultof<Tuple>

      static member inline InvokeTrd x=
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Trd : _ -> _) x)
         call x Unchecked.defaultof<Tuple>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Monoid =
      static member inline Zero (_ : Async<_>)   = Async.never ()
      static member inline Zero (_ : Option<_>)  = None
      static member inline Zero (_ : VOption<_>) = ValueNone
      static member inline Zero (_ : Seq<_>)     = Seq.empty
      static member inline Zero (_ : List<_>)    = []
      static member inline Zero (_ : _[])        = [||]
#if !FABLE_COMPILER
      static member inline Zero (_ : Vec<_>)     = Vec.empty
#endif
      static member inline Zero (_ : MVec<_>)    = MVec ()
      static member inline Zero (_ : Set<_>)     = Set.empty
      static member inline Zero (_ : String)     = ""

      static member inline Zero (x : Workaround<_>) = x

      static member inline InvokeZero () : ^a =
         let inline call (_ : ^w) =
            ((^a or ^w) : (static member Zero : _ -> _) Unchecked.defaultof<^a>)
         call Unchecked.defaultof<Monoid>

#if !FABLE_COMPILER
      static member inline Combine (x, y)   = Async.alt x y
#endif
      static member inline Combine (x, y)   = Option.alt x y
#if !FABLE_COMPILER
      static member inline Combine (x, y)   = VOption.alt x y
#endif
      static member inline Combine (xs, ys) = Seq.append xs ys
      static member inline Combine (xs, ys) = List.append xs ys
      static member inline Combine (xs, ys) = Array.append xs ys
#if !FABLE_COMPILER
      static member inline Combine (xs, ys) = Vec.append xs ys
#endif
      static member inline Combine (xs, ys) = MVec.append xs ys
      static member inline Combine (xs, ys) = Set.union xs ys
      static member inline Combine (s, t)   = String.append s t

      static member inline Combine (x : Workaround<^a>, _ : Workaround<^a>) = x

      static member inline InvokeCombine x y : ^a =
         let inline call (x : ^a) (y : ^a) (_ : ^w) =
            ((^a or ^w) : (static member Combine : _ * _ -> _) x, y)
         call x y Unchecked.defaultof<Monoid>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type ToVSeq =
      static member inline ToVSeq xs                 = List.toVSeq xs
      static member inline ToVSeq xs                 = Array.toVSeq xs
      static member inline ToVSeq xs                 = Seq.toVSeq xs
      static member inline ToVSeq (xs : #VSeq<_, _>) = xs
      static member inline ToVSeq xs                 = Vec.toVSeq xs
      static member inline ToVSeq xs                 = MVec.toVSeq xs
      static member inline ToVSeq xs                 = Set.toVSeq xs
      static member inline ToVSeq xs                 = Map.toVSeq xs
      static member inline ToVSeq xs                 = HashSet.toVSeq xs
      static member inline ToVSeq xs                 = HashMap.toVSeq xs

      static member inline Invoke x =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member ToVSeq : _ -> _) x)
         call x Unchecked.defaultof<ToVSeq>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Functor =
      static member inline Map ([<InlineIfLambda>] f, (a, b))       = f a, b
      static member inline Map ([<InlineIfLambda>] f, struct(a, b)) = struct(f a, b)

      static member inline Map ([<InlineIfLambda>] f, x : KeyValuePair<_, _>) =
         KeyValuePair (f x.Key, x.Value)

      static member inline Map ([<InlineIfLambda>] f, g)  = g >> f
      static member inline Map ([<InlineIfLambda>] f, x)  = Lazy.map f x
      static member inline Map ([<InlineIfLambda>] f, x)  = Async.map f x
      static member inline Map ([<InlineIfLambda>] f, x)  = Option.map f x
#if !FABLE_COMPILER
      static member inline Map ([<InlineIfLambda>] f, x)  = VOption.map f x
#endif
      static member inline Map ([<InlineIfLambda>] f, x)  = Choice.map f x
      static member inline Map ([<InlineIfLambda>] f, x)  = Result.map f x
      static member inline Map ([<InlineIfLambda>] f, xs) = Seq.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = VSeq'.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = List.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = Array.map f xs
#if !FABLE_COMPILER
      static member inline Map ([<InlineIfLambda>] f, xs) = Array2D.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = Array3D.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = Vec.map f xs
#endif
      static member inline Map ([<InlineIfLambda>] f, xs) = MVec.map f xs
#if !FABLE_COMPILER
      static member inline Map ([<InlineIfLambda>] f, xs) = Span.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = MSpan.map f xs
#endif
      static member inline Map ([<InlineIfLambda>] f, xs) = Set.map f xs
      static member inline Map ([<InlineIfLambda>] f, xs) = xs |> Map.map (fun _ v -> f v)
      static member inline Map ([<InlineIfLambda>] f, s)  = String.map f s

      static member inline Map (_ : Workaround -> Workaround, x : Workaround)                   = x
      static member inline Map (_ : Workaround<unit> -> Workaround<unit>, x : Workaround<unit>) = x

      static member inline InvokeMap ([<InlineIfLambda>] f : _ -> _) x : ^a =
         let inline call (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Map : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Functor>

      static member inline Iter ([<InlineIfLambda>] f, (a, _)) : unit                 = f a
      static member inline Iter ([<InlineIfLambda>] f, struct(a, _)) : unit           = f a
      static member inline Iter ([<InlineIfLambda>] f, x : KeyValuePair<_, _>) : unit = f x.Key

      static member inline Iter ([<InlineIfLambda>] f, x)  = x |> Lazy.force |> f
      static member inline Iter ([<InlineIfLambda>] f, x)  = Option.iter f x
#if !FABLE_COMPILER
      static member inline Iter ([<InlineIfLambda>] f, x)  = VOption.iter f x
#endif
      static member inline Iter ([<InlineIfLambda>] f, x)  = Choice.iter f x
      static member inline Iter ([<InlineIfLambda>] f, x)  = Result.iter f x
      static member inline Iter ([<InlineIfLambda>] f, xs) = Seq.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = VSeq'.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = List.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = Array.iter f xs
#if !FABLE_COMPILER
      static member inline Iter ([<InlineIfLambda>] f, xs) = Array2D.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = Array3D.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = Vec.iter f xs
#endif
      static member inline Iter ([<InlineIfLambda>] f, xs) = MVec.iter f xs
#if !FABLE_COMPILER
      static member inline Iter ([<InlineIfLambda>] f, xs) = Span.iter f xs
      static member inline Iter ([<InlineIfLambda>] f, xs) = MSpan.iter f xs
#endif
      static member inline Iter ([<InlineIfLambda>] f, xs) = Set.iter f xs

      static member inline Iter ([<InlineIfLambda>] f, xs) = xs |> Map.iter (fun _ v -> f v)
      static member inline Iter ([<InlineIfLambda>] f, s)  = String.iter f s

      static member inline Iter (_ : Workaround -> unit, _ : Workaround) = ()

      static member inline InvokeIter ([<InlineIfLambda>] f : _ -> unit) x : unit =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Iter : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Functor>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Zipable =
      inherit Priority0

      static member inline Zip (x, y)   = Lazy.zip x y
      static member inline Zip (x, y)   = Async.zip x y
      static member inline Zip (x, y)   = Option.zip x y
#if !FABLE_COMPILER
      static member inline Zip (x, y)   = VOption.zip x y
#endif
      static member inline Zip (x, y)   = Choice.zip x y
      static member inline Zip (x, y)   = Result.zip x y
      static member inline Zip (xs, ys) = Seq.zip xs ys
      static member inline Zip (xs, ys) = VSeq'.zip xs ys
      static member inline Zip (xs, ys) = List.zip xs ys
      static member inline Zip (xs, ys) = Array.zip xs ys

      static member inline InvokeZip x y =
         let inline call (x : ^a) (y : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Zip : _ * _ -> _) x, y)
         call x y Unchecked.defaultof<Zipable>

      static member inline Unzip (x, _ : Zipable)  = Option.unzip x
#if !FABLE_COMPILER
      static member inline Unzip (x, _ : Zipable)  = VOption.unzip x
#endif
      static member inline Unzip (x, _ : Zipable)  = Choice.unzip x
      static member inline Unzip (x, _ : Zipable)  = Result.unzip x
      static member inline Unzip (xs, _ : Zipable) = List.unzip xs
      static member inline Unzip (xs, _ : Zipable) = Array.unzip xs

      static member inline Unzip (x, _ : Priority0) =
         Functor.InvokeMap Tuple.InvokeFst x, Functor.InvokeMap Tuple.InvokeSnd x

      static member inline InvokeUnzip x =
         let inline call (x : ^a) (w : ^w) = ((^a or ^w) : (static member Unzip : _ * _ -> _) x, w)
         call x Unchecked.defaultof<Zipable>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Applicative =
      inherit Priority0

      static member inline Wrap (x, _ : Lazy<_>)      = lazy x
      static member inline Wrap (x, _ : Async<_>)     = Async.ofValue x
      static member inline Wrap (x, _ : Option<_>)    = Some x
      static member inline Wrap (x, _ : VOption<_>)   = ValueSome x
      static member inline Wrap (x, _ : Choice<_, _>) = Choice1Of2 x
      static member inline Wrap (x, _ : Result<_, _>) = Ok x
      static member inline Wrap (x, _ : Seq<_>)       = Seq.singleton x
      static member inline Wrap (x, _ : List<_>)      = [x]
      static member inline Wrap (x, _ : _[])          = [|x|]
#if !FABLE_COMPILER
      static member inline Wrap (x, _ : Vec<_>)         = Vec.singleton x
#endif
      static member inline Wrap (x, _ : MVec<_>)        = MVec.singleton x

      static member inline InvokeWrap x : ^a =
         let inline call (_ : ^w) =
            ((^a or ^w) : (static member Wrap : _ * _ -> _) x, Unchecked.defaultof<^a>)
         call Unchecked.defaultof<Applicative>

      static member inline Apply (f, x)   = Lazy.apply f x
      static member inline Apply (f, x)   = Async.apply f x
      static member inline Apply (f, x)   = Option.apply f x
#if !FABLE_COMPILER
      static member inline Apply (f, x)   = VOption.apply f x
#endif
      static member inline Apply (f, x)   = Choice.apply f x
      static member inline Apply (f, x)   = Result.apply f x
      static member inline Apply (fs, xs) = Seq.apply fs xs
      static member inline Apply (fs, xs) = List.apply fs xs
      static member inline Apply (fs, xs) = Array.apply fs xs

      static member inline InvokeApply f x =
         let inline call (f : ^a) (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Apply : _ * _ -> _) f, x)
         call f x Unchecked.defaultof<Applicative>

      static member inline Lift2 (([<InlineIfLambda>] f), x, y, _ : Applicative) =
         Option.map2 f x y
#if !FABLE_COMPILER
      static member inline Lift2 (([<InlineIfLambda>] f), x, y, _ : Applicative) =
         VOption.map2 f x y
#endif

      static member inline Lift2 (f, x, y, _ : Priority0) =
         Applicative.InvokeApply (Functor.InvokeMap f x) y

      static member inline InvokeLift2 ([<InlineIfLambda>] f : _ -> _ -> _) x y =
         let inline call (x : ^a) (w : ^w) =
            ((^a or ^w) : (static member Lift2 : _ * _ * _ * _ -> _) f, x, y, w)
         call x Unchecked.defaultof<Applicative>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Monad =
      static member inline Join x              = Lazy.flatten x
      static member inline Join x              = Async.flatten x
      static member inline Join x              = Option.flatten x
#if !FABLE_COMPILER
      static member inline Join x              = VOption.flatten x
#endif
      static member inline Join x              = Choice.flatten x
      static member inline Join x              = Result.flatten x
      static member inline Join xs             = Seq.concat xs
      static member inline Join (xs : List<_>) = List.concat xs
      static member inline Join (xs : _[])     = Array.concat xs

      static member inline InvokeJoin x =
         let inline call (x : ^a) (_ : ^w) = ((^a or ^w) : (static member Join : _ -> _) x)
         call x Unchecked.defaultof<Monad>

      static member inline Bind ([<InlineIfLambda>] f, x)  = Lazy.bind f x
      static member inline Bind ([<InlineIfLambda>] f, x)  = Async.bind f x
      static member inline Bind ([<InlineIfLambda>] f, x)  = Option.bind f x
#if !FABLE_COMPILER
      static member inline Bind ([<InlineIfLambda>] f, x)  = VOption.bind f x
#endif
      static member inline Bind ([<InlineIfLambda>] f, x)  = Choice.bind f x
      static member inline Bind ([<InlineIfLambda>] f, x)  = Result.bind f x
      static member inline Bind ([<InlineIfLambda>] f, xs) = Seq.collect f xs
      static member inline Bind ([<InlineIfLambda>] f, xs) = List.collect f xs
      static member inline Bind ([<InlineIfLambda>] f, xs) = Array.collect f xs
      static member inline Bind ([<InlineIfLambda>] f, s)  = String.collect f s

      static member inline Bind (_ : Workaround -> Workaround, x : Workaround) = x

      static member inline InvokeBind ([<InlineIfLambda>] f : _ -> ^a) x : ^a =
         let inline call (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Bind : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Monad>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Bifunctor =
      static member inline MapSnd ([<InlineIfLambda>] f, (a, b))       = a, f b
      static member inline MapSnd ([<InlineIfLambda>] f, struct(a, b)) = struct(a, f b)

      static member inline MapSnd ([<InlineIfLambda>] f, x : KeyValuePair<_, _>) =
         KeyValuePair (x.Key, f x.Value)

      static member inline MapSnd ([<InlineIfLambda>] f, x) = Choice.map2Of2 f x
      static member inline MapSnd ([<InlineIfLambda>] f, x) = Result.mapError f x

      static member inline InvokeMapSnd ([<InlineIfLambda>] f : _ -> _) x : ^a =
         let inline call (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member MapSnd : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Bifunctor>

      static member inline IterSnd ([<InlineIfLambda>] f, (_, b)) : unit       = f b
      static member inline IterSnd ([<InlineIfLambda>] f, struct(_, b)) : unit = f b

      static member inline IterSnd ([<InlineIfLambda>] f, x : KeyValuePair<_, _>) : unit =
         f x.Value

      static member inline IterSnd ([<InlineIfLambda>] f, x) = Choice.iter2Of2 f x
      static member inline IterSnd ([<InlineIfLambda>] f, x) = Result.iterError f x

      static member inline InvokeIterSnd ([<InlineIfLambda>] f : _ -> unit) x : unit =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member IterSnd : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Bifunctor>

      static member inline Bimap ([<InlineIfLambda>] f, [<InlineIfLambda>] g, (a, b)) = f a, g b

      static member inline Bimap ([<InlineIfLambda>] f, [<InlineIfLambda>] g, struct(a, b)) =
         struct(f a, g b)

      static member inline Bimap (
         [<InlineIfLambda>] f, [<InlineIfLambda>] g, x : KeyValuePair<_, _>) =
            KeyValuePair (f x.Key, g x.Value)

      static member inline Bimap ([<InlineIfLambda>] f, [<InlineIfLambda>] g, x) =
         Choice.bimap f g x

      static member inline Bimap ([<InlineIfLambda>] f, [<InlineIfLambda>] g, x) =
         Result.bimap f g x

      static member inline InvokeBimap (
         [<InlineIfLambda>] f : _ -> _) ([<InlineIfLambda>] g : _ -> _) x : ^a =
            let inline call (x : ^b) (_ : ^w) =
               ((^a or ^b or ^w) : (static member Bimap : _ * _ * _ -> _) f, g, x)
            call x Unchecked.defaultof<Bifunctor>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type BindSnd =
      static member inline BindSnd ([<InlineIfLambda>] f, x) = Choice.bind2Of2 f x
      static member inline BindSnd ([<InlineIfLambda>] f, x) = Result.bindError f x

      static member inline Invoke ([<InlineIfLambda>] f : _ -> ^a) x : ^a =
         let inline call (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member BindSnd : _ * _ -> _) f, x)
         call x Unchecked.defaultof<BindSnd>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Validation =
      static member inline Zipv (x, y) =
         match x, y with
         | Choice1Of2 a, Choice1Of2 c        -> Choice1Of2 (a, c)
         | Choice2Of2 b, Choice2Of2 d        -> Choice2Of2 (Monoid.InvokeCombine b d)
         | Choice2Of2 b, _ | _, Choice2Of2 b -> Choice2Of2 b

      static member inline Zipv (x, y) =
         match x, y with
         | Ok a, Ok c              -> Ok (a, c)
         | Error b, Error d        -> Error (Monoid.InvokeCombine b d)
         | Error b, _ | _, Error b -> Error b

      static member inline InvokeZipv x y =
         let inline call (x : ^a) (y : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Zipv : _ * _ -> _) x, y)
         call x y Unchecked.defaultof<Validation>

      static member inline Applyv (f, x) =
         match f, x with
         | Choice1Of2 g, Choice1Of2 a        -> Choice1Of2 (g a)
         | Choice2Of2 b, Choice2Of2 c        -> Choice2Of2 (Monoid.InvokeCombine b c)
         | Choice2Of2 b, _ | _, Choice2Of2 b -> Choice2Of2 b

      static member inline Applyv (f, x) =
         match f, x with
         | Ok g, Ok a              -> Ok (g a)
         | Error b, Error c        -> Error (Monoid.InvokeCombine b c)
         | Error b, _ | _, Error b -> Error b

      static member inline InvokeApplyv f x =
         let inline call (f : ^a) (x : ^b) (_ : ^w) =
            ((^a or ^b or ^w) : (static member Applyv : _ * _ -> _) f, x)
         call f x Unchecked.defaultof<Validation>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type ValueOr =
      static member inline ValueOr (v, x) = Option.defaultValue v x
#if !FABLE_COMPILER
      static member inline ValueOr (v, x) = VOption.defaultValue v x
#endif
      static member inline ValueOr (v, x) = Choice.defaultValue v x
      static member inline ValueOr (v, x) = Result.defaultValue v x

      static member inline InvokeValueOr a x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member ValueOr : _ * _ -> _) a, x)
         call x Unchecked.defaultof<ValueOr>

      static member inline ValueOrElse ([<InlineIfLambda>] f, x) = Option.defaultWith f x
#if !FABLE_COMPILER
      static member inline ValueOrElse ([<InlineIfLambda>] f, x) = VOption.defaultWith f x
#endif
      static member inline ValueOrElse ([<InlineIfLambda>] f, x) = Choice.defaultWith f x
      static member inline ValueOrElse ([<InlineIfLambda>] f, x) = Result.defaultWith f x

      static member inline ValueOrElse (v, x) =
         match x with
         | Some a -> a
         | None   -> Lazy.force v

      static member inline ValueOrElse (v, x) =
         match x with
         | ValueSome a -> a
         | ValueNone   -> Lazy.force v

      static member inline ValueOrElse (v, x) =
         match x with
         | Choice1Of2 a -> a
         | Choice2Of2 _ -> Lazy.force v

      static member inline ValueOrElse (v, x) =
         match x with
         | Ok a    -> a
         | Error _ -> Lazy.force v

      static member inline InvokeValueOrElse a x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member ValueOrElse : _ * _ -> _) a, x)
         call x Unchecked.defaultof<ValueOr>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type OrError =
      static member inline OrError (e : 'e when 'e : struct, x) =
         match x with
         | Some a -> Ok a
         | None   -> Error e

      static member inline OrError (e : 'e when 'e : struct, x) =
         match x with
         | ValueSome a -> Ok a
         | ValueNone   -> Error e

      static member inline OrError (e : 'e when 'e : struct, x) =
         match x with
         | Choice1Of2 a -> Ok a
         | Choice2Of2 _ -> Error e

      static member inline OrError (e : 'e when 'e : struct, x) =
         match x with
         | Ok a    -> Ok a
         | Error _ -> Error e

      static member inline OrError ((), x) = Result.ofOption x
      static member inline OrError ((), x) = Result.ofVOption x
      static member inline OrError ((), x) = x |> Result.ofChoice |> Result.mapError ignore
      static member inline OrError ((), x) = x |> Result.mapError ignore

      static member inline InvokeOrError e x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member OrError : _ * _ -> _) e, x)
         call x Unchecked.defaultof<OrError>

      static member inline OrElseError ([<InlineIfLambda>] f, x) =
         match x with
         | Some a -> Ok a
         | None   -> Error (f ())

      static member inline OrElseError ([<InlineIfLambda>] f, x) =
         match x with
         | ValueSome a -> Ok a
         | ValueNone   -> Error (f ())

      static member inline OrElseError ([<InlineIfLambda>] f, x) =
         match x with
         | Choice1Of2 a -> Ok a
         | Choice2Of2 _ -> Error (f ())

      static member inline OrElseError ([<InlineIfLambda>] f, x) =
         match x with
         | Ok a    -> Ok a
         | Error _ -> Error (f ())

      static member inline OrElseError (e, x) =
         match x with
         | Some a -> Ok a
         | None   -> Error (Lazy.force e)

      static member inline OrElseError (e, x) =
         match x with
         | ValueSome a -> Ok a
         | ValueNone   -> Error (Lazy.force e)

      static member inline OrElseError (e, x) =
         match x with
         | Choice1Of2 a -> Ok a
         | Choice2Of2 _ -> Error (Lazy.force e)

      static member inline OrElseError (e, x) =
         match x with
         | Ok a    -> Ok a
         | Error _ -> Error (Lazy.force e)

      static member inline InvokeOrElseError e x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member OrElseError : _ * _ -> _) e, x)
         call x Unchecked.defaultof<OrError>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Filter =
      inherit Priority0

      static member inline Filter ([<InlineIfLambda>] f, x)  = Option.filter f x
#if !FABLE_COMPILER
      static member inline Filter ([<InlineIfLambda>] f, x)  = VOption.filter f x
#endif
      static member inline Filter ([<InlineIfLambda>] f, xs) = Seq.filter f xs
      static member inline Filter ([<InlineIfLambda>] f, xs) = VSeq'.filter f xs
      static member inline Filter ([<InlineIfLambda>] f, xs) = List.filter f xs
      static member inline Filter ([<InlineIfLambda>] f, xs) = Array.filter f xs
#if !FABLE_COMPILER
      static member inline Filter ([<InlineIfLambda>] f, xs) = Vec.filter f xs
#endif
      static member inline Filter ([<InlineIfLambda>] f, xs) = MVec.filter f xs
#if !FABLE_COMPILER
      static member inline Filter ([<InlineIfLambda>] f, xs) = Span.filter f xs
      static member inline Filter ([<InlineIfLambda>] f, xs) = MSpan.filter f xs
#endif

      static member inline InvokeFilter ([<InlineIfLambda>] f : _ -> bool) x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member Filter : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Filter>

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Priority0) = Seq.choose f xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) =
         VSeq'.choose (f >> VOption.ofOption) xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) = List.choose f xs
      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) = Array.choose f xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Priority0) =
         Seq.choose (f >> Option.ofVOption) xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) = VSeq'.choose f xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) =
         List.choose (f >> Option.ofVOption) xs

      static member inline Choose ([<InlineIfLambda>] f, xs, _ : Filter) =
         Array.choose (f >> Option.ofVOption) xs

      static member inline InvokeChoose ([<InlineIfLambda>] f : _ -> _) x =
         let inline call (x : ^a) (w : ^w) =
            ((^a or ^w) : (static member Choose : _ * _ * _ -> _) f, x, w)
         call x Unchecked.defaultof<Filter>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Foldable =
      inherit Priority0

      static member inline Fold (([<InlineIfLambda>] f), acc, x) = Option.fold f acc x
#if !FABLE_COMPILER
      static member inline Fold (([<InlineIfLambda>] f), acc, x) = VOption.fold f acc x
#endif
      static member inline Fold (([<InlineIfLambda>] f), acc, x) = Choice.fold f acc x
      static member inline Fold (([<InlineIfLambda>] f), acc, x) = Result.fold f acc x
      static member inline Fold (f, acc, xs)                     = Seq.fold f acc xs
      static member inline Fold (f, acc, xs)                     = VSeq'.fold f acc xs
      static member inline Fold (f, acc, xs)                     = List.fold f acc xs
      static member inline Fold (f, acc, xs)                     = Array.fold f acc xs
#if !FABLE_COMPILER
      static member inline Fold (f, acc, xs)                     = Vec.fold f acc xs
#endif
      static member inline Fold (f, acc, xs)                     = MVec.fold f acc xs
#if !FABLE_COMPILER
      static member inline Fold (f, acc, xs)                     = Span.fold f acc xs
      static member inline Fold (f, acc, xs)                     = MSpan.fold f acc xs
#endif
      static member inline Fold (f, acc, xs)                     = Set.fold f acc xs

      static member inline Fold (f, acc, xs) =
         Map.fold (fun acc _ x -> f acc x) acc xs

      static member inline InvokeFold ([<InlineIfLambda>] f : ^a -> _ -> ^a) (acc : ^a) x =
         let inline call (x : ^b) (_ : ^w) =
            ((^b or ^w) : (static member Fold : _ * _ * _ -> _) f, acc, x)
         call x Unchecked.defaultof<Foldable>

      static member inline FoldBack (f, xs, acc) = Seq.foldBack f xs acc
      static member inline FoldBack (f, xs, acc) = List.foldBack f xs acc
      static member inline FoldBack (f, xs, acc) = Array.foldBack f xs acc
#if !FABLE_COMPILER
      static member inline FoldBack (f, xs, acc) = Vec.foldBack f xs acc
#endif
      static member inline FoldBack (f, xs, acc) = MVec.foldBack f xs acc
#if !FABLE_COMPILER
      static member inline FoldBack (f, xs, acc) = Span.foldBack f xs acc
      static member inline FoldBack (f, xs, acc) = MSpan.foldBack f xs acc
#endif
      static member inline FoldBack (f, xs, acc) = Set.foldBack f xs acc
      static member inline FoldBack (f, xs, acc) = Map.foldBack (fun _ x acc -> f x acc) xs acc

      static member inline InvokeFoldBack (f : _ -> ^a -> ^a) x (acc : ^a) =
         let inline call (x : ^b) (_ : ^w) =
            ((^b or ^w) : (static member FoldBack : _ * _ * _ -> _) f, x, acc)
         call x Unchecked.defaultof<Foldable>

      static member inline Loop (f, acc, xs) = Seq.loop f acc xs
      static member inline Loop (f, acc, xs) = VSeq'.loop f acc xs
      static member inline Loop (f, acc, xs) = List.loop f acc xs
      static member inline Loop (f, acc, xs) = Array.loop f acc xs
#if !FABLE_COMPILER
      static member inline Loop (f, acc, xs) = Vec.loop f acc xs
#endif
      static member inline Loop (f, acc, xs) = MVec.loop f acc xs
#if !FABLE_COMPILER
      static member inline Loop (f, acc, xs) = Span.loop f acc xs
      static member inline Loop (f, acc, xs) = MSpan.loop f acc xs
#endif

      static member inline InvokeLoop (f : ^a -> _ -> ^a * bool) (acc : ^a) x =
         let inline call (x : ^b) (_ : ^w) =
            ((^b or ^w) : (static member Loop : _ * _ * _ -> _) f, acc, x)
         call x Unchecked.defaultof<Foldable>

      static member inline LoopBack (f, xs, acc) = Seq.loopBack f xs acc
      static member inline LoopBack (f, xs, acc) = List.loopBack f xs acc
      static member inline LoopBack (f, xs, acc) = Array.loopBack f xs acc
#if !FABLE_COMPILER
      static member inline LoopBack (f, xs, acc) = Vec.loopBack f xs acc
#endif
      static member inline LoopBack (f, xs, acc) = MVec.loopBack f xs acc
#if !FABLE_COMPILER
      static member inline LoopBack (f, xs, acc) = Span.loopBack f xs acc
      static member inline LoopBack (f, xs, acc) = MSpan.loopBack f xs acc
#endif

      static member inline InvokeLoopBack (f : _ -> ^a -> ^a * bool) x (acc : ^a) =
         let inline call (x : ^b) (_ : ^w) =
            ((^b or ^w) : (static member LoopBack : _ * _ * _ -> _) f, x, acc)
         call x Unchecked.defaultof<Foldable>

      static member inline FoldWhile (f, acc, x, _ : Priority0) =
         let g acc x =
            let acc' = f (Option.get acc) x
            acc', Option.isSome acc'
         Foldable.InvokeLoop g (Some acc) x

      static member inline FoldWhile (f, acc, x, _ : Priority0) =
         let g acc x =
            let acc' = f (VOption.get acc) x
            acc', VOption.isSome acc'
         Foldable.InvokeLoop g (ValueSome acc) x

      static member inline FoldWhile (f, acc, x, _ : Priority0) =
         let g acc x =
            let acc' = f (Choice.get acc) x
            acc', Choice.isChoice1Of2 acc'
         Foldable.InvokeLoop g (Choice1Of2 acc) x

      static member inline FoldWhile (f, acc, x, _ : Priority0) =
         let g acc x =
            let acc' = f (Result.get acc) x
            acc', Result.isOk acc'
         Foldable.InvokeLoop g (Ok acc) x

      static member inline InvokeFoldWhile (f : ^a -> _ -> ^a option) (acc : ^a) x =
         let inline call (x : ^b) (w : ^w) =
            ((^b or ^w) : (static member FoldWhile : _ * _ * _ * _ -> _) f, acc, x, w)
         call x Unchecked.defaultof<Foldable>

      static member inline FoldBackWhile (f, x, acc, _ : Priority0) =
         let g x acc =
            let acc' = f x (Option.get acc)
            acc', Option.isSome acc'
         Foldable.InvokeLoopBack g x (Some acc)

      static member inline FoldBackWhile (f, x, acc, _ : Priority0) =
         let g x acc =
            let acc' = f x (VOption.get acc)
            acc', VOption.isSome acc'
         Foldable.InvokeLoopBack g x (ValueSome acc)

      static member inline FoldBackWhile (f, x, acc, _ : Priority0) =
         let g x acc =
            let acc' = f x (Choice.get acc)
            acc', Choice.isChoice1Of2 acc'
         Foldable.InvokeLoopBack g x (Choice1Of2 acc)

      static member inline FoldBackWhile (f, x, acc, _ : Priority0) =
         let g x acc =
            let acc' = f x (Result.get acc)
            acc', Result.isOk acc'
         Foldable.InvokeLoopBack g x (Ok acc)

      static member inline InvokeFoldBackWhile (f : _ -> ^a -> ^a option) x (acc : ^a) =
         let inline call (x : ^b) (w : ^w) =
            ((^b or ^w) : (static member FoldBackWhile : _ * _ * _ * _ -> _) f, x, acc, w)
         call x Unchecked.defaultof<Foldable>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Length =
      inherit Priority0

      static member inline Length (xs, _ : Priority1) = Seq.length xs
      static member inline Length (xs, _ : Length)    = VSeq'.length xs
      static member inline Length (xs, _ : Length)    = List.length xs
      static member inline Length (xs, _ : Length)    = Array.length xs
#if !FABLE_COMPILER
      static member inline Length (xs, _ : Length)    = Span.length xs
      static member inline Length (xs, _ : Length)    = MSpan.length xs
#endif
      static member inline Length (s, _ : Length)     = String.length s

      static member inline Length (x : ^a, _ : Priority0) = (^a : (member Count : int) x)

      static member inline Length (_ : Workaround, _ : Length) = 0

      static member inline Invoke x =
         let inline call (x : ^a) (w : ^w) =
            ((^a or ^w) : (static member Length : _ * _ -> _) x, w)
         call x Unchecked.defaultof<Length>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Item =
      inherit Priority0

      static member inline Item (i, xs, _ : Priority1) = Seq.item i xs
      static member inline Item (i, xs, _ : Item)      = Array.item i xs
      static member inline Item (i, s, _ : Item)       = String.item i s

#nowarn 77
      static member inline Item (a, x : ^a, _ : Priority0) = (^a : (member get_Item : _ -> _) x, a)
#warnon 77

      static member inline Item (_ : Workaround, x : Workaround, _ : Item) = x

      static member inline InvokeItem a x =
         let inline call (x : ^a) (w : ^w) =
            ((^a or ^w) : (static member Item : _ * _ * _ -> _) a, x, w)
         call x Unchecked.defaultof<Item>

      static member inline TryItem (i, xs, _ : Priority1) = Seq.tryItem i xs
      static member inline TryItem (i, xs, _ : Item)      = List.tryItem i xs
      static member inline TryItem (i, xs, _ : Item)      = Array.tryItem i xs
#if !FABLE_COMPILER
      static member inline TryItem (i, xs, _ : Item)      = Vec.tryItem i xs
#endif
      static member inline TryItem (i, xs, _ : Item)      = MVec.tryItem i xs
#if !FABLE_COMPILER
      static member inline TryItem (i, xs, _ : Item)      = Span.tryItem i xs
      static member inline TryItem (i, xs, _ : Item)      = MSpan.tryItem i xs
#endif
      static member inline TryItem (k, xs, _ : Item)      = Map.tryFind k xs
      static member inline TryItem (i, xs, _ : Item)      = String.tryItem i xs

      static member inline TryItem (a, xs, _ : Priority0) =
         try Some (Item.InvokeItem a xs) with _ -> None

      static member inline TryItem (_ : Workaround, x : Workaround, _ : Item) = x

      static member inline InvokeTryItem a x =
         let inline call (x : ^a ) (w : ^w) =
            ((^a or ^w) : (static member TryItem : _ * _ * _ -> _) a, x, w)
         call x Unchecked.defaultof<Item>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Head =
      inherit Priority0

      static member inline Head (xs, _ : Priority1) = Seq.head xs
      static member inline Head (xs, _ : Head)      = VSeq'.head xs
      static member inline Head (xs, _ : Head)      = List.head xs
      static member inline Head (xs, _ : Head)      = Array.head xs
#if !FABLE_COMPILER
      static member inline Head (xs, _ : Head)      = Vec.head xs
#endif
      static member inline Head (xs, _ : Head)      = MVec.head xs
#if !FABLE_COMPILER
      static member inline Head (xs, _ : Head)      = Span.head xs
      static member inline Head (xs, _ : Head)      = MSpan.head xs
#endif
      static member inline Head (xs, _ : Head)      = Set.minElement xs
      static member inline head (xs, _ : Head)      = Map.minKeyValue xs
      static member inline Head (xs, _ : Head)      = String.head xs

      static member inline Head (xs, _ : Priority0) = Item.InvokeItem 0 xs

      static member inline Head (x : Workaround, _ : Head) = x

      static member inline InvokeHead x =
         let inline call (x : ^a) (w : ^w) = ((^a or ^w) : (static member Head : _ * _ -> _) x, w)
         call x Unchecked.defaultof<Head>

      static member inline TryHead (xs, _ : Priority1) = Seq.tryHead xs
      static member inline TryHead (xs, _ : Head)      = VSeq'.tryHead xs
      static member inline TryHead (xs, _ : Head)      = List.tryHead xs
      static member inline TryHead (xs, _ : Head)      = Array.tryHead xs
#if !FABLE_COMPILER
      static member inline TryHead (xs, _ : Head)      = Vec.tryHead xs
#endif
      static member inline TryHead (xs, _ : Head)      = MVec.tryHead xs
#if !FABLE_COMPILER
      static member inline TryHead (xs, _ : Head)      = Span.tryHead xs
      static member inline TryHead (xs, _ : Head)      = MSpan.tryHead xs
#endif
      static member inline TryHead (xs, _ : Head)      = Set.tryHead xs
      static member inline Tryhead (xs, _ : Head)      = Map.tryHead xs
      static member inline TryHead (xs, _ : Head)      = String.tryHead xs

      static member inline TryHead (xs, _ : Priority0) =
         try Some (Item.InvokeItem 0 xs) with _ -> None

      static member inline TryHead (x : Workaround, _ : Head) = x

      static member inline InvokeTryHead x =
         let inline call (x : ^a ) (w : ^w) =
            ((^a or ^w) : (static member TryHead : _ * _ -> _) x, w)
         call x Unchecked.defaultof<Head>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Tail =
      inherit Priority0

      static member inline Tail (xs, _ : Priority0) = Seq.tail xs
      static member inline Tail (xs, _ : Tail)      = VSeq'.tail xs
      static member inline Tail (xs, _ : Tail)      = List.tail xs
#if !FABLE_COMPILER
      static member inline Tail (xs, _ : Tail)      = Span.tail xs
      static member inline Tail (xs, _ : Tail)      = MSpan.tail xs
#endif

      static member inline Invoke x =
         let inline call (x : ^a) (w : ^w) = ((^a or ^w) : (static member Tail : _ * _ -> _) x, w)
         call x Unchecked.defaultof<Tail>

   [<AbstractClass; Sealed>]
#if FABLE_COMPILER
   [<Fable.Core.Erase>]
#endif
   type Traversable =
      static member inline Traverse ([<InlineIfLambda>] f, x) =
         match x with
         | Some a -> a |> f |> Functor.InvokeMap Some
         | None   -> Applicative.InvokeWrap None

      static member inline Traverse ([<InlineIfLambda>] f, x) =
         match x with
         | ValueSome a -> a |> f |> Functor.InvokeMap ValueSome
         | ValueNone   -> Applicative.InvokeWrap ValueNone

      static member inline Traverse ([<InlineIfLambda>] f, x) =
         match x with
         | Ok a    -> a |> f |> Functor.InvokeMap Ok
         | Error e -> Applicative.InvokeWrap (Error e)

      static member inline Traverse ([<InlineIfLambda>] f, x) =
         match x with
         | Choice1Of2 a -> a |> f |> Functor.InvokeMap Choice1Of2
         | Choice2Of2 b -> Applicative.InvokeWrap (Choice2Of2 b)

      static member inline Traverse (f, xs) =
         let inline g x acc = acc |> Monad.InvokeBind (fun acc ->
            x |> f |> Functor.InvokeMap (fun x -> x :: acc))
         List.foldBack g xs (Applicative.InvokeWrap [])

      static member inline Traverse (f, xs) =
         let mutable ys = Applicative.InvokeWrap (Array.zeroCreate (Array.length xs))
         xs |> Array.iteri (fun i x ->
            ys <- ys |> Monad.InvokeBind (fun (ys : _[]) ->
               x |> f |> Functor.InvokeMap (fun x ->
                  ys[i] <- x
                  ys)))
         ys

      static member inline Traverse (f, xs) =
         let mutable ys = Applicative.InvokeWrap (MVec ())
         xs |> Seq.iter (fun x ->
            ys <- ys |> Monad.InvokeBind (fun ys ->
               x |> f |> Functor.InvokeMap (fun x -> MVec.add x ys)))
         ys

      static member inline Traverse (f, xs) =
         let mutable ys = Applicative.InvokeWrap (MVec ())
         xs |> VSeq'.iter (fun x ->
            ys <- ys |> Monad.InvokeBind (fun ys ->
               x |> f |> Functor.InvokeMap (fun x -> MVec.add x ys)))
         ys

#if !FABLE_COMPILER
      static member inline Traverse (f, xs) =
         let inline g acc x = acc |> Monad.InvokeBind (fun acc ->
            x |> f |> Functor.InvokeMap (fun x -> Vec.add x acc))
         Vec.fold g (Applicative.InvokeWrap Vec.empty) xs
#endif

      static member inline Traverse (f, xs) =
         let inline g acc x = acc |> Monad.InvokeBind (fun acc ->
            x |> f |> Functor.InvokeMap (fun x -> MVec.add x acc))
         MVec.fold g (Applicative.InvokeWrap (MVec (MVec.length xs))) xs

      static member inline InvokeTraverse ([<InlineIfLambda>] f : _ -> _) x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member Traverse : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Traversable>

      static member inline Traversev (f, xs) =
         let inline g x xs = Validation.InvokeApplyv (Functor.InvokeMap List.add (f x)) xs
         List.foldBack g xs (Applicative.InvokeWrap [])

      static member inline Traversev (f, xs) =
         let inline g xs x = Validation.InvokeApplyv (Functor.InvokeMap MVec.add (f x)) xs
         Functor.InvokeMap MVec.toArray (Array.fold g (Applicative.InvokeWrap (MVec ())) xs)

#if !FABLE_COMPILER
      static member inline Traversev (f, xs) =
         let inline g xs x = Validation.InvokeApplyv (Functor.InvokeMap Vec.add (f x)) xs
         Vec.fold g (Applicative.InvokeWrap Vec.empty) xs
#endif

      static member inline Traversev (f, xs) =
         let inline g xs x = Validation.InvokeApplyv (Functor.InvokeMap MVec.add (f x)) xs
         MVec.fold g (Applicative.InvokeWrap (MVec ())) xs

      static member inline InvokeTraversev ([<InlineIfLambda>] f : _ -> _) x =
         let inline call (x : ^a) (_ : ^w) =
            ((^a or ^w) : (static member Traversev : _ * _ -> _) f, x)
         call x Unchecked.defaultof<Traversable>
#warnon 64

module VSeq =
   let unfold    = VSeq'.unfold
   let ascending = VSeq'.ascending

   let inline take n xs      = VSeq'.take n (Typeclasses.ToVSeq.Invoke xs)
   let inline takeWhile f xs = VSeq'.takeWhile f (Typeclasses.ToVSeq.Invoke xs)
   let inline skip n xs      = VSeq'.skip n (Typeclasses.ToVSeq.Invoke xs)
   let inline skipWhile f xs = VSeq'.skipWhile f (Typeclasses.ToVSeq.Invoke xs)

   let inline map f xs   = VSeq'.map f (Typeclasses.ToVSeq.Invoke xs)
   let inline zip xs ys  = VSeq'.zip (Typeclasses.ToVSeq.Invoke xs) (Typeclasses.ToVSeq.Invoke ys)
   let inline indexed xs = VSeq'.indexed (Typeclasses.ToVSeq.Invoke xs)

   let inline filter f xs = VSeq'.filter f (Typeclasses.ToVSeq.Invoke xs)
   let inline choose f xs = VSeq'.choose f (Typeclasses.ToVSeq.Invoke xs)

   let inline length xs     = VSeq'.length (Typeclasses.ToVSeq.Invoke xs)
   let inline iter f xs     = VSeq'.iter f (Typeclasses.ToVSeq.Invoke xs)
   let inline fold f acc xs = VSeq'.fold f acc (Typeclasses.ToVSeq.Invoke xs)
   let inline loop f acc xs = VSeq'.loop f acc (Typeclasses.ToVSeq.Invoke xs)
   let inline forall p xs   = VSeq'.forall p (Typeclasses.ToVSeq.Invoke xs)
   let inline exists p xs   = VSeq'.exists p (Typeclasses.ToVSeq.Invoke xs)

   let inline head xs    = VSeq'.head (Typeclasses.ToVSeq.Invoke xs)
   let inline tryHead xs = VSeq'.head (Typeclasses.ToVSeq.Invoke xs)
   let inline tail xs    = VSeq'.head (Typeclasses.ToVSeq.Invoke xs)

module Builders =
   open Typeclasses

   type Applicative () =
      member inline _.Zero ()                              = Applicative.InvokeWrap ()
      member inline _.Return x                             = Applicative.InvokeWrap x
      member inline _.ReturnFrom x                         = x
      member inline _.Yield x                              = Applicative.InvokeWrap x
      member inline _.YieldFrom x                          = x
      member inline _.BindReturn (x, [<InlineIfLambda>] f) = Functor.InvokeMap f x

      member inline _.MergeSources (x, y) = Zipable.InvokeZip x y

      member inline _.Delay ([<InlineIfLambda>] f : unit -> _) = f
      member inline _.Run ([<InlineIfLambda>] f)               = f ()
      member inline _.TryWith ([<InlineIfLambda>] f, h)        = try f () with e -> h e
      member inline _.TryFinally ([<InlineIfLambda>] f, g)     = try f () finally g ()

      member inline this.Using (d : #System.IDisposable, [<InlineIfLambda>] f) =
         this.TryFinally ((fun _ -> f d), fun _ -> d.Dispose ())

   type Monad () =
      inherit Applicative ()

      member inline _.Bind (x, [<InlineIfLambda>] f) = Monad.InvokeBind f x
      member inline _.Combine (a, b)                 = a |> Monad.InvokeBind (fun _ -> b ())

      member inline _.While (p, [<InlineIfLambda>] f) =
         let rec loop' () =
            if p () then f () |> Monad.InvokeBind loop' else Applicative.InvokeWrap ()
         loop' ()

      member inline this.For (xs : #Seq<_>, [<InlineIfLambda>] f) =
         this.Using (xs.GetEnumerator (), fun xs' -> this.While (xs'.MoveNext, f))

   type Validation () =
      inherit Applicative ()

      member inline _.MergeSources (x, y) = Validation.InvokeZipv x y

[<AutoOpen>]
module Operators =
   open Typeclasses

   let inline u8    x = uint8 x
   let inline u16   x = uint16 x
   let inline u32   x = uint32 x
   let inline u64   x = uint64 x
   let inline usize x = unativeint x
   let inline i8    x = int8 x
   let inline i16   x = int16 x
   let inline i32   x = int32 x
   let inline i64   x = int64 x
   let inline isize x = nativeint x
   let inline f32   x = single x
   let inline f64   x = double x

   let VSome = ValueSome
   let VNone = ValueNone

   let inline (|VSome|VNone|) (x : VOption<^a>) =
      match x with
      | ValueSome a -> VSome a
      | ValueNone   -> VNone

   let inline (<=>) l r = compare l r

   let inline (=^) l r   = NonStructuralComparison.(=) l r
   let inline (<>^) l r  = NonStructuralComparison.(<>) l r
   let inline (<^) l r   = NonStructuralComparison.(<) l r
   let inline (<=^) l r  = NonStructuralComparison.(<=) l r
   let inline (>^) l r   = NonStructuralComparison.(>) l r
   let inline (>=^) l r  = NonStructuralComparison.(>=) l r
   let inline (<=>^) l r = NonStructuralComparison.compare l r

   let inline (=&) l r  = obj.ReferenceEquals (l, r)
   let inline (<>&) l r = obj.ReferenceEquals (l, r) |> not

#nowarn 1189
   let inline (=%)< ^a when ^a :> System.IEquatable<^a>> (l : ^a) (r : ^a)  = l.Equals r
   let inline (<>%)< ^a when ^a :> System.IEquatable<^a>> (l : ^a) (r : ^a) = l.Equals r |> not
   let inline (<%)< ^a when ^a :> System.IComparable<^a>> (l : ^a) r        = l.CompareTo r < 0
   let inline (<=%)< ^a when ^a :> System.IComparable<^a>> (l : ^a) r       = l.CompareTo r <= 0
   let inline (>%)< ^a when ^a :> System.IComparable<^a>> (l : ^a) r        = l.CompareTo r > 0
   let inline (>=%)< ^a when ^a :> System.IComparable<^a>> (l : ^a) r       = l.CompareTo r >= 0
#warnon 1189

   let inline const' x _ = x
   let inline flip f x y = f y x
   let rec    fix f x    = (f (fix f)) x

   let inline tap f x =
      f x
      x

   let inline (~%) (x : ^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
   let inline (!!) (x : ^a)      = (^a : (member Value : _) x)

   let inline force x = Thunk.Invoke x

   let inline tup a b    = Tuple.InvokeTup a b
   let inline tup3 a b c = Tuple.InvokeTup3 a b c
   let inline fst x      = Tuple.InvokeFst x
   let inline snd x      = Tuple.InvokeSnd x
   let inline trd x      = Tuple.InvokeTrd x

   let inline zero< ^a when (^a or Monoid) : (static member Zero : ^a -> ^a)> =
      Monoid.InvokeZero<^a> ()

   let inline combine x y = Monoid.InvokeCombine x y
   let inline (++) x y    = Monoid.InvokeCombine x y
   let inline (<|>) x y   = Monoid.InvokeCombine x y

   let inline vseq x = ToVSeq.Invoke x

   let inline map ([<InlineIfLambda>] f) x   = Functor.InvokeMap f x
   let inline iter ([<InlineIfLambda>] f) x  = Functor.InvokeIter f x
   let inline (|>>) x ([<InlineIfLambda>] f) = Functor.InvokeMap f x
   let inline (<<|) ([<InlineIfLambda>] f) x = Functor.InvokeMap f x
   let inline (<!>) ([<InlineIfLambda>] f) x = Functor.InvokeMap f x

   let inline zip x y = Zipable.InvokeZip x y
   let inline unzip x = Zipable.InvokeUnzip x

   let inline apply f x                        = Applicative.InvokeApply f x
   let inline wrap x                           = Applicative.InvokeWrap x
   let inline (<*>) f x                        = Applicative.InvokeApply f x
   let inline lift2 ([<InlineIfLambda>] f) x y = Applicative.InvokeLift2 f x y

   let inline join x                                                = Monad.InvokeJoin x
   let inline bind ([<InlineIfLambda>] f) x                         = Monad.InvokeBind f x
   let inline (>>=) x ([<InlineIfLambda>] f)                        = Monad.InvokeBind f x
   let inline (=<<) ([<InlineIfLambda>] f) x                        = Monad.InvokeBind f x
   let inline (>=>) ([<InlineIfLambda>] f) ([<InlineIfLambda>] g) x = x |> f |> Monad.InvokeBind g
   let inline (<=<) ([<InlineIfLambda>] g) ([<InlineIfLambda>] f) x = x |> f |> Monad.InvokeBind g

   let inline mapSnd ([<InlineIfLambda>] f) x                       = Bifunctor.InvokeMapSnd f x
   let inline iterSnd ([<InlineIfLambda>] f) x                      = Bifunctor.InvokeIterSnd f x
   let inline bimap ([<InlineIfLambda>] f) ([<InlineIfLambda>] g) x = Bifunctor.InvokeBimap f g x
   let inline (|!>) x ([<InlineIfLambda>] f)                        = Bifunctor.InvokeMapSnd f x
   let inline (<!|) ([<InlineIfLambda>] f) x                        = Bifunctor.InvokeMapSnd f x

   let inline bindSnd ([<InlineIfLambda>] f) x = BindSnd.Invoke f x
   let inline (>!=) x ([<InlineIfLambda>] f)   = BindSnd.Invoke f x
   let inline (=!<) ([<InlineIfLambda>] f) x   = BindSnd.Invoke f x

   let inline zipv x y   = Validation.InvokeZipv x y
   let inline applyv f x = Validation.InvokeApplyv f x
   let inline (<*+>) f x = Validation.InvokeApplyv f x

   let inline valueOr a x     = ValueOr.InvokeValueOr a x
   let inline (?|) x a        = ValueOr.InvokeValueOr a x
   let inline valueOrElse a x = ValueOr.InvokeValueOrElse a x
   let inline (?||) x a       = ValueOr.InvokeValueOrElse a x

   let inline orError e x     = OrError.InvokeOrError e x
   let inline (|!) x e        = OrError.InvokeOrError e x
   let inline orElseError e x = OrError.InvokeOrElseError e x
   let inline (||!) x e       = OrError.InvokeOrElseError e x

   let inline filter ([<InlineIfLambda>] f) x = Filter.InvokeFilter f x
   let inline choose ([<InlineIfLambda>] f) x = Filter.InvokeChoose f x

   let inline fold ([<InlineIfLambda>] f) acc x = Foldable.InvokeFold f acc x
   let inline foldBack f x acc                  = Foldable.InvokeFoldBack f acc x
   let inline loop f acc x                      = Foldable.InvokeLoop f acc x
   let inline loopBack f x acc                  = Foldable.InvokeLoopBack f acc x
   let inline foldWhile f acc x                 = Foldable.InvokeFoldWhile f acc x
   let inline foldBackWhile f x acc             = Foldable.InvokeFoldBackWhile f x acc

   let inline length x  = Length.Invoke x
   let inline isEmpty x = Length.Invoke x = 0

   let inline item a x    = Item.InvokeItem a x
   let inline tryItem a x = Item.InvokeTryItem a x

   let inline head x    = Head.InvokeHead x
   let inline tryHead x = Head.InvokeTryHead x
   let inline tail x    = Tail.Invoke x

   let inline traverse ([<InlineIfLambda>] f) x  = Traversable.InvokeTraverse f x
   let inline traversev ([<InlineIfLambda>] f) x = Traversable.InvokeTraversev f x
   let inline sequence x                         = Traversable.InvokeTraverse id x
   let inline sequencev x                        = Traversable.InvokeTraversev id x

   let applicative = Builders.Applicative ()
   let monad       = Builders.Monad ()
   let validation  = Builders.Validation ()

   type AsyncBuilder with
      member inline _.MergeSources (x, y) = Async.zip x y
