(*-----------------------------------------------------------------------
page 21
三角形
-------------------------------------------------------------------------*)
let rec choose n xs =
    match (n, xs) with
    | (0, _) -> [[]]
    | (_, []) -> []
    | (_, y::ys) ->
        let first = List.map (fun ps -> y::ps) (choose (n - 1) ys)
        let second = choose n ys
        first @ second

let findMaxCircumference (sticks: int list) =
    let makeTriangle = function
        | (a::b::c::_) -> a + b > c
        | _            -> false
    List.sort sticks |> choose 3
                     |> Seq.filter makeTriangle
                     |> Seq.map Seq.sum
                     |> Seq.max

let ansTriangle = findMaxCircumference [2; 3; 4; 5; 10]

(*-----------------------------------------------------------------------
page 23
Ants
POJ No. 1852
-------------------------------------------------------------------------*)
let antsMinMaxTime (stickLength: int) (antLocations: int list) =
    let closer x = min x (stickLength - x)
    let further x = max x (stickLength - x)
    let minTime = antLocations |> Seq.map closer |> Seq.max
    let maxTime = antLocations |> Seq.map further |> Seq.max
    (minTime, maxTime)
let ansAntsMinMaxTime = antsMinMaxTime 10 [2; 6; 7]


(*-----------------------------------------------------------------------
page 25
くじびき revisited
-------------------------------------------------------------------------*)
let binSearch target arr =
    let sorted = Array.sort arr
    let rec helper lo hi =
        match hi < lo with
        | true -> None
        | _    -> let mid = (lo + hi) / 2
                  match compare target sorted.[mid] with
                   | 0  -> Some mid
                   | -1 -> helper lo (mid - 1)
                   | _  -> helper (mid + 1) hi
    helper 0 (Array.length arr - 1)

let isFourSumEqualTo m arr =
    let series =
        seq {
            for p in arr do
            for q in arr do
            for r in arr do
            let s = m - p - q - r
            let res = binSearch s arr
            if Option.isSome res then
                yield (p, q, r, s)
            else
                ()
        }
    // series |> Seq.isEmpty |> not
    series |> Seq.toList

let ansRaffle = isFourSumEqualTo 15 [| 1 .. 10 |]

(*-----------------------------------------------------------------------
page 34
部分和問題
POJ No. 2386
-------------------------------------------------------------------------*)
let dfs successors isGoal start =
    let rec helper visited stack =
        seq {
            match stack with
            | []      -> ()
            | (x::xs) ->
                if isGoal x then yield x else ()
                if Set.contains x visited then
                    yield! helper visited xs
                else
                    yield! helper (Set.add x visited) ((successors x) @ xs)
        }
    helper Set.empty [start]

let partialSumEqualTo k xs =
    let isGoal (acc, _) = (Seq.sum acc = k)
    let successors (acc: int list, pool) =
        match pool with
        | []    -> []
        | y::ys -> [(y::acc, ys); (acc, ys)]
    let initial = ([], xs)
    let results = dfs successors isGoal initial
    results |> Seq.map (fst >> List.rev)
            |> Seq.distinct
            |> Seq.toList

let ansPartialSum = partialSumEqualTo 15 [1 .. 10]


(*-----------------------------------------------------------------------
page 35
Lake Counting
POJ No. 2386
-------------------------------------------------------------------------*)
let traverse successors start =
    let mutable visited = Set.empty
    let rec helper current =
        visited <- Set.add current visited
        seq {
            for x in successors current do
            if not <| Set.contains x visited then
                yield x
        }
        |> Seq.iter helper
    helper start
    visited

let lakeCount grid =
    let mutable chunks = []
    let n = Array.length grid
    let m = Array.length grid.[0]
    let withinGrid (i, j) = 0 <= i &&  i < n && 0 <= j && j < m
    let isPaddle (i, j) = grid.[i].[j] = 'W'
    let successor (i, j) =
        seq {
            for dx in [-1; 0; 1] do
            for dy in [-1; 0; 1] do
            if dx <> 0 || dy <> 0 then
                yield (i + dx, j + dy)
        }
        |> Seq.filter withinGrid
        |> Seq.filter isPaddle
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do
            let tup = (i, j)
            if isPaddle tup && Seq.forall (not << Set.contains tup) chunks then
                let newPaddle = traverse successor tup
                chunks <- newPaddle :: chunks
    List.length chunks

let grid = "W........WW.\n\
            .WWW.....WWW\n\
            ....WW...WW.\n\
            .........WW.\n\
            .........W..\n\
            ..W......W..\n\
            .W.W.....WW.\n\
            W.W.W.....W.\n\
            .W.W......W.\n\
            ..W.......W." |> fun s -> s.Split '\n'
                          |> Array.map Array.ofSeq

let ansPaddle = lakeCount grid




(*-----------------------------------------------------------------------
page 37
迷路の最短路
-------------------------------------------------------------------------*)
let bfs successors isGoal start =
    let rec run visited queue =
        seq {
            match queue with
            | []      -> ()
            | (x::xs) ->
                let node = Seq.head x
                if isGoal node then yield (List.rev x) else ()
                if Set.contains node visited then
                    yield! run visited xs
                else
                    let updatedHistory = List.map (fun n -> n::x) (successors node)
                    yield! run (Set.add node visited) (xs @ updatedHistory)
        }
    run Set.empty [[start]]

let solveMaze grid =
    let n = Array.length grid
    let m = Array.length grid.[0]
    let withinGrid (i, j) = 0 <= i &&  i < n && 0 <= j && j < m
    let isPassable (i, j) = grid.[i].[j] <> '#'
    let isGoal (i, j) = grid.[i].[j] = 'G'
    let successors (i, j) =
        seq {
            yield (i-1, j)
            yield (i+1, j)
            yield (i, j-1)
            yield (i, j+1)
        }
        |> Seq.filter withinGrid
        |> Seq.filter isPassable
        |> Seq.toList
    let start = seq {
                    for i in 0 .. n - 1 do
                    for j in 0 .. m - 1 do
                    if grid.[i].[j] = 'S' then
                        yield (i, j)
                }
                |> Seq.head
    bfs successors isGoal start |> Seq.head


let maze = "#S######.#\n\
            ......#..#\n\
            .#.##.##.#\n\
            .#........\n\
            ##.##.####\n\
            ....#....#\n\
            .#######.#\n\
            ....#.....\n\
            .####.###.\n\
            ....#...G#" |> fun s -> s.Split '\n'
                        |> Array.map Array.ofSeq

let ansMaze = solveMaze maze




(*-----------------------------------------------------------------------
page 42
硬貨の問題
-------------------------------------------------------------------------*)
let minCoinCount amount c1 c5 c10 c50 c100 c500 =
    let rec run acc coins remaining =
        match (coins, remaining) with
        | _, 0    -> Option.Some acc
        | [],   _ -> Option.None
        | (x::xs), _ ->
            let coinValue, coinCount = x
            let spend = min (remaining / coinValue) coinCount
            let acc' = (coinValue, spend) :: acc
            run acc' xs (remaining - coinValue * spend)
    let coins = [(500, c500); (100, c100); (50, c50); (10, c10); (5, c5); (1, c1)]
    run [] coins amount

let ansCoinCount = minCoinCount 620 3 2 1 3 0 2


(*-----------------------------------------------------------------------
page 43
区間スケジューリング問題
-------------------------------------------------------------------------*)
let segmentScheduling starting ending =
    let rec run acc pairs =
        match pairs with
        | []    -> acc
        | pair::rest ->
            let _, tf = pair
            let pairs' = List.filter (fun (time, _) -> time >= tf) rest
            run (pair::acc) pairs'
    let pairs = List.zip starting ending |> List.sortBy snd
    run [] pairs |> List.rev

let ansSegmentScheduling = segmentScheduling [1; 2; 4; 6; 8] [3; 5; 7; 9; 10]



(*-----------------------------------------------------------------------
page 45-46
Best Cow Line
POJ 3617
-------------------------------------------------------------------------*)

let bestCowLine (s: string) =
    let rec run arr =
        seq {
            if Array.isEmpty arr then
                ()
            else
                let rev = Array.rev arr
                if arr <= rev then
                    yield string arr.[0]
                    yield! run arr.[1 ..]
                else
                    yield string rev.[0]
                    yield! run rev.[1 ..]
        }
    run (Array.ofSeq s) |> String.concat ""

let ansBestCowLine = bestCowLine "ACDBCB"



