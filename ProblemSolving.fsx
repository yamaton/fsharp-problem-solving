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
    List.sort sticks
        |> choose 3
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

let solveFourSumEqualTo m arr =
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

let ansRaffle = solveFourSumEqualTo 15 [| 1 .. 10 |]

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

let solvePartialSumEqualTo k xs =
    let isGoal (acc, _) = (Seq.sum acc = k)
    let successors (acc: int list, pool) =
        match pool with
        | []    -> []
        | y::ys -> [(y::acc, ys); (acc, ys)]
    let initial = ([], xs)
    let results = dfs successors isGoal initial
    results
        |> Seq.map (fst >> List.rev)
        |> Seq.distinct
        |> Seq.toList

let ansPartialSum = solvePartialSumEqualTo 15 [1 .. 10]


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
            if (not << Set.contains x) visited then
                yield x
        }
            |> Seq.iter helper
    helper start
    visited

let solveLakeCount grid =
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
            ..W.......W."
        |> fun s -> s.Split '\n'
        |> Array.map Array.ofSeq

let ansLakeCount = solveLakeCount grid

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
                if isGoal node then
                    yield (List.rev x)
                else
                    ()
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
            ....#...G#"
            |> fun s -> s.Split '\n'
            |> Array.map Array.ofSeq

let ansMaze = solveMaze maze




(*-----------------------------------------------------------------------
page 42
硬貨の問題
-------------------------------------------------------------------------*)
let solveMinCoinCount amount c1 c5 c10 c50 c100 c500 =
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

let ansCoinCount = solveMinCoinCount 620 3 2 1 3 0 2


(*-----------------------------------------------------------------------
page 43
区間スケジューリング問題
-------------------------------------------------------------------------*)
let solveSegmentScheduling starting ending =
    let rec run pairs =
        seq {
            match pairs with
            | []         -> ()
            | pair::rest ->
                yield pair
                let _, tf = pair
                let pairs' = List.filter (fun (time, _) -> time >= tf) rest
                yield! run pairs'
        }

    let pairs = List.zip starting ending |> List.sortBy snd
    run pairs |> Seq.toList

let ansSegmentScheduling = solveSegmentScheduling [1; 2; 4; 6; 8] [3; 5; 7; 9; 10]



(*-----------------------------------------------------------------------
page 45-46
Best Cow Line
POJ 3617
-------------------------------------------------------------------------*)
let solveBestCowLine (s: string) =
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

let ansBestCowLine = solveBestCowLine "ACDBCB"




(*-----------------------------------------------------------------------
page 47
Saruman's Army
POJ 3069
-------------------------------------------------------------------------*)
let solveSarumansArmy xs radius =
    let rec run points =
        seq {
            match points with
            | []    -> ()
            | x::_  ->
                let rightMost = points |> Seq.takeWhile (fun y -> y <= x + radius)
                                       |> Seq.last
                yield rightMost
                let points' = List.skipWhile (fun y -> y <= rightMost + radius) points
                yield! run points'
        }
    run xs |> Seq.toList

let ansSarumansArmy = solveSarumansArmy [1; 7; 15; 20; 30; 50] 10



(*-----------------------------------------------------------------------
page 49
Fence Repair
POJ 3253
-------------------------------------------------------------------------*)
let solveFenceRepair (lengths: int list) =
    let rec run xs acc =
        match xs with
        | []   -> -1
        | [x]  -> acc
        | x::y::rest ->
            let cost = x + y
            let xs' = List.sort (cost::rest)
            run xs' (acc + cost)
    run (List.sort lengths) 0

let ansFenceRepair = solveFenceRepair [8; 5; 8]



(*-----------------------------------------------------------------------
page 52
01 ナップサック問題
-------------------------------------------------------------------------*)
// [TODO} Replace dp array with defaultdict
// [TODO] replace "state" of dp array cell with custom type like (value, history)
// [TODO] Use deque to remove List.rev
let inline solveKnapsack01 weightValuePairs weightUB =
    let criteria = Seq.sumBy snd
    let f arr (weight, value) =
            [|
                for (i, hist) in (Array.indexed arr) do
                if i - weight >= 0 then
                    let histAnother = arr.[i - weight]
                    yield [hist; (weight, value)::histAnother] |> List.maxBy criteria
                else
                    yield hist
            |]
    let ini = Array.create (weightUB + 1) []
    let res = List.fold f ini weightValuePairs
    res |> Array.maxBy criteria

let ansKnapsack01 = solveKnapsack01 [(2, 3); (1, 2); (3, 4); (2, 4)] 5 |> List.length



(*-----------------------------------------------------------------------
page 56
最長共通部分列問題 (longest common subsequence)
-------------------------------------------------------------------------*)
// [TODO] replace array with defaultdict
// [TODO] replace string list with string deque
let lcs left right =
    let mutable dp = Map.ofList<int*int, string list> [(-1, -1), []]
    let n = String.length left
    let m = String.length right
    for i in [0 .. n-1] do
        dp <- dp.Add ((i, -1), [])
    for j in [0 .. m-1] do
        dp <- dp.Add ((-1, j), [])
    for i in [0 .. n-1] do
        for j in [0 .. m-1] do
            let item =
                if left.[i] = right.[j] then
                    let c = string left.[i]
                    [c::dp.[i-1, j-1]; dp.[i-1, j]; dp.[i, j-1]]
                        |> List.maxBy List.length
                else
                    [dp.[i-1, j]; dp.[i, j-1]]
                        |> List.maxBy List.length
            dp <- dp.Add ((i, j), item)
    dp.[n-1, m-1] |> List.rev |> String.concat ""

let ansLCS = lcs "AGGTAB" "GXTXAYB"



(*-----------------------------------------------------------------------
page 58
個数制限なしナップサック問題 (knapsack with replacement)
-------------------------------------------------------------------------*)
let inline solveKnapsackWithReplacement weightValuePairs weightUB =
    let criteria = Seq.sumBy snd
    let f arr wvpairs =
        [|
            for (i, valHistPair) in (Array.indexed arr) do
                let anotherValHistPairs =
                    seq {
                        for (weight, value) in wvpairs do
                        if i - weight >= 0 then
                            yield (weight, value)::arr.[i - weight]
                    }
                let candidates = seq {yield valHistPair; yield! anotherValHistPairs}
                let best = Seq.maxBy criteria candidates
                yield best
        |]
    let ini = Array.create (weightUB + 1) []
    let repeatNum = weightUB / (Seq.map fst weightValuePairs |> Seq.min)
    let res = Seq.fold f ini (Seq.replicate repeatNum weightValuePairs)
    res |> Array.maxBy criteria |> List.rev

let ansKnapsackWithReplacement = solveKnapsackWithReplacement [(3, 4); (4, 5); (2, 3)] 7


(*-----------------------------------------------------------------------
page 60
01 ナップサック問題その 2
-------------------------------------------------------------------------*)

let ansKnapsackValue =
    let tuples = solveKnapsack01 [(2, 3); (1, 2); (3, 4); (2, 2)] 5
    Seq.sumBy snd tuples


(*-----------------------------------------------------------------------
page 62
個数制限付き部分和問題
-------------------------------------------------------------------------*)
// [TODO] Complexity is terrible. Rewreite.
let solveRestrictedSum intCountPairs target =
    let f (a, n) = Seq.map ((*) a) [0 .. n]
    let xss = List.map f intCountPairs
    let cartesianAdder acc xs = Seq.allPairs acc xs |> Seq.map (fun (tt, x) -> (tt + x))
    let ini = Seq.singleton 0
    let res = Seq.fold cartesianAdder ini xss |> Set.ofSeq
    Set.contains target res

let ansRestrictedSum = solveRestrictedSum (List.zip [3; 5; 8] [3; 2; 2]) 17



(*-----------------------------------------------------------------------
page 63
最長増加部分列問題 (LIS: Longest increasing subsequence)
-------------------------------------------------------------------------*)
//




(*-----------------------------------------------------------------------
page 66
分割数 (Partitioning)
-------------------------------------------------------------------------*)
//



(*-----------------------------------------------------------------------
page 67
重複組合せ (combinations)
-------------------------------------------------------------------------*)
//
