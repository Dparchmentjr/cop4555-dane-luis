// Homework #1 By Dane E. Parchment Jr. | 4925790 | Teammate: Luis Averhoff

// ============================== Problem #1 ==============================
// A fraction like 2/3 can be represented in F# as a pair of type int * int. 
// Define infix operators .+ and .* to do addition and multiplication of fractions:
// ========================================================================

// Helper Functions
let rec gcd = function
    | (a, 0) -> a
    | (a, b) -> gcd(b, a % b)

// Addition    
let (.+)(a,b)(c,d) = 
    let numerator = a * d + b * c
    let denominator = b * d
    let commonF = gcd(numerator, denominator)
    (numerator / commonF, denominator / commonF)

// Multiplication
let (.*)(a,b)(c,d) =
    let numerator = a * c
    let denominator = b * d
    let commonF = gcd(numerator, denominator)
    (numerator / commonF, denominator / commonF)


// Problem 1 Test Cases
(1,2) .+ (1,3);;            // Should be (5,6)
(1,2) .* (1,3);;            // Should be (1,6)
(1,2) .+ (2,3) .* (3,7);;   // Should be (11,14)

// ============================== Problem #2 ==============================
// Write an F# function revlists xs that takes a list of lists xs and 
// reverses all the sub-lists:
// ========================================================================
let revlists xs = List.map List.rev xs

//Problem 2 Test Cases
revlists [[0;1;1];[3;2];[];[5]];;   // Should return [[1; 1; 0]; [2; 3]; []; [5]]

// ============================== Problem #3 ==============================
// Write an F# function interleave(xs,ys) that interleaves two lists:
//
// * Assume that both lists have the same length
// ========================================================================
let rec interleave = function   
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x :: xs, y :: ys) -> x :: y :: interleave (xs, ys)

// Problem 3 Test Cases
interleave ([1;2;3],[4;5;6]);;  // Should return [1; 4; 2; 5; 3; 6]

// ============================== Problem #4 ==============================
// Write an F# function cut xs that cuts a list into two equal parts:
//
// First write a function called gencut that cuts a list into two n parts
// ========================================================================

// Helper Methods
let gencut (n, ls) =
    let rec loop = function
        | 0, xs, ys -> List.rev xs, ys  // Noticed that printed the list backwards, so I reversed it (no idea why backwards)
        | n, xs, [] -> (xs, [])
        | n, xs, y::ys -> loop(n - 1, y::xs, ys)
    loop(n, [], ls)

// Cut List Into 2 Parts
let cut = function
    | xs -> gencut(List.length xs / 2, xs)


// Problem 4 Test Cases
gencut(2, [1;3;4;2;7;0;9]);;
cut [1;2;3;4;5;6];;

// ============================== Problem #5 ==============================
// Write an F# function shuffle xs that takes an even-length list, cuts it 
// into two equal-sized pieces, and then interleaves the pieces:
// ========================================================================
let shuffle xs = interleave(cut(xs))

// Problem 5 Test Cases
shuffle [1;2;3;4;5;6;7;8];;

// ============================== Problem #6 ==============================
// Write an F# function shuffle xs that takes an even-length list, cuts it 
// into two equal-sized pieces, and then interleaves the pieces:
// ========================================================================

// Helper Methods
let count (deck, target) =
    let rec loop = function
        | (n, deck, target) when deck = target -> n
        | (n, deck, target) when deck <> target -> loop(n + 1, shuffle deck, target)
    loop(1, deck, target)

let countShuffles n =
    let shuffled = [1..n]
    let original = shuffled
    count(shuffle shuffled, original)



// Problem 6 Tests
countShuffles 4;;   // Should be 2
countShuffles 52;;  // Should be 8