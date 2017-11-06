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

// Problem 1 Write Up
// ------------------------------------------------------------------------
// This was one of the easier problems that simply required a basic
// understanding of the mathematics behind it. In order to perform a
// mathematical operation on a fraction one must have 3 things:
//
//      1. Numerator
//
//      2. Denominator
//
//      3. Common Factor
//
// So for both the addition and multiplication infix problems I had to 
// figure out what the numerator and denominators where. For multiplication
// it was pretty straightforward, the numerator is the product of both 
// fraction numerators (i.e. a x b). For addition it is a bit more
// complicated, but if you do the math by hand, you notice that the
// numerator will always be: a x d + b x c. This is due to having to make
// the fraction have equal denominators and what you due to the bottom
// you must do to the top as well. For both the denominator was simply
// the product of the two fraction's denominators. Now that I had the 
// denominators, the next task was to find the common factors between them.
// Luckily this was pretty straightforward as the helper method for gcd
// was provided for us. Once the GCD was found, all that was necessary
// was to return a tuple that contained the numerator and denominator
// divided by the common factor.
// ------------------------------------------------------------------------
// Problem 1 Bugs
// ------------------------------------------------------------------------
// None that I can think of, however, if 0 is in the denominator for any
// of the provided fractions the program will error. No error checking for
// 0 denominators was provided.

// ============================== Problem #2 ==============================
// Write an F# function revlists xs that takes a list of lists xs and 
// reverses all the sub-lists:
// ========================================================================
let revlists xs = List.map List.rev xs

//Problem 2 Test Cases
revlists [[0;1;1];[3;2];[];[5]];;   // Should return [[1; 1; 0]; [2; 3]; []; [5]]

// Problem 2 Write Up
// ------------------------------------------------------------------------
// This problem ended up being easier than the first one in terms of 
// implementation. However, it did not start like that. At first I was 
// unsure about how to approach the problem as I didn't really understand
// list traversal in F#. However, after reading up on List.map I realized
// that you could simply use it to apply a function to each index. Since
// I already knew that List.rev existed - which reverses a list - I figured
// that I could use List.map to iterate through each index and apply
// List.rev to it, reversing the lists. It worked out in the end.
// ------------------------------------------------------------------------
// Problem 2 Bugs
// ------------------------------------------------------------------------
// Since I do not completely understand the implementations of List.map 
// and List.rev I am unsure as to what the potential side effects of each
// function is. As such I can say that I do not know if there are any bugs
// in this problem, but I do know that it at least works for the test 
// input and my own tests.

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

// Problem 3 Write Up
// ------------------------------------------------------------------------
// This is the first problem in the homework in my opinion that really 
// made us have to think functionally. In order to interleave lists, I
// know that we need to basically append the head of each list into a new
// one until their are no longer any elements in both lists. So first
// I handle the pattern matching of which I believed to be 3 different
// patterns:
//
//      1 & 2. One of the lists is empty: In which case return the other
//      list.
//
//      3. Both lists still have a head: In which case we perform a 
//      recursive call on interleave, but the real magic happens before it.
//      Because interleave returns a list, we can append directly to it.
//      So with that in mind we append the first elements of both lists
//      to the interleave function, until it reaches the end of both lists.
//
// By perfoming all of the above, we are able to interleave the lists
// together.
// ------------------------------------------------------------------------
// Problem 3 Bugs
// ------------------------------------------------------------------------
// None that I can think of, the interpreter says I have covered all cases
// so I think this is a good solution. If issues do occurr, it would be 
// news to me.

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

// Problem 4 Write Up
// ------------------------------------------------------------------------
// This was definitely one of the two harder problems on this homework
// assignment (though of the two it was the easiest). For the longest time
// I could not figure out how loop through the list and return two
// separate ones made from it. However, after a lot of serious thinking, I
// was able to come up with a working solution!
//
// In order to solve the cut problem we had to create a helper method
// called gencut, that actually cuts the list up to the index you specify.
// 
// I knew gencut would be recursive, but I was unsure of how to call it.
// However, after reading up on function in F# I was introduced to the 
// auxillary functions, and how to implement them. To me this was a godsend
// as I was able to figure out how to implement a 'loop' without actually
// utilizing the looping methods within F# (as we aren't allowed to).
//
// So using this auxillary method I was able to perform the necessary 
// pattern matches within gencut, without actually having to make gencut
// recursive. By simply making the loop method recursive I could call it
// too loop through the list and make it stop whenever necessary.
// 
// Cut simply used gencut with the number to stop at being the halfway 
// point of the list: i.e. list length / 2.
// ------------------------------------------------------------------------
// Problem 4 Bugs
// ------------------------------------------------------------------------
// None that I can think of, the interpreter says I have covered all cases
// so I think this is a good solution. If issues do occurr, it would be 
// news to me.

// ============================== Problem #5 ==============================
// Write an F# function shuffle xs that takes an even-length list, cuts it 
// into two equal-sized pieces, and then interleaves the pieces:
// ========================================================================
let shuffle xs = interleave(cut(xs))

// Problem 5 Test Cases
shuffle [1;2;3;4;5;6;7;8];;

// Problem 5 Write Up
// ------------------------------------------------------------------------
// Of problems 4 - 6 this was the easiest to complete. All that was 
// necessary was utilizing the understanding what the problem was asking
// to solve. 
//
// Once we realized that all it was doing was interleaving a cut list,
// that is all we did. Called interleave on a list that was cut in half.
// ------------------------------------------------------------------------
// Problem 5 Bugs
// ------------------------------------------------------------------------
// None that I could think of. Though whatever issues can occur in the 
// interleave or cut methods can bleed over to here as well.

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
// Problem 6 Write Up
// ------------------------------------------------------------------------
// This was the hardest problem of the homework (go-figure it is the last
// one). To solve this I utilized the technique of auxillary functions
// that we used to solve problem 4.
// 
// The first thing that needed to be done was trying to figure out how to 
// call shuffle on a deck that doesn't exist, since countshuffles only
// accepted an integer and not a list itself. However, after a bit of 
// thinking I realized that the ability to create the list was already
// provided for us. By us the [n0...n] list syntax we could create a list
// that consisted of all the values n0 to n. So we created a list that 
// housed [1..n] in it's indexes. 
//
// The next matter involved tackling how to create a while loop in F# 
// without explicitly using it, and only with recursion. So my mind 
// automatically went to the auxillary function, and using the same logic
// as a while loop I set about creating my loop auxillary.
//
// So I created a few variables for the loop:
//          1. n -> The number of times we called shuffle
//          2. deck -> The deck being shuffled
//          3. target -> The deck before being shuffled
//
// Simply through the use of these three variables it is pretty obvious
// what we set out to do. However, the issue lie in trying to figure out
// how to determine equality since I was running into issues with the
// if statements. 
//
// After browsing online we figured out about the 'when' statement, and
// realized that this was the perfect solution for our problem. By using
// these when clauses we were able to determine when the list was equal
// and then exit the loop.
//
// With that implemented we completed our goal of solving problem 6.
// ------------------------------------------------------------------------
// Problem 6 Bugs
// ------------------------------------------------------------------------
// None that I could think of. Though whatever issues can occur in the 
// interleave or cut methods can bleed over to here as well.
