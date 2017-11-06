1.) Write an uncurried F# function cartesian (xs, ys) that takes as input two lists xs and ys and returns a list of pairs that represents the Cartesian product of xs and ys. (The pairs in the Cartesian product may appear in any order.).

Let rec cartesian = function
| (xs, []) -> []
| ([], ys) -> []
| (x::xs, ys) -> (List.map(fun y -> x,y) ys) @ (cartesian(xs,ys))

2.) An F# list can be thought of as representing a set, where the order of the elements in the list is irrelevant. Write an F# function powerset such that powerset set returns the set of all subsets of set.

let rec powerset = function
| [] -> [[]]
| (x::xs) ->  let xss = powerset xs 
List.map(fun xs -> x::xs) xss @ xss

3.) The transpose of a matrix M is the matrix obtained by reflecting Mabout its diagonal.
Write an efficient F# function to compute the transpose of an m-by-nmatrix:

let rec transpose matrix = match matrix with
| row::rows -> // When the row is not empty
   match row with
   | col::cols-> // When column is not empty
      let first -= List.map List.head matrix // Get all the elements from all rows in the list of lists
      let rest = transpose(list.map List.tail matrix) // Transpose the remaining elements.
      first::rest
    |  _ -> [] // column empty
 | _ -> [] // row empty
