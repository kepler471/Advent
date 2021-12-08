let input = 
    (System.IO.File.ReadAllText "inputs/07.txt").Split [|','|] 
    |> Seq.map float 
    // seq { 1.0;3.0;2.0 }
    
let mid ns =
    let mean =
        ns |> Seq.average

    let weight = 
        let right = ns |> Seq.filter (fun n -> n >= mean) |> Seq.length
        let left = ns |> Seq.filter (fun n -> n <= mean) |> Seq.length
        if right >= left then 1.0 else -1.0

    if weight = 1.0 then ceil mean else floor mean

let delta (x0: float) x = 
    x |> Seq.map (fun xi -> abs(xi - x0))

let inline median input = 
    let sorted = input |> Seq.toArray |> Array.sort
    let m1,m2 = 
        let len = sorted.Length-1 |> float
        len/2. |> floor |> int, len/2. |> ceil |> int 
    (sorted.[m1] + sorted.[m2] |> float)/2.

let burn n = 
    [ 1..n ] |> List.sum

// let ap n = 
    

let f ns x0 =
    ns |> Seq.map (fun n -> 0.5 * (abs(n - x0)**2.0 + abs(n - x0)))

let deriv g =
    let dx = 0.00001
    (fun x -> (g (x + dx) - g x) / dx)

    // ns |> Seq.map (fun n -> ap (n - x0) )
    // delta x0 ns
let test = [16;1;2;0;4;2;7;1;2;14] |> List.map float
// f 2.0 test |> Seq.sum
// f 5.0 test |> Seq.sum

f test 2.0
f test
deriv (f test)
(f >> deriv) test
// (*
// (define (deriv g)
//   (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

// (define dx 0.00001)

// (define (cube x) (* x x x))
// ;; ((deriv cube) 5)
// ;; > 75.00014999664018
// (define (newton-transform g)
//   (lambda (x) (- x (/ (g x) ((deriv g) x)))))
// (define (newtons-method g guess)
//   (fixed-point (newton-transform g) guess))

// (define (sqrt-newton x)
//   (newtons-method
//    (lambda (y) (- (square y) x)) 1.0))

// (define (fixed-point-of-transform g transform guess)
//   (fixed-point (transform g) guess))

// (define (cubic a b c)
//   (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

// (define (double f)
//   (lambda (x) (f (f x))))

// (define (compose f g)
//   (lambda (x) (f (g x))))

// ;; (define ((compose f g) x)
// ;;   (f (g x)))

// (define (repeated f n)
//   (define (iter i result)
//     (if (< i 1)
// 	result
// 	(iter (- i 1) (compose f result))))
//   (iter n identity))

// ;; http://community.schemewiki.org/?sicp-ex-1.43

//  (define (repeated f n)
//    (lambda (x) (cond ((= n 0) x)
//                      (else
//                       ((compose (repeated f (- n 1)) f) x)))))

// (define (repeated f n)
//   (accumulate compose identity (lambda (i) f) 1 inc n))
// *)


// delta (median test) test |> Seq.map (int >> burn)
// delta (median input) input |> Seq.sum
// delta (median input) input |> Seq.map (int >> burn) |> Seq.sum
// let newmedtest = delta (median test) test |> Seq.map (int >> burn) |> median
// let newmed = delta (median input) input |> Seq.map (int >> burn) |> median
// delta newmed input |> Seq.sum
// delta 2.0 test |> Seq.sum
// delta 5.0 test |> Seq.sum