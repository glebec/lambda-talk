// # Lambda as JS, or A Flock of Functions

// ### Combinators, Lambda Calculus, and Church Encodings in JavaScript

// #### Notes for [a talk by Gabriel Lebec (click for videos and slides)](https://github.com/glebec/lambda-talk)

const chalk = require('chalk')
const green = chalk.green.bind(chalk)
const red = chalk.red.bind(chalk)

const errs = []

function demo (msg, bool) {
	if (typeof bool === 'function') bool = bool(true)(false)
	if (!!bool !== bool) throw TypeError('second arg must be boolean (JS or LC)')
	console.log(`${bool ? green('✔') : red('✖')} ${msg}`)
	if (!bool) errs.push(Error(red(`Spec fail: ${msg} -> ${bool}`)))
}

function logErrsAndSetExitCode () {
	errs.forEach(err => console.error(err))
	if (errs.length) process.exitCode = 1
}

function header (str) {
	console.log('\n' + str + '\n')
}

// ## Introduction

// The Lambda Calculus is a symbol manipulation framework developed by the mathematician Alonzo Church in the 1930s. It was intended to be an extremely tiny syntax which could nevertheless suffice to calculate anything computable. The mathematical advantage of such a syntax is that with such extreme simplicity, it becomes easier to write formal proofs about computational logic – demonstrated when Church solved David Hilbert's famous Decision Problem using LC.

// Another famous mathematician, Alan Turing, formulated a different model of universal computation – the eponymous Turing Machine. A TM is a hypothetical device, again of extreme simplicity; it can read or write to cells on an infinite tape, each cell containing data or instructions. Turing published a paper also solving the Decision Problem, mere months after Church.

// Turing proved that the Turing Machine and Lambda Calculus are totally equivalent. Everything that one can calculate, the other can. Not only this, but Turing and Church posited (in the "Church-Turing Thesis") that these systems capture the definition of computability in a universal way.

// Turing Machines are exciting because if a hypothetical machine can compute anything computable, then perhaps a real machine can as well. Modern computers add many features and optimizations beyond what is featured in an actual Turing Machine; these features make the machine more convenient and performant, but do not compromise its essential nature as a universal computing device.

// As machine codes, assemblers, compilers, and higher-level languages have developed to program these real machines, they have largely evolved with a focus on the essence of the machine – memory, statefulness, effects, imperative instructions and so forth. Over time, some of these languages have shifted ever farther into pure abstractions and conceptual description over more machine-centric and stateful models.

// However, mathematicians and computer scientists have long known that the entirely abstract lambda calculus, being equivalent to a TM, meant that computations could be expressed in a style totally independent from machine instructions. Instead, a language consisting of first-class function expressions – aka lambda abstractions – could be subsequently compiled into machine code. Such a language would benefit from decades of mathematical research. And just as real computers extend Turing Machines with extra power and convenience, these _functional languages_ would extend the lambda calculus with additional features and under-the-hood shortcuts (such as hardware-based arithmetic).

// What follows is a demonstration, mostly for enjoyment and insight, of the surprising and delightful omnipotence of the lambda calculus. JavaScript, like any functional language, is closely related to LC; it contains the necessary ingredients (variables, parentheses, first-class functions) nestled among the additional luxuries most programmers take for granted.

// # First steps with simple combinators

// Starting simple. A lambda abstraction is simply an anonymous unary function. In LC there are no named functions, but for convenience we will use names. In mathematical notation, `:=` means "is defined as".


// ## Identity

// Here is an almost trivial function: Identity, defined as `λx.x`. `λ` is used to indicate the start of a lambda abstraction (read: function expression). The sole parameter is listed to the left of the `.`, and the return expression is written after the `.`.

header('I := λx.x')
const I = x => x

// In LC, variables may be static tokens that stand for whatever you want. We'll use ES2015 Symbols for this.

const tweet = Symbol('tweet')
const chirp = Symbol('chirp')

// In LC, juxtaposition is function application. That is, `M I` means "apply the `M` function to the `I` argument". A space is not strictly necessary; `fx` usually means "apply `f` to `x`". Some of our examples will use multi-letter variables; to disambiguate them from multiple single-letter variables, we will frequently use SPACED UPPERCASE.

// ```lc
// I tweet
// = (λx.x) tweet
// = tweet
// ```

demo('I tweet = tweet', I(tweet) === tweet)
demo('I chirp = chirp', I(chirp) === chirp)

// in LC, valid arguments include other lambda abstractions. This is the meaning of "first-class" in the phrase "first-class functions"; functions are values which may be passed into or returned from other functions. In this sense, functions are both like verbs (they can perform a calculation) and nouns (they can be the subject or object of a calculation).

// ```lc
// I I
// = (λx.x)(λx.x)
// = λx.x
// ```

demo('I I = I', I(I) === I)

// of course we can build up more complex expressions. In LC, function application associates left; that is, `a b c d` = `((a b) c) d`. Evaluating terms by substituting arguments into function bodies has the fancy term of "β-reduction". A reducible expression (or "redex") that has been completely simplified in this fashion is said to be in its "β-normal" or just "normal" form.

// ```lc
//      id    id       id    id  tweet   <--- the initial reducible expression ("redex")
// = (((id    id)      id)   id) tweet   <--- application associates left
// = ((    id          id)   id) tweet   <--- evaluating function applications ("β-reduction")
// = (           id          id) tweet   <--- continuing β-reduction
// =                    id       tweet   <--- more β-reduction
// =                             tweet   <--- normal form of the original redex
// ```

// LC has great overlap with combinatory logic. A pioneer of CL was the mathematician Haskell Curry, whose name now adorns the Haskell language as well as the functional concept of currying. (Curry actually cited the technique from Schönfinkel, and Frege used it even earlier.) Combinatory logic is concerned with combinators, that is, functions which operate only on their inputs. The Identity function is a combinator, often abbreviated I, sometimes called the Idiot bird.

header('Idiot := I')
const Idiot = I


// ## Self-Application

// Curry was an avid birdwatcher, and the logician Raymond Smullyan named many combinators after birds in his honor. The self-application combinator M (for "Mockingbird"), aka ω (lowercase omega), looks like this:

header('Mockingbird := M := ω := λf.ff')
// Remember, `fx` means "apply f to x." So `ff` means "apply f to itself".
const ω = fn => fn(fn)
const M = ω
const Mockingbird = M

// Can you guess what the Mockingbird of Identity is?

// ```lc
// M I
// = (λf.ff)(λx.x)
// = (λx.x)(λx.x) = I I
// = λx.x = I
// ```

// That's right, it's the same as Identity of Identity… which is Identity.

demo('M I = I I = I', M(I) === I(I) && I(I) === I)

// What about… the Mockingbird of Mockingbird?

// That's the Ω (big Omega) Combinator. It diverges (goes on forever).

// ```lc
// M M
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// ...
// ```

try {
  M(M)
} catch (err) {
  demo('M M = M M = M M = ' + err.message, true)
}


// ## Church Encodings: Booleans

header('First encodings: boolean value functions')

// OK this is nice and all, but how can we do anything real with only functions? Let's start with booleans. Here we need some multi-arg functions… in lambda calc, `λabc.a` is just shorthand for `λa.λb.λc.a`, or `λa.(λb.(λc.a)))`. In other words, all functions are curried.

// ### True and False

header('T := λxy.x')
const T = thn => els => thn

header('F := λxy.y')
const F = thn => els => els

// Hm. How can "true" and "false" be functions? Well, the point of booleans is to _select_ between a then-case and an else-case. The LC booleans are just functions which do that! `T` takes two vals and returns the first; `F` takes two vals and returns the second. If you apply an unknown bool func to two vals, the first val will be returned if the bool was true, else the second val will be returned.

demo('T tweet chirp = tweet', T(tweet)(chirp) === tweet)
demo('F tweet chirp = chirp', F(tweet)(chirp) === chirp)

// ### Flipping Arguments

// Another fun way we could have produced F was with the Cardinal combinator. The Cardinal, aka `C`, aka `flip`, takes a binary (two-argument) function, and produces a function with reversed argument order.

header('Cardinal := C := flip := λfab.fba')
const flip = func => a => b => func(b)(a)
const C = flip
const Cardinal = C

// With the Cardinal, we can derive `F` from the flip of `T`:

header('F = C T')

demo('flip T tweet chirp = chirp', flip(T)(tweet)(chirp) === chirp)

// Regardless of whether you define `F` manually, or as the flip of `T`, these functions are _encodings_ of boolean values. They represent booleans in useful and meaningful ways, which preserve the behavior of the values. Specifically, they are Church encodings – representations developed / discovered by Alonzo Church. Their real power is revealed when we start defining logical operations on them.

// ### Negation

header('NOT := λb.bFT')
const NOT = chooseOne => chooseOne(F)(T)

// Remember, booleans in the LC are really functions which select one of two arguments. Try reducing the expression `NOT T` yourself, then check your work below.

// NOT T
// = (λb.bFT) T
// = TFT
// = (λxy.x) F T
// = F

demo('NOT T = F', NOT(T) === F)
demo('NOT F = T', NOT(F) === T)

// There's another way we could define `NOT`, however, and we've already seen it. Remember how the flip of `T` (aka the Cardinal of `T`) is `F`? It works the other way too – `C F = T`! At least, `C F` yields a function that behaves identically to `T`:

demo('CF = T', C(F)(tweet)(chirp) === tweet)
demo('CT = F', C(T)(tweet)(chirp) === chirp)

// This means that when it comes to Church encodings of booleans, the Cardinal behaves just like our manually-defined `NOT`.

// ### Conjunction and Disjunction

// Can you construct a function for boolean `AND`? It will have to take two unknown boolean functions as parameters, and route to an output of the correct boolean result. Give it a try.

header('AND := λpq.pqF')

// (λpq.pqp also works; can you see why?)

const AND = p => q => p(q)(F)

demo('AND F F = F', AND(F)(F) === F)
demo('AND T F = F', AND(T)(F) === F)
demo('AND F T = F', AND(F)(T) === F)
demo('AND T T = T', AND(T)(T) === T)

// If you work through the logic, you might notice that this function exhibits short-circuiting. If `p = F`, we don't bother using `q`. Similarly, our `OR` function below short circuits; if `p = T`, we don't bother using `q`.

header('OR := λpq.pTq')

// (λpq.ppq also works)

const OR = p => q => p(T)(q)

demo('OR F F = F', OR(F)(F) === F)
demo('OR T F = T', OR(T)(F) === T)
demo('OR F T = T', OR(F)(T) === T)
demo('OR T T = T', OR(T)(T) === T)

// `λpq.ppq` also works because if `p` is true, it is supposed to select `T`, but `p = T`, so we can just reuse it. Notice something interesting here: `λpq.ppq` behaves exactly like the Mockingbird. It takes a value, `p`, and self-applies `p` (producing `pp`). Well, `Mp = pp`, and you can apply that result to another value `q`, to get `ppq`; therefore, `Mpq = ppq`. So `M` and `λpq.ppq` behave identically (i.e. they are "extensionally equivalent", and in fact `λpq.ppq` is sometimes called `M*` or "the Mockinbird once-removed". The Mockingbird works as a boolean `OR` function:

demo('M F F = F', M(F)(F) === F)
demo('M T F = T', M(T)(F) === T)
demo('M F T = T', M(F)(T) === T)
demo('M T T = T', M(T)(T) === T)

// ### Demo: De Morgan's Laws

// With working booleans, we can illustrate some more complex logic, such as one of De Morgan's Laws: `!(p && q) === (!p) || (!q)`.

header('De Morgan: not (and P Q) = or (not P) (not Q)')

function deMorgansLawDemo (p, q) { return NOT(AND(p)(q)) === OR(NOT(p))(NOT(q)) }

demo('NOT (AND F F) = OR (NOT F) (NOT F)', deMorgansLawDemo(F, F))
demo('NOT (AND T F) = OR (NOT T) (NOT F)', deMorgansLawDemo(T, F))
demo('NOT (AND F T) = OR (NOT F) (NOT T)', deMorgansLawDemo(F, T))
demo('NOT (AND T T) = OR (NOT T) (NOT T)', deMorgansLawDemo(T, T))

// ### Boolean Equality

// However, this whole time we have been cheating in the demonstrations. Lambda calculus doesn't have any `===` operator to check for equality! Don't fret though, everything is functions. We can define our own equality function for booleans.

header('BEQ := λpq.p (qTF) (qFT)')
const BEQ = p => q => p( q(T)(F) )( q(F)(T) )

demo('BEQ F F = T', BEQ( BEQ(F)(F) )(T))
demo('BEQ F T = F', BEQ( BEQ(F)(T) )(F))
demo('BEQ T F = F', BEQ( BEQ(T)(F) )(F))
demo('BEQ T T = T', BEQ( BEQ(T)(T) )(T))

// Not a JS operator in sight (our `demo` function accepts church encodings). But for clarity's sake, we'll go back to using JS's equality operator in our `demo` calls.


// ## Church Encodings: Numerals

header('Numbers')

// Booleans are neat, but surely to compute arithmetic you need language-supported math. Right? …Nah. You can construct math from scratch.

// The Church encoding for a natural number n is an n-fold "compositor" (composing combinator). In other words, "2" is a function that composes a function `f` twice: `2 f x = f(f(x))`. Whereas "4" is a function that composes any `f` four times: `4 f x = f(f(f(f(x)`. In this sense, 2 and 3 can be read more like "two-fold" and "three-fold", or "twice" and "thrice".

// ### Hard-Coded Numbers

// In this system, `ZERO` is a function which applies a function `f` zero times to `x`. So… `ZERO` ignores the function argument, just returning `x`; `0 f x = x`.

header('0 := λfx.x')
// (fun fact, `0 = F`)
const ZERO = fn => x => x

// Zero applications of Mockingbird to `tweet` is just `tweet`. Good thing too, because applying Mockingbird to `tweet` would throw an error in JS (though it would work fine in LC, just by not simplifying further).

demo('0 M tweet = tweet', ZERO(M)(tweet) === tweet)

// We could hard-code `ONCE` as a single application of `f`…

header('hard-coded 1 and 2')

header('1 := λfx.fx')
const ONCE = fn => x => fn(x)

// Another fun fact – this is one step removed from `I`, called `I*` ("I-star"). In fact we could have shortened this to be `1 := I`. So 0 is false and 1 is identity, how nice!

demo('1 I tweet = tweet', ONCE(I)(tweet) === tweet)

// Identity is a bit boring, however, because the n-fold composition of `I` is always `I`, even for n = 0. The above example doesn't really prove our function is doing anything interesting. Let's cheat a bit with some string ops (note, this is polluting LC with some JS, but it's just for demonstration):

const λ = 'λ'
const yell = str => str + '!'

demo('0 yell λ = λ', ZERO(yell)(λ) === 'λ')
demo('1 yell λ = yell λ = λ!', ONCE(yell)(λ) === 'λ!')

// we could also hard-code `TWICE`.

header('2 := λfx.f(fx)')
const TWICE = fn => x => fn(fn(x))

demo('2 yell λ = yell (yell λ) = λ!!', TWICE(yell)(λ) === 'λ!!')

// ### Successor

// This hard-coding works, but is very limiting. We can't do true arithmetic like this, where operations on numbers generate other numbers. What we need is a way to count up.

header('SUCCESSOR')

header('SUCCESSOR := λnfx.f(nfx)')
const SUCCESSOR = num => fn => x => fn(num(fn)(x))

// Don't get lost in the weeds. All we are saying is that the successor of `n` does `n` compositions of `f` to a value `x`, and then it does _one more_ application of `f` to the result. Therefore, it ends up doing 1 + n compositions of `f` in total.

const newOnce   = SUCCESSOR(ZERO)
const newTwice  = SUCCESSOR(SUCCESSOR(ZERO)) // we can use multiple successors on zero, or…
const newThrice = SUCCESSOR(newTwice) // …apply successor to already-obtained numbers.

demo('1 yell λ = λ!',     newOnce(yell)(λ) === 'λ!')
demo('2 yell λ = λ!!',   newTwice(yell)(λ) === 'λ!!')
demo('3 yell λ = λ!!!', newThrice(yell)(λ) === 'λ!!!')

// ### Composition and Point-Free Notation

// There is another way to write successor. Point-free (some joke "point-less") notation means to define a function purely as a combination of other functions, without explicitly writing final arguments. Sometimes this style reveals what a function *is* rather than what explain what it *does*. Other times it can be abused to produce incomprehensible gibberish. Successor is a reasonable candidate for it, however.

// We are doing n-fold compositions, so let's define an actual `compose` function to help. Composition is often notated as `∘` in infix position: `(f ∘ g) x = f(g(x))`. However, Lambda Calculus only includes prefix position function application. Smullyan named this the Bluebird after Curry's `B` combinator.

header('Bluebird := B := (∘) := compose := λfgx.f(gx)')
const compose = f => g => x => f(g(x))
const B = compose
const Bluebird = B

demo('(B NOT NOT)  T =  NOT (NOT T)', (B(NOT)(NOT))(T)  === NOT(NOT(T)))
demo('(B yell NOT) F = yell (NOT F)', (B(yell)(NOT))(F) === yell(NOT(F)))

// Now that we have an actual composition function, we can define successor without mentioning the final `x` value argument.

header('SUCC := λnf.f∘(nf) = λnf.Bf(nf)')
const SUCC = num => fn => compose( fn )( num(fn) )

// This is just a terse way of repeating what we already know: if a given `n` composes some function `f` n times, then the successor of n is a function which composes one additional `f`, for a total of 1 + n compositions.

const n0 = ZERO
const n1 = SUCC(n0)
const n2 = SUCC(SUCC(n0))
const n3 = SUCC(SUCC(SUCC(n0)))
const n4 = SUCC(n3)

demo('1 yell λ = λ!',    n1(yell)(λ) === 'λ!')
demo('2 yell λ = λ!!',   n2(yell)(λ) === 'λ!!')
demo('3 yell λ = λ!!!',  n3(yell)(λ) === 'λ!!!')
demo('4 yell λ = λ!!!!', n4(yell)(λ) === 'λ!!!!')

// ### Arithemtic

// #### Addition

// Things will get pretty slow if we can only increment by 1. Let's add addition.

header('ADD := λab.a(succ)b')
const ADD = numA => numB => numA(SUCC)(numB)

// Aha, addition is just the Ath successor of B. Makes sense. For example, `ADD 3 2 = 3 SUCC 2`, which could be read as "thrice successor of twice".

const n5 = ADD(n2)(n3)
const n6 = ADD(n3)(n3)

demo('ADD 5 2 yell λ = λ!!!!!!!', ADD(n5)(n2)(yell)(λ) === 'λ!!!!!!!')
demo('ADD 0 3 yell λ = λ!!!',     ADD(n0)(n3)(yell)(λ) === 'λ!!!')
demo('ADD 2 2 = 4',               ADD(n2)(n2)(yell)(λ) === n4(yell)(λ))

// These equivalence checks using `yell` and `'λ'` are a bit verbose. It's annoying, but a mathematical truth, that there can be no general algorithm to decide if two functions are equivalent – and we cannot rely on JS function equality because we are generating independent function objects. Since `yell` and `'λ'` are already impure non-LC code, we might as well go all the way and define `church` and `jsnum` to convert between Church encodings and JS numbers.

header('LC <-> JS: church & jsnum')

function church (n) { return n === 0 ? n0 : SUCC(church(n - 1)) }
function jsnum (c) { return c(x => x + 1)(0) }

demo(        'church(5) = n5', church(5)(yell)(λ) === n5(yell)(λ))
demo(       'jsnum(n5) === 5',          jsnum(n5) === 5)
demo('jsnum(church(2)) === 2',   jsnum(church(2)) === 2)

// #### Multiplication

// Back to math. How about multiplication?

header('MULT := λab.a∘b = compose')
const MULT = compose

// How beautiful! Multiplication is the composition of numbers. For example, `MULT 3 2 = 3 ∘ 2`, which could be read as "thrice of twice", or "three of (two of (someFn))". If you compose a function `f` two times — `f ∘ f` — and then you compose that result three times — `(f∘f) ∘ (f∘f) ∘ (f∘f)` — you get the six-fold composition of f: `f ∘ f ∘ f ∘ f ∘ f ∘ f`. It helps to know that composition is associative — `f ∘ (g ∘ h) = (f ∘ g) ∘ h`.

demo('MULT 1 5 = 5', jsnum( MULT(n1)(n5) ) === 5)
demo('MULT 3 2 = 6', jsnum( MULT(n3)(n2) ) === 6)
demo('MULT 4 0 = 0', jsnum( MULT(n4)(n0) ) === 0)
demo('MULT 6 2 yell λ = λ!!!!!!!!!!!!', MULT(n6)(n2)(yell)(λ) === 'λ!!!!!!!!!!!!')

const n8 = MULT(n4)(n2)
const n9 = SUCC(n8)

// #### Exponentiation

// Exponentiation is remarkably clean too. When we say 2^3, we are saying "multiply two by itself three times"; or putting it another way, "twice of twice of twice". So for any base and power, the result is the power-fold composition of the base:

header('Thrush := POW := λab.ba')
const POW = numA => numB => numB(numA)

// As you can see, we also call this combinator the Thrush. Unfortunately we have a name collision as we already defined `T` as the church encoding of true, so we omit the single-letter version of this combinator. There is another letter sometimes reserved for true, in which case we can use `T` for Thrush; the alternate true combinator name is coming up soon.

demo('POW 2 3 = 8', jsnum( POW(n2)(n3) ) === 8)
demo('POW 3 2 = 9', jsnum( POW(n3)(n2) ) === 9)
demo('POW 6 1 = 6', jsnum( POW(n6)(n1) ) === 6)
demo('POW 5 0 = 1', jsnum( POW(n5)(n0) ) === 1)

// ### Num -> Bool

// As stated earlier, there is no general function-equivalence algorithm (this lies at the heart of Church's research efforts into "decidability"). But just as we did for booleans, we can develop a specific equality check for numbers.

// #### Zero Equality

// We start with zero. If our input is zero, we want to produce `T`. Otherwise, we want to produce `F`. In our function below, if the input is zero, we run the inner function zero times, which means we return the final argument (`T`). However, if the input is any number greater than zero, we run the inner function at least once; that inner function is designed to always produce `F`.

header('ISZERO := λn.n(λ_.F)T')
const ISZERO = num => num(_ => F)(T)

demo('ISZERO 0 = T', ISZERO(n0) === T)
demo('ISZERO 1 = F', ISZERO(n1) === F)
demo('ISZERO 2 = F', ISZERO(n2) === F)

// #### The Kestrel

// ISZERO used a nice trick to produce a constant. We'll abstract that out. This is the Kestrel combinator `K`, named for the German word "Konstante". The `K` combinator takes a value, and produces a function which ignores its input, always returning the original value. So, `K0` is a function that always returns 0; (`K tweet`) is a function which always returns tweet.

header('Kestrel combinator and IS0')

header('Kestrel := K := konst := λk_.k')
const konst = k => _ => k
const K = konst
const Kestrel = K

demo('K0 tweet = 0', K(n0)(tweet) === n0)
demo('K0 chirp = 0', K(n0)(chirp) === n0)
demo('K tweet chirp = tweet', K(tweet)(chirp) === tweet)

// With K, we can redefine our isZero function more concisely.

header('IS0 := λn.n(KF)T')
const IS0 = num => num(K(F))(T)

demo('IS0 0 = T', IS0(n0) === T)
demo('IS0 1 = F', IS0(n1) === F)
demo('IS0 2 = F', IS0(n2) === F)

// `K` should look familiar; it's "alpha-equivalent" to `T`. Alpha-equivalence means it is identical except for variable names, which are arbitrary and don't affect the behavior: `λk_.k = λab.a`.

// #### The Kite

// We can also make `F` out of `K` and `I`. Try tracing through the logic and confirming that `KI = F`. This result is known as the Kite.

header('         K = T')
header('Kite := KI = F')
const Tru = K
const Fls = K(I)
const Kite = Fls

demo('K  tweet chirp = tweet', Tru(tweet)(chirp) === tweet)
demo('KI tweet chirp = chirp', Fls(tweet)(chirp) === chirp)
demo('De Morgan using K and KI', deMorgansLawDemo(K, K(I)))

// ### Interlude: Predecessor (So Far)

// We're still a way off from numeric equality. To get it working we'll need `succ`'s opposite, `pred` (predecessor). For a given num, `pred` gives you the number that came before, unless we're at 0, in which case it just gives you 0. It is possible to define predecessor using only the tools we've built thus far, but it's quite difficult to comprehend. Glance at it, but we'll be seeing a better version soon:

header('PREDECESSOR := λn.n (λg.IS0 (g 1) I (B SUCC g)) (K0) 0')
const PREDECESSOR = num => num(g => IS0(g(n1))(I)(B(SUCC)(g)))(K(n0))(n0)

demo('PREDECESSOR 0 = 0', jsnum( PREDECESSOR(n0) ) === 0)
demo('PREDECESSOR 1 = 0', jsnum( PREDECESSOR(n1) ) === 0)
demo('PREDECESSOR 2 = 1', jsnum( PREDECESSOR(n2) ) === 1)
demo('PREDECESSOR 3 = 2', jsnum( PREDECESSOR(n3) ) === 2)

// Idiots, Bluebirds, and Kestrels — oh my! The essence of this formulation for `PREDECESSOR` is a machine of sorts which, if skipped, yields `K0 0 = 0`; if run once, produces `I K0 0 = 0`; and if run n times, produces `(n - 1) succ (I K0 0) = n - 1`. However, there are other versions of predecessor, including one that is far clearer than this. To get there, we are going to have to take a small detour into functional data structures.

// ## Functional Data Structures: Pair

header('Pair: a Tiny Functional Data Structure')

// There are varying definitions for "data structure". In this context we will use it to mean a way of organizing information, plus an interface for accessing that information (some might argue that this is closer to the definition of an Abstract Data Type). In the lambda calculus, we do not have objects, arrays, sets or what-have-you… only functions. But we've already seen that functions capture values through *closure*, and are able to produce those values again later. That's the essence of the K combinator, in fact.

// Here we define a more complex entity, the Church encoding for "pair". Smullyan named this combinator the Vireo.

header('Vireo := V := PAIR := λabf.fab')
const PAIR = a => b => f => f(a)(b)
const V = PAIR
const Vireo = V

// `PAIR` takes two arbitrary values, a and b, and returns a function ("the pair") closing over those values. When the pair is fed a final argument f, f is applied to a and b. So our pair can "provide" a and b, in that order, to any binary function.

const examplePair = PAIR(tweet)(chirp)

demo('(PAIR tweet chirp) T = tweet', examplePair(T) === tweet)
demo('(PAIR tweet chirp) F = chirp', examplePair(F) === chirp)

// ### Opening the Closure

// As you can see, when a pair is fed the binary function `T` or `F`, it has the effect of extracting out one member of the pair. To make this more expressive and English-y, we can define FST (first) and SND (second) functions that perform this work for us:

header('FST := λp.pT; SND = λp.pF')
const FST = somePair => somePair(T)
const SND = somePair => somePair(F)

demo('FST (PAIR tweet chirp) = tweet', FST(examplePair) === tweet)
demo('SND (PAIR tweet chirp) = chirp', SND(examplePair) === chirp)

// ### Immutability

// There isn't any way to change the closed-over variables, but that's a good thing – immutability means that we never accidentally mess up data someone else was relying on. We can generate new data instead.

header('SET_FST := λcp.PAIR c (SND p)')
const SET_FST = newFirst => oldP => PAIR(newFirst)(SND(oldP))

demo('FST (SET_FST chirp (PAIR tweet tweet)) = chirp', FST( SET_FST(chirp)(PAIR(tweet)(tweet)) ) === chirp)
demo('SND (SET_FST chirp (PAIR tweet tweet)) = tweet', SND( SET_FST(chirp)(PAIR(tweet)(tweet)) ) === tweet)

// It would help to have a shorthand for pairs. We will use <a, b> to stand in for `(pair a b)`.

header('SET_SND := λcp.PAIR (FST p) c')
const SET_SND = newSecond => oldP => PAIR(FST(oldP))(newSecond)

demo('FST (SET_SND chirp <tweet, tweet>) = tweet', FST( SET_SND(chirp)(PAIR(tweet)(tweet)) ) === tweet)
demo('SND (SET_SND chirp <tweet, tweet>) = chirp', SND( SET_SND(chirp)(PAIR(tweet)(tweet)) ) === chirp)

// ### Counting Up with Memory

// Back on track towards our cleaner `PRED`, we will define a special pair function Φ (aka PHI) which moves the second element to the first, and increments the second element by 1. In shorthand, `Φ <a, b> = <b, b + 1>`. The use of this function will become apparent shortly.

header('PHI := Φ := λp.PAIR (SND p) (SUCC (SND p))')
const Φ = oldPair => PAIR(SND(oldPair))(SUCC(SND(oldPair)))
const PHI = Φ

const examplePairChirp0 = PAIR(chirp)(n0)
const examplePairTweet4 = PAIR(tweet)(n4)

demo('Φ <chirp, 0> = <0, 1>',
  jsnum( FST(Φ(examplePairChirp0)) ) === 0 &&
  jsnum( SND(Φ(examplePairChirp0)) ) === 1
)
demo('Φ <tweet, 4> = <4, 5>',
  jsnum( FST(Φ(examplePairTweet4)) ) === 4 &&
  jsnum( SND(Φ(examplePairTweet4)) ) === 5
)

// ## Back to Arithmetic

// ### A Cleaner Predecessor

// At long last, we can return to our predecessor function. With the help of Φ, it is wonderfully simple… well, relatively speaking at least.

header('PRED := λn.FST (n Φ <0, 0>)')
const PRED = n => FST( n(Φ)(PAIR(n0)(n0)) )

// All we do is successive applications of Φ to a seed pair <0, 0>. After n applications, the pair is <n - 1, n>. Then we can pluck off the first value of the pair! In effect, we count up to n, but keep the predecessor around for easy reference.

// n | n Φ <0, 0> | first of result pair
// --|------------|---------------------
// 0 |     <0, 0> | 0
// 1 |     <0, 1> | 0
// 2 |     <1, 2> | 1
// 3 |     <2, 3> | 2

demo('PRED 0 = 0', jsnum( PRED(n0) ) === 0)
demo('PRED 1 = 0', jsnum( PRED(n1) ) === 0)
demo('PRED 2 = 1', jsnum( PRED(n2) ) === 1)
demo('PRED 3 = 2', jsnum( PRED(n3) ) === 2)

// ### Subtraction At Last

// Now that the epic `pred` exists, we can do subtraction… at least, down to 0.

header('SUB := λab.b PRED a')
const SUB = a => b => b(PRED)(a)

const n7 = ADD(n4)(n3)

demo('SUB 5 2 = 3', jsnum( SUB(n5)(n2) ) === 3)
demo('SUB 4 0 = 4', jsnum( SUB(n4)(n0) ) === 4)
demo('SUB 2 2 = 0', jsnum( SUB(n2)(n7) ) === 0)
demo('SUB 2 7 = 0', jsnum( SUB(n2)(n7) ) === 0)

// ### Number Comparisons

// This kind of limited subtraction enables less-than-or-equal-to.

header('LEQ := λab.IS0(SUB a b)')
const LEQ = a => b => IS0(SUB(a)(b))

demo('LEQ 3 2 = F', LEQ(n3)(n2) === F)
demo('LEQ 3 3 = T', LEQ(n3)(n3) === T)
demo('LEQ 3 4 = T', LEQ(n3)(n4) === T)

// Finally we can test for numeric equality. 🎉🎉🎉

header('eq := λab.AND (LEQ a b) (LEQ b a)')
const EQ = a => b => AND (LEQ(a)(b)) (LEQ(b)(a))

demo('EQ 4 6 = F', EQ(n4)(n5) === F)
demo('EQ 7 3 = F', EQ(n7)(n3) === F)
demo('EQ 5 5 = T', EQ(n5)(n5) === T)
demo('EQ (ADD 3 6) (POW 3 2) = T', EQ(ADD(n3)(n6))(POW(n3)(n2)) === T)

// ## More Composition

// Of course we can use composition to create other results. At first glance, it would seem like greater-than can be the composition of `NOT` and `LEQ`. But this doesn't work because `LEQ` is a two-arg function, and compose only works with unary functions. Thanks to the beautiful Blackbird combinator, however, we can create point-free compositions where the rightmost function is binary.

header('Blackbird := B′ := BBB')
const B1 = compose(compose)(compose)
const Blackbird = B1

// If you find B1 confusing, the point-ful version might help: `B1 := λfgxy.f(g x y)`. Also, why Smullyan chose "B1" instead of "B2", I can only guess.

header('GT := B1 NOT LEQ')
const GT = B1(NOT)(LEQ)

demo('GT 6 2 = T', GT(n6)(n2) === T)
demo('GT 4 4 = F', GT(n4)(n4) === F)
demo('GT 4 5 = F', GT(n4)(n5) === F)

// Another use of Blackbird: `neq := B1 not eq`.

// ## Final Notes

// There are church encodings and lambda techniques for lists, types, rationals, reals, and anything else that is computable – plus the astonishing Y-combinator, which enables recursion in a syntax where functions are all anonymous. We also omitted the Starling, which can be combined with the Kestrel in various ways to produce *every other function.* For now, we will end with an actual real-world classic math problem, calculated entirely using lambda calculus – only converted back to JS numbers at the end, purely for display purposes.

header('FIB := λn.n (λfab.f b (ADD a b)) K 0 1')
const FIB = n => n(f => a => b => f(b)(ADD(a)(b)))(K)(n0)(n1)

demo('FIB 0 = 0', jsnum( FIB(n0) ) === 0)
demo('FIB 1 = 1', jsnum( FIB(n1) ) === 1)
demo('FIB 2 = 1', jsnum( FIB(n2) ) === 1)
demo('FIB 3 = 2', jsnum( FIB(n3) ) === 2)
demo('FIB 4 = 3', jsnum( FIB(n4) ) === 3)
demo('FIB 5 = 5', jsnum( FIB(n5) ) === 5)
demo('FIB 6 = 8', jsnum( FIB(n6) ) === 8)

logErrsAndSetExitCode()
