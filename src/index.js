const chalk = require('chalk')
const green = chalk.green.bind(chalk)
const red = chalk.red.bind(chalk)

function demo (msg, bool) {
	if (typeof bool === 'function') bool = bool(true)(false)
	if (!!bool !== bool) throw TypeError('second arg must be boolean (JS or LC)')
  console.log(`${bool ? green('✔') : red('✖')} ${msg}`)
  if (!bool) throw Error(`Spec fail: ${msg} -> ${bool}`)
}

function header (str) {
	console.log('\n' + str + '\n')
}

/**
 * Everything as Functions: Lambda Calculus in JS
 * Notes for a talk by Gabriel Lebec
 *
 * The Lambda Calculus is a symbol manipulation framework developed
 * by mathematician Alonzo Church in the 1930s. It was intended to be
 * an extremely tiny syntax which could nevertheless suffice to
 * calculate anything computable. The mathematical advantage of such
 * a syntax is that with such extreme simplicity, it becomes easier
 * to write formal proofs about computational logic.
 *
 * Church had a very famous student, Alan Turing, who formulated a
 * different model of universal computation – the eponymous Turing
 * Machine. A TM is a hypothetical device, again of extreme simplicity;
 * it can read or write to cells in an infinite tape, and the cells
 * can contain data or instructions.
 *
 * Turing proved, in what is called the Church-Turing thesis, that the
 * Turing Machine and Lambda Calculus are totally equivalent. Everything
 * that one can calculate, the other can.
 *
 * Turing Machines are exciting because if a hypothetical machine can
 * compute anything computable, then maybe a real machine can as well.
 * Modern computers add many features and optimizations beyond what is
 * featured in an actual Turing Machine; these features make the machine
 * more convenient and performant, but do not compromise its essential
 * nature as a universal computing device.
 *
 * As machine codes, assemblers, compilers, and higher-level languages
 * have developed to program these real machines, they have largely
 * evolved with a focus on the essence of the machine – memory,
 * statefulness, effects, imperative instructions and so forth.
 *
 * However, mathematicians and computer scientists knew that the
 * entirely abstract lambda calculus, being equivalent to a TM,
 * meant that languages could be written in a style independent
 * from the concept of machine instructions. Instead, a language
 * consisting primarily of first-class functions – aka lambda
 * abstractions – could be compiled into machine code. Such a language,
 * meanwhile, would benefit greatly from decades of mathematical
 * research. And as real computers extend Turing Machines with extra
 * power and convenience, these _functional languages_ would extend
 * the lambda calculus with additional features and under-the-hood
 * shortcuts (such as hardware-based math).
 *
 * What follows is a demonstration, mostly for enjoyment and insight,
 * of the surprising and delightful omnipotence of the lambda calculus.
 * JavaScript, like any functional language, is closely related to LC; it
 * contains the necessary ingredients (variables, parens, first-class
 * functions) as well as additional luxuries.
 */

header('First steps with simple combinators')

// Starting simple. A lambda abstraction is simply an anonymous unary function. In LC there are no named functions, but for convenience we will use names. In mathematical notation, `:=` means "is defined as".

// Here is an almost trivial function: Identity.

header('id := λx.x')
const id = x => x

// note: λ is used to indicate a lambda abstraction (read: function). The sole argument is listed to the left of the `.`, and the return expression is written after the `.`.

// In LC, juxtaposition is function application. That is, `qux baz` means "apply the qux function to the baz argument". A space is not strictly necessary; `fx` means "apply f to x". Also in LC, you can have meaningless variables that stand for whatever you want. We'll use ES2015 symbols for this.

// id foo
// = (λx.x) foo
// = foo

const foo = Symbol('foo')
const bar = Symbol('bar')

demo('id foo = foo', id(foo) === foo)
demo('id bar = bar', id(bar) === bar)

// in LC, valid arguments include other lambda abstractions. This is the meaning of "first-class" in the phrase "first-class functions"; functions are values which may be passed into or returned from other functions.

// id id
// = (λx.x)(λx.x)
// = λx.x

demo('id id = id', id(id) === id)

// of course we can build up more complex expressions. In LC, function application associates left; that is, `a b c d` = `((a b) c) d`. Evaluating terms by substituting arguments into function bodies has the fancy term of "β-reduction". A reducible expression (or "redex") that has been completely simplified in this fashion is said to be in its "β-normal" or just "normal" form.

//      id    id       id    id  foo   <--- the initial reducible expression ("redex")
// = (((id    id)      id)   id) foo   <--- application associates left
// = ((    id          id)   id) foo   <--- evaluating function applications ("β-reduction")
// = (           id          id) foo   <--- continuing β-reduction
// =                    id       foo   <--- more β-reduction
// =                             foo   <--- normal form of the original redex

// LC has great overlap with combinatory logic. A pioneer of CL was the mathematician Haskell Curry, whose name now adorns the Haskell language as well as the functional concept of currying. (Curry actually cited the technique from Schönfinkel, and Frege used it even earlier.) Combinatory logic is concerned with combinators, that is, functions which operate only on their inputs. The Identity function is a combinator, often abbreviated I, sometimes called the Idiot bird.

header('Idiot := I := id')
const I = id
const Idiot = I

// Curry was an avid birdwatcher, and the logician Raymond Smullyan named many combinators after birds in his honor. The self-application combinator M (for "Mockingbird"), aka ω (little omega), looks like this:

header('Mockingbird := M := ω := λf.ff')
// Remember, `fx` means "apply f to x." So `ff` means "apply f to itself".
const ω = fn => fn(fn)
const M = ω
const Mockingbird = M

// Can you guess what the Mockingbird of Identity is?

// M I
// = (λf.ff)(λx.x)
// = (λx.x)(λx.x) = I I
// = λx.x = I

// That's right, it's the same as id of id… which is id.

demo('M I = I I = I', M(I) === I(I) && I(I) === I)

// What about… the Mockingbird of Mockingbird?

// That's the Ω (big Omega) Combinator. It diverges (goes on forever).

// M M
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// = (λf.ff)(λf.ff)
// ...

try {
  M(M)
} catch (err) {
  demo('M M = M M = M M = ' + err.message, true)
}

header('First encodings: boolean value functions')

// OK this is nice and all, but how can we do anything real with only functions? Let's start with booleans. Here we need some multi-arg functions… in lambda calc, λabc.a is just shorthand for λa.λb.λc.a, or λa.(λb.(λc.a))). In other words, all functions are curried.

header('T := λxy.x')
const T = thn => els => thn

header('F := λxy.y')
const F = thn => els => els

// Hm. How can "true" and "false" be functions? Well, the point of booleans is to _select_ between a then-case and an else-case. The LC booleans are just functions which do that! T takes two vals and returns the first; F takes two vals and returns the second. If you apply an unknown bool func to two vals, the first val will be returned if the bool was true, else the second val will be returned.

demo('T foo bar = foo', T(foo)(bar) === foo)
demo('F foo bar = bar', F(foo)(bar) === bar)

// Another fun way we could have produced F was with the Cardinal combinator. The Cardinal, aka C, aka `flip`, takes a binary (two-argument) function, and produces a function with reversed argument order.

header('Cardinal := C := flip := λfab.fba')
const flip = func => a => b => func(b)(a)
const C = flip
const Cardinal = C

// With the Cardinal, we can derive F from the flip of T:

header('F = C T')

demo('flip T foo bar = bar', flip(T)(foo)(bar) === bar)

// Regardless of whether you define F manually, or as the flip of T, these functions are _encodings_ of boolean values. They represent booleans in useful and meaningful ways, which preserve the behavior of the values. Specifically, they are Church encodings – representations developed / discovered by Alonzo Church. Their real power is revealed when we start defining logical operations on them.

header('not := λb.bFT')
const not = chooseOne => chooseOne(F)(T)

// Remember, booleans in the LC are really functions which select one of two arguments. Try reducing the expression `not T` yourself, then check your work below.

// not T
// = (λb.bFT) T
// = TFT
// = (λxy.x) F T
// = F

demo('not T = F', not(T) === F)
demo('not F = T', not(F) === T)

// There's another way we could define `not`, however, and we've already seen it. Remember how the flip of T (aka the Cardinal of T) is F? It works the other way too – CF = T! At least, CF yields a function that behaves identically to T:

demo('CF = T', C(F)(foo)(bar) === foo)
demo('CT = F', C(T)(foo)(bar) === bar)

// This means that when it comes to Church encodings of booleans, the Cardinal behaves just like our manually-defined `not`.

// Can you construct a function for boolean AND? It will have to take two unknown boolean functions, and route to an output of the correct boolean. Give it a try.

header('and := λpq.pqF')
//            (λpq.pqp also works)
const and = p => q => p(q)(F)

demo('and F F = F', and(F)(F) === F)
demo('and T F = F', and(T)(F) === F)
demo('and F T = F', and(F)(T) === F)
demo('and T T = T', and(T)(T) === T)

// If you work through the logic, you might notice that this function exhibits short-circuiting; if p = F, we don't bother using q. Similarly, our OR function below short circuits; if p is T, we don't bother using q.

header('or := λpq.pTq')
//           (λpq.ppq also works)
const or = p => q => p(T)(q)

demo('or F F = F', or(F)(F) === F)
demo('or T F = T', or(T)(F) === T)
demo('or F T = T', or(F)(T) === T)
demo('or T T = T', or(T)(T) === T)

// λpq.ppq also works because if p is true, it is supposed to select T, but p = T, so we can just reuse it. Notice something interesting here: λpq.ppq behaves exactly like the Mockingbird. It takes a value, p, and self-applies p (producing `pp`). Well, Mp = pp. Then you can apply that result to another value q, to get ppq; Mpq = ppq. So M = λpq.ppq. The Mockingbird works as a boolean OR function:

demo('M F F = F', M(F)(F) === F)
demo('M T F = T', M(T)(F) === T)
demo('M F T = T', M(F)(T) === T)
demo('M T T = T', M(T)(T) === T)

// With working booleans, we can illustrate some more complex logic, such as one of De Morgan's Laws: `!(p && q) === (!p) || (!q)`.

header('De Morgan: not (and P Q) = or (not P) (not Q)')

function deMorgansLawDemo (p, q) { return not(and(p)(q)) === or(not(p))(not(q)) }

demo('not (and F F) = or (not F) (not F)', deMorgansLawDemo(F, F))
demo('not (and T F) = or (not T) (not F)', deMorgansLawDemo(T, F))
demo('not (and F T) = or (not F) (not T)', deMorgansLawDemo(F, T))
demo('not (and T T) = or (not T) (not T)', deMorgansLawDemo(T, T))

// However, this whole time we have been cheating in the demonstrations. Lambda calculus doesn't have any `===` operator to check for equality! Don't fret though, everything is functions. We can define our own equality function for booleans.

header('beq := λpq.p (qTF) (qFT)')
const beq = p => q => p( q(T)(F) )( q(F)(T) )

demo('beq F F = T', beq( beq(F)(F) )(T))
demo('beq F T = F', beq( beq(F)(T) )(F))
demo('beq T F = F', beq( beq(T)(F) )(F))
demo('beq T T = T', beq( beq(T)(T) )(T))

// Not a JS operator in sight (our `demo` function accepts church encodings). But for clarity's sake, we'll go back to using JS's equality operator in our `demo` calls.

header('Numbers')

// Booleans are neat, but surely to compute arithmetic you need language-supported math. Right? …Nah. You can construct math from scratch.

// The Church encoding for a natural number n is an n-fold compositor (composing combinator). In other words, "2" is a function that composes a function f twice: 2 f x = f(f(x)). Whereas "4" is a function that composes any f four times: 4 f x = f(f(f(f(x). In this sense, 2 and 3 can be read more like "two-fold" and "three-fold", or "twice" and "thrice".

// In this system, zero is a function which applies a function f zero times to x. So… zero ignores the function argument, just returning x; 0 f x = x.

header('0 := λfx.x')
// (fun fact, 0 = F)
const zero = fn => x => x

// zero Mockingbirds of `foo` is just `foo`. Good thing too, because applying Mockingbird to `foo` would throw an error in JS (though it would work fine in LC).
demo('0 M foo = foo', zero(M)(foo) === foo)

// we could hard-code one as a single application of f…

header('hard-coded 1 and 2')

header('1 := λfx.fx')
const once = fn => x => fn(x)

// another fun fact – this is one step removed from I, called I* ("I-star"). In fact we could have shortened this to be `1 := I`. So 0 is false and 1 is identity, how nice.

demo('1 I foo = foo', once(I)(foo) === foo)

// id is boring however because the n-fold composition of id is always id, even for n=0. The above example doesn't really prove our function is doing anything interesting. Let's cheat a bit with some string ops (note, this is polluting LC with some JS, but it's just for demonstration):

const λ = 'λ'
const yell = str => str + '!'

demo('0 yell λ = λ', zero(yell)(λ) === 'λ')
demo('1 yell λ = yell λ = λ!', once(yell)(λ) === 'λ!')

// we could also hard-code two.

header('2 := λfx.f(fx)')
const twice = fn => x => fn(fn(x))

demo('2 yell λ = yell (yell λ) = λ!!', twice(yell)(λ) === 'λ!!')

// This hard-coding is lame though. We can't do math like this. What we need is a way to count up.

header('successor')

header('successor := λnfx.f(nfx)')
const successor = num => fn => x => fn(num(fn)(x))

// Don't get lost in the weeds. All we are saying is that the successor of n does n compositions of f to a value x, and then it does _one more_ application of f to the result. Therefore, it ends up doing 1 + n compositions of f in total.

const newOnce   = successor(zero)
const newTwice  = successor(successor(zero)) // we can use multiple successors on zero, or…
const newThrice = successor(newTwice) // …apply successor to already-obtained numbers.

demo('1 yell λ = λ!',     newOnce(yell)(λ) === 'λ!')
demo('2 yell λ = λ!!',   newTwice(yell)(λ) === 'λ!!')
demo('3 yell λ = λ!!!', newThrice(yell)(λ) === 'λ!!!')

// There is another way to write successor. Point-free (some joke "point-less") notation means to define a function purely as a combination of other functions, without explicitly writing arguments. Sometimes this style reveals what a function *is* rather than what explain what it *does*. Other times it can be abused to produce incomprehensible gibberish. Successor is a good candidate for it, however.

// We are doing n-fold compositions, so let's define an actual `compose` function to help. Composition is often notated as ∘ in infix position: (f ∘ g) x = f(g(x)). However, Lambda Calculus only includes prefix position function application. Smullyan named this the Bluebird or B combinator.

header('Bluebird := B := (∘) := compose := λfgx.f(gx)')
const compose = f => g => x => f(g(x))
const B = compose
const Bluebird = B

demo('(B not not)  T =  not (not T)', (B(not)(not))(T)  === not(not(T)))
demo('(B yell not) F = yell (not F)', (B(yell)(not))(F) === yell(not(F)))

// Now that we have an actual composition function, we can define successor without mentioning the final `x` value argument.

header('succ := λnf.f∘(nf) = λnf.Bf(nf)')
const succ = num => fn => compose( fn )( num(fn) )

// This is just a terse way of repeating what we already know: if a given `n` composes some function `f` n times, then the successor of n is a function which composes one additional `f`, for a total of 1 + n compositions.

const n0 = zero
const n1 = succ(n0)
const n2 = succ(succ(n0))
const n3 = succ(succ(succ(n0)))
const n4 = succ(n3)

demo('1 yell λ = λ!',    n1(yell)(λ) === 'λ!')
demo('2 yell λ = λ!!',   n2(yell)(λ) === 'λ!!')
demo('3 yell λ = λ!!!',  n3(yell)(λ) === 'λ!!!')
demo('4 yell λ = λ!!!!', n4(yell)(λ) === 'λ!!!!')

// Things will get pretty slow if we can only increment by 1. Let's add addition.

header('add := λab.a(succ)b')
const add = numA => numB => numA(succ)(numB)

// Aha, addition is just the Ath successor of B. Makes sense. For example, `add 3 2 = 3 succ 2`, which could be read as "thrice successor of twice".

const n5 = add(n2)(n3)
const n6 = add(n3)(n3)

demo('add 5 2 yell λ = λ!!!!!!!', add(n5)(n2)(yell)(λ) === 'λ!!!!!!!')
demo('add 0 3 yell λ = λ!!!',     add(n0)(n3)(yell)(λ) === 'λ!!!')
demo('add 2 2 = 4',               add(n2)(n2)(yell)(λ) === n4(yell)(λ))

// These equivalence checks using `yell` and 'λ' are a bit verbose. It's annoying, but a mathematical truth, that there can be no general algorithm to decide if two functions are equivalent – and we cannot rely on JS function equality because we are generating independent function objects. Since `yell` and 'λ' are already impure non-LC code, we might as well go all the way and define `church` and `jsnum` to convert between Church encodings and JS numbers.

header('LC <-> JS: church & jsnum')

function church (n) { return n === 0 ? n0 : succ(church(n - 1)) }
function jsnum (c) { return c(x => x + 1)(0) }

demo(        'church(5) = n5', church(5)(yell)(λ) === n5(yell)(λ))
demo(       'jsnum(n5) === 5',          jsnum(n5) === 5)
demo('jsnum(church(2)) === 2',   jsnum(church(2)) === 2)

// Back to math. How about multiplication?

header('mult := λab.a∘b = compose')
const mult = compose

// How beautiful! Multiplication is the composition of numbers. For example, `mult 3 2 = 3 ∘ 2`, which could be read as "thrice of twice", or "three of (two of (someFn))". If you compose a function f two times — `f ∘ f` — and then you compose that result three times — `(f∘f) ∘ (f∘f) ∘ (f∘f)` — you get the six-fold composition of f: `f ∘ f ∘ f ∘ f ∘ f ∘ f`. It helps to know that composition is associative — `f ∘ (g ∘ h) = (f ∘ g) ∘ h`.

demo('mult 1 5 = 5', jsnum( mult(n1)(n5) ) === 5)
demo('mult 3 2 = 6', jsnum( mult(n3)(n2) ) === 6)
demo('mult 4 0 = 0', jsnum( mult(n4)(n0) ) === 0)
demo('mult 6 2 yell λ = λ!!!!!!!!!!!!', mult(n6)(n2)(yell)(λ) === 'λ!!!!!!!!!!!!')

// Exponentiation is remarkably clean too. When we say 2^3, we are saying "multiply two by itself three times"; or putting it another way, "twice of twice of twice". So for any base and power, the result is the power-fold composition of the base:

header('Thrush := pow := λab.ba')
const pow = numA => numB => numB(numA)

// As you can see, we also call this combinator the Thrush. Unfortunately we have a name collision as we already defined T as the church encoding of true, so we omit the single-letter version of this combinator. There is another letter sometimes reserved for true, in which case we can use T for Thrush; the alternate true combinator name is coming up soon.

const n8 = mult(n4)(n2)
const n9 = succ(n8)

demo('pow 2 3 = 8', jsnum( pow(n2)(n3) ) === 8)
demo('pow 3 2 = 9', jsnum( pow(n3)(n2) ) === 9)
demo('pow 6 1 = 6', jsnum( pow(n6)(n1) ) === 6)
demo('pow 5 0 = 1', jsnum( pow(n5)(n0) ) === 1)

// As stated earlier, there is no general function-equivalence algorithm (this lies at the heart of Church's research efforts into "decidability"). But just as we did for booleans, we can develop a specific equality check for numbers.

// We start with zero. If our input is zero, we want to produce T. Otherwise, we want to produce F. In our function below, if the input is zero, we run the inner function zero times, which means we return the final argument (T). However, if the input is any number greater than zero, we run the inner function at least once; that inner function is designed to always produce F.

header('isZero := λn.n(λ_.F)T')
const isZero = num => num(_ => F)(T)

demo('isZero 0 = T', isZero(n0) === T)
demo('isZero 1 = F', isZero(n1) === F)
demo('isZero 2 = F', isZero(n2) === F)

// isZero used a nice trick to produce a constant. We'll abstract that out. This is the Kestrel combinator K, named for the German word "Konstante". The K combinator takes a value, and produces a function which ignores its input, always returning the original value. So, K0 is a function that always returns 0; (K foo) is a function which always returns foo.

header('Kestrel combinator and is0')

header('Kestrel := K := konst := λk_.k')
const konst = k => _ => k
const K = konst
const Kestrel = K

demo('K0 foo = 0', K(zero)(foo) === zero)
demo('K0 bar = 0', K(zero)(bar) === zero)
demo('K foo bar = foo', K(foo)(bar) === foo)

// With K, we can redefine our isZero function more concisely.

header('is0 := λn.n(KF)T')
const is0 = num => num(K(F))(T)

demo('is0 0 = T', is0(n0) === T)
demo('is0 1 = F', is0(n1) === F)
demo('is0 2 = F', is0(n2) === F)

// K should look familiar; it's alpha-equivalent to T. Alpha-equivalence means it is identical except for variable names, which are arbitrary and don't affect the behavior: (λk_.k = λab.a). We can also make F out of K and I. Try tracing through the logic and confirming that KI = F. This result is known as the Kite.

header('         K = T')
header('Kite := KI = F')
const Tru = K
const Fls = K(I)
const Kite = Fls

demo('K  foo bar = foo', Tru(foo)(bar) === foo)
demo('KI foo bar = bar', Fls(foo)(bar) === bar)
demo('De Morgan using K and KI', deMorgansLawDemo(K, K(I)))

// We're still a way off from numeric equality. To get it working we'll need `succ`'s opposite, `pred` (predecessor). For a given num, `pred` gives you the number that came before, unless we're at 0, in which case it just gives you 0. It is possible to define predecessor using only the tools we've built thus far, but it's quite difficult to comprehend. Glance at it, but we'll be seeing a better version soon:

header('predecessor := λn.n (λg.is0 (g 1) I (B succ g)) (K0) 0')
const predecessor = num => num(g => is0(g(n1))(I)(B(succ)(g)))(K(n0))(n0)

demo('predecessor 0 = 0', jsnum( predecessor(n0) ) === 0)
demo('predecessor 1 = 0', jsnum( predecessor(n1) ) === 0)
demo('predecessor 2 = 1', jsnum( predecessor(n2) ) === 1)
demo('predecessor 3 = 2', jsnum( predecessor(n3) ) === 2)

// Idiots, Bluebirds, and Kestrels — oh my! The essence of this formulation for `predecessor` is a machine of sorts which, if skipped, yields `K0 0 = 0`; if run once, produces `I K0 0 = 0`; and if run n times, produces `(n - 1) succ (I K0 0) = n - 1`. However, there are other versions of predecessor, including one that is far clearer than this. To get there, we are going to have to take a small detour into functional data structures.

header('Pair: a Tiny Functional Data Structure')

// There are varying definitions for "data structure". In this context we will use it to mean a way of organizing information, plus an interface for accessing that information (some might argue that this is closer to the definition of an Abstract Data Type). In the lambda calculus, we do not have objects, arrays, sets or what-have-you… only functions. But we've already seen functions capture values through *closure*, and be able to produce those values again later. That's the essence of the K combinator, in fact.

// Here we define a more complex entity, the Church encoding "pair". Smullyan named this combinator V for Vireo.

header('Vireo := V := pair := λabf.fab')
const pair = a => b => f => f(a)(b)
const V = pair
const Vireo = V

// `pair` takes two arbitrary values, a and b, and returns a function ("the pair") closing over those values. When the pair is fed a final argument f, f is applied to a and b. So our pair can "provide" a and b, in that order, to any binary function.

const examplePair = pair(foo)(bar)

demo('(pair foo bar) T = foo', examplePair(T) === foo)
demo('(pair foo bar) F = bar', examplePair(F) === bar)

// As you can see, when a pair is fed the binary function T or F, it has the effect of extracting out one member of the pair. To make this more expressive and English-y, we can define fst (first) and snd (second) functions that perform this work for us:

header('fst := λp.pT; snd = λp.pF')
const fst = somePair => somePair(T)
const snd = somePair => somePair(F)

demo('fst (pair foo bar) = foo', fst(examplePair) === foo)
demo('snd (pair foo bar) = bar', snd(examplePair) === bar)

// There isn't any way to change the closed-over variables, but that's a good thing – immutability means that we never accidentally mess up data someone else was relying on. We can generate new data instead.

header('setFst := λcp.pair c (snd p)')
const setFst = newFirst => oldP => pair(newFirst)(snd(oldP))

demo('fst (setFst bar (pair foo foo)) = bar', fst( setFst(bar)(pair(foo)(foo)) ) === bar)
demo('snd (setFst bar (pair foo foo)) = foo', snd( setFst(bar)(pair(foo)(foo)) ) === foo)

// It would help to have a shorthand for pairs. We will use <a, b> to stand in for `(pair a b)`.

header('setSnd := λcp.pair (fst p) c')
const setSnd = newSecond => oldP => pair(fst(oldP))(newSecond)

demo('fst (setSnd bar <foo, foo>) = foo', fst( setSnd(bar)(pair(foo)(foo)) ) === foo)
demo('snd (setSnd bar <foo, foo>) = bar', snd( setSnd(bar)(pair(foo)(foo)) ) === bar)

// Back on track towards our cleaner `pred`, we will define a special pair function Φ (aka Phi) which moves the second element to the first, and increments the second element by 1. In shorthand, `Φ <a, b> = <b, b + 1>`. The use of this function will become apparent shortly.

header('Phi := Φ := λp.pair (snd p) (succ (snd p))')
const Φ = oldPair => pair(snd(oldPair))(succ(snd(oldPair)))
const Phi = Φ

const examplePairBar0 = pair(bar)(n0)
const examplePairFoo4 = pair(foo)(n4)

demo('Φ <bar, 0> = <0, 1>',
  jsnum( fst(Φ(examplePairBar0)) ) === 0 &&
  jsnum( snd(Φ(examplePairBar0)) ) === 1
)
demo('Φ <foo, 4> = <4, 5>',
  jsnum( fst(Φ(examplePairFoo4)) ) === 4 &&
  jsnum( snd(Φ(examplePairFoo4)) ) === 5
)

// At long last, we can return to our predecessor function. With the help of Φ, it is wonderfully simple… well, relatively speaking at least.

header('pred := λn.fst (n Φ <0, 0>)')
const pred = n => fst( n(Φ)(pair(n0)(n0)) )

// All we do is successive applications of Φ to a seed pair <0, 0>. After n applications, the pair is <n - 1, n>. Then we can pluck off the first value of the pair! In effect, we count up to n, but keep the predecessor around for easy reference.

// n | n Φ <0, 0> | first of result pair
// --|------------|---------------------
// 0 |     <0, 0> | 0
// 1 |     <0, 1> | 0
// 2 |     <1, 2> | 1
// 3 |     <2, 3> | 2

demo('pred 0 = 0', jsnum( pred(n0) ) === 0)
demo('pred 1 = 0', jsnum( pred(n1) ) === 0)
demo('pred 2 = 1', jsnum( pred(n2) ) === 1)
demo('pred 3 = 2', jsnum( pred(n3) ) === 2)

//------------------------

// Now that the epic `pred` exists, we can do subtraction… at least, down to 0.

header('sub := λab.b pred a')
const sub = a => b => b(pred)(a)

const n7 = add(n4)(n3)

demo('sub 5 2 = 3', jsnum( sub(n5)(n2) ) === 3)
demo('sub 4 0 = 4', jsnum( sub(n4)(n0) ) === 4)
demo('sub 2 2 = 0', jsnum( sub(n2)(n7) ) === 0)
demo('sub 2 7 = 0', jsnum( sub(n2)(n7) ) === 0)

// This kind of limited subtraction enables less-than-or-equal-to.

header('leq := λab.is0(sub a b)')
const leq = a => b => is0(sub(a)(b))

demo('leq 3 2 = F', leq(n3)(n2) === F)
demo('leq 3 3 = T', leq(n3)(n3) === T)
demo('leq 3 4 = T', leq(n3)(n4) === T)

// Finally we can test for numeric equality. 🎉🎉🎉

header('eq := λab.and (leq a b) (leq b a)')
const eq = a => b => and (leq(a)(b)) (leq(b)(a))

demo('eq 4 6 = F', eq(n4)(n5) === F)
demo('eq 7 3 = F', eq(n7)(n3) === F)
demo('eq 5 5 = T', eq(n5)(n5) === T)
demo('eq (add 3 6) (pow 3 2) = T', eq(add(n3)(n6))(pow(n3)(n2)) === T)

// Of course we can use composition to create other results. At first glance, it would seem like greater-than can be the composition of not and leq. But this doesn't work because leq is a two-arg function, and compose only works with unary functions. Thanks to the beautiful Blackbird combinator, however, we can create point-free compositions where the rightmost function is binary.

header('Blackbird := B′ := BBB')
const B1 = compose(compose)(compose)
const Blackbird = B1
// If you find B1 confusing, the point-ful version might help: `B1 := λfgxy.f(g x y)`. Also, why Smullyan chose "B1" instead of "B2", I can only guess.

header('gt := B1 not leq')
const gt = B1(not)(leq)

demo('gt 6 2 = T', gt(n6)(n2) === T)
demo('gt 4 4 = F', gt(n4)(n4) === F)
demo('gt 4 5 = F', gt(n4)(n5) === F)

// Another use of Blackbird: `neq := B1 not eq`.

// There are church encodings and lambda techniques for lists, types, rationals, reals, and anything else that is computable – plus the astonishing Y-combinator, which enables recursion in a syntax where functions are all anonymous. We also omitted the Starling, which can be combined with the Kestrel in various ways to produce *every other function.* For now, we will end with an actual real-world classic math problem, calculated entirely using lambda calculus – only converted back to JS numbers at the end, purely for display purposes.

header('fib := λn.n (λfab.f b (add a b)) K 0 1')
const fib = n => n(f => a => b => f(b)(add(a)(b)))(K)(n0)(n1)

demo('fib 0 = 0', jsnum( fib(n0) ) === 0)
demo('fib 1 = 1', jsnum( fib(n1) ) === 1)
demo('fib 2 = 1', jsnum( fib(n2) ) === 1)
demo('fib 3 = 2', jsnum( fib(n3) ) === 2)
demo('fib 4 = 3', jsnum( fib(n4) ) === 3)
demo('fib 5 = 5', jsnum( fib(n5) ) === 5)
demo('fib 6 = 8', jsnum( fib(n6) ) === 8)
