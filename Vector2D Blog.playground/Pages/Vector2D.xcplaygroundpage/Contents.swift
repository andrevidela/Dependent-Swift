import Foundation

// Part 1
/*:
 # Protocol Programing

 During the holidays I ended up having a lot of free time and no phone nor
 computer, I only had my iPad with me.
 From this experiment I learned two things:
  1. I am physically unable to spend a couple days without programming on a
     computer
  2. Using Apple's Playground app to write and compile Swift is a delight

 Since I haven't been writting Swift for a long time I thought it would be a
 nice experiment to see how far I could take the language since they introduced
 [conditional conformances](COND).

 The motivation spanned from an attempt to write a library for linear algebra.
 This took another direction midway through but the result is a useful set
 of protocols that encapsulate the behaviour of complex numbers (ℂ), 2d
 vectors (ℝ²) and other linear spaces.

 ## Common operations

 Vector spaces define a *field*, a field has a pretty big set of properties
 (commutativity, etc.). Unfortunately, Swift's type system cannot check for
 those properties. Instead we are going to focus on a couple of properties
 that we need and that Swift allows us to implement and assume they will be
 sufficient.

 The properties we will use are:

 - Add two things together and have an identity value for addition.
 - Multiply two things together and have an identity value for multiplication.
 - Abstract over two-dimensional objects.
 - Multiply two-dimensional objects with a scalar.
 - Compute the magnitude of a two-dimensional object.

 The goal is that once we have all this machinery weaved together, adding our
 functionalities to a type will be just as easy as.

 ```
 extension Vector2D: Field {}
 ```

 Let's get started.

 ## Mutliplicative

 The multiplicative protocol defines a function `*` between two
 values of the same type and returns the same type. It also has an identity value
 such that `a * id = a`.

 */

/// Protocol for multiplication with identity
protocol Multiplicative {
    static var multId: Self { get }
    static func * (_ lhs: Self, _ rhs: Self) -> Self
}

/*:

 * note: "lhs" and "rhs" stand for "left hand side" and "right hand side"
   respectively.

 Given this, we can implement Multiplicative for the `Int` type.

 Since the `Int` type already has a `*` operator that fits the definition of
 the protocol we only need to define the `mIdentity` value.
 */

// Ints are Multiplicative, the identity is 1
extension Int: Multiplicative {
    static var multId: Int {
        return 1
    }
}

/*:
 ## Additive

 The additive protocol will be similar with a value for addition identity and
 an addition operator `+`.
 */

/// Protocol for addition with identity
protocol Additive {
    static var addId: Self { get }
    static func + (_ lhs: Self, _ rhs: Self) -> Self
}

//: And implement it for `Int`:

// Ints are Additive, the identity is 0
extension Int: Additive {
    static var addId: Int { return 0 }
}

/*:
 * note: Those two protocols are [_monoids_](MONOID)

 ## TwoDimensions

 We also need a way to abstract over the fact that the values we are going to
 manipulate have two dimensions. They need to be able to project each component
 and create itself from two component values.

 [MONOID]: https://en.wikipedia.org/wiki/Monoid
 */

/// Protocol for 2D coordinate systems
protocol TwoDimensions {
    associatedtype ComponentVal
    static func make2D(_ fst: ComponentVal, _ snd: ComponentVal) -> Self
    var fst: ComponentVal { get }
    var snd: ComponentVal { get }
}

/*:
 We do not have any type to conform to this protocol but we can already
 implement `Additive` for it. Indeed, any type is `Additive` provided it has
 two dimensions and its components are `Additive` as well.

 As for Identity, since `Additive` is implemented by adding each component
 together we can define identity by creating a 2D object with identity values
 for each component.
 */

extension Additive where Self: TwoDimensions, Self.ComponentVal: Additive {
    static var addId: Self {
        return Self.make2D(Self.ComponentVal.addId, Self.ComponentVal.addId)
    }
    static func + (lhs: Self, rhs: Self) -> Self {
        return make2D(lhs.fst + rhs.fst, lhs.snd + rhs.snd)
    }
}

/*:
 ## Magnitude

 The [magnitude](MAG) of a value is a way to have a representation of the
 "size" of it. Typically for 2D vectors the magnitude is the length of the
 vector. For `Int`s it is their absolute value. This value is useful to
 compare two different values even if there is not necessarily a total order
 between all values.

 To describe a type that has a magnitude we need to say that it has
 a property that returns a value of some type and this value needs to be
 `Comparable`.

 [MAG]: https://en.wikipedia.org/wiki/Magnitude_(mathematics)
 */

/// A protocol for getting the magnitude of an object
protocol Magnitude {
    associatedtype MagVal: Comparable
    var magSquare: MagVal { get }
}

/*:
 Since the magnitude usually requires a square root operation but isn't a very
 general operation we are going to assume that `magSquare` returns the
 magnitude squared.

 Since we know how to compute the magnitude of a value in the general sense
 (given a value `v` with projections `x` and `y` the magnitude squared is given
 by `v.x * v.x + v.y * v.y`), any type can have a magnitude provided:
 - has two dimensions
 - the components are multiplicative
 - the components are additive
 - the types of the components are the same as the result of computing the
   magnitude
 */
extension Magnitude where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
    Self.ComponentVal: Additive,
    Self.ComponentVal == Self.MagVal {
    var magSquare: Self.MagVal {
        return self.fst * self.fst + self.snd * self.snd
    }
}

/*:

 ## Scalar

 The scalar protocol should allow to "scale" a value using another value. For
 this we are going to add a new operator to differentiate between regular
 multiplication and scalar multiplication. The `◊` operator can be found on
 Apple keyboard with `alt-shift-v`.
 */

infix operator ◊: MultiplicationPrecedence

/// Protocol for scalar multiplication operation
protocol Scalar {
    associatedtype ScalarVal
    static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self
}

/*:
 This protocol can also be implemented automatically for any type provided:
  - has two dimensions
  - the components are multiplicative
  - the types of the components are the same as the scale factor's type
 */
extension Scalar where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
    ScalarVal == Self.ComponentVal {
    static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self {
        return Self.make2D(lhs * rhs.fst, lhs * rhs.snd)
    }
}

/*:
 Now that we have all the protocols we wanted we can start to implement actual
 datatypes.

 # 2D Vectors

 The definition of 2D vectors is pretty simple. It's a struct parameterized over
 the type of each of its components.
 */

struct Vector<A> {
    let fst: A
    let snd: A
}

extension Vector: CustomStringConvertible where A: CustomStringConvertible {
    var description: String {
        return "Vector<\(String(describing: A.self))>(\(fst.description), \(snd.description))"
    }
}

/*:
 * note: We implemented `CustomStringConvertible because we are going to need it
   when printing more complex values.

 Then, we can implement `TwoDimensions` and omit `fst` and `snd` since they are
 already implemented by the struct.
 */

extension Vector: TwoDimensions {
    static func make2D(_ fst: A, _ snd: A) -> Vector<A> {
        return Vector(fst: fst, snd: snd)
    }
}

/*:
 And finally, we can trivially (there is nothing to do) implement `Additive`,
 `Magnitude` and `Scalar` for `Vector`. And add `Equatable` in the mix as well.
 */

extension Vector: Additive where A: Additive { }
extension Vector: Magnitude where A: Additive, A: Multiplicative, A: Comparable {
    typealias MagVal = A
}
extension Vector: Scalar where A: Multiplicative {
    typealias ScalarVal = A
}

extension Vector: Equatable where A: Equatable {}

/*:
 There is no `Multiplicative` implementation since it isn't clear how to
 multiply two 2D Vectors. (Dot product? Cross product? Multiply the components?)

 Now we can use `+`, `magSquare` and `◊`
 */

let x = Vector(fst: 1, snd: 0)
let y = Vector(fst: 0, snd: 1)
let xy = x + y
xy
3 ◊ xy
xy.magSquare

/*:
 Computing the magnitude of a vector is a bit awkward let's see how we can
 improve that.

 ## Improving magnitude with square root

 Let us add a new protocol for types that have a square root:
 */

prefix operator √

/// A protocol for getting the square root of a value
protocol SquareRoot {
    static prefix func √(_ square: Self) -> Self
}

/*:
 This protocol uses the prefix operator `√` which can be found with `alt-v` on
 Apple platforms.

 We can now extend `Magnitude` to provide a `magnitude` field whenever the
 `MagVal` has a square root.
 */


// Magnitude has an extra field for types which have a square root function
extension Magnitude where Self.MagVal: SquareRoot {
    var magnitude: Self.MagVal {
        return √(self.magSquare)
    }
}

//: We can now try that:

//print(xy.magnitude)

/*:
 But we get an error

 "Type 'Int' does not conform to protocol `SquareRoot`"

 Indeed, we haven't added a `SquareRoot` extension for `Int`. But that is
 because there is no easy way to compute the square root of an integer number
 (Do we assume it's a double and round down? round up?  truncate?)

 Instead we are going to give `Double` all the necessary protocols in order to
 have vectors support the `magnitude` function when they are vectors of `Doubles`
 */

extension Double: Multiplicative {
    static var multId: Double { return 1.0 }
}

extension Double: Additive {
    static var addId: Double {
        return 0.0
    }
}

extension Double: SquareRoot {
    static prefix func √ (square: Double) -> Double {
        return sqrt(square)
    }
}

//: We can now write

let doubleVect = Vector(fst: 1.0, snd: 1.0)
doubleVect.magnitude

/*:
 and get `1.414213…` as expected. Now we can use `magnitude` when dealing with
 `Vector` of `Double` and `magSquare` when using `Int`.

 * note: Note that this definition of square root is incomplete since most
 values have multiple roots.

 # Complex Numbers

 Why stop here? We already have all those protocol so let us use them with a new
 datatype. Let us implement
 [Complex Numbers](https://en.wikipedia.org/wiki/Complex_number).
 */


// Complex numbers ----------------------
struct Complex: Equatable {
    let real: Double
    let imm: Double
}

extension Complex: CustomStringConvertible {
    var description: String { return "\(real) + \(imm)i" }
}

extension Complex: TwoDimensions {
    typealias ComponentVal = Double
    static func make2D(_ fst: Double, _ snd: Double) -> Complex {
        return Complex(real: fst, imm: snd)
    }
    var fst: Double { return real }
    var snd: Double { return imm }
}

extension Complex: Additive {}

extension Complex: Scalar { typealias ScalarVal = Double }

extension Complex: Magnitude {}

// Complex numbers have a multiplication such that
// (a + bi) * (c + di) = a * c + (a * d)i + (b * c)i + (b * d)i²
// since i² = -1 we have
// (a + bi) * (c + di) = a * c - b * d + (a * d + b * c)
extension Complex: Multiplicative {
    static var multId: Complex {
        return Complex(real: 1.0 , imm: 0.0)
    }
    static func * (_ lhs: Complex, _ rhs: Complex) -> Complex {
        return Complex(real: lhs.real * rhs.real - lhs.imm * rhs.imm,
                       imm:  lhs.real * rhs.imm  + lhs.imm * rhs.real)
    }
}

/*:
 As we can see, appart from `Multiplicative` there is no real work to do in
 order to implement those protocols.

 This allows us to write the following without any extra work.
 */

let i = Complex(real: 0.0, imm: 1.0)
let one = Complex(real: 1.0, imm: 0.0)
let other = Complex(real: Double.pi, imm: sqrt(2.0))
i * i == (-1) ◊ one
Complex.multId * i == i
other.magnitude
17.3 ◊ other

/*:

 # Linear functions

 We can take this to another level and implement linear functions.
 A linear function is a function of the form `f(x) = a * x + b`

 */

struct Linear<A> {
    let x: A
    let c: A // c stands for 'constant', it's the constant factor to the linar function
}

extension Linear: CustomStringConvertible where A: CustomStringConvertible {
    var description: String { return "f(x) = \(x)x + \(c)" }
}

extension Linear: Equatable where A: Equatable {}

extension Linear: TwoDimensions {
    typealias ComponentVal = A
    static func make2D(_ fst: A, _ snd: A) -> Linear {
        return Linear(x: fst, c: snd)
    }
    var fst: A { return x }
    var snd: A { return c }
}

extension Linear: Additive where A: Additive {}

extension Linear: Scalar where A: Multiplicative { typealias ScalarVal = A }

//: Like before, this allows us to write

let constOne = Linear(x: 0, c: 1)
let constThree = Linear(x: 0, c: 3)
let verySteep = Linear(x: 200, c: -5)
let moveUpByThree = { $0 + constThree }
constOne + Linear.addId == constOne
moveUpByThree(constOne)
285 ◊ verySteep

/*:
 As we can see, we left out `magnitude` since the semantics of magnitude for a
 linear function are unclear (at least to me). We also left out `Multiplicative`
 because multiplying two linear functions results in a quadratic one. And this
 is a problem because our definition of `Multiplicative` expects to return a
 value of the same time as its argument.

 ## Fancy multiplication

 We can fix the multiplication problem by introducing another protocol
 `FancyMult` which multiplies two values of the same type and returns a value of
 another type.
 */

infix operator **: MultiplicationPrecedence

protocol FancyMult {
    associatedtype FancyVal
    static func ** (lhs: Self, rhs: Self) -> FancyVal
}


//: We can use this to implement the product between two linear functions:

struct Quadratic<A> {
    let x²: A
    let x: A
    let c: A
}

extension Quadratic: Equatable where A: Equatable {}

extension Linear: FancyMult where A: Additive, A: Multiplicative {
    typealias FancyVal = Quadratic<A>
    static func ** (lhs: Linear<A>, rhs: Linear<A>) -> Quadratic<A> {
        return Quadratic(x²: lhs.x * rhs.x,
                         x: lhs.x * rhs.c + lhs.c * rhs.x,
                         c: lhs.c * rhs.c)
    }
}

//: Or even dot product of two vectors:

extension Vector: FancyMult where A: Additive, A: Multiplicative {
    typealias FancyVal = A
    static func ** (lhs: Vector<A>, rhs: Vector<A>) -> A {
        return lhs.fst * rhs.fst + lhs.fst * rhs.fst
    }
}


//: This allows us to write the  identity `(a²x² - b²) = (ax + b) * (ax - b)`

let linearMinus = Linear(x: 3, c: -5)
let linearPlus = Linear(x: 3, c: 5)
let quadMinus = Quadratic(x²: 9, x: 0, c: -25)
linearPlus ** linearMinus == quadMinus


/*:
 ## Power interlude

 For entertainment purposes here is an implementation of a power operator `^^`
 based on the implementation of `Multiplicative`
 */

infix operator ^^: PowerPrecedence
precedencegroup PowerPrecedence {
    higherThan: MultiplicationPrecedence
    associativity: right
}

func ^^ <A: Multiplicative> (lhs: A, rhs: Int) -> A {
    var a = A.multId
    for _ in 0..<rhs {
        a = a * lhs
    }
    return a
    // This function is equivalent to
    // `return Array(repeating: lhs, count: rhs).reduce(A.multId, *)`
}

//: This allows us to write

// 3²x² + 5²
let quadPlus = Quadratic(x²: 3 ^^ 2, x: 30, c: 5 ^^ 2)
linearPlus ** linearPlus == quadPlus

/*:
 # Combining everything

 Now we can even combine everything and have it work as
 expected. For example we can have complex vectors:
 */

let ai = Complex(real: 18.9, imm: -19.3)
let bi = Complex(real: 4.4, imm: 0.2)
let vecti = Vector(fst: ai, snd: bi)
one ◊ vecti
vecti + vecti
vecti ** vecti

//: And we can even have linear functions of complex vectors:

let vectj = Vector(fst: Complex.multId, snd: Complex.addId)
let fVecti = Linear(x: Vector.addId, c: vecti)
let fVectj = Linear(x: vectj, c: vectj)

fVecti + fVectj
Linear.addId + fVectj == fVectj

/*:

 This really demonstrates the power of protocols and conditional conformance.
 They allow us to reuse our code and combine different structures in very
 flexible ways without us having to worry about the details of how future code
 will be implemented.

 # Conclusion

 Our protocol-oriented architecture perfectly encapsulates the concept of
 [DRY](DRY). We only define the relevant implementation once and the rest
 follows from the structure of our program. This makes the code very easy to
 extend and makes every component of it relatively simple. Unfortunately, our
 example only allows for 2-dimensional types. In the next part we are going
 to see how to use protocols to emulate dependent types and have vectors of
 arbitrary size.

 [DRY]: https://en.m.wikipedia.org/wiki/Don%27t_repeat_yourself

 */

// Part 2

/*:

 # Sized vectors in Swift

 In the last part we used protocols in order to model our different operations and
 combine them to great effects.

 But we were constrained to 2-dimensional types. This is a bit limiting, we would
 like to manipulate values of more than 2 dimensions. This would require making sure
 that we are manipulating values of the same dimensions.

 What's following is seriously considered a "do not try this at home" and is purely
 presented for it's academic interest rather than its real world practicality in Swift.

 We are going to implement dependent types using protocols

 ## N-dimensional vectors, first attempt

 In order to better understand the problem we are going to attempt to implement n-dimensional
 vectors using a simple array. Most existing implementation use arrays in order to represent
 n-dimensional vectors but they have pretty big flaws:


 ```
 struct NVector<A> {
     let vec: [A]
 }

 extension NVector: Additive where A: Additive {
     static func + (lhs: NVector<A>, rhs: NVector<A>) -> NVector<A> {
         if lhs.vec.count == rhs.vec.count {
             return NVector(vec: zip(lhs.vec, rhs.vec).map(+))
         } else {
             // err…what now?
             fatalError()
         }
     }

     static var aIdentity: NVector<A> {
         // return… wait, what's the size of the array we need to return?
         // we can't just return [] or [A.aIdentity] we need to return the same size as the vector we're expecting to combine with
         fatalError()
     }
 }
 ```


 We actually cannot implement `Additive` for `NVector` because it does not carry
 enough information about the size of the array. We need to encode the size of
 the array somehow in the data. This will both allow us to reuse code as well as
 avoid errors like the following:

 ```
 let empty = NVector(vec: Array<Int>())
 let nonEmpty = NVector(vec: [1])
 empty + nonEmpty // This does not make sense yet it would compile
 ```

 In order to fix this we are going to need our `NVector` struct to be declared
 with a `Size` type parameter and the type of the values it holds.

 ```
 struct NVector<Size, A> { … }
 ```

 and our `+` function would look like this:

 ```
 func + <N: Size, A>(lhs: NVector<N, A>, rhs: NVector<N, A>) -> NVector<N, A> { … }
 ```

 Since the first type parameter is the size of the array we enforce that it is
 the same for the two vectors of the addition.

 It turns out that Swift already has enough technology to implement this kind of
 functionality.

 First we are going to define a new type `Nat` in order to represent natural
 numbers

 ## A world of Nats

 "Nat" is the shorthand for "natural number" which are the designation for
 integer numbers from 0 to infinity. They are defined recursively using those
 two definitions:
  - Zero is a natural number
  - Any successor to a natural number is also a natural number
 In Swift we can write this as:

 */

protocol Nat {} // There exist natural numbers.
enum Zero: Nat {} // Zero is a natural number.
enum Succ<N: Nat>: Nat {} // Succ is a natural number that wraps another one.


//: We can now define a couple of natural numbers:

typealias One   = Succ<Zero>
typealias Two   = Succ<One>
typealias Three = Succ<Two>
typealias Four  = Succ<Three>

/*:
 As you can see the numbers are recursively defined in terms of the previous
 ones. You can also see that we are using empty `enum` declaration which have no
 values. That mean that `Nat` only exists as *types* and not as *values*. There
 is no *value* for the type `Three`.

 We can however create a function that, given a type `Nat` can retrieve the
 `Int` value that it represents. So that we can get the value `3` (of type
 `Int`) from the type `Three`.

 Here is how:

 First we need a protocol that will declare that the type that conforms to it
 has an `Int` representation
 */

protocol NatToInt {
    static var int: Int { get }
}

/*:
 This protocol states that a type conforming to it has a static `Int` value
 associated with it.

 Then, we need to implement this protocol for the `Zero` type
 and for the `Succ` type. `Zero` is straight-forward to implement: `Zero` is
 represented by the number `0`.
 */

extension Zero: NatToInt {
    static var int: Int { return 0 }
}

/*:
 Finally we need to implement the same protocol for `Succ`. `Succ` is a bit
 trickier because it is recursively defined. So we need to make use of
 conditional conformance in order to ask that the inner `N` type also has an
 `Int` representation. This allows us to compute the value using the `NatToInt`
 implementation of the wrapped `Nat`.

 `Succ` returns `1` plus the `Int` value of `N`. So that way the type `Two`
 returns `1` plus the int value of `One` which itself returns `1` + the `Int`
 value of `Zero` which we said was `0`. The total amounts to `1 + 1 + 0 = 2`.
 [COND]: https://swift.org/blog/conditional-conformance/
 */

extension Succ: NatToInt where N: NatToInt {
    static var int: Int { return 1 + N.int}
}

//: We can now write

Three.int

/*:
 And get `3` as expected.

 # Dependent NVector

 Now that we have Natural numbers in type we can use this type for defining a
 vector type that uses `Nat` in order to carry it's size information.

 Unfortunately if we try to implement `NVector` like we do `Lists` we run into
 a problem:

 ```
 enum NVector<N: Nat, A> {
     case empty // N should be Zero here
     case cons(A, NVector<???, A>)
 ```

 We cannot have a different `N` for each case, ideally we would like something
 like

 ```
 enum NVector<N: Nat, A> {
     case empty where N == Zero
     case cons<Pred: Nat>(A, NVector<Pred, A>) where Succ<Pred> == N
 }
 ```

 * note: This suspiciously look like _GADTs_

 Instead we are going to use the same strategy that we used with `Nat`, we are going to define a protocol and have multiple datatypes implementing it.

 */

protocol NVector {
    associatedtype Size: Nat
    associatedtype Elements
}

/*:
 This says that a N-dimensional vector depends on two types, the size as a `Nat`
 and the `Elements` inside the vector.

 Since we are going to define Vects as linked lists we are going to need two
 datatypes. One for the empty list and one for the list that has one element and
 a reference to a smaller list.
 */

struct NilVect<A>: NVector {
    typealias Size = Zero
    typealias Elements = A
}

struct ConsVect<VS: NVector>: NVector {
    typealias Size = Succ<VS.Size>
    typealias Elements = VS.Elements
    let value: Elements
    let tail: VS
}

/*:
 The definition for the empty vector is quite straight forward, it's an empty
 struct which has size `Zero` and carries any type `A`.

 For the `Cons` case we have a struct with two fields. One for the head of the
 list, and one for the tail of the list. The type of the tail (`VS`) has to
 also be a `NVector`. This allows us to define the size of the vector (the
 vector has to be of size one more than the size of the tail) and what kind of
 element it holds (the same elements as the tail).

 ## Writing dependent functions

 We can now start using our sized vectors but they are not very ergonomic to
 create. If we need to construct a vector of size 3 we need to make 3 calls to
 `ConsVect` and one call to `NilVect`:

 */

let ohgodVect = ConsVect(value: 3, tail: ConsVect(value: 2, tail: ConsVect(value: 1, tail: NilVect())))

/*:
 This is unreadable.

 In order to fix this we are going to introduce a "cons" operator. Traditionally
 this is a double colon (`::`), but Swift reserves the colon symbol for type
 definitions and it is not a valid operator symbol. Instead we are going to use
 `§§` which can be found with `alt-6` on Apple platforms.
 */

infix operator §§ : AssignmentPrecedence

func §§ <V: NVector>(lhs: V.Elements, rhs: V) -> ConsVect<V> {
    return ConsVect(value: lhs, tail: rhs)
}

/*:
 As you can see, the type signature of this function is
 ```
 <V: NVector>(V.Elements, V) -> ConsVect<V>
 ```
 which reads as "for all Vectors `V`, given an element of `V` and a vector `V`,
 construct a vector of size one bigger than `V`". The last part is crucial:
 `ConsVect<V>` means "one size bigger than `V`".

 To further facilitate the usage of Vectors we are going to add the function
 `empty` to create empty vectors.

 */

func empty<A>() -> NilVect<A> {
    return NilVect()
}

//: And use both of those in conjunction to create vectors:


let four1 = 4 §§ 3 §§ 2 §§ 1 §§ empty()
let four2 = 8 §§ 7 §§ 6 §§ 5 §§ empty()
let twoV = 2 §§ 1 §§ empty()
let BIGVECT = 9 §§ 8 §§ 7 §§ 6 §§ 5 §§ 4 §§ 3 §§ 2 §§ 1 §§ 0 §§ empty()


/*:
 ## Using type information

 A very nice property from having the size information in the type is that we
 do not even have to inspect our data in order to compute useful data. For
 example, by reusing our `int` property on `Nat` we can compute the size of a
 vector without iterating though it.
 */

func length<V: NVector>(_ vec: V) -> Int where V.Size: NatToInt {
    return V.Size.int
}

length(BIGVECT)

/*:

 The usefulness of this particular example is limited but it reveals to us a
 new concept: There exist structures (like vectors) which have properties that
 are independent from the values they hold. Those properties can be expressed
 in types and will hold for all possible instances. This form of polymorphism
 is stronger than simple generics because it allows us to write more complex
 constraints (like size constraints) while staying agnostic of the nature of the
 data we manipulate.

 ## Implementing protocols

 As you might have noticed, printing vectors doesn't output a very pretty or
 readable text. In order to fix that we need to implement Swift's standard
 library protocol `CustomStringConvertible`. It only has one field and it's a
 `description` getter which returns a `String`. We are going to make our vectors
 conform to `CustomStringConvertible` in order to print out prettier messages.
 */

extension NilVect: CustomStringConvertible {
    var description: String { return ""}
}

extension ConsVect: CustomStringConvertible where
    VS: CustomStringConvertible {
    var description: String {
        return "\(self.value) \(self.tail.description)"
    }
}

/*:
 We do not print anything for the empty vector and for the `ConsVect` we print
 the head and then recursively print the tail with a space between the two.

 Now we can even print matrices using our dependent vectors
 */

let twoVect = 2 §§ 2 §§ empty()
let twoMatrix = twoVect §§ twoVect §§ empty()
twoMatrix

/*:
 ## Additive

 Now that we have working n-dimensional vectors and we know how to implement
 functions on them we can add again all the functionalities that we had with
 `Vector<A>`

 Unlike before, we cannot have `Additive` be automatically implemented because
 we only have a default implementation for types that conform to `TwoDimensions`.
 Moreover our `NVector` type isn't defined as a plain datatype, but is split in
 two instances of the `NVector` protocol which isn't a native construction of
 the language that we can abstract over.

 This means that we have to implement `Additive` ourselves for both the
 `NilVect` and the `ConsVect`.
 */

extension NilVect: Additive {
    static var addId: NilVect { return NilVect() }
    static func + (lhs: NilVect<A>, rhs: NilVect<A>) -> NilVect<A> {
        return NilVect()
    }
}

/*:
 Adding empty vectors together is pretty easy since they do not carry any value
 we can just return the empty vector every time.

 For `ConsVect` we will have to recursively define addition in term of the
 current value and the `tail` vector assuming the tail is `Additive` as well.
 */

extension ConsVect: Additive where VS.Elements: Additive, VS: Additive {
    static var addId: ConsVect<VS> {
        return ConsVect(value: VS.Elements.addId, tail: VS.addId)
    }
    static func + (lhs: ConsVect<VS>, rhs: ConsVect<VS>) -> ConsVect<VS> {
        return ConsVect(value: lhs.value + rhs.value,
                        tail: lhs.tail + rhs.tail)
    }
}

/*:
 As you can see an identity `ConsVect` for addition is a vector of identity
 values. And adding two vectors together amounts to creating a new vector with
 the sum of the two heads and the sum of the two tails. We can now write
 */

twoMatrix + twoMatrix

/*:
 and get the expected result "4 4  4 4".

 ## FancyMult, Scalar and Magnitude

 We can conform to all other existing protocols by implementing them with a
 base case for `NilVect` and a recursive one for `ConsVect`.

 */

extension NilVect: FancyMult where A: Additive {
    typealias FancyVal = A
    static func ** (lhs: NilVect<A>, rhs: NilVect<A>) -> A {
        return A.addId
    }
}

extension ConsVect: FancyMult where
    VS.Elements: Additive,
    VS.Elements: Multiplicative,
    VS: FancyMult,
    VS.FancyVal == VS.Elements {
    typealias FancyVal = Elements
    static func ** (lhs: ConsVect<VS>, rhs: ConsVect<VS>) -> Elements {
        return lhs.value * rhs.value + (lhs.tail ** rhs.tail)
    }
}

twoVect ** twoVect

extension NilVect: Magnitude where A: Additive, A: Comparable {
    typealias MagVal = A
    var magSquare: A {
        return A.addId
    }
}

extension ConsVect: Magnitude where
    VS.Elements: Additive,
    VS.Elements: Multiplicative,
    VS: Magnitude,
    VS.Elements == VS.MagVal {
    typealias MagVal = VS.Elements
    var magSquare: VS.Elements {
        return self.value ^^ 2 + self.tail.magSquare
    }
}

let doubleNVect = √2.0 §§ √3.0 §§ empty()
doubleNVect.magnitude

extension NilVect: Scalar {
    typealias ScalarVal = A
    static func ◊ (lhs: A, rhs: NilVect<A>) -> NilVect<A> {
        return empty()
    }
}

extension ConsVect: Scalar where
    VS.Elements: Multiplicative,
    VS: Scalar,
    VS.ScalarVal == VS.Elements {
    typealias ScalarVal = VS.Elements
    static func ◊ (lhs: VS.Elements, rhs: ConsVect<VS>) -> ConsVect<VS> {
        return ConsVect<VS>(value: lhs * rhs.value, tail: lhs ◊ rhs.tail)
    }
}

3 ◊ twoVect

/*:
 As you will notice they all follow the same pattern:
 - `NilVect` returns some sort of empty value or identity
 - `ConsVect` makes some operation on the head and make a recursive call to the tail.

 But Swift is unable to implement those protocols automatically like it did for
 `Vector`, `Complex` and `Linear`.
 Interestingly enough there *is* a part of the language that can abstract over
 this pattern and that's the baked-in Equatable protocol:
 */

extension NilVect: Equatable {}
extension ConsVect: Equatable where VS: Equatable, VS.Elements: Equatable {}

twoVect == twoVect

/*:
 It would be nice if the compiler exposed the tools used to make this possible
 so that we didn't have to write all those functions ourselves.

 # Final remarks

 An astute reader  might notice that we haven't implemented the most famous
 function `map` over vectors. That's because it's currently impossible at least
 with my current understanding of  the type system. Let's give it a try:
 */

//  protocol Functor {
//      static func map<Src, Dest>(_ f: (Src) -> Dest, _ value: Self) -> Self
//  }

/*:
 * note: a type is `Functor` if it has a `map` function

 This definition is incorrect because it indicates that we transform `Self`
 into `Self` which suggest that `Self` does not change. However, we want to
 transform by changing its type parameter, that is we transform `Self<Src>`
 into `Self<Dest>`. We need to try something else:
 */

protocol Wrapper {
    associatedtype InnerType
    static func pure(_ value: InnerType) -> Self
}

extension Array: Wrapper {
    typealias InnerType = Element
    static func pure(_ value: Element) -> [Element] { return [value] }
}

/*:
 Here, we create a protocol `Wrapper` which abstract over types that have one
 type parameter and can create themselves given a value of that type.
 We also gave an implementation of `Wrapper` for `Array`. Now we can try again
 to design a `Functor` protocol around `Wrapper`:
 */

  protocol Functor: Wrapper {
       func fmap<Dest, R>(_ f: (InnerType) -> Dest) -> R where R.InnerType == Dest, R: Functor
  }

/*:
 * note: We're calling map `fmap` to avoid conflict with the `map` function
 from arrays

 This definition has *another* problem. It returns `R` which can be of any type
 as long as it conforms to `Functor`. This is incorrect, we want `R` to be the
 same type as `Self` except parameterized over `Dest` and not `InnerType`. If
 we keep trying we can still implement our `fmap` function but the result is
 quite undewhelming.

 * Experiment:
   - remove the comments before the extension
   - remove the comment before `as! R`
 */


//extension Array: Functor {
//    func fmap<Dest, R>(_ f: (Element) -> Dest) -> R where Dest == R.InnerType, R : Functor {
//        return self.map(f) //as! R
//    }
//}

/*:
 This is what we would like to write but the compiler tells us that it cannot
 convert `[Dest]` into `R` and that's because it cannot prove that `R` and
 `Self` are necessarily the same. This is a constraint we cannot express. The
 only suggestion from the compiler is to add `as! R` at the end. But we would
 still have problems:
 1. regardless of `as! R` it cannot tell why this is wrong:
 */

//extension Array: Functor {
//    func fmap<Dest, R>(_ f: (Element) -> Dest) -> R where Dest == R.InnerType, R : Functor {
//        return self as! R
//    }
//}

/*:
 2. We lose type inference
 */

//let mapped = [1,2,3].fmap { $0 + 3 }

/*:
 It says

 "Cannot invoke 'fmap' with an argument list of type '((Int) -> Int)'"

 Which is extremely confusing. We *can* invoke `fmap` with `(Int) -> Int` as
 argument but Swift cannot deduce that the return type should be `[Int]` since
 we're only telling it to return `R`. The following works
 */

//let mapped: [Int] = [1,2,3].fmap { $0 + 3 }

/*:
 By telling map to return `R` without being able to constrain it further we
 lose the ability to infer the return type of `fmap` which is unusable as an
 API.

 If Swift had type-level functions or Higher Kinded Types we could write
 something similar to:
 */

//  protocol Functor: <*> -> * {
//      func fmap<Dest>(_ f: (InnerType) -> Dest) -> Self<Dest>
//  }

/*:
 * note: I'm reusing the `(Type) -> Type` syntax that we use for function signatures and replacing the parenthesis by angle brackets to make it clear that this is about Type constructors and not Value constructors

 With `<*> -> *` representing the types which are parameterized over a single type argument.

 This would allow the compiler to deduce that the return type of `fmap` is the same type than the type that conforms to it, except it's parameterized over a different `InnerType`.

 We encounter a similar problem when trying to multiply two sized matrices (Vectors of Vectors) together. Given a matrix of size `M x N` and another of size `N x O` we need to return a matrix of size `M x O`. Unfortunately, Swift does not allow us to define a function that returns a new value of the same type but parameterized over `M x O`.

 # Conclusion

 At first we were able to design an architecture that allowed to abstract over the behavior of very generic structures and we were able to combine them in very interesting ways. Then we tried to take this abstraction further and were able to implement _sized_ vectors, something that is rarely possible in other programming languages. Unfortunately that is also where we hit the limits of Swift. The lack of true dependent types prevent us from constructing sized-vectors from the runtime (they all have to be statically defined at compile type) and the lack of higher kinded types prevent us from defining functions like `map` on vectors or matrix multiplication.
 Despite those shortcomings, Swift's type system has gotten way more powerful and useful with the introduction of conditional conformances and is more than capable to handle most programing tasks. It's a shame we still have to rely on runtime errors in order to multiply matrices.

 Such abstraction would also allow to develop type-safe state machines, network protocols and better optimisations. I hope to see them enter the mainstream sooner rather than later.

 */
