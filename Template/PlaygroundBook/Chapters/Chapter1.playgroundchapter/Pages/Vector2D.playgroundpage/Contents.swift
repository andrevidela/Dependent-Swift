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
 such that `a * id = a`. As we will see, for `Int` and `Double` the identity
 value will be `1` and `1.0` respectively but this isn't the case for more
 complex types.

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
 * note: Those two protocols are [_monoids_](https://en.wikipedia.org/wiki/Monoid)

 ## TwoDimensions

 In order to simplify our future work we will abstract over the fact that some
 types have two dimensions. As such, they will need to be able to project each
 component and create itself from two component values.

 This protocol will allow us to define default implementation for types that
 have two dimensions.
 */

/// Protocol for 2D coordinate systems
protocol TwoDimensions {
    associatedtype ComponentVal
    static func make2D(_ fst: ComponentVal, _ snd: ComponentVal) -> Self
    var fst: ComponentVal { get } // The first component
    var snd: ComponentVal { get } // The second component
}

/*:
 We do not have any type to conform to this protocol but we can already
 implement `Additive` for it. Indeed, any type is `Additive` provided it has
 two dimensions and its components are `Additive` as well.

 As for Identity, since `Additive` is implemented by adding each component
 together we can define identity by creating a 2D object with identity values
 for each component.

 As we can see, we have an identity for addition that isn't `1` but is a value
 which uses the identity value of it's component type.
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

 The [magnitude](https://en.wikipedia.org/wiki/Magnitude_(mathematics)) of a
 value is a way to have a representation of the
 "size" of it. Typically for 2D vectors the magnitude is the length of the
 vector. For `Int`s it is their absolute value. This value is useful to
 compare two different values even if there is not necessarily a total order
 between all values.

 To describe a type that has a magnitude we need to say that it has
 a property that returns a value of some type and this value needs to be
 `Comparable`.

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
 - it has two dimensions
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
 - it has two dimensions
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
 Now that we have all the protocols we wanted we can start to implement concrete
 types.

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
 * note: We implemented `CustomStringConvertible` because we are going to need
 it when printing more complex values.

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

 * note: for the unfamiliar `typealias` relates an `associatedtype` with a
         concrete type coming from the instance we are implementing.

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
    let c: A // c stands for 'constant', it's the constant factor to the linear function
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
 value of the same type as its argument.

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

 Our protocol-oriented approach perfectly encapsulates the concept of
 [DRY](DRY). We only define the relevant implementation once and the rest
 follows from the structure of our program. This makes the code very easy to
 extend and makes every component of it relatively simple. Unfortunately, our
 example only allows for 2-dimensional types. In the next part we are going
 to see how to use protocols to emulate dependent types and have vectors of
 arbitrary size.

 [DRY]: https://en.m.wikipedia.org/wiki/Don%27t_repeat_yourself

 */
