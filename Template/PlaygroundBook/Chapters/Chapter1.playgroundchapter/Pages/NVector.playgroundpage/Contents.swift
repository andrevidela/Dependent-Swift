//#-hidden-code
import Foundation
protocol Multiplicative {
    static var multId: Self { get }
    static func * (_ lhs: Self, _ rhs: Self) -> Self
}
//#-end-hidden-code
//#-hidden-code
extension Int: Multiplicative {
    static var multId: Int {
        return 1
    }
}
protocol Additive {
    static var addId: Self { get }
    static func + (_ lhs: Self, _ rhs: Self) -> Self
}
extension Int: Additive {
    static var addId: Int { return 0 }
}
protocol TwoDimensions {
    associatedtype ComponentVal
    static func make2D(_ fst: ComponentVal, _ snd: ComponentVal) -> Self
    var fst: ComponentVal { get }
    var snd: ComponentVal { get }
}
extension Additive where Self: TwoDimensions, Self.ComponentVal: Additive {
    static var addId: Self {
        return Self.make2D(Self.ComponentVal.addId, Self.ComponentVal.addId)
    }
    static func + (lhs: Self, rhs: Self) -> Self {
        return make2D(lhs.fst + rhs.fst, lhs.snd + rhs.snd)
    }
}
protocol Magnitude {
    associatedtype MagVal: Comparable
    var magSquare: MagVal { get }
}
extension Magnitude where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
    Self.ComponentVal: Additive,
Self.ComponentVal == Self.MagVal {
    var magSquare: Self.MagVal {
        return self.fst * self.fst + self.snd * self.snd
    }
}
infix operator ◊: MultiplicationPrecedence
protocol Scalar {
    associatedtype ScalarVal
    static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self
}
extension Scalar where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
ScalarVal == Self.ComponentVal {
    static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self {
        return Self.make2D(lhs * rhs.fst, lhs * rhs.snd)
    }
}
struct Vector<A> {
    let fst: A
    let snd: A
}
extension Vector: CustomStringConvertible where A: CustomStringConvertible {
    var description: String {
        return "Vector<\(String(describing: A.self))>(\(fst.description), \(snd.description))"
    }
}
extension Vector: TwoDimensions {
    static func make2D(_ fst: A, _ snd: A) -> Vector<A> {
        return Vector(fst: fst, snd: snd)
    }
}
extension Vector: Additive where A: Additive { }
extension Vector: Magnitude where A: Additive, A: Multiplicative, A: Comparable {
    typealias MagVal = A
}
extension Vector: Scalar where A: Multiplicative {
    typealias ScalarVal = A
}
extension Vector: Equatable where A: Equatable {}
prefix operator √
protocol SquareRoot {
    static prefix func √(_ square: Self) -> Self
}
extension Magnitude where Self.MagVal: SquareRoot {
    var magnitude: Self.MagVal {
        return √(self.magSquare)
    }
}
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
extension Complex: Multiplicative {
    static var multId: Complex {
        return Complex(real: 1.0 , imm: 0.0)
    }
    static func * (_ lhs: Complex, _ rhs: Complex) -> Complex {
        return Complex(real: lhs.real * rhs.real - lhs.imm * rhs.imm,
                       imm:  lhs.real * rhs.imm  + lhs.imm * rhs.real)
    }
}
struct Linear<A> {
    let x: A
    let c: A
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
infix operator **: MultiplicationPrecedence
protocol FancyMult {
    associatedtype FancyVal
    static func ** (lhs: Self, rhs: Self) -> FancyVal
}
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
extension Vector: FancyMult where A: Additive, A: Multiplicative {
    typealias FancyVal = A
    static func ** (lhs: Vector<A>, rhs: Vector<A>) -> A {
        return lhs.fst * rhs.fst + lhs.fst * rhs.fst
    }
}
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
}
//#-end-hidden-code
//#-editable-code

// Part 2

/*:

 # Sized vectors in Swift

 In the previous part we used protocols in order to model for some properties
 we were interested in and found multiple examples that fit the model (2D
 vectors, complex numbers and linear functions).

 But these were constrained to 2-dimensional types. This is too limiting, we
 would like to manipulate more than 2 dimensions. This would require making sure
 that we are manipulating values of the same dimensions.

 **disclaimer**

 What follows is seriously considered a "do not try this at home" and is purely
 presented for it's academic interest rather than its real world practicality
 in Swift.

 We are going to implement dependent types using protocols.

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

 Unfortunatly, we cannot implement `Additive` for `NVector` because it does not carry
 enough information about its size. We need to encode the size of the vector
 somehow. This will both allow us to reuse code as well as
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

 This can be written in Swift like so:

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
 values. That mean that `Nat` only exists as *type* and not as *value*. There
 is no *value* for the type `Three`.

 We can however create a function that, given a type `Nat` can return the
 `Int` value that it represents. So that we can get the value `3` (of type
 `Int`) from the type `Three`.

 Here is how:

 1. First we need a protocol that will declare that the type that conforms to it
 has an `Int` representation
 */

protocol IntRepr {
    static var int: Int { get }
}

/*:
 This protocol states that a type conforming to it has a static `Int` value
 associated with it.

 2. Then, we need to implement this protocol for the `Zero` type
 and for the `Succ` type. `Zero` is straight-forward to implement: `Zero` is
 represented by the number `0`.
 */

extension Zero: IntRepr {
    static var int: Int { return 0 }
}

/*:
 3. Finally we need to implement the same protocol for `Succ`. `Succ` is a bit
 trickier because it is recursively defined. So we need to make use of
 conditional conformance in order to ask that the inner `N` type also has an
 `Int` representation. The wrapped type `N` is actually the predecessor of the
 type we are currently computing.

 Therefore, `Succ` returns `1` plus the `Int` value of `N`, it's predecessor.
 This way the type `Two` returns `1` plus the int value of `One` which itself
 returns `1` + the `Int` value of `Zero` which is `0`.
 The total amounts to `1 + 1 + 0 = 2`.
 */

extension Succ: IntRepr where N: IntRepr {
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
     case cons(A, NVector<???, A>) // What should fit here?
 }
 ```

 We cannot have a different `N` for each case, ideally we would like something
 like

 ```
 enum NVector<N: Nat, A> {
     case empty where N == Zero
     case cons<Pred: Nat>(A, NVector<Pred, A>) where Succ<Pred> == N
 }
 ```

 * note: This suspiciously look like [_GADTs_](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)

 Instead we are going to use the same strategy that we used with `Nat`, we are
 going to define a protocol and have concrete type implementing it.

 */

protocol NVector {
    associatedtype Size: Nat
    associatedtype Elements
}

/*:
 This says that a N-dimensional vector depends on two types, the size as a `Nat`
 and the `Elements` inside the vector.

 Interestingly enough, we cannot use arrays to define our vectors since they
 do not carry size information at the type level. Instead we are going to
 implement vectors like we implement linked lists. As a reminder here is how
 we would implement linked lists in swift

 ```
 indirect enum LinkedList<A> {
     case empty
     case cons(A, LinkedList<A>)
 }
 ```

 That is, a `LinkedList` could be either empty, or have a head element and a
 tail `LinkedList`. Again we cannot use `enum` here because of our size
 constraint but we _are_ going to use the same idea: An empty vector holds
 nothing, and a "cons" vector hold both an element and a tail vector.

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
 vector, and one for the tail of the vector. The type of the tail (`VS`) also
 has to be a `NVector`. This allows us to define the size of the vector to be
 one more than the size of the tail and that the element has to be of the same
 type as the elements held by the tail.

 ## Writing dependent functions

 We can now start using our sized vectors but they are not very ergonomic to
 create. In order to construct a vector of size 3 we need to make 3 calls to
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
 do not even have to inspect our data in order to compute useful programs. For
 example, by reusing our `int` property on `Nat` we can compute the size of a
 vector without iterating though it.

 * note: This still requires iterating through the `Nat` type.
 */

func length<V: NVector>(_ vec: V) -> Int where V.Size: IntRepr {
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
 data we manipulate. Indeed, the implementation for `NVector` could be different
 but as long as it uses `Nat` to enforce its size then this implementation will
 work.

 You might also notice that we are able to go from `Nat` to `Int` but we
 have not provided a way to go from `Int` to `Nat`. This is beacuse it is
 impossible for swift to deduce which instance of a protocol to use from a
 run-time value. Indeed, if the value `3` came from standard input. How would
 Swift be able to tell that it is the representation of `Succ<Succ<Succ<Zero>>>`
 ?

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

 Unfortunately, Swift is unable to implement those protocols automatically like
 it did for `Vector`, `Complex` and `Linear`.
 Interestingly enough there *is* a part of the language that can abstract over
 this pattern and that's the Equatable protocol:
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
 with my current understanding of  the type system. To see why, let's give it a
 try:
 */

//  protocol Functor {
//      static func map<Src, Dest>(_ f: (Src) -> Dest, _ value: Self) -> Self
//  }

/*:
 * note: A type is `Functor` if it has a `map` function


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
 type parameter and can create themselves given a value of that type. As an
 example we gave an implementation of `Wrapper` for `Array`. Now we can try
 again to design a `Functor` protocol around `Wrapper`:
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
 - Remove the comments before the extension.
 - Remove the comment before `as! R`.
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

 * experiment: Remove the comments to try it.

 2. We lose type inference
 */

//let mapped = [1,2,3].fmap { $0 + 3 }

/*:
 It says

 "Cannot invoke 'fmap' with an argument list of type '((Int) -> Int)'"

 * experiment: Remove the comment to see the error.

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

 In the first part we were able to design an set of protocols that allowed us
 to abstract over the behavior of very generic structures and we were able to
 combine them in very interesting ways (As long as they were 2D). Then we tried
 to abstract over the size of our types and implemented _sized_ vectors.
 Unfortunately that is also where we hit the limits of Swift; The lack of true
 dependent types prevent us from constructing sized-vectors from the runtime
 (they all have to be statically defined at compile type) and the lack of
 type-level functions or higher kinded types prevent us from
 defining functions like `map` on vectors or matrix multiplication.

 Despite those shortcomings, Swift's type system has gotten way more powerful
 and useful with the introduction of conditional conformances and is more than
 capable to handle most programing tasks. It's a shame we still have to rely on
 runtime errors in order to multiply matrices.

 If Dependent Types were included in Swift we would also allow to develop
 type-safe state machines, network protocols and better optimisations. Even if
 we "only" get Higher Kinded Types, at least we could implement `map` on our
 `NVector`.
 */
//#-end-editable-code
