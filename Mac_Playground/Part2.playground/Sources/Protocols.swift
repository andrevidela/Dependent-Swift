
import Foundation

public protocol Multiplicative {
    static var multId: Self { get }
    static func * (_ lhs: Self, _ rhs: Self) -> Self
}
extension Int: Multiplicative {
    public static var multId: Int {
        return 1
    }
}
public protocol Additive {
    static var addId: Self { get }
    static func + (_ lhs: Self, _ rhs: Self) -> Self
}
extension Int: Additive {
    public static var addId: Int { return 0 }
}
public protocol TwoDimensions {
    associatedtype ComponentVal
    static func make2D(_ fst: ComponentVal, _ snd: ComponentVal) -> Self
    var fst: ComponentVal { get }
    var snd: ComponentVal { get }
}
public extension Additive where Self: TwoDimensions, Self.ComponentVal: Additive {
    static var addId: Self {
        return Self.make2D(Self.ComponentVal.addId, Self.ComponentVal.addId)
    }
    static func + (lhs: Self, rhs: Self) -> Self {
        return make2D(lhs.fst + rhs.fst, lhs.snd + rhs.snd)
    }
}
public protocol Magnitude {
    associatedtype MagVal: Comparable
    var magSquare: MagVal { get }
}
extension Magnitude where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
    Self.ComponentVal: Additive,
Self.ComponentVal == Self.MagVal {
    public var magSquare: Self.MagVal {
        return self.fst * self.fst + self.snd * self.snd
    }
}
infix operator ◊: MultiplicationPrecedence
public protocol Scalar {
    associatedtype ScalarVal
    static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self
}
extension Scalar where
    Self: TwoDimensions,
    Self.ComponentVal: Multiplicative,
ScalarVal == Self.ComponentVal {
    public static func ◊ (_ lhs: ScalarVal, _ rhs: Self) -> Self {
        return Self.make2D(lhs * rhs.fst, lhs * rhs.snd)
    }
}
public struct Vector<A> {
    public let fst: A
    public let snd: A
}
extension Vector: CustomStringConvertible where A: CustomStringConvertible {
    public var description: String {
        return "Vector<\(String(describing: A.self))>(\(fst.description), \(snd.description))"
    }
}
extension Vector: TwoDimensions {
    public static func make2D(_ fst: A, _ snd: A) -> Vector<A> {
        return Vector(fst: fst, snd: snd)
    }
}
extension Vector: Additive where A: Additive { }
extension Vector: Magnitude where A: Additive, A: Multiplicative, A: Comparable {
    public typealias MagVal = A
}
extension Vector: Scalar where A: Multiplicative {
    public typealias ScalarVal = A
}
extension Vector: Equatable where A: Equatable {}
prefix operator √
public protocol SquareRoot {
    static prefix func √(_ square: Self) -> Self
}
extension Magnitude where Self.MagVal: SquareRoot {
    public var magnitude: Self.MagVal {
        return √(self.magSquare)
    }
}
extension Double: Multiplicative {
    public static var multId: Double { return 1.0 }
}
extension Double: Additive {
    public static var addId: Double {
        return 0.0
    }
}
extension Double: SquareRoot {
    public static prefix func √ (square: Double) -> Double {
        return sqrt(square)
    }
}
public struct Complex: Equatable {
    let real: Double
    let imm: Double
}
extension Complex: CustomStringConvertible {
    public var description: String { return "\(real) + \(imm)i" }
}
extension Complex: TwoDimensions {
    public typealias ComponentVal = Double
    public static func make2D(_ fst: Double, _ snd: Double) -> Complex {
        return Complex(real: fst, imm: snd)
    }
    public var fst: Double { return real }
    public var snd: Double { return imm }
}
extension Complex: Additive {}
extension Complex: Scalar { public typealias ScalarVal = Double }
extension Complex: Magnitude {}
extension Complex: Multiplicative {
    public static var multId: Complex {
        return Complex(real: 1.0 , imm: 0.0)
    }
    public static func * (_ lhs: Complex, _ rhs: Complex) -> Complex {
        return Complex(real: lhs.real * rhs.real - lhs.imm * rhs.imm,
                       imm:  lhs.real * rhs.imm  + lhs.imm * rhs.real)
    }
}
public struct Linear<A> {
    public let x: A
    public let c: A
}
extension Linear: CustomStringConvertible where A: CustomStringConvertible {
    public var description: String { return "f(x) = \(x)x + \(c)" }
}
extension Linear: Equatable where A: Equatable {}
extension Linear: TwoDimensions {
    public typealias ComponentVal = A
    public static func make2D(_ fst: A, _ snd: A) -> Linear {
        return Linear(x: fst, c: snd)
    }
    public var fst: A { return x }
    public var snd: A { return c }
}
extension Linear: Additive where A: Additive {}
extension Linear: Scalar where A: Multiplicative { public typealias ScalarVal = A }
infix operator **: MultiplicationPrecedence
public protocol FancyMult {
    associatedtype FancyVal
    static func ** (lhs: Self, rhs: Self) -> FancyVal
}
public struct Quadratic<A> {
    public let x²: A
    public let x: A
    public let c: A
}
extension Quadratic: Equatable where A: Equatable {}
extension Linear: FancyMult where A: Additive, A: Multiplicative {
    public typealias FancyVal = Quadratic<A>
    public static func ** (lhs: Linear<A>, rhs: Linear<A>) -> Quadratic<A> {
        return Quadratic(x²: lhs.x * rhs.x,
                         x: lhs.x * rhs.c + lhs.c * rhs.x,
                         c: lhs.c * rhs.c)
    }
}
extension Vector: FancyMult where A: Additive, A: Multiplicative {
    public typealias FancyVal = A
    public static func ** (lhs: Vector<A>, rhs: Vector<A>) -> A {
        return lhs.fst * rhs.fst + lhs.fst * rhs.fst
    }
}
infix operator ^^: PowerPrecedence
precedencegroup PowerPrecedence {
    higherThan: MultiplicationPrecedence
    associativity: right
}
public func ^^ <A: Multiplicative> (lhs: A, rhs: Int) -> A {
    var a = A.multId
    for _ in 0..<rhs {
        a = a * lhs
    }
    return a
}
