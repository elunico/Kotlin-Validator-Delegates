package com.tom.validators


interface EndPoints<T> where T : Comparable<T>, T : Any {
    val lowerBound: T?
    val upperBound: T?

    operator fun contains(value: T): Boolean
}

/**
 * A closed range includes its endpoints. It may or may not have upper or lower limits
 */
interface ClosedRange<T> : EndPoints<T> where T : Comparable<T>, T : Any {
    operator fun contains(other: ClosedRange<T>): Boolean {
        return this.covers(other)
    }

    infix fun intersect(other: ClosedRange<T>): ClosedRange<T>
    infix fun union(other: ClosedRange<T>): ClosedRange<T>

    infix fun overlaps(other: ClosedRange<T>): Boolean
    infix fun covers(other: ClosedRange<T>): Boolean

    val isEmpty: Boolean
    val isFullyBounded: Boolean
}

/**
 * Like a [ClosedRange] but cannot have unbounded ends. A finite range including its endpoints
 */
interface BoundedClosedRange<T> : ClosedRange<T> where T : Any, T : Comparable<T> {
    override val lowerBound: T
    override val upperBound: T

    override operator fun contains(value: T): Boolean {
        return lowerBound <= value && upperBound >= value
    }

    override val isEmpty: Boolean
        get() = lowerBound >= upperBound

    override val isFullyBounded get() = true
}

/**
 * A Constraint is a small class that represents a restriction on a
 * range of some <T> where T: Comparable<T>.
 *
 * This class is used by [String.acceptableLength] and can be used by
 * anyone subclassing [BaseValidator] and works well with the utility
 * functions [atLeast], [atMost], [between], and [exactly] which are useful for
 * natural language construction of Constraint instances
 *
 * It can be used in a variety of contexts
 *
 * Note that like the built-in Kotlin [kotlin.ranges.ClosedRange] this class INCLUDES
 * both endpoints, WHEN the endpoints are both defined. However, unlike the Kotlin class
 * this class can have unbounded forms on either the left (-∞, n] or the right [n, ∞) in which
 * case the respective, infinite endpoint does not exist in the range
 */
class Constraint<T> private constructor(start: T?, endInclusive: T?) : ClosedRange<T> where T : Comparable<T>, T : Any {

    override val lowerBound: T? = start
    override val upperBound: T? = endInclusive

    fun valid(value: T) = contains(value)

    /**
     * Two ranges overlap iff. their intersection is not empty
     * @see [intersect]
     * @see [isEmpty]
     */
    override infix fun overlaps(other: ClosedRange<T>): Boolean {
        return !this.intersect(other).isEmpty
    }

    /**
     * the union of two ranges is the bound by the min of the lowerBounds and the max of the upperBounds
     * if any of the bounds are null, the new bound is also null
     */
    override infix fun union(other: ClosedRange<T>): ClosedRange<T> {
        fun <T> min(a: T?, b: T?): T? where T : Comparable<T>, T : Any {
            if (a == null || b == null) return null
            return if (a < b) a else b
        }

        fun <T> max(a: T?, b: T?): T? where T : Comparable<T>, T : Any {
            if (a == null || b == null) return null
            return if (a > b) a else b
        }

        return Constraint(min(lowerBound, other.lowerBound), max(upperBound, other.upperBound))
    }

    /**
     * the intersection of two ranges is the bound by the max of the lowerBounds and the min of the upperBounds
     * if both of the bounds are null, the new bound is also null
     */
    override infix fun intersect(other: ClosedRange<T>): ClosedRange<T> {
        fun <T> min(a: T?, b: T?): T? where T : Comparable<T>, T : Any {
            if (a == null && b == null) return null
            if (a == null) return b
            if (b == null) return a
            return if (a < b) a else b
        }

        fun <T> max(a: T?, b: T?): T? where T : Comparable<T>, T : Any {
            if (a == null && b == null) return null
            if (a == null) return b
            if (b == null) return a
            return if (a > b) a else b
        }
        return Constraint(max(lowerBound, other.lowerBound), min(upperBound, other.upperBound))
    }

    /**
     * A range covers another range if every value which other range contains is also found in this range
     */
    override infix fun covers(other: ClosedRange<T>): Boolean {
        val olb = other.lowerBound
        val oub = other.upperBound

        val isThisLowerBelow =
            (lowerBound == null && olb == null) || (lowerBound != null && olb != null && lowerBound < olb)
        val isThisUpperAbove =
            (upperBound == null && oub == null) || (upperBound != null && oub != null && upperBound > oub)

        return isThisLowerBelow && isThisUpperAbove
    }

    /**
     * A value is contained by a range if it compares between the bounds
     */
    override operator fun contains(value: T): Boolean {
        return if (lowerBound == null && upperBound == null) {
            true
        } else if (lowerBound == null && upperBound != null) {
            value <= upperBound
        } else if (lowerBound != null && upperBound == null) {
            value >= lowerBound
        } else if (lowerBound != null && upperBound != null) {
            value in lowerBound..upperBound
        } else {
            error("UNREACHABLE")
        }
    }

    /**
     * a range is empty if the lowerBound is strictly greater than the upperBound
     * Note that a range where lowerBound == upperBound  is not empty
     */
    override val isEmpty: Boolean
        get() = if (lowerBound == null || upperBound == null) false else {
            lowerBound > upperBound
        }

    /**
     * A range is fully bounded if both bounds are not null. If there is an upper and lower limit on the values contained by the range
     */
    override val isFullyBounded: Boolean
        get() = lowerBound != null && upperBound != null

    override fun toString(): String {
        return "$lowerBound..$upperBound"
    }

    companion object {
        /**
         * Public factory method for creating new Constraints
         */
        @JvmStatic
        @JvmName("create")
        operator fun <T> invoke(start: T, endInclusive: T): Constraint<T> where T : Comparable<T>, T : Any {
            return Constraint(start, endInclusive)
        }

        /**
         * Represents a Constraint<T> that has no bounds
         * The [valid] method will always return true
         */
        @JvmStatic
        fun <T> unbounded(): Constraint<T> where T : Any, T : Comparable<T> = Constraint(null, null)

        /**
         * Represents a Constraint that has no upperBound. It contains every value which compares greater than to or equal to
         * [start]
         */
        @JvmStatic
        fun <T> rightUnbounded(start: T): Constraint<T> where T : Any, T : Comparable<T> = Constraint(start, null)

        /**
         * Represents a Constraint that has no lowerBound. It contains every value which compares less than to or equal to
         * [endInclusive]
         */
        @JvmStatic
        fun <T> leftUnbounded(endInclusive: T): Constraint<T> where T : Any, T : Comparable<T> =
            Constraint(null, endInclusive)
    }
}

/**
 * atLeast is a utility function for creating particular classes that specify
 * various requirements of existing subclasses of [BaseValidator]. The sort of
 * things it returns depends on its overloads. The single Int overload
 * (this one) returns a [Constraint]<Int> that specifies a minimum value of [n]
 * and a maximum value of [Int.MAX_VALUE].
 *
 * This is used primarily when constructing string length limits
 * but it can be used for any class that requires a [Constraint] instance
 * including user-defined subclasses if they operate with Constraints
 */
fun <T> atLeast(n: T) where T : Number, T : Comparable<T> = Constraint.rightUnbounded(n)


/**
 * atMost is a utility function for creating particular classes that specify
 * various requirements of existing subclasses of [BaseValidator]. The sort of
 * things it returns depends on its overloads. The single Int overload
 * (this one) returns a [Constraint]<Int> that specifies a maximum value of [n]
 * and a minimum value of [Int.MIN_VALUE].
 *
 * This is used primarily when constructing string length limits
 * but it can be used for any class that requires a [Constraint] instance
 * including user-defined subclasses if they operate with Constraints
 */
fun <T> atMost(n: T) where T : Number, T : Comparable<T> = Constraint.leftUnbounded(n)

/**
 * between is a utility function for creating particular classes that specify
 * various requirements of existing subclasses of [BaseValidator]. The sort of
 * things it returns depends on its overloads.
 *
 * This is used primarily when constructing string length limits
 * but it can be used for any class that requires a [Constraint] instance
 * including user-defined subclasses if they operate with Constraints
 */
fun <T> between(low: T, high: T) where T : Comparable<T> = Constraint(low, high)

/**
 * Creates a [Constraint] with the same upper and lower bound; a single value rather than a range
 */
fun <T> exactly(value: T) where T : Comparable<T> = Constraint(value, value)

