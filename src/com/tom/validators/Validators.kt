package com.tom.validators

import com.tom.validators.MustHave.*
import java.util.*
import java.util.function.Predicate
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty


infix fun <T> ((T) -> Boolean).and(other: (T) -> Boolean): (T) -> Boolean {
    return { this(it) && other(it) }
}

infix fun <T> ((T) -> Boolean).or(other: (T) -> Boolean): (T) -> Boolean {
    return { this(it) || other(it) }
}

operator fun <T> ((T) -> Boolean).not(): (T) -> Boolean {
    return { !this(it) }
}

infix fun CharClass.or(other: CharClass): CharClass {
    return CharClass.satisfying { this(it) || other(it) }
}

internal fun <T> ((T) -> Boolean).jPredicate(): Predicate<T> = Predicate { this(it) }

/**
 * Equivalent to [validate] but returns a Boolean instead of throwing an exception. Is NOT used by the class itself
 * when used as a delegate which always throws an exception in [setValue] by calling [validate] but can be used
 * by users of the class outside the delegate pattern when exceptions are undesirable
 *
 * This is a final method so that subclasses cannot introduce discrepancies between validate throwing an
 * [IllegalArgumentException] and [isValid] returning false.
 *
 * Note that [isValid] WILL STILL THROW any exception that is NOT [IllegalArgumentException]
 */
fun <R, T> Validator<R, T>.isValid(data: T): Boolean =
    try {
        validate(data).let { true }
    } catch (e: IllegalArgumentException) {
        false
    }

typealias Reason = String

/**
 * Interface describing all Validators. Classes which do not need to inherit a superclass should instead
 * subclass the abstract call [BaseValidator] which provides additional features and implementation that makes
 * creating a custom Validator easier.
 *
 * Classes which require a superclass other than this can implement [Validator] but then must
 * implement their own [getValue] and [setValue]. _The implementer __must__ ensure [setValue] calls
 * [validate]_
 *
 * See [BaseValidator] for more
 * @see BaseValidator
 */
interface Validator<R, T> : ReadWriteProperty<R, T> {
    /**
     * Overridden by subclasses to perform the validation required
     * This is where all the validation of data must occur in the
     * in subclasses.
     *
     * @see [isValid]
     * @throws IllegalArgumentException if data is not valid
     * @return Unit if data is valid
     */
    @Throws(IllegalArgumentException::class)
    fun validate(data: T)
}


/**
 * Superclass for all Validating delegates. Requires an initialValue of type T
 * and has a [getValue] that works like a simple getter and a [setValue] that delegates
 * to the abstract [validate] method
 *
 * ALL classes that extend this class are *REQUIRED* to call [validate] on
 * the initial value at the end of their constructors
 * The contract of Validator classes is that if the initial is not valid,
 * the constructor will fail. Since the [validate] method is overridden
 * by the subclasses, [BaseValidator] cannot call validate on the [initValue]
 * because subclasses have not been initialized in the constructor for
 * Validator; it is therefore the responsibility of subclasses to call
 * [validate] on their initial values at the end of the constructor
 *
 * Classes can extend this class and override [validate] to create a Validator
 * [validate] will throw on invalid data see the method for more
 *
 * Classes which require a superclass other than this can implement [Validator] but then must
 * implement their own [getValue] and [setValue]. _The implementer __must__ ensure [setValue] calls
 * [validate]_
 * @see Validator
 */
abstract class BaseValidator<R, T>(protected var initValue: T) : Validator<R, T> {

    override operator fun getValue(thisRef: R, property: KProperty<*>): T {
        return initValue
    }

    override operator fun setValue(thisRef: R, property: KProperty<*>, value: T) {
        initValue = value.also { validate(it) }
    }
}

/**
 * Object containing the validators that can be used as delegates
 * The actual object is not used, it is only used for namespacing
 */
object Validators {
    /**
     * Represents a delegate that validates Integers
     *
     * Takes an initial value that will be validated before finishing initialization
     * Construction of the delegate will fail if the initial value is invalid
     *
     * The class can optionally take a min value and a max value both of
     * which are inclusive
     *
     * It can also take a list of [DescribedPredicate]s which can be used as additional
     * tests on what is to be set
     *
     * See [BaseValidator] and [BaseValidator.validate] for more about how validation is handled
     */
    class Integer<R>(
        initValue: Int,
        val minimum: Int = Int.MIN_VALUE,
        val maximum: Int = Int.MAX_VALUE,
        val predicates: List<DescribedPredicate<Int>> = listOf()
    ) : BaseValidator<R, Int>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: Int) {
            if (data < minimum || data > maximum)
                throw IllegalArgumentException("Value $data is out of range. Must be $minimum <= value <= $maximum")

            for (predicate in predicates)
                if (!predicate(data))
                    throw IllegalArgumentException("$data is invalid. Predicate failed says: ${predicate.description}")
        }

    }

    /**
     * Represents a range constraint. Built on Constraint<T> this validator accepts any value in a
     * range of any comparable type
     */
    class ValueInRange<R, T>(initValue: T, val constraint: EndPoints<T>) :
        BaseValidator<R, T>(initValue) where T : Comparable<T> {
        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            if (data !in constraint) {
                throw IllegalArgumentException("Value $data failed to satisfy constraint $constraint")
            }
        }

        operator fun contains(other: T): Boolean = other in constraint
    }

    /**
     * Creates a new validator for a particular range
     */
    fun <R, T> inRange(initValue: T, range: Constraint<T>): ValueInRange<R, T> where T : Comparable<T> =
        ValueInRange(initValue, range)

    /**
     * Creates a new validator for a range using the helper functions [atLeast], [atMost], [between], and [exactly]
     */
    fun <R, T> inRange(initValue: T, range: kotlin.ranges.ClosedRange<T>): ValueInRange<R, T> where T : Comparable<T> =
        ValueInRange(initValue, Constraint(range.start, range.endInclusive))


    /**
     * Represents a delegate that validates [kotlin.String]
     *
     * Takes an initial value that will be validated before finishing initialization
     * Construction of the delegate will fail if the initial value is invalid
     *
     * The class can optionally take a min value and a max value both of
     * which are inclusive for the length of the string. These are of the type
     * [Constraint] and can be created using the [atLeast], [exactly],
     * and [atMost] helper functions. It should use the n: Int overloads
     * of these function which return [Constraint] objects
     * By default it uses [Constraint.unbounded] meaning there is no
     * constraint on length.
     *
     * It can also take a [StringRules.MustHave] instance which can also be created
     * using [atLeast], [exactly], and [atMost] helper functions. In this case
     * they do not return [Constraint] since they have a different overload.
     * This would use the vararg pair: [Pair]<[CharClass], [Int]> overload returning
     * [StringRules.StringContentsRule] which are used to create a [StringRules.MustHave] using the constructor.
     *
     * The [atLeast], [exactly], and [atMost] helper functions also work well with
     * the extension properties on Int returning Pair<CharClass, Int>. See [Int.whitespace]
     * as an example
     *
     * See [BaseValidator] and [BaseValidator.validate] for more about how validation is handled
     */
    class String<R>(
        initValue: kotlin.String,
        val acceptableLength: AcceptableLength = AcceptableLength.unbound(),
        val mustHave: MustHave
    ) : BaseValidator<R, kotlin.String>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: kotlin.String) {
            require(acceptableLength.isValid(data.length)) { "Length of \"$data\" (${data.length}) is invalid: Length must be in one of the ranges: $acceptableLength" }
            val (hasAllValid, reason) = mustHave.isValid(data)
            require(hasAllValid) { reason ?: "" }
        }

        companion object {
            @JvmStatic
            fun <R> alphanumeric(initValue: kotlin.String): String<R> = String<R>(
                initValue,
                AcceptableLength.unbound(),
                MustHave { Only { CharClass.alphanumeric } }
            )

            @JvmStatic
            fun <R> alphanumericAndWhitespace(initValue: kotlin.String, locale: Locale): String<R> = String(
                initValue,
                AcceptableLength.unbound(),
                MustHave { Only { CharClass.alphanumeric or CharClass.whitespace } }
            )

            @JvmStatic
            fun <R> noWhitespace(initValue: kotlin.String): String<R> = String(
                initValue,
                AcceptableLength.unbound(),
                MustHave { No { CharClass.whitespace } }
            )
        }
    }

    /**
     * This validator class represents a simple predicate with a description
     *
     * The initialValue will before validated before finishing initialization
     *
     * It will test the initialValue and all subsequence set values using the
     * predicate and throw [IllegalArgumentException] with a generic message
     * or with [DescribedPredicate.description] if a [DescribedPredicate]
     * is given.
     */
    class Requirements<R, T>(initValue: T, val predicate: (T) -> Boolean) : BaseValidator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        constructor(initValue: T, describedPredicate: DescribedPredicate<T>) : this(
            initValue, describedPredicate as (T) -> Boolean
        )

        infix fun and(predicate: (T) -> Boolean): Requirements<R, T> {
            return Requirements(super.initValue, this.predicate and predicate)
        }

        infix fun or(predicate: (T) -> Boolean): Requirements<R, T> {
            return Requirements(super.initValue, this.predicate or predicate)
        }

        override fun validate(data: T) {
            if (!predicate(data)) {
                var message = "$data did not satisfy the predicate"
                if (predicate is DescribedPredicate<*>) {
                    message += ": ${predicate.description}"
                }
                throw IllegalArgumentException(message)
            }
        }
    }


    /**
     * Returns a [Validator] object using the [initValue] which requires the values to satisfy the predicate
     */
    fun <R, T> ((T) -> Boolean).require(initValue: T): Requirements<R, T> =
        Requirements(initValue, this)


    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * all of which must be satisfied by the values passed through the delegate
     */
    class AllSatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : BaseValidator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            for (predicate in predicates) {
                if (!predicate(data)) {
                    var message = "Value $data does not satisfy predicate"
                    if (predicate is DescribedPredicate) {
                        message += ": ${predicate.description}"
                    }
                    throw IllegalArgumentException(message)
                }
            }
        }

        infix fun and(other: AllSatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.all { it(t) } && other.predicates.all { it(t) } }

        infix fun or(other: AllSatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.all { it(t) } || other.predicates.all { it(t) } }

        infix fun and(other: AnySatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.all { it(t) } && other.predicates.any { it(t) } }

        infix fun or(other: AnySatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.all { it(t) } || other.predicates.any { it(t) } }
    }

    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * any of which must be satisfied by the values passed through the delegate
     */
    class AnySatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : BaseValidator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            for (predicate in predicates) if (predicate(data)) return
            throw IllegalArgumentException("Value $data does not satisfy any of its required predicates")
        }

        infix fun and(other: AllSatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.any { it(t) } && other.predicates.all { it(t) } }

        infix fun or(other: AllSatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.any { it(t) } || other.predicates.all { it(t) } }

        infix fun and(other: AnySatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.any { it(t) } && other.predicates.any { it(t) } }

        infix fun or(other: AnySatisfy<R, T>) =
            Requirements<R, T>(initValue) { t -> predicates.any { it(t) } || other.predicates.any { it(t) } }

        fun negate() = NoneSatisfy<R, T>(initValue, *predicates)
        operator fun not() = negate()
    }

    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * none of which must be satisfied by the values passed through the delegate
     */
    class NoneSatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : BaseValidator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            for (predicate in predicates) {
                if (predicate(data)) {
                    var message = "Value $data satisfied a disallowed predicate"
                    if (predicate is DescribedPredicate) {
                        message += ": ${predicate.description}"
                    }
                    throw IllegalArgumentException(message)
                }
            }
        }

        fun negate() = AnySatisfy<R, T>(initValue, *predicates)
        operator fun not() = negate()
    }


    /**
     * Represents a delegate that validates any type based on its presence in a collection
     *
     * This class is constructed in one of two ways. The first is with an initial value
     * and a collection of values which are valid. Any sets to the delegated property
     * will result in checking the about-to-set value against that list of values. The
     * second way is by only passing the list of valid values. In this case the initial
     * value will just be taken to be the initial value of the list
     *
     * In either case it will only allow values passed to setValue that ARE present
     * in the given collection provided on construction
     *
     * See [BaseValidator] and [BaseValidator.validate] for more about how validation is handled
     */
    class AnyOf<R, T> internal constructor(initValue: T, val choices: Set<T>) : BaseValidator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        operator fun contains(value: T): Boolean = value in choices

        /**
         * Creates an AnyOf with the choices given with an initial value of the first element
         */
        constructor(vararg choices: T) : this(choices[0], choices.toSet())

        /**
         * Creates an AnyOf with the choices given with an initial value of the first element
         */
        constructor(choices: List<T>) : this(choices[0], choices.toSet())
        constructor(initialValue: T, choices: List<T>) : this(initialValue, choices.toSet())

        override fun validate(data: T) {
            if (data !in choices) {
                throw IllegalArgumentException("$data is not in the list of valid choices")
            }
        }

        /**
         * Returns a validator that is the negation of AnyOf. Marking data
         * that appears in the given list of values as <b>invalid</b> and
         * all other values as valid
         */
        fun negate(): NoneOf<R, T> = NoneOf(initValue, choices)

        operator fun not() = negate()
    }


    /**
     * Represents a delegate that validates any type based on its absence in a collection
     *
     * This class is constructed in one of two ways. The first is with an initial value
     * and a collection of values which are valid. Any sets to the delegated property
     * will result in checking the about-to-set value against that list of values. The
     * second way is by only passing the list of valid values. In this case the initial
     * value will just be taken to be the initial value of the list
     *
     * In either case it will only allow values passed to setValue that are NOT present
     * in the given collection provided on construction
     *
     * See [BaseValidator] and [BaseValidator.validate] for more about how validation is handled
     */
    class NoneOf<R, T> internal constructor(initialValue: T, val choices: Set<T>) : BaseValidator<R, T>(initialValue) {

        init {
            validate(initialValue)
        }

        operator fun contains(value: T): Boolean = value in choices

        /**
         * Creates an NoneOf with the choices given with an initial value of the first element
         */
        constructor(vararg choices: T) : this(choices[0], choices.toSet())

        /**
         * Creates an NoneOf with the choices given with an initial value of the first element
         */
        constructor(choices: List<T>) : this(choices[0], choices.toSet())
        constructor(initValue: T, choices: List<T>) : this(initValue, choices.toSet())

        override fun validate(data: T) {
            if (data in choices) {
                throw IllegalArgumentException("$data is not in the list of valid choices")
            }
        }

        /**
         * Returns a validator that is the negation of NoneOf. Marking data
         * that appears in the given list of values as <b>valid</b> and all
         * other values as invalid
         */
        fun negate(): AnyOf<R, T> = AnyOf(initValue, choices)
        operator fun not() = negate()
    }
}

