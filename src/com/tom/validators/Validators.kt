package com.tom.validators

import com.tom.validators.Validators.StringRules.CharClass
import com.tom.validators.Validators.StringRules.MustHave.*
import com.tom.validators.Validators.Validator
import com.tom.validators.Validators.exactly
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

infix fun <T> Validators.DescribedPredicate<T>.and(other: Validators.DescribedPredicate<T>): Validators.DescribedPredicate<T> {
    return Validators.DescribedPredicate(this.description + " && " + other.description) { this(it) && other(it) }
}

infix fun <T> Validators.DescribedPredicate<T>.or(other: Validators.DescribedPredicate<T>): Validators.DescribedPredicate<T> {
    return Validators.DescribedPredicate(this.description + " || " + other.description) { this(it) || other(it) }
}

infix fun CharClass.or(other: CharClass): CharClass {
    return CharClass.satisfying { this(it) || other(it) }
}

operator fun <T> ((T) -> Boolean).not(): (T) -> Boolean {
    return { !this(it) }
}

private fun <T> ((T) -> Boolean).jPredicate(): Predicate<T> = Predicate { this(it) }

private fun <T> ((T) -> Boolean).withDescription(description: String): Validators.DescribedPredicate<T> {
    return Validators.DescribedPredicate(description, this)
}

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

object Utilities

/**
 * Object containing the validators that can be used as delegates
 * The actual object is not used, it is only used for namespacing
 */
object Validators {

    /**
     * A class that represents a Predicate (represented as (T) -> Boolean in Kotlin) with a description
     * When a predicate fails in Kotlin, there is no way to describe what it expresses normally.
     * To fix this, we use this class that simply pairs a String description with a
     * (T) -> Boolean
     *
     * Notice that this class implements (T) -> Boolean so it can be passed anywhere
     * a (T) -> Boolean is also expected. Implementations can therefore choose to
     * handle the is DescribedPredicate<*> case as [Requirements] does or simply treat it
     * as any other (T) -> Boolean
     */
    open class DescribedPredicate<T>(
        val description: kotlin.String, val predicate: (T) -> Boolean
    ) : (T) -> Boolean by predicate, Predicate<T> by predicate.jPredicate()


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

    /**
     * Ties a string and a (T) -> Boolean together in a [DescribedPredicate]
     * Used as sugar for more natural-language-like construction of Validators
     */
    fun <T> ensure(first: kotlin.String, second: (T) -> Boolean): DescribedPredicate<T> {
        return DescribedPredicate(first, second)
    }

    /**
     * Ties many strings and a ((T) -> Boolean)s together in a list of [DescribedPredicate]
     * Used as sugar for more natural-language-like construction of Validators
     */
    fun <T> ensure(vararg pairs: Pair<kotlin.String, (T) -> Boolean>): List<DescribedPredicate<T>> {
        return pairs.map { (first, second) -> DescribedPredicate(first, second) }
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
    fun <R, T> ((T) -> Boolean).require(initValue: T): Requirements<R, T> = Requirements(initValue, this)

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
     * Note that minimum and maximum are both inclusive in this class. This also means
     * that if minimum == maximum then the Constraint repesents that single value only
     */
    open class Constraint<T> : ClosedRange<T> where T : Comparable<T>, T : Any {
        // only null in object of unbound()
        // not checked in that subclass
        private var _minimum: T? = null
        private var _maximum: T? = null

        // only non-private constructor requires initializing both fields
        val minimum: T get() = _minimum!!
        val maximum: T get() = _maximum!!

        constructor(minimum: T, maximum: T) {
            this._minimum = minimum
            this._maximum = maximum
        }

        private constructor()

        open fun valid(value: T) = contains(value)

        override fun toString(): kotlin.String {
            return "$start..$endInclusive"
        }

        companion object {
            /**
             * Represents a Constraint<T> that has no bounds
             * The [valid] method will always return true
             */
            @JvmStatic
            fun <T : Comparable<T>> unbound(): Constraint<T> = object : Constraint<T>() {
                override fun valid(value: T) = true
            }
        }

        override val endInclusive: T
            get() = maximum
        override val start: T
            get() = minimum
    }

    /**
     * Represents a range constraint. Built on Constraint<T> this validator accepts any value in a
     * range of any comparable type
     */
    class ValueInRange<R, T>(initValue: T, val constraint: ClosedRange<T>) :
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
    fun <R, T> inRange(initValue: T, range: ClosedRange<T>): ValueInRange<R, T> where T : Comparable<T> =
        ValueInRange(initValue, Constraint(range.start, range.endInclusive))

    /**
     * Creates a new validator for a range using the helper functions [atLeast], [atMost], [between], and [exactly]
     */
    fun <R, T> inRange(initValue: T, range: Constraint<T>): ValueInRange<R, T> where T : Comparable<T> =
        ValueInRange(initValue, range)

    /**
     * Convenience method for creating [AcceptableLength] objects for [String] using integers
     */
    val Int.charactersLong: AcceptableLength
        get() = AcceptableLength(this..this)

    /**
     * Convenience method for creating [AcceptableLength] objects for [String] using closed ranges
     */
    val ClosedRange<Int>.charactersLong: AcceptableLength
        get() = AcceptableLength(this)

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
    fun atLeast(n: Int) = Constraint(n, Int.MAX_VALUE)

    /**
     * Additional overloads for [atLeast]. See atLeast(Int) for more
     */
    fun atLeast(n: Double) = Constraint(n, Double.MAX_VALUE)

    /**
     * Additional overloads for [atLeast]. See atLeast(Int) for more
     */
    fun atLeast(n: Float) = Constraint(n, Float.MAX_VALUE)

    /**
     * Additional overloads for [atLeast]. See atLeast(Int) for more
     */
    fun atLeast(n: Long) = Constraint(n, Long.MAX_VALUE)

    /**
     * Additional overloads for [atLeast]. See atLeast(Int) for more
     */
    fun atLeast(n: Short) = Constraint(n, Short.MAX_VALUE)

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
    fun atMost(n: Int) = Constraint(Int.MIN_VALUE, n)

    /**
     * Additional overloads for [atMost]. See atMost(Int) for more
     */
    fun atMost(n: Double) = Constraint(Double.MIN_VALUE, n)

    /**
     * Additional overloads for [atMost]. See atMost(Int) for more
     */
    fun atMost(n: Float) = Constraint(Float.MIN_VALUE, n)

    /**
     * Additional overloads for [atMost]. See atMost(Int) for more
     */
    fun atMost(n: Long) = Constraint(Long.MIN_VALUE, n)

    /**
     * Additional overloads for [atMost]. See atMost(Int) for more
     */
    fun atMost(n: Short) = Constraint(Short.MIN_VALUE, n)

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

    /**
     *
     */
    object StringRules {

        /**
         * Creates a new Constraint with [this]'s [Constraint.maximum] and [n] as a minimum
         * overwriting [Constraint.minimum]
         */
        @Deprecated(
            "May lead to confusing expressions, use between() instead",
            replaceWith = ReplaceWith("between(n, this.maximum)")
        )
        fun Constraint<Int>.atLeast(n: Int): Constraint<Int> = Constraint(n, this.maximum)

        /**
         * Creates a new Constraint with [this]'s [Constraint.minimum] and [n] as a maximum
         * overwriting [Constraint.maximum]
         */
        @Deprecated(
            "May lead to confusing expressions, use between() instead",
            replaceWith = ReplaceWith("between(this.minimum, n)")
        )
        fun Constraint<Int>.atMost(n: Int) = Constraint(this.minimum, n)


        /**
         * Enumeration representing commonly used classes of Chars for use in [MustHave] objects in
         * [String] validators
         */
        open class CharClass internal constructor(val isMember: (Char) -> Boolean) : (Char) -> Boolean by isMember,
            Predicate<Char> by isMember.jPredicate() {

            var description: kotlin.String? = null

            fun described(description: kotlin.String) = this.apply { this.description = description }

            companion object {
                @JvmStatic
                val whitespace = CharClass(Char::isWhitespace).described("whitespace")

                @JvmStatic
                val newline = CharClass { it == '\n' || it == '\r' }.described("new lines")

                @JvmStatic
                val alphabetic = CharClass(Char::isLetter).described("letter")

                @JvmStatic
                val lowercaseLetters = CharClass(Char::isLowerCase).described("lowercase letter")

                @JvmStatic
                val uppercaseLetters = CharClass(Char::isUpperCase).described("uppercae letter")

                @JvmStatic
                val numbers = CharClass(Char::isDigit).described("digit")

                @JvmStatic
                val alphanumeric = CharClass(Char::isLetterOrDigit).described("letter or digit")

                @JvmStatic
                val specialCharacters =
                    CharClass(!Char::isLetterOrDigit and !Char::isWhitespace).described("special character")

                /**
                 * Returns a new CharClass instance that will satisfy the condition passed to the function
                 * Used for natural-language predicate construction such as when saying
                 * `MustHave(no(charsSatisfying { it in 'A'..'M' } ))`
                 */
                @JvmStatic
                fun satisfying(condition: (Char) -> Boolean): CharClass = CharClass(condition)
            }

            override fun toString(): kotlin.String {
                return "CharClass[${this.description}]"
            }
        }

        /**
         * MustHave is a class that describes which characters and in what quantity are
         * required of a [String].
         *
         * This class uses the [AtLeast], [AtMost], [Exactly], and [None] classes to help build specific requirements
         *
         * This class is usually constructed by using the extension properties for CharClass
         * on Int and the 4 helper functions which are then passed to the constructor
         */
        open class MustHave(builder: MustHave.() -> Unit) {
            protected val rules: MutableList<StringContentsRule> = mutableListOf()

            init {
                this.builder()
            }


            open fun isValid(data: kotlin.String): Pair<Boolean, Reason?> {
                for (rule in rules) {
                    val (valid, reason) = rule.test(data)
                    if (!valid) {
                        return valid to reason
                    }
                }
                return true to null
            }

            companion object {
                @JvmStatic
                fun noRequirements() = object : MustHave({}) {
                    override fun isValid(data: kotlin.String): Pair<Boolean, Reason?> {
                        return true to null
                    }
                }
            }

            /**
             * Used as a super class for all the [AtLeast], [AtMost], [Exactly], [Only] DSL builders so that they
             * all have access to the extension function on [Int]
             */
            abstract inner class QuantitySpec {
                abstract fun comparator(): (Int, Int) -> Boolean


                /**
                 * Returns a new CharClass instance that will satisfy the condition passed to the function
                 * Used for natural-language predicate construction such as when saying
                 * `MustHave(no(charsSatisfying { it in 'A'..'M' } ))`
                 */
                fun satisfying(condition: (Char) -> Boolean) {
                    this@MustHave.rules.add(StringContentsRule(comparator(), CharClass(condition) to -1))
                }

                /**
                 * This extension function associates numbers and user defined [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                fun Int.charsSatisfying(predicate: (Char) -> Boolean): Pair<CharClass, Int> =
                    (CharClass(predicate) to this)

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.whitespace: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.whitespace to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.newline: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.newline to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.alphabetic: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.alphabetic to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.lowercaseLetters: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.lowercaseLetters to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.uppercaseLetters: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.uppercaseLetters to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.digits: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.numbers to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.alphanumeric: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.alphanumeric to this)))
                    }

                /**
                 * These extension properties associate numbers and [CharClass]es
                 * to facilitate the creation of [MustHave] objects for use in
                 * describing [String] validators
                 */
                val Int.specialCharacters: Unit
                    get() {
                        this@MustHave.rules.add(StringContentsRule(comparator(), (CharClass.specialCharacters to this)))
                    }
            }

            /**
             * AtLeast is a DSL builder class that specifies
             * various requirements of existing subclasses of [BaseValidator].
             *
             * This is used primarily when constructing string [MustHave] rules where it serves as a part of the
             * DSL to construct String rules
             */
            inner class AtLeast(builder: AtLeast.() -> Unit) : QuantitySpec() {
                init {
                    this.builder()
                }

                override fun comparator(): (Int, Int) -> Boolean {
                    return { act, exp -> act >= exp }
                }
            }

            /**
             * AtMost is a DSL builder class that specifies
             * various requirements of existing subclasses of [BaseValidator].
             *
             * This is used primarily when constructing string [MustHave] rules where it serves as a part of the
             * DSL to construct String rules
             */
            inner class AtMost(builder: AtMost.() -> Unit) : QuantitySpec() {
                init {
                    this.builder()
                }

                override fun comparator(): (Int, Int) -> Boolean {
                    return { act, exp -> act <= exp }
                }
            }

            /**
             * Exactly is a DSL builder class that specifies
             * various requirements of existing subclasses of [BaseValidator].
             *
             * This is used primarily when constructing string [MustHave] rules where it serves as a part of the
             * DSL to construct String rules
             */
            inner class Exactly(builder: Exactly.() -> Unit) : QuantitySpec() {
                init {
                    this.builder()
                }

                override fun comparator(): (Int, Int) -> Boolean {
                    return { act, exp -> act == exp }
                }
            }

            /**
             * Used as a super class for [No] and [Exactly] so that they can access [CharClass]es with the side-effect
             * of adding them to the class itself
             */
            abstract inner class NoQuantity : QuantitySpec() {
                /**
                 * These overloads are needed here so that [No] and [Exactly] can use [CharClass]es without numbers
                 * but still have them added to the [MustHave] specification
                 *
                 * Redundant so that other uses cna still access [CharClass] constants from the class itself
                 */
                val whitespace: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isWhitespace).described("whitespace") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val newline: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass { it == '\n' || it == '\r' }.described("new lines") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val alphabetic: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isLetter).described("letter") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val lowercaseLetters: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isLowerCase).described("lowercase letter") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val uppercaseLetters: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isUpperCase).described("uppercae letter") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val numbers: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isDigit).described("digit") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val alphanumeric: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(Char::isLetterOrDigit).described("letter or digit") to -1
                            )
                        )
                    }

                /**
                 * See [whitespace]
                 */
                val specialCharacters: Unit
                    get() {
                        this@MustHave.rules.add(
                            StringContentsRule(
                                comparator(),
                                CharClass(!Char::isLetterOrDigit and !Char::isWhitespace).described("special character") to -1
                            )
                        )
                    }

            }

            /**
             * No is a DSL builder class that specifies
             * various requirements of existing subclasses of [BaseValidator].
             *
             * This is used primarily when constructing string [MustHave] rules where it serves as a part of the
             * DSL to construct String rules
             */
            inner class No(builder: No.() -> Unit) : NoQuantity() {
                init {
                    this.builder()
                }

                override fun comparator(): (Int, Int) -> Boolean {
                    return { act, exp -> act == 0 }
                }


            }


            /**
             * Only is a DSL builder class that specifies
             * various requirements of existing subclasses of [BaseValidator].
             *
             * This is used primarily when constructing string [MustHave] rules where it serves as a part of the
             * DSL to construct String rules
             */
            inner class Only(builder: Only.() -> Unit) : NoQuantity() {
                init {
                    this.builder()
                }

                infix fun CharClass.or(other: CharClass) {
                    this@MustHave.rules.add(
                        StringContentsRule(
                            comparator(),
                            CharClass.satisfying { this(it) || other(it) } to -1)
                    )
                }

                override fun comparator(): (Int, Int) -> Boolean {
                    return { act, exp -> act == -1 }
                }
            }
        }


        /**
         * Used by [MustHave] for [String] Validators. TODO: Fix this
         */
        class StringContentsRule(
            val isAcceptable: (actualCount: Int, expectedCount: Int) -> Boolean,
            vararg pairs: Pair<CharClass, Int>
        ) : Map<CharClass, Int> by mapOf(*pairs) {

            fun test(s: kotlin.String): Pair<Boolean, Reason?> {
                for ((type, expectedCount) in entries) {
                    if (!isAcceptable(s.count(type.isMember), expectedCount)) {
                        return false to "String did not satisfy char expected count of $expectedCount of type $type"
                    }
                }
                return true to null

            }

            override fun toString(): kotlin.String {
                return "[${entries.joinToString(", ") { (key, value) -> "$key: $value" }}]"
            }

        }


    }

    /**
     * Aggregate disjunctive collection of [Constraint] that are used on the [Validators.String] class
     * to specify the required length of the string
     */
    open class AcceptableLength(private vararg val constraints: ClosedRange<Int>) {
        companion object {
            @JvmStatic
            fun unbound(): AcceptableLength = object : AcceptableLength() {
                override fun isValid(data: Int) = true
            }
        }

        override fun toString(): kotlin.String {
            var s = StringBuilder()
            for (constraint in constraints) {
                s.append(constraint)
                s.append(" OR ")
            }
            s.delete(s.length - 4, s.length)
            return s.toString()
        }

        open fun isValid(data: Int) = constraints.any { data in it }
    }

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
     * By default it uses [Constraint.unbound] meaning there is no
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
        val mustHave: StringRules.MustHave
    ) : BaseValidator<R, kotlin.String>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: kotlin.String) {
            if (!acceptableLength.isValid(data.length)) {
                throw IllegalArgumentException("Length of \"$data\" (${data.length}) is invalid: Length must be in one of the ranges: $acceptableLength")
            }
            val (hasAllValid, reason) = mustHave.isValid(data)
            if (!hasAllValid) {
                throw IllegalArgumentException(reason)
            }
        }

        companion object {
            @JvmStatic
            fun <R> alphanumeric(initValue: kotlin.String): String<R> = String<R>(
                initValue, AcceptableLength.unbound(), StringRules.MustHave { Only { CharClass.alphanumeric } }
            )

            @JvmStatic

            fun <R> alphanumericAndWhitespace(initValue: kotlin.String, locale: Locale): String<R> = String(
                initValue,
                AcceptableLength.unbound(),
                StringRules.MustHave {
                    Only { CharClass.alphanumeric or CharClass.whitespace }
                }
            )

            @JvmStatic
            fun <R> noWhitespace(initValue: kotlin.String): String<R> = String(
                initValue, AcceptableLength.unbound(), StringRules.MustHave { No { CharClass.whitespace } }
            )
        }
    }

    /**
     * Makes the language more natural than `to` but is not necessary
     */
    infix fun <T> kotlin.String.so(other: (T) -> Boolean): Pair<kotlin.String, (T) -> Boolean> = this to other
}
