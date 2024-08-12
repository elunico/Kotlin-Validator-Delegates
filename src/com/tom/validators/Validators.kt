package com.tom.validators

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

operator fun <T> ((T) -> Boolean).not(): (T) -> Boolean {
    return { !this(it) }
}

private fun <T> ((T) -> Boolean).jPredicate(): Predicate<T> = Predicate { this(it) }

typealias Reason = String

/**
 * Object containing the classes and helper functions that work as validators
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
    data class DescribedPredicate<T>(
        val description: kotlin.String, val predicate: (T) -> Boolean
    ) : (T) -> Boolean by predicate, Predicate<T> by predicate.jPredicate()


    /**
     * Superclass for all Validating delegates. Requires an initialValue of type T
     * and has a [getValue] that works like a simple getter and a [setValue] that delegates
     * to the abstract [validate] method
     *
     * ALL classes that extend this class are *REQUIRED* to call [validate] on
     * the initial value at the end of their constructors
     * The contract of Validator classes is that if the initial is not valid,
     * the constructor will fail. Since the [validate] method is overridden
     * by the subclasses, [Validator] cannot call validate on the [initValue]
     * because subclasses have not been initialized in the constructor for
     * Validator; it is therefore the responsibility of subclasses to call
     * [validate] on their initial values at the end of the constructor
     *
     * Classes can extend this class and override [validate] to create a Validator
     * [validate] will throw on invalid data see the method for more
     */
    abstract class Validator<R, T>(protected var initValue: T) : ReadWriteProperty<R, T> {

        override operator fun getValue(thisRef: R, property: KProperty<*>): T {
            return initValue
        }

        override operator fun setValue(thisRef: R, property: KProperty<*>, value: T) {
            initValue = value.also { validate(it) }
        }

        /**
         * Overridden by subclasses to perform the validation required
         * This is where all the validation of data must occur in the
         * in subclasses.
         * @throws IllegalArgumentException if data is not valid
         * @return Unit if data is valid
         */
        @Throws(IllegalArgumentException::class)
        abstract fun validate(data: T)
    }

    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * all of which must be satisfied by the values passed through the delegate
     */
    class AllSatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : Validator<R, T>(initValue) {

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
    }

    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * any of which must be satisfied by the values passed through the delegate
     */
    class AnySatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : Validator<R, T>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            for (predicate in predicates) if (predicate(data)) return
            throw IllegalArgumentException("Value $data does not satisfy any of its required predicates")
        }

        fun negate() = NoneSatisfy<R, T>(initValue, *predicates)
        operator fun not() = negate()
    }

    /**
     * A delegate which is a composite class composed of multiple predicates or [DescribedPredicate] tests
     * none of which must be satisfied by the values passed through the delegate
     */
    class NoneSatisfy<R, T>(initValue: T, vararg val predicates: (T) -> Boolean) : Validator<R, T>(initValue) {

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
     * See [Validator] and [Validator.validate] for more about how validation is handled
     */
    class Integer<R>(
        initValue: Int,
        val minimum: Int = Int.MIN_VALUE,
        val maximum: Int = Int.MAX_VALUE,
        val predicates: List<DescribedPredicate<Int>> = listOf()
    ) : Validator<R, Int>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: Int) {
            if (data < minimum || data > maximum) {
                throw IllegalArgumentException("Value $data is out of range. Must be $minimum <= value <= $maximum")
            }
            for (predicate in predicates) {
                if (!predicate(data)) {
                    throw IllegalArgumentException("$data is invalid. Predicate failed says: ${predicate.description}")
                }
            }

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
     * See [Validator] and [Validator.validate] for more about how validation is handled
     */
    class AnyOf<R, T> internal constructor(initValue: T, val choices: Set<T>) : Validator<R, T>(initValue) {

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
     * See [Validator] and [Validator.validate] for more about how validation is handled
     */
    class NoneOf<R, T> internal constructor(initialValue: T, val choices: Set<T>) : Validator<R, T>(initialValue) {

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
    class Requirements<R, T>(initValue: T, val predicate: (T) -> Boolean) : Validator<R, T>(initValue) {

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
            if (predicate is DescribedPredicate<*>) {
                if (!predicate(data)) {
                    throw IllegalArgumentException("$data did not satisfy the predicate: ${predicate.description}")
                }
            } else {
                if (!predicate(data)) {
                    throw IllegalArgumentException("$data did not satisfy predicate")
                }
            }
        }
    }

    fun <R, T> ((T) -> Boolean).validator(initValue: T): Requirements<R, T> {
        return Requirements(initValue, this)
    }

    /**
     * A Constraint is a small class that represents a restriction on a
     * range of some <T> where T: Comparable<T>.
     *
     * This class is used by [String.lengthConstraint] and can be used by
     * anyone subclassing [Validator] and works well with the utility
     * functions [atLeast], [atMost], [between], and [exactly] which are useful for
     * natural language construction of Constraint instances
     *
     * It can be used in a variety of contexts
     *
     * Note that minimum and maximum are both inclusive in this class. This also means
     * that if minimum == maximum then the Constraint repesents that single value only
     */
    open class Constraint<T> where T : Comparable<T>, T : Any {
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

        open fun valid(value: T) = value in _minimum!!.._maximum!!

        operator fun contains(value: T): Boolean = value in minimum..maximum

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
    }

    /**
     * Represents a range constraint. Built on Constraint<T> this validator accepts any value in a
     * range of any comparable type
     */
    class RangeConstraint<R, T>(initValue: T, val constraint: Constraint<T>) :
        Validator<R, T>(initValue) where T : Comparable<T> {
        init {
            validate(initValue)
        }

        override fun validate(data: T) {
            if (constraint.valid(data)) {
                return
            }
            throw IllegalArgumentException("Value $data failed to satisfy constraint $constraint")
        }

        operator fun contains(other: T): Boolean {
            return constraint.valid(other)
        }
    }

    /**
     * Creates a new validator for a particular range
     */
    fun <R, T> inRange(initValue: T, range: ClosedRange<T>): RangeConstraint<R, T> where T : Comparable<T> =
        RangeConstraint(initValue, Constraint(range.start, range.endInclusive))

    /**
     * Creates a new validator for a range using the helper functions [atLeast], [atMost], [between], and [exactly]
     */
    fun <R, T> inRange(initValue: T, range: Constraint<T>): RangeConstraint<R, T> where T : Comparable<T> =
        RangeConstraint(initValue, range)

    /**
     * atLeast is a utility function for creating particular classes that specify
     * various requirements of existing subclasses of [Validator]. The sort of
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
     * various requirements of existing subclasses of [Validator]. The sort of
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
     * various requirements of existing subclasses of [Validator]. The sort of
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
         * This extension function associates numbers and user defined [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        fun Int.charsSatisfying(predicate: (Char) -> Boolean): Pair<CharClass, Int> = (CharClass(predicate) to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.whitespace: Pair<CharClass, Int> get() = (CharClass.whitespace to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.newline: Pair<CharClass, Int> get() = (CharClass.newline to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.alphabetic: Pair<CharClass, Int> get() = (CharClass.alphabetic to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.lowercaseLetters: Pair<CharClass, Int> get() = (CharClass.lowercaseLetters to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.uppercaseLetters: Pair<CharClass, Int> get() = (CharClass.uppercaseLetters to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.digits: Pair<CharClass, Int> get() = (CharClass.numbers to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.alphanumeric: Pair<CharClass, Int> get() = (CharClass.alphanumeric to this)

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.specialCharacters: Pair<CharClass, Int> get() = (CharClass.specialCharacters to this)

        /**
         * Enumeration representing commonly used classes of Chars for use in [MustHave] objects in
         * [String] validators
         */
        open class CharClass(val isMember: (Char) -> Boolean) : (Char) -> Boolean by isMember,
            Predicate<Char> by isMember.jPredicate() {

            //@formatter:off
        companion object {
            @JvmStatic val whitespace = CharClass(Char::isWhitespace)
            @JvmStatic val newline = CharClass { it == '\n' || it == '\r' }
            @JvmStatic val alphabetic = CharClass(Char::isLetter)
            @JvmStatic val lowercaseLetters = CharClass(Char::isLowerCase)
            @JvmStatic val uppercaseLetters = CharClass(Char::isUpperCase)
            @JvmStatic val numbers = CharClass(Char::isDigit)
            @JvmStatic val alphanumeric = CharClass(Char::isLetterOrDigit)
            @JvmStatic val specialCharacters = CharClass(!Char::isLetterOrDigit and !Char::isWhitespace)
        }
        //@formatter:on

            /**
             * Returns a new CharClass instance that will satisfy the condition passed to the function
             * Used for natual-language predicate construction such as when saying
             * `MustHave(no(charsSatisfying { it in 'A'..'M' } ))`
             */
            open fun charsSatisfying(condition: (Char) -> Boolean): CharClass = CharClass(condition)

            override fun toString(): kotlin.String {
                return "CharClass[${this::isMember.name}]"
            }
        }

        /**
         * MustHave is a class that describes which characters and in what quantity are
         * required of a [String].
         *
         * [StringContentsRule] is nothing more than a stand in for [Map] but it is a sealed
         * class so that it can be subclasses into [atLeast], [exactly], and [atMost] (not
         * to be confused with the similarly named helper functions)
         *
         * This class interacts well with [atLeast], [exactly], and [atMost] helper functions
         * which create the classes of similar name. This allows you to specify which types
         * of characters and in what quantity are required by the string. There is also
         * the [no] function which is a simple alias for exactly(0.<CharClass>).
         *
         * This class is usually constructed by using the extension properties for CharClass
         * on Int and the 4 helper functions which are then passed to the constructor
         */
        open class MustHave(vararg rules: StringContentsRule) {
            open val maps: List<StringContentsRule> = rules.toList()

            open fun isValid(data: kotlin.String): Pair<Boolean, Reason?> {
                for (rule in maps) {
                    val (valid, reason) = rule.test(data)
                    if (!valid) {
                        return valid to reason
                    }
                }
                return true to null
            }
        }


        /**
         * A stand in for [Map] of CharClass to Ints so that there can be a sealed class hierarchy
         *
         * Used by [MustHave] for [String] Validators
         */
        sealed class StringContentsRule(vararg pairs: Pair<CharClass, Int>) : Map<CharClass, Int> by mapOf(*pairs) {

            /**
             * Represents a map of [CharClass] to [Int] that specifies a string must contain
             * at least a certain number of characters from a [CharClass]
             */
            class AtLeast internal constructor(vararg pairs: Pair<CharClass, Int>) : StringContentsRule(*pairs) {
                override fun test(s: kotlin.String): Pair<Boolean, Reason?> {
                    for ((type, count) in entries) {
                        if (s.count { type.isMember(it) } < count) {
                            return false to "String requires at least $count characters of class $type"
                        }
                    }
                    return true to null
                }
            }

            /**
             * Represents a map of [CharClass] to [Int] that specifies a string must contain
             * at most a certain number of characters from a [CharClass]
             */
            class AtMost internal constructor(vararg pairs: Pair<CharClass, Int>) : StringContentsRule(*pairs) {
                override fun test(s: kotlin.String): Pair<Boolean, Reason?> {
                    for ((type, count) in entries) {
                        if (s.count { type.isMember(it) } > count) {
                            return false to "String requires at most $count characters of class $type"
                        }
                    }
                    return true to null
                }
            }

            /**
             * Represents a map of [CharClass] to [Int] that specifies a string must contain
             * exactly a certain number of characters from a [CharClass]
             */
            class Exactly internal constructor(vararg pairs: Pair<CharClass, Int>) : StringContentsRule(*pairs) {
                override fun test(s: kotlin.String): Pair<Boolean, Reason?> {
                    for ((type, count) in entries) {
                        if (s.count { type.isMember(it) } != count) {
                            return false to "String requires exactly $count characters of class $type"
                        }
                    }
                    return true to null
                }
            }

            override fun toString(): kotlin.String {
                return "[${entries.joinToString(", ") { (key, value) -> "$key: $value" }}]"
            }

            abstract fun test(s: kotlin.String): Pair<Boolean, Reason?>
        }

        /**
         * atLeast is a utility function for creating particular classes that specify
         * various requirements of existing subclasses of [Validator]. The sort of
         * things it returns depends on its overloads. The vararg pairs: [Pair]<[CharClass], [Int]>
         * overload (this overload) returns a [StringContentsRule.AtLeast] class
         * which is a mapping of [CharClass] to [Int]. This indicates
         * to the [String] validator that the [kotlin.String] must have at least
         * the given Int number of characters that match the given CharClass
         *
         * This is used primarily when constructing string [MustHave] rules
         * but it can be used for any class that requires a [StringContentsRule] instance
         * including user-defined subclasses if they operate with Constraints
         */
        fun atLeast(vararg pairs: Pair<CharClass, Int>) = StringContentsRule.AtLeast(*pairs)

        /**
         * atMost is a utility function for creating particular classes that specify
         * various requirements of existing subclasses of [Validator]. The sort of
         * things it returns depends on its overloads. The vararg pairs: [Pair]<[CharClass], [Int]>
         * overload (this overload) returns a [StringContentsRule.AtMost] class
         * which is a mapping of [CharClass] to [Int]. This indicates
         * to the [String] validator that the [kotlin.String] must have at most
         * the given Int number of characters that match the given CharClass
         *
         * This is used primarily when constructing string [MustHave] rules
         * but it can be used for any class that requires a [StringContentsRule] instance
         * including user-defined subclasses if they operate with Constraints
         */
        fun atMost(vararg pairs: Pair<CharClass, Int>) = StringContentsRule.AtMost(*pairs)

        /**
         * exactly is a utility function for creating particular classes that specify
         * various requirements of existing subclasses of [Validator]. The sort of
         * things it returns depends on its overloads. The vararg pairs: [Pair]<[CharClass], [Int]>
         * overload (this overload) returns a [StringContentsRule.Exactly] class
         * which is a mapping of [CharClass] to [Int]. This indicates
         * to the [String] validator that the [kotlin.String] must have exactly
         * the given Int number of characters that match the given CharClass
         *
         * This is used primarily when constructing string [MustHave] rules
         * but it can be used for any class that requires a [StringContentsRule] instance
         * including user-defined subclasses if they operate with Constraints
         */
        fun exactly(vararg pairs: Pair<CharClass, Int>) = StringContentsRule.Exactly(*pairs)

        /**
         * no is a utility function for creating [StringContentsRule] instances
         * This function returns an object that indicates to the [String]
         * validator that the [kotlin.String] it is validating must NOT contain any
         * of the characters that match the given CharClass
         *
         * It is an alias for calling [exactly] with 0 as the number for all [CharClass]es
         *
         * This is used primarily when constructing string [MustHave] rules
         * but it can be used for any class that requires a [StringContentsRule] instance
         * including user-defined subclasses if they operate with Constraints
         */
        fun no(vararg classes: CharClass) = exactly(*classes.map { it to 0 }.toTypedArray())
    }

    open class LengthConstraint(private vararg val constraints: Constraint<Int>) {
        companion object {
            @JvmStatic
            fun unbound(): LengthConstraint = object : LengthConstraint() {
                override fun isValid(data: Int) = true
            }
        }

        val minimum: Int
            get() = constraints.minOf { it.minimum }

        val maximum: Int
            get() = constraints.minOf { it.maximum }

        open fun isValid(data: Int) = constraints.all { it.valid(data) }
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
     * See [Validator] and [Validator.validate] for more about how validation is handled
     */
    class String<R>(
        initValue: kotlin.String,
        val lengthConstraint: LengthConstraint = LengthConstraint.unbound(),
        val mustHave: StringRules.MustHave
    ) : Validator<R, kotlin.String>(initValue) {

        init {
            validate(initValue)
        }

        override fun validate(data: kotlin.String) {
            if (!lengthConstraint.isValid(data.length)) {
                throw IllegalArgumentException("$data length is invalid. Must be between ${lengthConstraint.minimum} and ${lengthConstraint.maximum}")
            }
            val (hasAllValid, reason) = mustHave.isValid(data)
            if (!hasAllValid) {
                throw IllegalArgumentException(reason)
            }
        }
    }


    /**
     * Makes the language more natural than `to` but is not necessary
     */
    infix fun <T> kotlin.String.so(other: (T) -> Boolean): Pair<kotlin.String, (T) -> Boolean> = this to other
}
