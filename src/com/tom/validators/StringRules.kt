package com.tom.validators

import com.tom.validators.MustHave.*
import java.util.function.Predicate

/**
 * Enumeration representing commonly used classes of Chars for use in [MustHave] objects in
 * [String] validators
 */
open class CharClass internal constructor(val isMember: (Char) -> Boolean) : (Char) -> Boolean by isMember,
    Predicate<Char> by isMember.jPredicate() {

    var description: String? = null

    fun described(description: String) = this.apply { this.description = description }

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

    override fun toString(): String {
        return "CharClass[${this.description}]"
    }
}

@DslMarker
annotation class MustHaveDSL

/**
 * MustHave is a class that describes which characters and in what quantity are
 * required of a [String].
 *
 * This class uses the [AtLeast], [AtMost], [Exactly], and [None] classes to help build specific requirements
 *
 * This class is usually constructed by using the extension properties for CharClass
 * on Int and the 4 helper functions which are then passed to the constructor
 */
@MustHaveDSL
open class MustHave(builder: MustHave.() -> Unit) {
    protected val rules: MutableList<StringContentsRule> = mutableListOf()

    init {
        this.builder()
    }


    open fun isValid(data: String): Pair<Boolean, Reason?> {
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
            override fun isValid(data: String): Pair<Boolean, Reason?> {
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

        private fun add(clazz: CharClass, i: Int) {
            this@MustHave.rules.add(StringContentsRule(comparator(), clazz to i))
        }


        /**
         * Returns a new CharClass instance that will satisfy the condition passed to the function
         * Used for natural-language predicate construction such as when saying
         * `MustHave(no(charsSatisfying { it in 'A'..'M' } ))`
         */
        fun satisfying(condition: (Char) -> Boolean) {
            add(CharClass(condition), -1)
        }

        /**
         * This extension function associates numbers and user defined [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        fun Int.charsSatisfying(predicate: (Char) -> Boolean) {
            add(CharClass(predicate), this)
        }

        //@formatter:off
        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.whitespace: Unit get() { add(CharClass.whitespace, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.newline: Unit get() { add(CharClass.newline, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.alphabetic: Unit get() { add(CharClass.alphabetic, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.lowercaseLetters: Unit get() { add(CharClass.lowercaseLetters, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.uppercaseLetters: Unit get() { add(CharClass.uppercaseLetters, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.digits: Unit get() { add(CharClass.numbers, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.alphanumeric: Unit get() { add(CharClass.alphanumeric, this) }

        /**
         * These extension properties associate numbers and [CharClass]es
         * to facilitate the creation of [MustHave] objects for use in
         * describing [String] validators
         */
        val Int.specialCharacters: Unit get() { add(CharClass.specialCharacters, this) }
        //@formatter:on
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
        private fun add(clazz: CharClass) {
            this@MustHave.rules.add(StringContentsRule(comparator(), clazz to -1))
        }

        /**
         * These overloads are needed here so that [No] and [Exactly] can use [CharClass]es without numbers
         * but still have them added to the [MustHave] specification
         *
         * Redundant so that other uses cna still access [CharClass] constants from the class itself
         */
        val whitespace: Unit
            get() {
                add(CharClass(Char::isWhitespace).described("whitespace"))
            }

        /**
         * See [whitespace]
         */
        val newline: Unit
            get() {
                add(CharClass { it == '\n' || it == '\r' }.described("new lines"))
            }

        /**
         * See [whitespace]
         */
        val alphabetic: Unit
            get() {
                add(CharClass(Char::isLetter).described("letter"))
            }

        /**
         * See [whitespace]
         */
        val lowercaseLetters: Unit
            get() {
                add(CharClass(Char::isLowerCase).described("lowercase letter"))
            }

        /**
         * See [whitespace]
         */
        val uppercaseLetters: Unit
            get() {
                add(CharClass(Char::isUpperCase).described("uppercae letter"))
            }

        /**
         * See [whitespace]
         */
        val numbers: Unit
            get() {
                add(CharClass(Char::isDigit).described("digit"))
            }

        /**
         * See [whitespace]
         */
        val alphanumeric: Unit
            get() {
                add(CharClass(Char::isLetterOrDigit).described("letter or digit"))
            }

        /**
         * See [whitespace]
         */
        val specialCharacters: Unit
            get() {
                add(CharClass(!Char::isLetterOrDigit and !Char::isWhitespace).described("special character"))
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

    fun test(s: String): Pair<Boolean, Reason?> {
        for ((type, expectedCount) in entries) {
            if (!isAcceptable(s.count(type.isMember), expectedCount)) {
                return false to "String did not satisfy char expected count of $expectedCount of type $type"
            }
        }
        return true to null
    }

    override fun toString(): String {
        return "[${entries.joinToString(", ") { (key, value) -> "$key: $value" }}]"
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

    override fun toString(): String {
        val s = StringBuilder()
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
 * Convenience method for creating [AcceptableLength] objects for [String] using integers
 */
val Int.charactersLong: AcceptableLength
    get() = AcceptableLength(exactly(this))

/**
 * Convenience method for creating [AcceptableLength] objects for [String] using closed ranges
 */
val ClosedRange<Int>.charactersLong: AcceptableLength
    get() = AcceptableLength(this)

/**
 * Convenience method for creating [AcceptableLength] objects for [String] using closed ranges
 */
val kotlin.ranges.ClosedRange<Int>.charactersLong
    get() = AcceptableLength(between(this.start, this.endInclusive))
