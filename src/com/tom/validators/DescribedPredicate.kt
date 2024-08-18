package com.tom.validators

import java.util.function.Predicate

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
    val description: String, val predicate: (T) -> Boolean
) : (T) -> Boolean by predicate, Predicate<T> by predicate.jPredicate() {
    infix fun and(other: DescribedPredicate<T>): DescribedPredicate<T> {
        return DescribedPredicate(this.description + " && " + other.description) { this(it) && other(it) }
    }

    infix fun or(other: DescribedPredicate<T>): DescribedPredicate<T> {
        return DescribedPredicate(this.description + " || " + other.description) { this(it) || other(it) }
    }
}

/**
 * Ties a string and a (T) -> Boolean together in a [DescribedPredicate]
 * Used as sugar for more natural-language-like construction of Validators
 */
fun <T> ensure(first: String, second: (T) -> Boolean): DescribedPredicate<T> {
    return DescribedPredicate(first, second)
}

/**
 * Ties many strings and a ((T) -> Boolean)s together in a list of [DescribedPredicate]
 * Used as sugar for more natural-language-like construction of Validators
 */
fun <T> ensure(vararg pairs: Pair<String, (T) -> Boolean>): List<DescribedPredicate<T>> {
    return pairs.map { (first, second) -> DescribedPredicate(first, second) }
}