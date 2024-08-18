package com.tom.validators.example

infix fun <A, B> Iterable<A>.cartesianProduct(other: Iterable<B>): Iterable<Pair<A, B>> {
    val l = mutableListOf<Pair<A, B>>()
    for (elt in this) {
        for (that in other) {
            l.add(elt to that)
        }
    }
    return l
}

fun test(lb: Int?, ub: Int?, olb: Int?, oub: Int?): Boolean {
    val isThisLowerBelow = (lb == null && olb == null) || (lb != null && olb != null && lb < olb)
    val isThisUpperAbove = (ub == null && oub == null) || (ub != null && oub != null && ub > oub)

    return isThisLowerBelow && isThisUpperAbove
}

fun main() {
    val a = listOf<Int?>(null, null, null, null, 1, 10, 4, 5)
    val b = listOf<Int?>(1, 10, 4, 5, null, null, null, null)

    val c = a cartesianProduct b

    val d = c cartesianProduct c

    for (elt in d) {
        print(elt)
        print(" ")
        println(test(elt.first.first, elt.first.second, elt.second.first, elt.second.second))
    }
}