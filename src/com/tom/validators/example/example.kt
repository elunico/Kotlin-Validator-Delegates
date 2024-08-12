package com.tom.validators.example

import com.tom.validators.Validators
import com.tom.validators.Validators.LengthConstraint
import com.tom.validators.Validators.StringRules.CharClass.Companion.alphabetic
import com.tom.validators.Validators.StringRules.CharClass.Companion.specialCharacters
import com.tom.validators.Validators.StringRules.CharClass.Companion.whitespace
import com.tom.validators.Validators.StringRules.MustHave
import com.tom.validators.Validators.StringRules.atLeast
import com.tom.validators.Validators.StringRules.digits
import com.tom.validators.Validators.StringRules.exactly
import com.tom.validators.Validators.StringRules.lowercaseLetters
import com.tom.validators.Validators.StringRules.no
import com.tom.validators.Validators.StringRules.specialCharacters
import com.tom.validators.Validators.StringRules.uppercaseLetters
import com.tom.validators.Validators.between
import com.tom.validators.Validators.ensuring
import com.tom.validators.Validators.validator
import com.tom.validators.and
import java.util.*


/********************************************************************************
 * Some example classes using the validators for reasonably expected validation *
 ********************************************************************************/

class Account(owner: Person?) {
    var balance: Int by Validators.Integer(initValue = 0, minimum = 0)
    val owner: Person? by (ensuring<Person?>("Account owner must be at least 18") { (it?.age ?: 0) >= 18 } and
            ensuring("name is at least 2 letters long") { (it?.name?.length ?: 0) >= 2 }).validator(owner)
}

class Person(val name: String, var age: Int, username: String, password: String) {

    var username: String by Validators.String(
        username,
        LengthConstraint(between(5, 35)),
        MustHave(no(specialCharacters, whitespace))
    )

    var loudName: String by Validators.String(
        username.uppercase(Locale.getDefault()),
        // mustHave = MustHave(exactly(username.count{it.isLetter()}.charsSatisfying { it in 'A'..'Z' }))
        mustHave = MustHave(exactly(username.count(alphabetic).uppercaseLetters))
    )

    var password: String by Validators.String(
        password,
        LengthConstraint(between(6, 20)),
        MustHave(atLeast(1.specialCharacters, 1.uppercaseLetters, 1.lowercaseLetters, 1.digits))
    )
}

class Page(text: String) {
    val text: String by Validators.String(text, mustHave = MustHave(no(specialCharacters)))

    var background: String by Validators.AnyOf("red", "blue", "green", "yellow", "white")

    var margin: Double by Validators.inRange(0.5, 0.05..1.5)

}

fun main() {

    val mainPage = Page("Some sample text")
    mainPage.background = "white"

    val alice = Person("Alice", 32, "alice1", "Password!1")

    val account = Account(alice)

    account.balance += 10

    /* Invalid data
    account.balance -= 200 // IllegalArugmentException

    val bad = Person("Bob", 100, "invalid name", "Password1!") // illegal argument exception for username

    */
}
