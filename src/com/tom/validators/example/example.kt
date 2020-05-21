package com.tom.validators.example

import com.tom.validators.Validators
import com.tom.validators.Validators.CharClass.Companion.specialCharacters
import com.tom.validators.Validators.CharClass.Companion.whitespace
import com.tom.validators.Validators.MustHave
import com.tom.validators.Validators.atLeast
import com.tom.validators.Validators.between
import com.tom.validators.Validators.digits
import com.tom.validators.Validators.exactly
import com.tom.validators.Validators.lowercaseLetters
import com.tom.validators.Validators.no
import com.tom.validators.Validators.specialCharacters
import com.tom.validators.Validators.uppercaseLetters


/********************************************************************************
 * Some example classes using the validators for reasonably expected validation *
 ********************************************************************************/

class Account(owner: Person?) {
    var balance: Int by Validators.Integer(initValue = 0, minimum = 0)
    val owner: Person? by Validators.Predicated(
        owner,
        Validators.ensure("Account owner must be at least 18") { (it?.age ?: 0) >= 18 }
    )

}

class Person(val name: String, var age: Int, username: String, password: String) {

    var username: String by Validators.String(
        username,
        between(5, 35),
        MustHave(no(specialCharacters, whitespace))
    )

    var loudName: String by Validators.String(
        username.toUpperCase(),
        // mustHave = MustHave(exactly(username.count{it.isLetter()}.charsSatisfying { it in 'A'..'Z' }))
        mustHave = MustHave(exactly(username.count { it.isLetter() }.uppercaseLetters))
    )

    var password: String by Validators.String(
        password,
        between(6, 20),
        MustHave(atLeast(1.specialCharacters, 1.uppercaseLetters, 1.lowercaseLetters, 1.digits))
    )
}

class Page(text: String) {
    val text: String by Validators.String(text, mustHave = MustHave(no(specialCharacters)))

    var background: String by Validators.AnyOf("red", "blue", "green", "yellow", "white")
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