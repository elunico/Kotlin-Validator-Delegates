package com.tom.validators.example

import com.tom.validators.StringRules.AcceptableLength
import com.tom.validators.StringRules.MustHave
import com.tom.validators.Validators
import com.tom.validators.between
import com.tom.validators.ensure
import java.util.*


/********************************************************************************
 * Some example classes using the validators for reasonably expected validation *
 ********************************************************************************/

class Account(owner: Person?) {
    var balance: Int by Validators.Integer(initValue = 0, minimum = 0)
    val owner: Person? by Validators.Requirements(
        owner,
        ensure<Person?>("Account owner is at least 18") {
            (it?.age ?: 0) >= 18
        } and ensure("Account balance is greater than 0") { it?.username != null })
}

class Person(val name: String, var age: Int, username: String, password: String) {

    var username: String by Validators.String(
        username,
        AcceptableLength(between(5, 35)),
        MustHave {
            No {
                specialCharacters
                whitespace
            }
        }
    )

    var loudName: String by Validators.String(
        username.uppercase(Locale.getDefault()),
        AcceptableLength.unbound(),
        MustHave { Exactly { username.length.uppercaseLetters } }
    )

    var password: String by Validators.String(
        password,
        AcceptableLength(between(6, 20)),
        MustHave {
            AtLeast {
                1.specialCharacters
                1.uppercaseLetters
                1.lowercaseLetters
                1.digits
            }
        }
    )
}

class Page(text: String) {
    val text: String by Validators.String(text, mustHave = MustHave { No { specialCharacters } })

    var background: String by Validators.AnyOf("red", "blue", "green", "yellow", "white")

    var margin: Double by Validators.inRange(0.5, 0.05..1.5)

    var grade: Char by Validators.inRange('A', 'A'..'F')

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
