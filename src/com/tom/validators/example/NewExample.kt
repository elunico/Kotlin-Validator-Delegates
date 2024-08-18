package com.tom.validators.example

import com.tom.validators.*

//fun <T> T.printed(): T = this.also { println(it) }


fun <T> trying(block: () -> T): T? = try {
    block()
} catch (e: Exception) {
    null
}


fun <T> T.printed(): T = this.also(::println)

class BankAccountUser(user: String, pw: String) {
    val username: String by Validators.String(
        user,
        (8..30).charactersLong,
        MustHave {
            No { whitespace }
            No { specialCharacters }
        }
    )

    val password: String by Validators.String(
        pw,
        atLeast(8).charactersLong,
        MustHave {
            AtLeast {
                1.specialCharacters
                1.uppercaseLetters
                1.lowercaseLetters
            }
            No { whitespace }
        }
    )

    val description: String by Validators.String("", AcceptableLength.unbound(), MustHave.noRequirements())

    val something: String by Validators.String.noWhitespace("Hello")
}

fun main() {
    println(BankAccountUser("tom123456", "Password12345!"))
    println(trying { BankAccountUser("ThomasPov", "Password! 2") })
}

