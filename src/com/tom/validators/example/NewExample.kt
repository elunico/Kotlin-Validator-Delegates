package com.tom.validators.example

import com.tom.validators.Validators
import com.tom.validators.Validators.AcceptableLength
import com.tom.validators.Validators.StringRules.CharClass
import com.tom.validators.Validators.StringRules.MustHave
import com.tom.validators.Validators.StringRules.atLeast
import com.tom.validators.Validators.StringRules.digits
import com.tom.validators.Validators.StringRules.lowercaseLetters
import com.tom.validators.Validators.StringRules.no
import com.tom.validators.Validators.StringRules.specialCharacters
import com.tom.validators.Validators.StringRules.uppercaseLetters
import com.tom.validators.Validators.atLeast
import com.tom.validators.Validators.charactersLong

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
//        AcceptableLength(between(8, 30)),
        (8..30).charactersLong,
        MustHave(
            no(CharClass.whitespace),
            no(CharClass.specialCharacters)
        )

    )

    val password: String by Validators.String(
        pw,
//        AcceptableLength(atLeast(8)),
        atLeast(8).charactersLong,
        MustHave(
            atLeast(1.specialCharacters, 1.uppercaseLetters, 1.lowercaseLetters, 1.digits),
            no(CharClass.whitespace)
        )
    )

    val description: String by Validators.String(
        "",
        AcceptableLength.unbound(),
        MustHave.noRequirements()
    )

    val something: String by Validators.String.noWhitespace("Hello")
}

fun main() {

    println(BankAccountUser("tom", "Password12345!"))
    println(trying { BankAccountUser("ThomasPov", "Password! 2") })
}

