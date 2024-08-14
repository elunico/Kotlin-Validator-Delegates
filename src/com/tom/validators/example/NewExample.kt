package com.tom.validators.example

import com.tom.validators.Validators
import com.tom.validators.Validators.LengthConstraint
import com.tom.validators.Validators.StringRules.CharClass
import com.tom.validators.Validators.StringRules.MustHave
import com.tom.validators.Validators.StringRules.atLeast
import com.tom.validators.Validators.StringRules.digits
import com.tom.validators.Validators.StringRules.lowercaseLetters
import com.tom.validators.Validators.StringRules.no
import com.tom.validators.Validators.StringRules.specialCharacters
import com.tom.validators.Validators.StringRules.uppercaseLetters
import com.tom.validators.Validators.between

//fun <T> T.printed(): T = this.also { println(it) }

fun <T> T.printed(): T = this.also(::println)

class BankAccountUser(user: String, pw: String) {
    val username: String by Validators.String(
        user,
        LengthConstraint(between(8, 30)),
        MustHave(
            no(CharClass.whitespace),
            no(CharClass.specialCharacters)
        )
    )

    val password: String by Validators.String(
        pw,
        LengthConstraint(10..32),
        MustHave(
            atLeast(1.specialCharacters, 1.uppercaseLetters, 1.lowercaseLetters, 1.digits),
            no(CharClass.whitespace)
        )
    )

    val description: String by Validators.String(
        "",
        LengthConstraint.unbound(),
        MustHave.noRequirements()
    )

    val something: String by Validators.String.noWhitespace("Hello")
}

fun main() {

}

