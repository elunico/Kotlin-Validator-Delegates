package com.tom.validators.example;

import com.tom.validators.Constraint;
import com.tom.validators.StringRules;
import com.tom.validators.Validators;
import com.tom.validators.ValidatorsKt;

public class JavaExample {

    public static void main(String[] args) {
        var v = new Validators.String<Object>(
                "Hello",
                new StringRules.AcceptableLength(new Constraint<>(1, 10)),
                StringRules.MustHave.noRequirements()
        );
        ValidatorsKt.isValid(v, "world");
    }
}
