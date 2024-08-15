package com.tom.validators.example;

import com.tom.validators.Validators;
import com.tom.validators.ValidatorsKt;

public class JavaExample {
    public static void main(String[] args) {
        var v = new Validators.String<Object>(
                "Hello",
                new Validators.AcceptableLength(new Validators.Constraint<>(1, 10)),
                Validators.StringRules.MustHave.noRequirements()
        );
        ValidatorsKt.isValid(v, "world");
    }
}
