package com.tom.validators.example;

import com.tom.validators.*;

public class JavaExample {
    public static void main(String[] args) {
        var v = new Validators.String<Object>(
                "Hello",
                new AcceptableLength(Constraint.create(1, 10)),
                MustHave.noRequirements()
        );
        ValidatorsKt.isValid(v, "world");
    }


}
