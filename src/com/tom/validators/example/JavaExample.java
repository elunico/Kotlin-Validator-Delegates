package com.tom.validators.example;

import com.tom.validators.AcceptableLength;
import com.tom.validators.Constraint;
import com.tom.validators.MustHave;
import com.tom.validators.Validators;

public class JavaExample {
    private final Validators.String<Object> name$validator = new Validators.String<Object>(
            "Hello",
            new AcceptableLength(Constraint.create(1, 10)),
            MustHave.noRequirements()
    );
    private String name = null;

    public static void main(String[] args) {
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        name$validator.validate(name);
        this.name = name;
    }
}
