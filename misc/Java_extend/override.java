package org.perlito.examples;

import java.security.InvalidParameterException;

public class JUser {
    private String _name;
    private String _email;

    public JUser(String name, String email) {
        if (validateEmail(email)) {
            this._email = email;
            System.out.println("Java: email Validation PASS");
        }
        else {
            throw new InvalidParameterException("Invalid email");
        }

        if (validateName(name)) {
            this._name = name;
            System.out.println("Java: name Validation PASS");
        }
        else {
            throw new InvalidParameterException("Invalid name");
        }
    }

    public String getName() {
        return _name;
    }

    public String getEmail() {
        return _email;
    }

    protected Boolean validateEmail(String email) {
        return null;
    }

    protected Boolean validateName(String name) {
        return null;
    }
}

