---
title: "String to UUID conversion in Java"
date: 2022-09-29T11:07:10+06:00
image: "images/post/uuid.jpg"
description: "Convert String to UUID in Java"
categories: ["String","UUID","Conversion"]
draft: false
---

## Overview
In this tutorial, we're going to have a look at some ways of validating UUID (Universally Unique Identifier) strings in Java.

We'll go through one of the UUID class methods, and then we'll use regular expressions.

##  Using UUID.fromString()
   One of the quickest ways of checking if a String is a UUID is by trying to map it using the static method fromString belonging to the UUID class. Let's try it out:

```java
@Test
void shouldReturnValidUUID_UsingUUIDFromString() {
    final String validUUID = "23c8fd18-078f-453e-8c9e-bade508c7132";
    Assertions.assertEquals(UUID.fromString(validUUID).toString(), validUUID);
}

@Test
void shouldThrowIllegalArgumentException_UsingUUIDFromString() {
    final String invalidUUID = "23c8fd18-078f-453e";
    Assertions.assertThrows(IllegalArgumentException.class, () -> UUID.fromString(invalidUUID));
}
```

In the above code snippet, we can see that in case the string we're trying to validate doesn't represent a UUID, then an IllegalArgumentException will be thrown. However, the method fromString will return `00000001-0001-0001-0001-000000000001` for strings such as `1-1-1-1-1`. So we have included the string comparison to take care of this case.

Some might argue that using exceptions is not a good practice for flow control, so we're going to see a different way of achieving the same result.

## Using Regular Expressions
   Another way of validating a UUID is to use a regular expression that will match exactly the format.

Firstly, we need to define a Pattern that will be used to match the string.
```java
Pattern UUID_REGEX =
Pattern.compile("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");
```
Then, we can use this pattern to try matching it to a string in order to validate whether or not it is a UUID:

```java
@Test
public void whenUUIDIsValidatedUsingRegex_thenValidationSucceeds() {
Pattern UUID_REGEX =
Pattern.compile("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");

    Assertions.assertTrue(UUID_REGEX.matcher("26929514-237c-11ed-861d-0242ac120002").matches());

    Assertions.assertFalse(UUID_REGEX.matcher("invalid-uuid").matches());
}
```

## Conclusion
   In this article, we've learned how to validate a UUID string by using regular expressions or by taking advantage of the static method of the UUID class.

As always, the code for these examples is available over on GitHub.
