---
title: "String Concatenation in Java"
date: 2022-09-24T11:07:10+06:00
image: "images/post/1330x400px_sorucecode.jpeg"
description: "this is meta description"
categories: ["String"]
draft: false
---
## Overview
   String concatenation in Java is one of the most common operations. In this tutorial, we'll walk through some approaches to string concatenation. But, we'll focus on describing how to use concat() and the “+” operator approaches. Finally, we'll discuss how to select the correct one depending on what we need to do.

## Approaches to Concatenation
   In general, there are different approaches to concatenating two or more strings in Java. Furthermore, we'll look at some examples with a description of each one.

# Using the “+” Operator
One of the most common concatenation approaches in Java is using the “+” operator.

The “+” operator provides more flexibility for string concatenation over other approaches. First, it doesn't throw any exceptions for null values. Second, it converts null into its string representation. Besides, we can use it for concatenating more than two strings.

Let's see a code example:

```java
@Test
void whenUsingPlusOperatorANull_thenAssertEquals() {
  String stringOne = "Hello ";
  String stringTwo = null;
  assertEquals("Hello null", stringOne + stringTwo);
}
```

## Overview
   String concatenation in Java is one of the most common operations. In this tutorial, we'll walk through some approaches to string concatenation. But, we'll focus on describing how to use concat() and the “+” operator approaches. Finally, we'll discuss how to select the correct one depending on what we need to do.

## Approaches to Concatenation
   In general, there are different approaches to concatenating two or more strings in Java. Furthermore, we'll look at some examples with a description of each one.

# Using the “+” Operator
One of the most common concatenation approaches in Java is using the “+” operator.

The “+” operator provides more flexibility for string concatenation over other approaches. First, it doesn't throw any exceptions for null values. Second, it converts null into its string representation. Besides, we can use it for concatenating more than two strings.

Let's see a code example:
```java
@Test
void whenUsingPlusOperatorANull_thenAssertEquals() {
String stringOne = "Hello ";
String stringTwo = null;
assertEquals("Hello null", stringOne + stringTwo);
}
```
The compiler internally transforms the”+” operator to a StringBuilder(or StringBuffer) class and its append() method.

Since the “+” operator silently converts the argument to a String (using the toString() method for objects), we avoid the NullPointerException. However, we need to consider if our final string result works for us with the “null” in the string body.

# Using the concat() Method
The contact() method in the String class appends a specified string at the end of the current string. It returns a combined string.

Let's tests this behavior:

```java
@Test
void whenUsingConcat_thenAssertEquals() {
String stringOne = "Hello";
String stringTwo = " World";
assertEquals("Hello World", stringOne.concat(stringTwo));
}
```

In the previous example, stringOne variable is the base string. With the concat() method, stringTwo is appended at the end of stringOne. The concat() operation is immutable, so we need an explicit assignment. The next example illustrates this case:


```java
@Test
void whenUsingConcatWithOutAssignment_thenAssertNotEquals() {
String stringOne = "Hello";
String stringTwo = " World";
stringOne.concat(stringTwo);
assertNotEquals("Hello World", stringOne); // we get only Hello
}
```
Additionally, to get our final concatenated string in this case, we need to assign the concat() result to a variable:

stringOne = stringOne.concat(stringTwo);
assertEquals("Hello World", stringOne);
Another useful feature of concat() is when we need to concatenate multiple String objects. This method allows it. Moreover, we can also append space and special characters:
```java
@Test
void whenUsingConcatToMultipleStringConcatenation_thenAssertEquals() {
String stringOne = "Hello";
String stringTwo = "World";
String stringThree = ", in Jav";
stringOne = stringOne.concat(" ").concat(stringTwo).concat(stringThree).concat("@");
assertEquals("Hello World, in Jav@", stringOne);
}
```
What about nulls? Neither the current string nor the string to be appended can be null values. Otherwise, the concat() method throws a NullPointerException:
```java
@Test
void whenUsingConcatAppendANull_thenAssertEquals() {
String stringOne = "Hello";
String stringTwo = null;
assertThrows(NullPointerException.class, () -> stringOne.concat(stringTwo));
}
```

# StringBuilder Class
Firstly, we have the StringBuilder class. This class provides the append() method to perform concatenation operations. The next example shows us how it works:

```java
@Test
void whenUsingStringBuilder_thenAssertEquals() {
StringBuilder builderOne = new StringBuilder("Hello");
StringBuilder builderTwo = new StringBuilder(" World");
StringBuilder builder = builderOne.append(builderTwo);
assertEquals("Hello World", builder.toString());
}
```

On the other hand, a similar concatenation approach is the StringBuffer class. Contrary to the StringBuilder, which is non-synchronized(i.e., not thread-safe), StringBuffer is synchronized(i.e., thread-safe). But it has worse performance than StringBuilder. It has an append() method just like StringBuilder does.

# String format() Method
Another way to perform string concatenation is using the format() method in the String class. Using format specifiers like %s, we can concatenate multiple strings by their string value or object:

```java
@Test
void whenUsingStringFormat_thenAssertEquals() {
String stringOne = "Hello";
String stringTwo = " World";
assertEquals("Hello World", String.format("%s%s", stringOne, stringTwo));
}
```

# Approaches to Concatenation in Java 8 and Above
The method join() in the String class, for Java 8 and above, can perform string concatenation. In this case, this method takes as the first argument a delimiter used between the strings that'll be concatenated:
```java
@Test
void whenUsingStringJoin_thenAssertEquals() {
String stringOne = "Hello";
String stringTwo = " World";
assertEquals("Hello World", String.join("", stringOne, stringTwo));
}
```
Since Java 8, StringJoiner class was added. This class joins Strings using delimiter, prefix, and suffix. The following code snippet is an example of its use:

```java
@Test
void whenUsingStringJoiner_thenAssertEquals() {
StringJoiner joiner = new StringJoiner(", ");
joiner.add("Hello");
joiner.add("World");
assertEquals("Hello, World", joiner.toString());
}
```

Additionally, in Java 8, with the addition of the Stream API, we can find Collectors. The Collectors class has the joining() method. This method works similarly to the join() method in the String class. It's used for collections. The following example code snippet shows us how it works:

```java
@Test
void whenUsingCollectors_thenAssertEquals() {
List<String> words = Arrays.asList("Hello", "World");
String collect = words.stream().collect(Collectors.joining(", "));
assertEquals("Hello, World", collect);
} 
```
## Choosing an Approach
   Finally, if we need to choose between the concat() method and the “+” operator, we need to consider some aspects.

First, the concat() method only accepts strings. Meanwhile, the “+” operator takes any type and converts it to a string. On the other hand, the concat() method raises a NullPointerExeption on null values, which is not so with the “+” operator.

Moreover, there's a performance difference between both. The concat() method performs better than the “+” operator. The latter always creates a new string irrespective of the length of the string. Additionally, we need to take into account that the concat() method only creates a new string when the string to be appended has a length greater than 0. Otherwise, it returns the same object.

## Conclusion
   In this article, we did a quick overview of string concatenation in Java. Additionally, we discussed in detail the use of concat() and the “+” operator to perform string concatenations. Finally, we performed a comparative analysis between the concat() method and the “+” operator and how we can choose one of them in different contexts.

As always, all snippets used in this article are available over on GitHub.
