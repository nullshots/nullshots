---
title: "Java Code Quality Series - 001"
date: 2022-10-29T11:07:10+06:00
author: "Pratiyush Kumar Singh"
image: "images/post/post-4.jpg"
description: "Convert String to UUID in Java"
categories: ["Java","Code Quality","Code Review"]
draft: false
---
## Overview
Let's begin the journey to better and bug free code. This series would focus on writing code optimization and quality in java.

##  Code Review and Code Quality Issue - 001

# How we can simplify below code snippet

```java
 /**
  * Here, Payment Indicator means if you are doing online shopping or with Point of Sale at Shop.
  * And, 1 corresponds to online shopping and 0 corresponds by Point of Sale. Additionally, by 
  * default it should 0 Point of Sale.
  */
private int getPaymentIndicator(Boolean value) {
        if (value == null) {
            return 0;
        } else if (value.booleanValue()) {
            return 1;
        } else {
            return 0;
        }
    }
```
# Refactoring : 1
* Add final to parameter
* Method can be static
* Last else is not required.
* Second, else if can be changed to if.

```java
    private static int getPaymentIndicatorRefactoringOne(final Boolean value) {
        if (value == null) {
            return 0;
        }
        if (value.booleanValue()) {
            return 1;
        }
        return 0;
    }
```
# Refactoring : 2
Possible values of Boolean can be Boolean.True , Boolean.FALSE, NULL. In this particular case, we can further simplify the method by understanding the mapping of possible values.

* NULL and BOOLEAN.FALSE are mapped to 0
* BOOLEAN.TRUE is mapped to 1

```java
    private static int getPaymentIndicatorRefactoringTwo(final Boolean value) {
        return Boolean.TRUE.equals(value) ? 1 : 0;
    }
```

We can code *return Boolean.FALSE.equals(value) ? 0 : 1;* Usually, FALSE, NULL, NOT EQUAL these are negative words and I think I use positive words makes me feel good and bring good vibes.

And, Bang we have reduced the 9 line of code to just one 3 lines.
     