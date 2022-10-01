---
title: "Rotate Int Array in Java"
date: 2022-10-01T11:07:10+06:00
image: "images/post/rotate_new_moon.gif"
description: "Convert String to UUID in Java"
categories: ["String","UUID","Conversion"]
draft: false
---

## Overview
Given an array of integers arr[] of size N and an integer, the task is to rotate the array elements to the left by d positions.

##  Array Rotation One By One

# Solution Synopsis

At each iteration, shift the elements by one position to the left circularly (i.e., first element becomes the last).
Perform this operation d times to rotate the elements to the left by d position.

>Example:

* Let us take arr[] = [1, 2, 3, 4, 5, 6, 7], d = 2.
* First Step: => Rotate to left by one position. => arr[] = {2, 3, 4, 5, 6, 7, 1}
* Second Step: => Rotate again to left by one position => arr[] = {3, 4, 5, 6, 7, 1, 2}
* Rotation is done by 2 times. So the array becomes arr[] = {3, 4, 5, 6, 7, 1, 2}

>Steps to solve the array rotation problem.

* Rotate the array to left by one position. For that do the following:
* Store the first element of the array in a temporary variable.
* Shift the rest of the elements in the original array by one place.
* Update the last index of the array with the temporary variable.
* Repeat the above steps for the number of left rotations required.

**Time Complexity: O(N * d)**
**Auxiliary Space: O(1)**

```java
package com.nullshots.algorithm.array.rotation;

import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ArrayRotationOneByOne {

    private static final Logger LOGGER = LoggerFactory.getLogger(ArrayRotationOneByOne.class);

    public static void main(final String[] args) {
        final int arrayToBeRotated[] = {1, 2, 3, 4, 5, 6, 7};
        final int d = 2;
        final int n = arrayToBeRotated.length;
        rotate(arrayToBeRotated, d, n);
    }

    public static void rotate(final int[] arrayToBeRotated, final int d, final int n) {
        for (int i = 0; i < d; i++) {
            int firstIndexData = arrayToBeRotated[0];
            for (int j = 0; j < n - 1; j++) {
                arrayToBeRotated[j] = arrayToBeRotated[j + 1];
            }
            arrayToBeRotated[n - 1] = firstIndexData;
            LOGGER.info(Arrays.toString(arrayToBeRotated));
        }
    }
}
```

## Conclusion

In this article we have learned different ways of integer array rotation.

