---
title: "Rotate Int Array in Java"
date: 2022-10-01T11:07:10+06:00
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


## Solution Synopsis

# Array Rotation using Auxiliary Array. 
After rotating d positions to the left, the first d elements become the last d elements of the array

* First store the elements from index d to N-1 into the temp array.
* Then store the first d elements of the original array into the temp array.
* Copy back the elements of the temp array into the original array
* 
>Example:
* Suppose the give array is arr[] = [1, 2, 3, 4, 5, 6, 7], d = 2.
* First Step:
     => Store the elements from 2nd index to the last.
     => temp[] = [3, 4, 5, 6, 7]
 
* Second Step:
     => Now store the first 2 elements into the temp[] array.
     => temp[] = [3, 4, 5, 6, 7, 1, 2]
 
* Third Steps:
     => Copy the elements of the temp[] array into the original array.
     => arr[] = temp[] So arr[] = [3, 4, 5, 6, 7, 1, 2]

>Steps to solve the array rotation problem.

* Follow the steps below to solve the given problem.
* Initialize a temporary array(temp[n]) of length same as the original array
* Initialize an integer(k) to keep a track of the current index
* Store the elements from the position d to n-1 in the temporary array
* Now, store 0 to d-1 elements of the original array in the temporary array
* Lastly, copy back the temporary array to the original array

> Time complexity: O(N)
> Auxiliary Space: O(N)

```java
package com.nullshots.algorithm.array.rotation;

import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ArrayRotationUsingTempArray {

    private static final Logger LOGGER = LoggerFactory.getLogger(ArrayRotationUsingTempArray.class);

    public static void main(final String[] args) {
        final int arrayToBeRotated[] = {1, 2, 3, 4, 5, 6, 7};
        final int d = 2;
        final int n = arrayToBeRotated.length;
        rotate(arrayToBeRotated, d, n);
    }

    public static void rotate(final int[] arrayToBeRotated, final int d, final int n) {

        // Current Value: {0,0,0,0,0,0,0}
        int rotatedArray[] = new int[n];
        int currentIndex = 0;

        // Moving numbers from index D - N
        // After Iteration : {3,4,5,6,7,0,0}
        for (int i = d; i < n; currentIndex++, i++) {
            rotatedArray[currentIndex] = arrayToBeRotated[i];
            LOGGER.info(Arrays.toString(rotatedArray));
        }

        // Moving numbers from index 0 - D
        // After Iteration : {3,4,5,6,7,1,2}
        for (int i = 0; i < d; currentIndex++, i++) {
            rotatedArray[currentIndex] = arrayToBeRotated[i];
            LOGGER.info(Arrays.toString(rotatedArray));
        }

        LOGGER.info(Arrays.toString(rotatedArray));
    }
}
```
## Solution Synopsis

# Array Rotation Using Juggling : 

Instead of moving one by one, divide the array into different sets where the number of sets is equal to the GCD of N and d (say X. So the elements which are X distance apart are part of a set) and rotate the elements within sets by 1 position to the left.

* Calculate the GCD between the length and the distance to be moved.
* The elements are only shifted within the sets.
* We start with temp = arr[0] and keep moving arr[I+d] to arr[I] and finally store temp at the right place.

>Example:

Let arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12} and d = 3
 
* First step:
         => First set is {1, 4, 7, 10}.
         => Rotate this set by one position to the left.
         => This set becomes {4, 7, 10, 1}
         => Array arr[] = {4, 2, 3, 7, 5, 6, 10, 8, 9, 1, 11, 12}
 
* Second step:
         => Second set is {2, 5, 8, 11}.
         => Rotate this set by one position to the left.
         => This set becomes {5, 8, 11, 2}
         => Array arr[] = {4, 5, 3, 7, 8, 6, 10, 11, 9, 1, 2, 12}
 
* Third step:
         => Third set is {3, 6, 9, 12}.
         => Rotate this set by one position to the left.
         => This set becomes {6, 9, 12, 3}
         => Array arr[] = {4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3}

>Steps to solve the array rotation problem.
* Perform d%n in order to keep the value of d within the range of the array where d is the number of times the array is rotated and N is the size of the array.
* Calculate the GCD(N, d) to divide the array into sets.
* Run a for loop from 0 to the value obtained from GCD.
* Store the value of arr[i] in a temporary variable (the value of i denotes the set number).
* Run a while loop to update the values according to the set.
* After exiting the while loop assign the value of arr[j] as the value of the temporary variable (the value of j denotes the last element of the ith set).

> Time complexity : O(N)
> Auxiliary Space : O(1)


```java
package com.nullshots.algorithm.array.rotation;

import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ArrayRotationUsingJuggling {

    private static final Logger LOGGER = LoggerFactory.getLogger(ArrayRotationUsingJuggling.class);

    public static void main(final String[] args) {
        final int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13};
        final int d = 3;
        final int n = arr.length;
        rotate(arr, d, n);
    }

    public static void rotate(final int[] arr, int d, final int n) {
        // Special Case: When d >= n
        d = d % n;
        int gcd = getGreatestCommonDivisor(d, n);
        for (int i = 0; i < gcd; i++) {
            int temp = arr[i];
            int j = i;
            while (true) {
                int k = j + d;
                if (k >= n) k = k - n;
                if (k == i) break;
                arr[j] = arr[k];
                j = k;
                LOGGER.info(Arrays.toString(arr));
            }
            arr[j] = temp;
        }
        LOGGER.info(Arrays.toString(arr));
    }

    /** READ HOW TO CALCULATE GCD AND WHAT IS GCD. */
    private static int getGreatestCommonDivisor(final int a, final int b) {
        if (b == 0) return a;
        else return getGreatestCommonDivisor(b, a % b);
    }
}
```

## Solution Synopsis

#Array Rotation Using Reversal 

Instead of moving one by one, divide the array into different sets where the number of sets is equal to the GCD of N and d (say X. So the elements which are X distance apart are part of a set) and rotate the elements within sets by 1 position to the left.

* Calculate the GCD between the length and the distance to be moved.
* The elements are only shifted within the sets.
* We start with temp = arr[0] and keep moving arr[I+d] to arr[I] and finally store temp at the right place.
 
>Example:
* If we observe closely, we can see that a group of array elements is changing its position. For example see the following array:
* arr[] = {1, 2, 3, 4, 5, 6, 7} and d = 2. The rotated array is {3, 4, 5, 6, 7, 1, 2}
* The group having the first two elements is moving to the end of the array. This is like reversing the array.
* But the issue is that if we only reverse the array, it becomes {7, 6, 5, 4, 3, 2, 1}.
* After rotation the elements in the chunks having the first 5 elements {7, 6, 5, 4, 3} and the last 2 elements {2, 1} should be in the actual order as of the initial array [i.e., {3, 4, 5, 6, 7} and {1, 2}]but here it gets reversed.
* So if those blocks are reversed again we get the desired rotated array.
>So the sequence of operations is:
* Reverse the whole array
* Then reverse the last ‘d’ elements and
* Then reverse the first (N-d) elements.
> As we are performing reverse operations it is also similar to the following sequence:
* Reverse the first ‘d’ elements
* Reverse last (N-d) elements
* Reverse the whole array.
>Steps to solve the array rotation problem.
* Algorithm reverse(arr, start, end): mid = (start + end)/2 loop from i = start to mid: swap (arr[i], arr[end-(mid-i+1)])
* Algorithm rotate(arr, d, N): reverse(arr, 1, d) ; reverse(arr, d + 1, N); reverse(arr, 1,N);

> Time complexity : O(N)
> Auxiliary Space : O(1)

>Example:
* For example take the array arr[] = {1, 2, 3, 4, 5, 6, 7} and d = 2.
 +---+---+---+---+---+---+---+
 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
 +---+---+---+---+---+---+---+
* The rotated array will look like:
 +---+---+---+---+---+---+---+
 | 3 | 4 | 5 | 6 | 7 | 1 | 2 |
 +---+---+---+---+---+---+---+
 
* 1st Step: Consider the array as a combination of two blocks. One containing the first two elements and the other containing the remaining elements as shown above.
 +---+---+        +---+---+---+---+---+
 | 1 | 2 |        | 3 | 4 | 5 | 6 | 7 |
 +---+---+        +---+---+---+---+---+
 FIRST BLOCK      SECOND BLOCK
 Considered 2 blocks
 
* 2nd Step: Now reverse the first d elements. It becomes as shown in the image
+---+---+                +---+---+---+---+---+
| 2 | 1 |                | 3 | 4 | 5 | 6 | 7 |
+---+---+                +---+---+---+---+---+
REVERSED FIRST BLOCK     SECOND BLOCK
Reverse the first K elements
 
* 3rd Step: Now reverse the last (N-d) elements. It become as it is shown in the below image:
 
+---+---+                +---+---+---+---+---+
| 2 | 1 |                | 7 | 6 | 5 | 4 | 3 |
+---+---+                +---+---+---+---+---+
REVERSED FIRST BLOCK     REVERSED SECOND BLOCK
Reverse the last (N-K) elements

* 4th Step: Now the array is the exact reversed form of how it should be if left shifted d times. So reverse the whole array and you will get the required rotated array.

+---+---+---+---+---+---+---+ 
| 3 | 4 | 5 | 6 | 7 | 1 | 2 |
+---+---+---+---+---+---+---+
The total array is reversed

See that the array is now the same as the rotated array.


```java
package com.nullshots.algorithm.array.rotation;

import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ArrayRotationUsingReversal {

    private static final Logger LOGGER = LoggerFactory.getLogger(ArrayRotationUsingReversal.class);

    public static void main(final String[] args) {
        final int arr[] = {1, 2, 3, 4, 5, 6, 7};
        final int d = 2;
        final int n = arr.length;
        rotate(arr, d, n);
    }

    public static void rotate(final int[] arr, int d, final int n) {
        // Special Case 1: When D = 0, NO ROTATION REQUIRED
        if (d == 0) return;

        // Special Case 2: When D >= N
        d = d % n;

        // Reverse : First part of sub-array from index 0 - (D - 1)
        reverse(arr, 0, d - 1);

        // Reverse : Second part of sub-array from index D - (N - 1)
        reverse(arr, d, n - 1);

        // Reverse : Whole Array
        reverse(arr, 0, n - 1);

        LOGGER.info(Arrays.toString(arr));
    }

    private static void reverse(final int[] arr, int startIndex, int lastIndex) {
        while (startIndex <= lastIndex) {
            int temp = arr[startIndex];
            arr[startIndex] = arr[lastIndex];
            arr[lastIndex] = temp;
            lastIndex--;
            startIndex++;
        }
    }
}
```
## Conclusion

In this article we have learned different ways of integer array rotation.

