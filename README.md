# SMPL USER MANUAL 
SMPL is a simple declarative language for conjunctive queries. SMPL programs are concise, each one is a single line at most. The grammar is designed to be as human readable as possible, so a program written in SMPL should be perfectly understandable at a glance.
## SYNTAX
Here is a simple program we’ll use to explain how the syntax of SMPL works:

```
for 1,2 in A.csv do 1,2
```

We can read our program from left to right and see exactly what it does,

`for 1,2`  In SMPL variables are named as positive integers, here we declare two variables 1 and 2. While variables are assigned in their local scope, they exist in a global scope, this will be explained later in more detail.

`in A.csv`  Next, we define where the variables we declared will be assigned their values. is the relative path (from the directory that the interpreter is run) to a csv file that contains the data to be assigned. The variables declared get assigned to the columns of the csv file from left to right. If more variables are defined than there are columns in the file, the extra variables are ignored.

_(Note: the variable numbers do not correspond to the column numbers)_

`do 1,2`  Finally, we need to display our data. The variables listed here are the ones that will be outputted by our program, in the order that they will appear.

**A BRIEF INTERLUDE**
In this guide we will frequently refer to the ‘scope’ of an expression. All expressions in SMPL have variables that are declared within them, like by using the for operator as we did in the example above. The list of variables (and their values) that are declared in an expression and all of its sub expressions is what we refer to when we talk about the scope.

We can see the basic syntax is as follows:

**THE `for _ in _` OPERATOR** allows us to specify as many positive integers as we like, as variable names that will be assigned to the columns of the csv file at the relative path specified.

**THE `do` OPERATOR** takes the result of the expression on the left and displays the variables specified on the right in the order that they are specified. The rows are outputted in lexicographical order. If you attempt to output a variable that does not exist in the scope of the expression, that variable is ignored but the rest are still shown.

This is one of the simplest programs you can write in SMPL, but the language is capable of expressing more complicated queries as well. Now that we know the structure of a basic program we can examine some more complex ones.

**THE `&` OPERATOR** performs a logical conjunction between two expressions. So given two expressions `A` and `B`, the expression `A & B` will return the list of rows that satisfy *both* the individual expressions. It is equivalent to an *intersection* in set theory.

**THE `=` OPERATOR** performs an equality check between two variables. So for some expression `A` and variables `1` and `2`, the expression `A & (1=2)` will return the list of rows in expression `A` that satisfy the condition that variables and have the same value, given that both variables are defined in the scope of . As previously mentioned, variables in SMPL have a global scope, this means that if the same variable is defined anywhere within the scope of a program, the interpreter will assert that they must have equal values.

**THE `ifexist` OPERATOR** acts as a filter, if any of the variables specified do not exist in the scope of the expression, the expression is not evaluated. For a given expression the syntax is as follows: `ifexist 1,2,3 in (A) do 1` if the scope of expression A does not include all three variables, then the entire expression is ignored. If they do exist then expression A is evaluated as normal.

**THE `as` OPERATOR** allows you to alias a csv file that you load with a new name, the syntax works as follows, `for 1,2 in A.csv`. The file is now referred to by a combination of its path and the alias (in this case name) in the interpreter. So as far as the interpreter is concerned our file is now called ‘`A.csv|name` ’. This means that different files can be given the same alias and still won’t clash, but the same file must be given different aliases to work correctly. **Note: all files are given the alias of ‘A’ by default, so be careful when using ‘A’ as an alias yourself!*

**JOINING EXPRESSIONS** is accomplished by the use of parentheses, they can be used to apply filters such as equality and existence to multiple joined queries. Conjunctions can also be applied ad infinitum. Appendix 3 shows how expressions can be joined.

**COMMENTS** can be written on any line after the first. Given that all SMPL programs are one line, anything that is written on subsequent lines will be ignored by the interpreter. The first line however must always contain the program.

## ERROR CHECKING
SMPL employs simple error checking to find lexical errors in programs. The design of the grammar means that if a program is lexically correct, then it is almost always logically sound as well (except in a few cases). Here’s an example of an incorrect SMPL program:
```
for 1,2 in A.csv d 1,2
```
```
myinterpreter: Error at row 1, col 18
```
We can see that the interpreter has detected the error with the `do` operator and shown the location of the error as the 18th column (or character).
```
(for (for 1,2 in B.csv) in A.csv) do 1,2
```
```
myinterpreter: Error at row 1, col 6
```
Here we can see that the interpreter also detects expressions of correct syntax in invalid locations and output the column in question.
```
(for (for 1,2 in B.csv) in A.csv) d 1,2)
```
```
myinterpreter: Error at row 1, col 6
```
It is important to note that the interpreter will always show the first error it finds and then stop, so a single error shown does not equate to there being a single error in the program.

There are however a few ways in which logical errors can make their way into SMPL programs, the main ones being through mistakes with the csv files that are loaded and mistakes made in the definition of variables.
## ERROR HANDLING
**CSV ERRORS** 
 - Empty csv – If one or more of the csv files in an expression are empty, the result of that expression will be empty
 - Mismatched columns in csv – If a csv file has different numbers of columns in each row, variables that can be assigned, will be assigned but variables that cannot will be assigned as empty.

**VARIABLE ERRORS**
 - **Output of undeclared variable** – If you attempt to display a variable that has not been declared in the scope of any of your expressions the variable is ignored during the output.
 - **Output of unassigned variable** – If you attempt to display a variable that has been declared but not been assigned a value, the output will be empty for that variable. For example if we have a file A.csv that contains two columns, and we run the following program: `for 1,2,3 in A.csv do 1,2,3`
Variable 3 is declared, but it will never be assigned and so the output of this program will include an empty value for variable 3.
  - **Equality of undeclared variable** – If you attempt to check equality of an undeclared variable, the variable in question, and therefore the equality, is ignored and so will have no effect.
  - **Equality of unassigned variable** – SMPL uses alpha substitution to detect equality between variables. So if you attempt to check the equality of a variable that has not been assigned, it will be assigned to whichever variable you checked. For example consider a file A.csv that has two columns: `((for 1,2,3 in A.csv) & (2=3)) do 1,2,3`
In this program, the variable 3 has not been assigned, so when we check equality between 2 and 3, the interpreter will set the value of 2 to 3. So the comparator has become an assignment. The output of this program will be all rows in A.csv with the last column being a repeat of the second. In essence when checking if two variables are equal, SMPL asserts that they are the same variable, if the variables are assigned (differently) this is impossible, so it fails and return false. However, if one is not assigned the assertion forces itself to be true. This is explained further in later on.
 - **Equality of two unassigned variables** – Following from above if both variables are unassigned, they are given empty values by default and so they will return as being equal to each other.

## SOME CURIOSITIES
**“Think first, act later”** – SMPL is designed to think first and act later, it reads the program and carries out the logic before loading any of the data. The interpreter keeps track of which variables are assigned to which files and which variables should be equal to each other. Once this is done it then loads the files, takes the necessary cross products and then, line by line, determines which values were meant for each of the variables and filters any results that do not adhere to the equality rules it previously determined.

**Checking for equality** – Given the above, it might not be immediately apparent how SMPL checks variables for equality. When the interpreter reads the program, it flags any variable that the requires an equality check with the variable that it must be equal to. Later it enumerates all such variables that need to be equal and performs alpha conversions on the whole scope of the program renaming these variables to a single free variable. It then loads and assigns the data from the files and asserts that any time a variable appears more than once in the scope, they must have the same value. This is why name has global scope for variables, even if they are assigned in two unconnected smaller scopes

### VALID DERIVATION TREE
![Derivation Tree](https://raw.githubusercontent.com/harikulendran/SMPL/master/img/SMPL-grammar-tree.png "Derivation Tree")
