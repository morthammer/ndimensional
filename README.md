# CSV Parser

Simple CSV Parser

### Prerequisites

```
sbt 0.13
java 1.8
scala 2.12
```

### Installing

Clone project from git

```
git clone https://github.com/morthammer/ndimensional.git
```

change directory or cd into ndimensional

run the project with sbt

```
sbt "run file:///home/someUserDirectory/Iris.csv"
```

## Running the tests

The unit tests can be run from the root directory using sbt with the command

```
sbt test
```

## Assumptions

It can be performance prohibitive to do large file manipulation on the JVM

Few variations in formats except what was introduced in the sample csv files are allowed, espcially for DateTime

