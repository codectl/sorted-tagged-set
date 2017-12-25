# SortedTaggedSet

[![Build Status](https://travis-ci.org/rena2damas/sorted-tagged-set.svg?branch=master)](https://travis-ci.org/rena2damas/sorted-tagged-set)
[![Coverage Status](https://coveralls.io/repos/github/rena2damas/sorted-tagged-set/badge.svg)](https://coveralls.io/github/rena2damas/sorted-tagged-set)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/rena2damas/sorted-tagged-set/blob/master/LICENSE))

## Description

This is an Haskell module that provides a tagged set datatype. A tagged set datatype is a set where each element has associated a sequence of strings, designated by tag. Because it is a set, there is no repetition of elements nor tags within each tag. Different elements can, however, share the same tag. This module also guarantees sorting for both elements and tags. The supported operations for the SortedTaggedSet are:

* *empty* -  returns empty set
* *nullSet* - checks whether the set is empty
* *belongs* - checks whether an elements belongs to the set
* *lengthSet* - returns the number of elements in the set
* *singleton* - creates a new set with a single element with empty tags
* *insertSet* - inserts a new element to the set. If element exists, the set remains unchanged
* *removeSet* - removes an element from the set. If the element does not belong to the set, the set remains unchanged
* *insertTag* - inserts a new tag to an existent tag. If the element does not belong to the set, the set remains unchanged
* *merge* - returns the set resulting from the merge of two sets respecting the properties of the set described above

## Install
The project was built under the [Haskell Tool Stack](https://docs.haskellstack.org). Therefore the project is built by running:

```bash
$ stack build
```

The main method provides a couple of examples on how to perform operations for different SortedTaggedSet data structures. To run it:

```bash
$ stack exec sorted-tagged-set-exe
```

## License
Code and documentation released under the [MIT License](https://github.com/rena2damas/sorted-tagged-set/blob/master/LICENSE)