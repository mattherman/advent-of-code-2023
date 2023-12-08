"
A class that can be used to map integer strings (ex. 'two') to their values as strings (ex. '2').
"
Class {
	#name : #IntegerNames,
	#superclass : #Object,
	#classVars : [
		'NameMap'
	],
	#category : #'AdventOfCode2023-Day01Exercise2'
}

{ #category : #'class initialization' }
IntegerNames class >> initialize [
	NameMap := Dictionary newFrom: { 
		'one' -> '1'.
		'two' -> '2'.
		'three' -> '3'.
		'four' -> '4'.
		'five' -> '5'.
		'six' -> '6'.
		'seven' -> '7'.
		'eight' -> '8'.
		'nine' -> '9'
	}
]

{ #category : #enumerating }
IntegerNames class >> replace: aString [ 
	| names result index nextOccurrence |
	result := aString.
	index := 1.
	
	names := NameMap keys.

	[ index == 0 ] whileFalse: [ 
		nextOccurrence := StringSearcher findNextOccurrenceOf: names in: result startingAt: index.
		nextOccurrence
			ifNil: [ index := 0 ]
			ifNotNil: [ 
				| matchedValue newValue |
				index := nextOccurrence startIndex.
				matchedValue := nextOccurrence matchedValue.
				"This is for a dumb edge case - a value like 'sevenine' is supposed to result in '79'
				 not '7ine'. The workaround maintains the first and last character in order to get this to
				 work properly. For example, `sevenine` will end up with `s7nn9e`."
				newValue := (matchedValue first asString) , (NameMap at: matchedValue) , (matchedValue last asString).
				result := result
					copyReplaceFrom: nextOccurrence startIndex
					to: (nextOccurrence endIndex) - 1
					with: newValue ] 
	].

	^ result
]
