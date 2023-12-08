"
Allows searching for matches within strings
"
Class {
	#name : #StringSearcher,
	#superclass : #Object,
	#category : #'AdventOfCode2023-Day01Exercise2'
}

{ #category : #accessing }
StringSearcher class >> findNextOccurrenceOf: aCollection in: aString startingAt: index [
	| occurrences result |
	occurrences :=  aCollection
		collect: [ :valueToMatch |
			| indexOfString |
			indexOfString := aString findString: valueToMatch startingAt: index.
			indexOfString == 0
				ifTrue: [ nil ]
				ifFalse: [ Match value: valueToMatch in: aString at: indexOfString ] ]
		thenReject: #isNil.
		
	result := nil.
	
	occurrences do: [ :match |
		result
			ifNil: [ result := match ]
			ifNotNil: [ 
				(match startIndex < result startIndex)
				ifTrue: [ result := match ] ] ].
		
	^ result
	
]
