"
Object that represents a match returned from a StringSearcher
"
Class {
	#name : #Match,
	#superclass : #Object,
	#instVars : [
		'sourceString',
		'matchedValue',
		'startIndex'
	],
	#category : #'AdventOfCode2023-Exercise01Day2'
}

{ #category : #accessing }
Match >> endIndex [
	^ startIndex + matchedValue size
]

{ #category : #accessing }
Match >> matchedValue [
	^ matchedValue
]

{ #category : #accessing }
Match >> matchedValue: aString [
	matchedValue := aString
]

{ #category : #accessing }
Match >> sourceString [
	^ sourceString
]

{ #category : #accessing }
Match >> sourceString: aString [
	sourceString := aString
]

{ #category : #accessing }
Match >> startIndex [
	^ startIndex
]

{ #category : #accessing }
Match >> startIndex: anInteger [
	startIndex := anInteger
]
