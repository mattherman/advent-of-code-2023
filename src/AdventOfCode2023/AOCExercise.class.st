"
Base class for Advent of Code solutions
"
Class {
	#name : #AOCExercise,
	#superclass : #Object,
	#instVars : [
		'inputLines'
	],
	#category : #AdventOfCode2023
}

{ #category : #'class initialization' }
AOCExercise class >> withInput: anArrayOfLines [
	^ self new inputLines: (anArrayOfLines reject: [ :line | line isEmptyOrNil ])
]

{ #category : #accessing }
AOCExercise >> inputLines [
	^ inputLines
	
]

{ #category : #accessing }
AOCExercise >> inputLines: anArrayOfLines [
	inputLines := anArrayOfLines
]
