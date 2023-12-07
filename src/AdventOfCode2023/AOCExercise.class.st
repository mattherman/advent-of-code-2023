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

{ #category : #accessing }
AOCExercise >> inputLines: anArrayOfLines [
	inputLines := anArrayOfLines
]
