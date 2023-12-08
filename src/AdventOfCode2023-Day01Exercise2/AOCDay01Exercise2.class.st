"
Advent of Code 2023 - Day 1 Exercise 2 solution
"
Class {
	#name : #AOCDay01Exercise2,
	#superclass : #AOCExercise,
	#category : #'AdventOfCode2023-Day01Exercise2'
}

{ #category : #commands }
AOCDay01Exercise2 >> solve [
	| transformedLines digitList calibrationValues |
	transformedLines := inputLines collect: [ :line | IntegerNames replace: line ].
	digitList := transformedLines collect: [ :line | line select: #isDigit ].
	calibrationValues := digitList collect: [ :line |
		                     (line first asString , line last asString)
			                     asNumber ].
	^ calibrationValues sum
]
