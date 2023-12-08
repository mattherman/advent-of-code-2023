"
Advent of Code 2023 - Day 1 Exercise 1 solution
"
Class {
	#name : #AOCDay01Exercise1,
	#superclass : #AOCExercise,
	#category : #'AdventOfCode2023-Day01Exercise1'
}

{ #category : #commands }
AOCDay01Exercise1 >> solve [
	| digitList calibrationValues |
	digitList := inputLines collect: [ :line | line select: #isDigit ].
	calibrationValues := digitList collect: [ :line |
		                     (line first asString , line last asString)
			                     asNumber ].
	^ calibrationValues sum
]
