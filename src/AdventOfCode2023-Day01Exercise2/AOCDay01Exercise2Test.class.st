Class {
	#name : #AOCDay01Exercise2Test,
	#superclass : #TestCase,
	#category : #'AdventOfCode2023-Day01Exercise2'
}

{ #category : #tests }
AOCDay01Exercise2Test >> testExampleInput [ 
	| exercise |
	exercise := AOCDay01Exercise2 withInput: { 
		'two1nine'.
		'eightwothree'.
		'abcone2threexyz'.
		'xtwone3four'.
		'4nineeightseven2'.
		'zoneight234'.
		'7pqrstsixteen'
	}.
	self assert: exercise solve equals: 281
]

{ #category : #tests }
AOCDay01Exercise2Test >> testPuzzleInput [
	| input exercise |
	input := (FileLocator home / 'Development/aoc/2023/inputs/01.txt') contents lines.
	exercise := AOCDay01Exercise2 withInput: input.
	self assert: exercise solve equals: 54100
]
